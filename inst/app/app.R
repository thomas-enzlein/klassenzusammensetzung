library(shiny)
library(bslib)
library(DT)
library(plotly)
library(readxl)
library(dplyr)
library(anticlust)
library(lpSolve)
library(shinycssloaders)
library(shinyhelper)

# Source alle ausgelagerten Business-Logiken aus dem R-Verzeichnis
invisible(lapply(list.files("R", pattern = "\\.R$", full.names = TRUE), source))

ui <- page_sidebar(
  title = "Klassenzusammensetzung Dashboard",
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#2c3e50"),
  
  sidebar = sidebar(
    title = "Konfiguration",
    width = 350,
    
    fileInput("upload_file", "Excel-Datei hochladen (.xls, .xlsx)", 
              accept = c(".xls", ".xlsx"), 
              buttonLabel = "Auswählen", 
              placeholder = "Keine Datei ausgewählt"),
              
    helper(numericInput("num_rooms", "Anzahl Ziel-Räume:", value = 10, min = 1, max = 20, step = 1),
           type = "markdown", content = "num_rooms"),
           
    helper(numericInput("leverage_thresh", "Must-Link Leverage-Schwelle:", value = 3.5, min = 1.0, max = 10.0, step = 0.1),
           type = "markdown", content = "leverage_thresh"),
           
    helper(numericInput("small_school_n", "Max. Größe kleine Schule:", value = 3, min = 1, max = 10, step = 1),
           type = "markdown", content = "small_school_n"),
           
    helper(checkboxInput("do_micro_swaps", "Post-Hoc Micro-Swaps durchführen (beseitigt Isolierte)", value = TRUE),
           type = "markdown", content = "do_micro_swaps"),
    
    hr(),
    h6("Clustering-Details"),
    helper(selectizeInput("cluster_vars", "Zu optimierende Variablen:",
                   choices = c("Ø-einfach (de)" = "de", 
                               "Ø-gewichtet (dg)" = "dg", 
                               "Ø-Sprache (ds)" = "ds", 
                               "Geschlecht" = "geschlecht", 
                               "Migration (mig)" = "mig", 
                               "Übergangsempf. (ue)" = "ue", 
                               "Grundschule" = "abgebende_schule"),
                   selected = c("dg", "ds", "geschlecht"),
                   multiple = TRUE),
           type = "markdown", content = "cluster_vars"),
    
    uiOutput("weight_controls"),
    
    hr(),
    actionButton("run_btn", "Zuteilung starten", class = "btn-primary w-100", icon = icon("play")),
    br(),
    downloadButton("download_res", "Ergebnis als CSV", class = "btn-success w-100")
  ),
  
  navset_card_underline(
    nav_panel("Erklärung", 
      includeMarkdown("explanation.md")
    ),
    nav_panel("Interaktiver Plot", 
      plotlyOutput("raum_plot", height = "800px") %>% 
        withSpinner(type = 8, color = "#2c3e50")
    ),
    nav_panel("Räume (Diagnose)", 
      h5("Statistischer Überblick pro Raum"),
      DTOutput("stats_table"),
      hr(),
      h5("Schulen mit Must-Links"),
      DTOutput("must_links_table")
    ),
    nav_panel("Rohdaten (Vorschau)", 
      DTOutput("raw_data_table")
    )
  )
)

server <- function(input, output, session) {
  
  observe_helpers()
  
  rv <- reactiveValues(df = NULL, final_df = NULL, stats = NULL, plot = NULL, ml_table = NULL)
  
  observeEvent(input$upload_file, {
    req(input$upload_file)
    tryCatch({
      df <- read_and_clean_data(input$upload_file$datapath)
      rv$df <- df
      showNotification("Datei erfolgreich geladen!", type = "message")
    }, error = function(e) {
      showNotification(paste("Fehler beim Laden:", e$message), type = "error")
    })
  })
  
  output$raw_data_table <- renderDT({
    req(rv$df)
    datatable(rv$df, options = list(pageLength = 10, scrollX = TRUE)) %>%
      formatRound(columns = c("dg", "ds", "de"), digits = 1)
  })
  
  output$weight_controls <- renderUI({
    req(input$cluster_vars)
    tagList(
      lapply(input$cluster_vars, function(v) {
        label <- switch(v,
                        de = "Gewicht Ø-einfach:",
                        dg = "Gewicht Ø-gewichtet:",
                        ds = "Gewicht Ø-Sprache:",
                        geschlecht = "Gewicht Geschlecht:",
                        mig = "Gewicht Migration:",
                        ue = "Gewicht Ü-Empfehlung:",
                        abgebende_schule = "Gewicht Grundschule:")
        helper(numericInput(paste0("weight_", v), label, value = ifelse(v == "geschlecht", 2, 1), min = 0.1, max = 10, step = 0.1),
               type = "markdown", content = "weight")
      })
    )
  })
  
  observeEvent(input$run_btn, {
    req(rv$df)
    
    withProgress(message = 'Berechne Zuteilung...', value = 0, {
      df <- rv$df
      
      # Sammle Gewichte
      weights <- sapply(input$cluster_vars, function(v) {
        w <- input[[paste0("weight_", v)]]
        if(is.null(w)) return(1)
        return(w)
      })
      names(weights) <- input$cluster_vars
      
      incProgress(0.1, detail = "Must-Links identifizieren")
      ml_res <- calculate_must_links(df, threshold = input$leverage_thresh, small_school_max_n = input$small_school_n,
                                     selected_features = input$cluster_vars,
                                     feature_weights = weights)
      
      # Stabilitäts-Schleife: Versuche bis zu 5 Mal ein perfektes Ergebnis (0 Isolierte) zu finden
      max_attempts <- 5
      attempt <- 1
      perfect_found <- FALSE
      
      while(attempt <= max_attempts && !perfect_found) {
        incProgress(0.1, detail = paste0("Berechnung Versuch ", attempt, "..."))
        
        current_df <- ml_res$df
        
        # 1. Anticlustering
        current_df <- run_anticlustering(current_df, ml_res$must_links_vector, 
                                         K_groups = input$num_rooms * 2,
                                         selected_features = input$cluster_vars,
                                         feature_weights = weights)
        
        # 2. ILP Matching
        current_df <- run_ilp_matching(current_df, K_rooms = input$num_rooms)
        
        # 3. Micro-Swaps
        if(input$do_micro_swaps) {
          swap_res <- run_micro_swaps(current_df)
          current_df <- swap_res$df
        }
        
        # 4. Evaluation des Versuchs
        eval_res <- create_interactive_plot(current_df, ml_res$small_schools_leverage)
        
        if(eval_res$n_isolated == 0) {
          perfect_found <- TRUE
          df <- current_df
        } else if(attempt == max_attempts) {
          # Letzter Versuch, nimm das beste was wir haben
          df <- current_df
        }
        
        attempt <- attempt + 1
      }
      
      incProgress(0.9, detail = "Erstelle Auswertungen")
      rv$final_df <- df
      rv$ml_table <- ml_res$small_schools_leverage
      rv$stats <- calculate_room_stats(df)
      eval_res <- create_interactive_plot(df, ml_res$small_schools_leverage)
      rv$plot <- eval_res$plot_interactive
      
      if(eval_res$n_isolated > 0) {
        showNotification(paste("Warnung: Auch nach", max_attempts, "Versuchen verbleiben", eval_res$n_isolated, "isolierte(s) Kind(er)."), type = "warning")
      } else {
        showNotification(paste0("Zuteilung perfekt! (Gefunden in Versuch ", attempt-1, ")"), type = "message")
      }
      
      incProgress(1, detail = "Fertig!")
    })
  })
  
  output$raum_plot <- renderPlotly({
    req(rv$plot)
    rv$plot
  })
  
  output$stats_table <- renderDT({
    req(rv$stats)
    datatable(rv$stats, options = list(pageLength = 10, dom = 't'))
  })
  
  output$must_links_table <- renderDT({
    req(rv$ml_table)
    
    formatted_ml <- rv$ml_table %>%
      rename(
        `Grundschule` = abgebende_schule,
        `Kinder (n)` = n,
        `Hebel Leistung & UE` = hebel_leistung,
        `Hebel Sozial (Geschlecht/Mig)` = hebel_sozial,
        `Gesamt-Hebel (Summe)` = max_leverage
      ) %>%
      select(
        `Grundschule`, `Kinder (n)`,
        `Hebel Leistung & UE`, `Hebel Sozial (Geschlecht/Mig)`,
        `Gesamt-Hebel (Summe)`
      )
      
    num_cols <- c("Hebel Leistung & UE", "Hebel Sozial (Geschlecht/Mig)", "Gesamt-Hebel (Summe)")
                  
    datatable(formatted_ml, options = list(pageLength = 10, scrollX = TRUE)) %>%
      formatRound(columns = num_cols, digits = 1)
  })
  
  output$download_res <- downloadHandler(
    filename = function() {
      paste("raumzuteilung_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(rv$final_df)
      export_df <- rv$final_df %>%
        arrange(raum, gruppe) %>%
        select(raum, gruppe, everything()) %>%
        mutate(across(where(is.numeric), ~round(.x, 1)))
      write.csv(export_df, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
}

shinyApp(ui, server)
