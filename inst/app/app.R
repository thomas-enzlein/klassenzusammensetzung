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
      h5("Kohorten-Übersicht (Gesamter Jahrgang)"),
      uiOutput("global_stats_ui"),
      hr(),
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
  
  rv <- reactiveValues(df = NULL, final_df = NULL, stats = NULL, global_stats = NULL, plot = NULL, ml_table = NULL)
  
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
      rv$ml_table <- calculate_school_stats(df, ml_res$all_schools_leverage, ml_res$must_link_schools)
      
      stats_list <- calculate_room_stats(df)
      rv$stats <- stats_list$room_stats
      rv$global_stats <- stats_list$global_stats
      
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
  
  output$global_stats_ui <- renderUI({
    req(rv$global_stats)
    g <- rv$global_stats
    
    total_n <- nrow(rv$final_df)
    males <- sum(rv$final_df$geschlecht == "m", na.rm = TRUE)
    females <- sum(rv$final_df$geschlecht == "w", na.rm = TRUE)
    m_prop <- round(males / max(females, 1), 1)
    
    # Berechne UE Verteilung für die globale Box
    ue_tab <- table(factor(round(rv$final_df$ue), levels = 1:5))
    ue_props <- round(prop.table(ue_tab) * 100, 1)
    ue_html <- HTML(paste0(
      "<div style='font-size:0.85em; line-height:1.2; margin-top:5px;'>",
      "Gy: ", ue_props["5"], "% | Gy/RS: ", ue_props["4"], "%<br>",
      "RS: ", ue_props["3"], "% | RS/HS: ", ue_props["2"], "%<br>",
      "HS: ", ue_props["1"], "%",
      "</div>"
    ))
    
    layout_column_wrap(
      width = 1/7,
      value_box(title = "Schüler Gesamt", value = total_n, showcase = icon("users"), theme = "primary"),
      value_box(title = "Ø de (einfach)", value = round(g$mean_de, 2), p(paste0("± ", round(g$sd_de, 2))), showcase = icon("book"), theme = "info"),
      value_box(title = "Ø dg (gewichtet)", value = round(g$mean_dg, 2), p(paste0("± ", round(g$sd_dg, 2))), showcase = icon("graduation-cap"), theme = "info"),
      value_box(title = "Ø ds (Sprache)", value = round(g$mean_ds, 2), p(paste0("± ", round(g$sd_ds, 2))), showcase = icon("language"), theme = "info"),
      value_box(title = "Ø ue (Übergang)", value = round(g$mean_ue, 2), p(ue_html), showcase = icon("chart-bar"), theme = "info"),
      value_box(title = "Jungen:Mädchen", value = paste0(m_prop, " : 1"), p(paste0(males, " M, ", females, " W")), showcase = icon("venus-mars"), theme = "secondary"),
      value_box(title = "Migrationsquote", value = paste0(round(g$mig_quote * 100, 1), "%"), showcase = icon("globe"), theme = "secondary")
    )
  })
  
  output$stats_table <- renderDT({
    req(rv$stats)
    
    # Define color mappings for deviations. 
    # Use max expected deviations to scale the background bars fairly
    max_dev_de <- max(rv$stats$dev_de, 0.5, na.rm = TRUE)
    max_dev_dg <- max(rv$stats$dev_dg, 0.5, na.rm = TRUE)
    max_dev_ds <- max(rv$stats$dev_ds, 0.5, na.rm = TRUE)
    max_dev_mig <- max(rv$stats$dev_mig, 0.1, na.rm = TRUE)
    max_dev_gen <- max(rv$stats$dev_gender, 0.1, na.rm = TRUE)
    
    dt <- datatable(rv$stats, 
                    options = list(
                      pageLength = 20, 
                      dom = 't',
                      scrollX = TRUE,
                      columnDefs = list(
                        list(visible = FALSE, targets = c("dev_de", "dev_dg", "dev_ds", "dev_mig", "dev_gender", "mean_de", "mean_dg", "mean_ds", "mean_ue")),
                        list(orderData = which(names(rv$stats) == "mean_de") - 1, targets = which(names(rv$stats) == "MW de (+/-SD)") - 1),
                        list(orderData = which(names(rv$stats) == "mean_dg") - 1, targets = which(names(rv$stats) == "MW dg (+/-SD)") - 1),
                        list(orderData = which(names(rv$stats) == "mean_ds") - 1, targets = which(names(rv$stats) == "MW ds (+/-SD)") - 1),
                        list(orderData = which(names(rv$stats) == "mean_ue") - 1, targets = which(names(rv$stats) == "UE (Bar)") - 1)
                      )
                    ),
                    escape = FALSE,
                    rownames = FALSE) %>%
      formatStyle('MW de (+/-SD)', 'dev_de',
                  background = styleColorBar(c(0, max_dev_de), '#ffc107'),
                  backgroundSize = '100% 70%', backgroundRepeat = 'no-repeat', backgroundPosition = 'center') %>%
      formatStyle('MW dg (+/-SD)', 'dev_dg',
                  background = styleColorBar(c(0, max_dev_dg), '#ffc107'),
                  backgroundSize = '100% 70%', backgroundRepeat = 'no-repeat', backgroundPosition = 'center') %>%
      formatStyle('MW ds (+/-SD)', 'dev_ds',
                  background = styleColorBar(c(0, max_dev_ds), '#ffc107'),
                  backgroundSize = '100% 70%', backgroundRepeat = 'no-repeat', backgroundPosition = 'center') %>%
      formatStyle('Mig.-Quote', 'dev_mig',
                  background = styleColorBar(c(0, max_dev_mig), '#17a2b8'),
                  backgroundSize = '100% 70%', backgroundRepeat = 'no-repeat', backgroundPosition = 'center') %>%
      formatStyle('Verhaeltnis (J:M)', 'dev_gender',
                  background = styleColorBar(c(0, max_dev_gen), '#17a2b8'),
                  backgroundSize = '100% 70%', backgroundRepeat = 'no-repeat', backgroundPosition = 'center')
                  
    return(dt)
  })
  
  output$must_links_table <- renderDT({
    req(rv$ml_table)
    
    # We will use the same kind of deviations logic as the room table.
    max_dev_de <- max(rv$ml_table$dev_de, 0.5, na.rm = TRUE)
    max_dev_dg <- max(rv$ml_table$dev_dg, 0.5, na.rm = TRUE)
    max_dev_ds <- max(rv$ml_table$dev_ds, 0.5, na.rm = TRUE)
    max_dev_mig <- max(rv$ml_table$dev_mig, 0.1, na.rm = TRUE)
    max_dev_gen <- max(rv$ml_table$dev_gender, 0.1, na.rm = TRUE)
    
    dt <- datatable(rv$ml_table, 
                    filter = 'top',
                    options = list(
                      pageLength = 10, 
                      dom = 'ftip',
                      scrollX = TRUE,
                      columnDefs = list(
                        list(visible = FALSE, targets = c("dev_de", "dev_dg", "dev_ds", "dev_mig", "dev_gender", "max_leverage", "mean_de", "mean_dg", "mean_ds", "mean_ue")),
                        list(orderData = which(names(rv$ml_table) == "mean_de") - 1, targets = which(names(rv$ml_table) == "MW de (+/-SD)") - 1),
                        list(orderData = which(names(rv$ml_table) == "mean_dg") - 1, targets = which(names(rv$ml_table) == "MW dg (+/-SD)") - 1),
                        list(orderData = which(names(rv$ml_table) == "mean_ds") - 1, targets = which(names(rv$ml_table) == "MW ds (+/-SD)") - 1),
                        list(orderData = which(names(rv$ml_table) == "mean_ue") - 1, targets = which(names(rv$ml_table) == "UE (Bar)") - 1),
                        list(orderData = which(names(rv$ml_table) == "max_leverage") - 1, targets = which(names(rv$ml_table) == "Max Leverage") - 1)
                      )
                    ),
                    escape = FALSE,
                    rownames = FALSE) %>%
      formatStyle('MW de (+/-SD)', 'dev_de',
                  background = styleColorBar(c(0, max_dev_de), '#ffc107'),
                  backgroundSize = '100% 70%', backgroundRepeat = 'no-repeat', backgroundPosition = 'center') %>%
      formatStyle('MW dg (+/-SD)', 'dev_dg',
                  background = styleColorBar(c(0, max_dev_dg), '#ffc107'),
                  backgroundSize = '100% 70%', backgroundRepeat = 'no-repeat', backgroundPosition = 'center') %>%
      formatStyle('MW ds (+/-SD)', 'dev_ds',
                  background = styleColorBar(c(0, max_dev_ds), '#ffc107'),
                  backgroundSize = '100% 70%', backgroundRepeat = 'no-repeat', backgroundPosition = 'center') %>%
      formatStyle('Mig.-Quote', 'dev_mig',
                  background = styleColorBar(c(0, max_dev_mig), '#17a2b8'),
                  backgroundSize = '100% 70%', backgroundRepeat = 'no-repeat', backgroundPosition = 'center') %>%
      formatStyle('Verhaeltnis (J:M)', 'dev_gender',
                  background = styleColorBar(c(0, max_dev_gen), '#17a2b8'),
                  backgroundSize = '100% 70%', backgroundRepeat = 'no-repeat', backgroundPosition = 'center') %>%
      formatStyle('Must-Link (*)', 
                  backgroundColor = styleEqual(c("X", ""), c("#28a745", "white")),
                  color = styleEqual(c("X", ""), c("white", "black")))
                  
    return(dt)
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
