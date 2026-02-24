#' @importFrom dplyr mutate group_by summarise n n_distinct rename ungroup as_tibble first if_else left_join select arrange desc distinct
#' @importFrom magrittr %>%
#' @importFrom stats sd
#' @importFrom stringr str_sub
#' @importFrom ggplot2 ggplot aes geom_col scale_fill_manual scale_color_manual theme_minimal labs theme element_text
#' @importFrom plotly ggplotly layout
#' @export
#' @name calculate_room_stats
#' @title Berechnet die statistische uebersichtstabelle der Raeume
#' 
#' @description 
#' Erzeugt eine aggregierte uebersichtstabelle fuer die Shiny-App, die 
#' wichtige Kennzahlen pro Raum (Mittelwerte, Standardabweichungen, 
#' Geschlechterverhaeltnis, Schuluebergaenge, Migrationsquote) uebersichtlich darstellt.
#' 
#' @param df Ein Dataframe mit den finalen Schuelerdaten (inkl. Zuordnung in `raum`).
#'
#' @return Eine Liste mit zwei Elementen: 
#' `room_stats` (ein Dataframe mit berechneten Raumspezifischen Metriken und Abweichungen) 
#' und `global_stats` (Basis-Statistiken ueber alle Kinder).
calculate_room_stats <- function(df) {
  # Hilfsfunktion fuer Mittelwert +/- SD
  fmt_stats <- function(m, s) {
    if (is.na(s)) return(as.character(round(m, 2)))
    return(paste0(round(m, 2), " +/- ", round(s, 2)))
  }
  
  # Globale Werte (als Basis fuer Visualisierungen und Deviance-Berechnungen)
  df_global <- df %>%
    mutate(mig_numeric = as.numeric(mig == "ja")) %>%
    summarise(
      mean_de = mean(de, na.rm = TRUE),
      sd_de = sd(de, na.rm = TRUE),
      mean_dg = mean(dg, na.rm = TRUE),
      sd_dg = sd(dg, na.rm = TRUE),
      mean_ds = mean(ds, na.rm = TRUE),
      sd_ds = sd(ds, na.rm = TRUE),
      mig_quote = mean(mig_numeric, na.rm = TRUE),
      prop_m = mean(geschlecht == "m", na.rm = TRUE)
    )
  
  df_stats <- df %>%
    mutate(mig_numeric = as.numeric(mig == "ja")) %>%
    group_by(raum) %>%
    summarise(
      `Schuelerzahl` = n(),
      mean_de = mean(de, na.rm = TRUE),
      sd_de = sd(de, na.rm = TRUE),
      mean_dg = mean(dg, na.rm = TRUE),
      sd_dg = sd(dg, na.rm = TRUE),
      mean_ds = mean(ds, na.rm = TRUE),
      sd_ds = sd(ds, na.rm = TRUE),
      mig_room = mean(mig_numeric, na.rm = TRUE),
      prop_m_room = mean(geschlecht == "m", na.rm = TRUE),
      `Anzahl Grundschulen` = n_distinct(abgebende_schule),
      `Groesste Grundschule` = max(table(abgebende_schule)),
      `Verhaeltnis (J:M)` = paste0(
        round(sum(geschlecht == "m") / max(sum(geschlecht == "w"), 0.01), 1),
        " (", sum(geschlecht == "m"), "m:", sum(geschlecht == "w"), "w)"
      ),
      `Gy (n)` = sum(round(ue) == 5, na.rm = TRUE),
      `Gy/RS (n)` = sum(round(ue) == 4, na.rm = TRUE),
      `RS (n)` = sum(round(ue) == 3, na.rm = TRUE),
      `RS/HS (n)` = sum(round(ue) == 2, na.rm = TRUE),
      `HS (n)` = sum(round(ue) == 1, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      `MW de (+/-SD)` = mapply(fmt_stats, mean_de, sd_de),
      `MW dg (+/-SD)` = mapply(fmt_stats, mean_dg, sd_dg),
      `MW ds (+/-SD)` = mapply(fmt_stats, mean_ds, sd_ds),
      `Mig.-Quote` = paste0(round(mig_room * 100, 0), " %"),
      
      # Versteckte Deviance-Spalten fuer das UI
      dev_de = abs(mean_de - df_global$mean_de),
      dev_dg = abs(mean_dg - df_global$mean_dg),
      dev_ds = abs(mean_ds - df_global$mean_ds),
      dev_mig = abs(mig_room - df_global$mig_quote),
      dev_gender = abs(prop_m_room - df_global$prop_m)
    ) %>%
    rename(`Raum` = raum) %>%
    select(
      `Raum`, `Schuelerzahl`, 
      `MW de (+/-SD)`, `MW dg (+/-SD)`, `MW ds (+/-SD)`,
      `Anzahl Grundschulen`, `Groesste Grundschule`,
      `Verhaeltnis (J:M)`, `Mig.-Quote`,
      `Gy (n)`, `Gy/RS (n)`, `RS (n)`, `RS/HS (n)`, `HS (n)`,
      dev_de, dev_dg, dev_ds, dev_mig, dev_gender
    )
    
  return(list(
    room_stats = df_stats,
    global_stats = df_global
  ))
}

#' @export
#' @name create_interactive_plot
#' @title Erstellt die interaktive Plotly Grafik basierend auf dem Datensatz
#' 
#' @description 
#' Erstellt ein interaktives Balkendiagramm, das die Verteilung der Kinder 
#' aus den verschiedenen Grundschulen auf die Zielraeume visualisiert. 
#' Isolierte Kinder werden spezifisch markiert (z.B. farblich hervorgehoben).
#' Tooltips zeigen beim Hovern Noten und Geschlecht der einzelnen Kinder an.
#' 
#' @param df Ein Dataframe mit den finalen Schuelerdaten (inkl. Zuordnung in `raum`).
#' @param small_schools_leverage Ein Dataframe (Output von `calculate_must_links`), 
#' der Informationen darueber enthaelt, welche Schulen Must-Links hatten.
#'
#' @return Eine Liste mit drei Elementen:
#' \itemize{
#'   \item \code{plot_static}: Das zugrundeliegende `ggplot2` Objekt.
#'   \item \code{plot_interactive}: Das interaktive `plotly` Objekt.
#'   \item \code{n_isolated}: Die Anzahl der verbliebenen isolierten Kinder.
#' }
create_interactive_plot <- function(df, small_schools_leverage) {
  df_plot <- df %>%
    group_by(raum, abgebende_schule) %>%
    mutate(n_in_raum = n()) %>%
    ungroup() %>%
    mutate(
      is_komplett_vereint = (n == n_in_raum),
      has_isolated = (n > 1 & n_in_raum == 1),
      has_klumpen = (n_in_raum > 3)
    )
    
  school_flags <- df_plot %>%
    group_by(abgebende_schule) %>%
    summarise(
      all_vereint = all(is_komplett_vereint),
      any_isolated = any(has_isolated),
      any_klumpen = any(has_klumpen),
      n_total = first(n) # Gesamtgroesse der Schule
    ) %>%
    mutate(
      legend_name = as.character(abgebende_schule),
      legend_name = if_else(all_vereint & abgebende_schule %in% small_schools_leverage$abgebende_schule, paste0(legend_name, " (*)"), legend_name),
      legend_name = if_else(any_isolated, paste0(legend_name, " (+)"), legend_name),
      legend_name = if_else(any_klumpen, paste0(legend_name, " (\u00A7)"), legend_name)
    )
    
  df_plot <- df_plot %>%
    left_join(school_flags %>% select(abgebende_schule, legend_name), by = "abgebende_schule") %>%
    mutate(
      abgebende_schule_plot = if_else(n == 1, "Einzelmeldungen (n=1)", legend_name),
      is_isolated = if_else(n > 1 & n_in_raum == 1, "Isoliert", "Okay")
    )
    
  df_plot_agg <- df_plot %>%
    group_by(raum, abgebende_schule_plot, is_isolated) %>%
    summarise(
      n_in_bars = n(),
      hover_details = paste(
        paste0(vorname, " ", str_sub(name, 1, 1), ". (", geschlecht, "): dg ", round(dg, 1), ", ds ", round(ds, 1)), 
        collapse = "<br>"
      ),
      .groups = "drop"
    ) %>%
    mutate(
      hover_text = paste0(
        "<b>", abgebende_schule_plot, "</b><br>",
        "Raum: ", raum, "<br>",
        "Kinder in diesem Raum: ", n_in_bars, "<br>",
        "Status: ", is_isolated, "<br>",
        "-----------------------<br>",
        hover_details
      )
    )
    
  school_sizes <- df_plot %>%
    distinct(abgebende_schule_plot, n) %>%
    arrange(desc(n), abgebende_schule_plot)

  unique_schools <- school_sizes$abgebende_schule_plot
  unique_schools <- c(unique_schools[unique_schools != "Einzelmeldungen (n=1)"], "Einzelmeldungen (n=1)")
  unique_schools <- unique(unique_schools)

  df_plot_agg$abgebende_schule_plot <- factor(df_plot_agg$abgebende_schule_plot, levels = unique_schools)

  colors_needed <- length(unique_schools) - 1
  school_colors <- scales::viridis_pal(option = "plasma", direction = -1, begin = 0.1, end = 0.9)(colors_needed)
  names(school_colors) <- unique_schools[unique_schools != "Einzelmeldungen (n=1)"]
  school_colors["Einzelmeldungen (n=1)"] <- "grey60"
  
  p_raeume <- df_plot_agg %>%
    ggplot(aes(x = factor(raum), y = n_in_bars, fill = abgebende_schule_plot, color = is_isolated, text = hover_text)) + 
    geom_col(linewidth=0.8) +
    scale_fill_manual(values = school_colors) +
    scale_color_manual(values = c("Isoliert" = "blue", "Okay" = "black"), 
                       name = "Status im Raum") +
    theme_minimal(base_size = 14) +
    labs(x = "Raum", y = "Anzahl Kinder", fill = "Grundschule") +
    theme(legend.position="right", legend.text=element_text(size=9))
    
  p_interactive <- ggplotly(p_raeume, tooltip = "text") %>%
    layout(
      title = list(text = "Raum-Verteilung<br><sup>Hover ueber Balken fuer Noten/Geschlecht der Kinder</sup>"),
      margin = list(t = 100, b = 40, r = 150) # Extra Platz oben (title) und rechts (legend)
    )
    
  return(list(
    plot_static = p_raeume,
    plot_interactive = p_interactive,
    n_isolated = sum(df_plot$is_isolated == "Isoliert", na.rm = TRUE)
  ))
}
