#' @importFrom dplyr mutate group_by summarise n n_distinct rename ungroup as_tibble first if_else left_join select arrange desc distinct
#' @importFrom magrittr %>%
#' @importFrom stats sd
#' @importFrom stringr str_sub
#' @import ggplot2 plotly
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
#' @return Ein Dataframe (`tibble`), bei dem jede Zeile einem zugeteilten Raum 
#' entspricht, inklusive berechneter Metriken zur Evaluation der Gleichverteilung.
calculate_room_stats <- function(df) {
  # Hilfsfunktion fuer Mittelwert +/- SD
  fmt_stats <- function(x) {
    m <- mean(x, na.rm = TRUE)
    s <- sd(x, na.rm = TRUE)
    if (is.na(s)) return(as.character(round(m, 2)))
    return(paste0(round(m, 2), " +/- ", round(s, 2)))
  }
  
  df_stats <- df %>%
    mutate(mig_numeric = as.numeric(mig == "ja")) %>%
    group_by(raum) %>%
    summarise(
      `Schuelerzahl` = n(),
      `Mittelwert de (+/-SD)` = fmt_stats(de),
      `Mittelwert dg (+/-SD)` = fmt_stats(dg),
      `Mittelwert ds (+/-SD)` = fmt_stats(ds),
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
      `Mig.-Quote` = paste0(round(mean(mig_numeric, na.rm = TRUE) * 100, 0), " %"),
      .groups = "drop"
    ) %>%
    rename(`Raum` = raum)
    
  return(df_stats)
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
