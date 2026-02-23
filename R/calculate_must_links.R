#' @importFrom dplyr count left_join filter group_by summarise ungroup mutate select
#' @importFrom rlang .data
#' @importFrom stats sd
#' @export
#' @name calculate_must_links
#' @title Berechnet Leverage und identifiziert die kleinen Schulen für Must-Links
#' 
#' @description 
#' Berechnet für jede Grundschule den Einfluss ("Leverage") auf die gesamte 
#' Klassenzusammensetzung. Kleine Schulen, deren Schüler eine hohe Leistungs-
#' oder Sozialvarianz (Hebel) aufweisen, werden für Must-Link Constraints
#' im Anticlustering markiert, um eine Auftrennung in zu viele verschiedene 
#' Zielklassen zu verhindern.
#' 
#' @param df Ein Dataframe mit den bereinigten Schülerdaten (Output von `read_and_clean_data`).
#' @param threshold Ein numerischer Schwellenwert für die Hebelwirkung (Standard: 3.5).
#' @param small_school_max_n Die maximale Anzahl an Schülern, bei der eine Schule 
#' noch als "klein" gilt und potenziell zusammengehalten werden soll (Standard: 3).
#'
#' @return Ein Dataframe auf Schulebene (abgebende_schule) mit folgenden Spalten:
#' \describe{
#'   \item{abgebende_schule}{Name der Schule.}
#'   \item{n}{Anzahl der angemeldeten Kinder dieser Schule.}
#'   \item{hebel_leistung}{Standardabweichung der kombinierten Deutschnoten (dg+ds).}
#'   \item{hebel_sozial}{Variationskoeffizient des Migrationshintergrunds.}
#'   \item{leverage}{Gesamte Hebelwirkung (Summe der beiden Hebel).}
#'   \item{must_link}{Ein logischer Wert (`TRUE`/`FALSE`), der angibt, ob 
#'   diese Schule aufgrund geringer Größe (`n <= small_school_max_n`) und 
#'   vergleichsweise hohem Leverage (`leverage > threshold`) zusammenbleiben soll.}
#' }
calculate_must_links <- function(df, threshold = 3.5, small_school_max_n = 3,
                                 selected_features = c("dg", "ds", "geschlecht"),
                                 feature_weights = c(dg = 1, ds = 1, geschlecht = 2)) {
  
  # Linter-Fehler unterdrücken
  utils::globalVariables(c("abgebende_schule", "n", "m", "leverage", "hebel_leistung", 
                           "hebel_sozial", "p", "max_leverage"))
                           
  
  school_counts <- df %>% count(abgebende_schule)
  df <- df %>% left_join(school_counts, by = "abgebende_schule", suffix = c("", "_total"))
  
  # Hilfs-Dataframe für Hebel/Leverage-Berechnung
  # Wir brauchen für jede Variable den globalen Durchschnitt
  
  small_schools_leverage <- df %>%
    filter(n <= small_school_max_n) %>%
    group_by(abgebende_schule) %>%
    summarise(n = n()) %>%
    ungroup()
    
  # Initialisiere Hebel-Spalten
  small_schools_leverage$hebel_leistung <- 0
  small_schools_leverage$hebel_sozial <- 0
  
  # Berechne Hebel pro Variable
  for (f in selected_features) {
    weight <- if(f %in% names(feature_weights)) feature_weights[[f]] else 1
    
    if (f %in% c("de", "dg", "ds", "ue")) {
      # Leistungs-Faktor: Standardisierte Abweichung
      school_means <- df %>%
        group_by(abgebende_schule) %>%
        summarise(m = mean(.data[[f]], na.rm = TRUE))
      
      global_m <- mean(df[[f]], na.rm = TRUE)
      global_sd <- sd(df[[f]], na.rm = TRUE)
      
      school_means <- school_means %>%
        mutate(leverage = abs((m - global_m) / global_sd) * weight)
        
      small_schools_leverage <- small_schools_leverage %>%
        left_join(school_means %>% select(abgebende_schule, leverage), by = "abgebende_schule")
      
      small_schools_leverage$hebel_leistung <- small_schools_leverage$hebel_leistung + 
                                               ifelse(is.na(small_schools_leverage$leverage), 0, small_schools_leverage$leverage)
      small_schools_leverage$leverage <- NULL
      
    } else if (f %in% c("geschlecht", "mig")) {
      # Sozialer Faktor: Abweichung im Anteil
      val_to_check <- if(f == "geschlecht") "w" else "ja"
      
      school_props <- df %>%
        group_by(abgebende_schule) %>%
        summarise(p = mean(.data[[f]] == val_to_check, na.rm = TRUE))
        
      global_p <- mean(df[[f]] == val_to_check, na.rm = TRUE)
      
      school_props <- school_props %>%
        mutate(leverage = abs(p - global_p) * weight)
        
      small_schools_leverage <- small_schools_leverage %>%
        left_join(school_props %>% select(abgebende_schule, leverage), by = "abgebende_schule")
        
      small_schools_leverage$hebel_sozial <- small_schools_leverage$hebel_sozial + 
                                             ifelse(is.na(small_schools_leverage$leverage), 0, small_schools_leverage$leverage)
      small_schools_leverage$leverage <- NULL
    }
  }
  
  small_schools_leverage <- small_schools_leverage %>%
    mutate(max_leverage = hebel_leistung + hebel_sozial) %>%
    filter(max_leverage < threshold)
    
  must_link_schools <- small_schools_leverage$abgebende_schule
  
  # Setze Must-Link IDs
  # Alle Schüler aus diesen kleinen "Low-Leverage" Schulen bekommen dieselbe ID, 
  # damit sie beim K-Means Anticlustering immer zusammen bleiben.
  ml_counter <- 1
  df$must_link_id <- NA
  for(s in must_link_schools) {
    df$must_link_id[df$abgebende_schule == s] <- ml_counter
    ml_counter <- ml_counter + 1
  }
  
  must_links_vector <- df$must_link_id
  
  return(list(
    df = df, 
    small_schools_leverage = small_schools_leverage, 
    must_link_schools = must_link_schools,
    must_links_vector = must_links_vector
  ))
}
