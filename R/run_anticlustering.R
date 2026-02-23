#' @importFrom dplyr count filter as_tibble bind_cols
#' @importFrom magrittr %>%
#' @importFrom anticlust anticlustering categories_to_binary
#' @export
#' @name run_anticlustering
#' @title Führt das K-Means Anticlustering aus, um n-Gruppen zu erzeugen
#' 
#' @description 
#' Verwendet den K-Means Anticlustering-Algorithmus aus dem `anticlust`-Paket, 
#' um die Schüler in K gleich große und maximal heterogene Basis-Gruppen aufzuteilen.
#' Dabei können bestimmte Kategorien (z.B. Geschlecht) stärker gewichtet werden, 
#' und kleine Schulen (Must-Links) werden in dieselben Gruppen gezwungen.
#' 
#' @param df Ein Dataframe mit den Schülerdaten, inklusive der Spalte `must_link_id`
#' (generiert von `calculate_must_links`).
#' @param must_links Ein Logikwert (`TRUE`/`FALSE`). Gibt an, ob Must-Links 
#' für kleine Schulen mit hohem Hebel erzwungen werden sollen.
#' @param K_groups Ein numerischer Wert. Die Anzahl der zu bildenden initialen Gruppen 
#' (sollte ein Vielfaches der späteren Raumanzahl sein, z.B. 20 Gruppen für 10 Räume).
#' @param selected_features Ein Vektor von Strings. Die Spaltennamen der Variablen,
#' die für das Clustering verwendet werden sollen (z.B. Deutschnoten, Geschlecht).
#' @param feature_weights Ein benannter numerischer Vektor. Die Gewichtungen 
#' für die `selected_features` bei der Berechnung der Euklidischen Distanz.
#'
#' @return Der Eingabe-Dataframe, erweitert um eine neue numerische Spalte `gruppe`,
#' die anzeigt, welcher der `K_groups`-Basisgruppen das jeweilige Kind zugeordnet wurde.
run_anticlustering <- function(df, must_links, K_groups = 20, 
                               selected_features = c("dg", "ds", "geschlecht"),
                               feature_weights = c(dg = 1, ds = 1, geschlecht = 2)) {
  
  utils::globalVariables(c("abgebende_schule", "must_link_id", "n"))
  
  # Baue Features dynamisch auf
  feature_list <- list()
  
  for (f in selected_features) {
    weight <- if(f %in% names(feature_weights)) feature_weights[[f]] else 1
    
    if (f %in% c("de", "dg", "ds", "ue")) {
      # Numerische / Ordinale Variablen: Skalieren & Gewichten
      vals <- df[[f]]
      if (all(is.na(vals))) next
      
      scaled_vals <- scale(vals)[, 1] * weight
      feature_list[[f]] <- scaled_vals
      
    } else if (f %in% c("geschlecht", "mig", "abgebende_schule")) {
      # Kategorische Variablen: In Binärspalten umwandeln & Gewichten
      binary_cols <- categories_to_binary(df[[f]])
      binary_cols <- as.matrix(binary_cols) * weight
      
      # Als Tibble/Dataframe einfügen
      colnames(binary_cols) <- paste0(f, "_", colnames(binary_cols))
      feature_list[[f]] <- as_tibble(binary_cols)
    }
  }
  
  df_features <- bind_cols(feature_list)
  
  # "Echte" Einzelanmeldungen (die auch keinen Must-Link haben)
  school_counts_raw <- df %>% filter(is.na(must_link_id)) %>% count(abgebende_schule)
  einzel_schulen <- school_counts_raw$abgebende_schule[school_counts_raw$n == 1]
  
  df$is_einzelmeldung <- df$abgebende_schule %in% einzel_schulen & is.na(df$must_link_id)
  
  # Behandle Einzelanmeldungen als Must-Links (dürfen beliebig verteilt werden, 
  # aber jede bekommt eine EIGENE ID)
  einzel_idx <- which(df$is_einzelmeldung)
  
  if(length(einzel_idx) > 0) {
    max_ml_id <- max(must_links, na.rm = TRUE)
    if(is.infinite(max_ml_id)) max_ml_id <- 0
    
    for(i in seq_along(einzel_idx)) {
      must_links[einzel_idx[i]] <- max_ml_id + i
    }
  }
  
  # Die restlichen Kinder (ohne ML) bekommen jede eine eigene ID
  na_ml_idx <- which(is.na(must_links))
  if(length(na_ml_idx) > 0) {
    max_ml_id <- max(must_links, na.rm = TRUE)
    must_links[na_ml_idx] <- (max_ml_id + 1):(max_ml_id + length(na_ml_idx))
  }
  
  # K-Means Anticlustering (Gleichmäßige Leistungs- und Geschlechterverteilung auf 20 Gruppen)
  # must_link erzwingt, dass Kinder mit gleicher Must-Link-ID im selben Cluster landen.
  assigned_groups <- anticlustering(
    df_features,
    K = K_groups,
    must_link = must_links,
    repetitions = 50
  )
  
  df$gruppe <- assigned_groups
  
  return(df)
}
