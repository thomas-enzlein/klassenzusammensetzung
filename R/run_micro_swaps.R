#' @importFrom dplyr group_by mutate n ungroup row_number filter pull slice arrange
#' @importFrom magrittr %>%
#' @export
#' @name run_micro_swaps
#' @title Führt stille Micro-Swaps durch, um übriggebliebene Isolierte in Paare zu überführen.
#' 
#' @description 
#' Identifiziert Schüler, die in ihrem zugeteilten Raum isoliert sind (d.h. sie sind 
#' die einzigen Vertreter ihrer Grundschule in diesem Raum, obwohl es insgesamt 
#' an der Schule mehr als ein Kind gibt). Der Algorithmus sucht in anderen Räumen nach 
#' geeigneten Tauschpartnern gleichen Geschlechts und ähnlicher Leistung, 
#' um diese Isolation ohne signifikanten Qualitätsverlust der Räume aufzulösen.
#' 
#' @param df Ein Dataframe mit den Schülerdaten, der bereits die Zuteilung `raum` 
#' enthält (z.B. vom Output von `run_ilp_matching`).
#'
#' @return Eine Liste mit zwei Elementen: 
#' \itemize{
#'   \item \code{df}: Der aktualisierte Dataframe mit den neuen Raumzuteilungen.
#'   \item \code{swaps_count}: Die Anzahl der erfolgreich durchgeführten Tauschvorgänge.
#' }
run_micro_swaps <- function(df) {
  
  utils::globalVariables(c("raum", "abgebende_schule", "kid_id", "geschlecht", 
                           "dg", "ds", "diff", "n_in_raum"))
  
  df <- df %>% 
    group_by(raum, abgebende_schule) %>% 
    mutate(n_in_raum = n()) %>% 
    ungroup() %>%
    mutate(kid_id = row_number())
  
  swaps_count <- 0
  isoliert <- df %>% filter(n > 1 & n_in_raum == 1)
  
  while(nrow(isoliert) > 0) {
    swapped_any <- FALSE
    for(i in 1:nrow(isoliert)) {
      kid_iso <- isoliert[i, ]
      
      # Generiere Pools für sicheren Tausch
      target_rooms <- df %>% filter(abgebende_schule == kid_iso$abgebende_schule & raum != kid_iso$raum) %>% pull(raum) %>% unique()
      schulen_in_orig_raum <- df %>% filter(raum == kid_iso$raum & kid_id != kid_iso$kid_id) %>% pull(abgebende_schule) %>% unique()
      
      swapped <- FALSE
      
      tolerances <- c(0.6, 1.5, 2.5)
      
      for(tol in tolerances) {
        if(swapped) break
        
        for(tr in target_rooms) {
          candidates <- df %>%
            filter(raum == tr, geschlecht == kid_iso$geschlecht, abgebende_schule != kid_iso$abgebende_schule,
                   (n_in_raum > 2 | n == 1), (n == 1 | abgebende_schule %in% schulen_in_orig_raum),
                   abs(dg - kid_iso$dg) <= tol, abs(ds - kid_iso$ds) <= tol)
                   
          if(nrow(candidates) > 0) {
             partner <- candidates %>% mutate(diff = abs(dg - kid_iso$dg) + abs(ds - kid_iso$ds)) %>% arrange(diff) %>% slice(1)
             df$raum[df$kid_id == kid_iso$kid_id] <- tr
             df$raum[df$kid_id == partner$kid_id] <- kid_iso$raum
             swapped <- TRUE
             break
          }
        }
      }
      
      if(swapped) {
        swapped_any <- TRUE
        swaps_count <- swaps_count + 1
        df <- df %>% group_by(raum, abgebende_schule) %>% mutate(n_in_raum = n()) %>% ungroup()
        isoliert <- df %>% filter(n > 1 & n_in_raum == 1)
        break
      }
    }
    if(!swapped_any) break
  }
  
  # Rückgabe von df und Metadaten (für die UI)
  message(sprintf("Micro-Swaps vollzogen: %d", swaps_count))
  return(list(df = df, swaps_count = swaps_count))
}
