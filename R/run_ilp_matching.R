#' @importFrom dplyr group_by summarise n mutate ungroup filter bind_rows left_join select
#' @importFrom magrittr %>%
#' @importFrom lpSolve lp
#' @importFrom purrr map2_dbl
#' @importFrom tibble tibble
#' @export
#' @name run_ilp_matching
#' @title Global optimales Matching von abstrakten Gruppen in finale Räume
#' 
#' @description 
#' Kombiniert (Matcht) die zuvor gebildeten `K_groups` paarweise zu exakt `K_rooms` Räumen.
#' Hierzu wird ein Integer Linear Program (ILP) via `lpSolve` gelöst, um die globale
#' Strafpunkt-Summe (Penalty) aller Paare zu minimieren. Bestraft werden Ungleichgewichte 
#' in der Größe, im Geschlechterverhältnis und isolierte Schüler.
#' 
#' @param df Ein Dataframe, der die Spalte `gruppe` (aus `run_anticlustering`) enthält.
#' @param K_rooms Ein numerischer Wert. Die Anzahl der finalen Schulräume, die gebildet werden sollen.
#'
#' @return Der Eingabe-Dataframe, erweitert um eine neue numerische Spalte `raum` 
#' (von 1 bis `K_rooms`), die anzeigt, in welchen finalen Klassenraum das Kind kommt.
run_ilp_matching <- function(df, K_rooms = 10) {
  # Profil pro Gruppe (N=20)
  group_profiles <- df %>%
    group_by(gruppe) %>%
    summarise(
      n_total = n(),
      n_w = sum(geschlecht == "w", na.rm = TRUE),
      n_m = sum(geschlecht == "m", na.rm = TRUE),
      dg_mean = mean(dg, na.rm = TRUE),
      ds_mean = mean(ds, na.rm = TRUE),
      schools = list(unique(abgebende_schule)),
      n_einzel = sum(is_einzelmeldung)
    ) %>%
    ungroup()
  
  groups <- unique(df$gruppe)
  pairs <- expand.grid(g1 = groups, g2 = groups) %>%
    filter(g1 < g2) # Nur eindeutige Paare
    
  # Bewertungsfunktion für ein Raumpaar
  evaluate_pair <- function(g1_id, g2_id, profile_data) {
    p1 <- profile_data[profile_data$gruppe == g1_id, ]
    p2 <- profile_data[profile_data$gruppe == g2_id, ]
    
    score <- 1000 
    
    n_total <- p1$n_total + p2$n_total
    ideal_size <- nrow(df) / K_rooms
    size_penalty <- abs(n_total - ideal_size) * 50
    score <- score - size_penalty
    
    n_w_total <- p1$n_w + p2$n_w
    n_m_total <- p1$n_m + p2$n_m
    gender_diff <- abs(n_w_total - n_m_total)
    gender_penalty <- gender_diff * 40
    score <- score - gender_penalty
    
    # Isolation sanktionieren (Schulen, die nur mit 1 Kind in DIESEM Raum-Paar wären)
    combined_kids <- df %>% filter(gruppe %in% c(g1_id, g2_id))
    isolated_kids <- combined_kids %>% 
      filter(!is_einzelmeldung) %>%
      group_by(abgebende_schule) %>%
      summarise(count = n()) %>%
      filter(count == 1) %>%
      nrow()
      
    isolation_penalty <- isolated_kids * 250
    score <- score - isolation_penalty
    
    return(score)
  }
  
  pairs$score <- purrr::map2_dbl(pairs$g1, pairs$g2, evaluate_pair, profile_data = group_profiles)
  
  num_pairs <- nrow(pairs)
  A <- matrix(0, nrow = nrow(group_profiles), ncol = num_pairs)
  
  for(p in 1:num_pairs) {
    g1 <- pairs$g1[p]
    g2 <- pairs$g2[p]
    A[g1, p] <- 1
    A[g2, p] <- 1
  }
  
  b <- rep(1, nrow(group_profiles))
  dir <- rep("=", nrow(group_profiles))
  
  # ILP lösen
  ilp_result <- lp("max", objective.in = pairs$score, 
                   const.mat = A, const.dir = dir, const.rhs = b, 
                   all.bin = TRUE)
                   
  if(ilp_result$status != 0) {
    stop("ILP Optimizer konnte keine Raumpaarung finden.")
  }
  
  selected_pairs_idx <- which(ilp_result$solution > 0.5)
  best_pairs <- pairs[selected_pairs_idx, ]
  
  rooms <- list()
  for(i in 1:nrow(best_pairs)) {
    rooms[[i]] <- tibble(raum = i, gruppe = c(best_pairs$g1[i], best_pairs$g2[i]), score = best_pairs$score[i])
  }
  
  room_assignment <- bind_rows(rooms)
  
  df <- df %>% left_join(room_assignment %>% select(gruppe, raum), by = "gruppe")
  return(df)
}
