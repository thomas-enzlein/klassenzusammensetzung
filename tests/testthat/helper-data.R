# Helper function to generate mock student data for testing
get_dummy_data <- function(n_students = 100) {
  set.seed(42) # For reproducibility
  
  data.frame(
    kid_id = 1:n_students,
    vorname = paste0("Kind", 1:n_students),
    name = paste0("Nachname", 1:n_students),
    geschlecht = factor(sample(c("m", "w"), n_students, replace = TRUE), levels = c("m", "w")),
    de = round(rnorm(n_students, 2.5, 0.8), 1),
    dg = round(rnorm(n_students, 2.5, 0.8), 1),
    ds = round(rnorm(n_students, 2.5, 0.8), 1),
    ue = sample(1:5, n_students, replace = TRUE),
    mig = sample(c(0, 1), n_students, replace = TRUE, prob = c(0.7, 0.3)),
    abgebende_schule = sample(
      c("Schule_A", "Schule_B", "Schule_C", "Schule_Small"), 
      n_students, 
      replace = TRUE, 
      prob = c(0.4, 0.3, 0.28, 0.02)
    ),
    stringsAsFactors = FALSE
  )
}
