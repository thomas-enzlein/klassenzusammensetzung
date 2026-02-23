test_that("calculate_must_links returns correct structure", {
  df <- get_dummy_data(50)
  
  # Wir sorgen dafür, dass Schule_Small wirklich klein ist (zwingend)
  df$abgebende_schule <- c(rep("Klein_1", 2), rep("Gross_1", 48))
  
  weights <- c(dg = 1, ds = 1, geschlecht = 2)
  res <- calculate_must_links(df, threshold = 10.0, small_school_max_n = 5,
                              selected_features = c("dg", "ds", "geschlecht"),
                              feature_weights = weights)
  
  expect_type(res, "list")
  expect_true(all(c("df", "must_links_vector", "small_schools_leverage") %in% names(res)))
  
  # Da threshold sehr hoch ist, sollte Klein_1 einen ML erhalten
  expect_true(any(!is.na(res$must_links_vector)))
})

test_that("calculate_must_links splits if threshold is exceeded", {
  df <- get_dummy_data(50)
  df$abgebende_schule <- c(rep("Klein_1", 2), rep("Gross_1", 48))
  
  # Mit Threshold 0.0 wird JEDER Hebel triggern, also kein ML
  res <- calculate_must_links(df, threshold = 0.0, small_school_max_n = 5,
                              selected_features = c("dg", "ds", "geschlecht"),
                              feature_weights = c(dg=1, ds=1, geschlecht=2))
                              
  # Vector sollte NA oder leere Elemente sein, da kein ML gebildet
  expect_true(all(is.na(res$must_links_vector)))
})
