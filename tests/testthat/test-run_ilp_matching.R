test_that("run_ilp_matching pairs groups into expected room numbers", {
  df <- get_dummy_data(100)
  
  # Fake data to bypass prior steps
  df$gruppe <- rep(1:20, length.out = 100)
  df$geschlecht_num <- ifelse(df$geschlecht == "m", 1, 0)
  df$is_einzelmeldung <- FALSE
  
  res_df <- run_ilp_matching(df, K_rooms = 10)
  
  expect_true("raum" %in% names(res_df))
  expect_equal(length(unique(res_df$raum)), 10)
})
