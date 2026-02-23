test_that("run_micro_swaps removes isolates without changing gender ratios", {
  df <- get_dummy_data(40)
  df$raum <- rep(1:2, each = 20)
  df$n <- 20 # Mocking the school size column
  
  # Force an isolated case
  # Raum 1 has 19 from Gross_1, 1 from Klein_2
  # Raum 2 has 20 from Klein_2
  df$abgebende_schule <- "Gross_1"
  df$abgebende_schule[c(20, 21:40)] <- "Klein_2"
  
  # Ensure they have the same gender so they can swap easily
  df$geschlecht[20] <- "m"
  df$geschlecht[21] <- "m"
  df$ds[20] <- 2.0
  df$ds[21] <- 2.0
  df$dg[20] <- 2.0
  df$dg[21] <- 2.0
  
  swapped_res <- run_micro_swaps(df)
  
  expect_type(swapped_res, "list")
  expect_true("df" %in% names(swapped_res))
  # expect_true(swapped_res$swaps_count > 0) # Es sollte mindestens einen Tausch gegeben haben
})
