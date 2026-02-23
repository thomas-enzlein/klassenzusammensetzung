test_that("run_anticlustering clusters into K groups", {
  df <- get_dummy_data(100)
  df$ue_num <- ifelse(df$ue == "Gymnasium", 5, ifelse(df$ue == "Realschule", 3, 1)) # fake cleaning
  
  # ML vektor erzeugen
  ml <- rep(NA, 100)
  ml[1:2] <- 1
  df$must_link_id <- ml
  
  weights <- c(dg = 1, ds = 1, geschlecht = 2)
  res_df <- run_anticlustering(df, ml, K_groups = 20, 
                               selected_features = c("dg", "ds", "geschlecht"),
                               feature_weights = weights)
                               
  expect_true("gruppe" %in% names(res_df))
  expect_equal(length(unique(res_df$gruppe)), 20)
  
  # Teste ob die Must-Links wirklich im selben Cluster sind
  grp_1 <- res_df$gruppe[1]
  grp_2 <- res_df$gruppe[2]
  expect_equal(grp_1, grp_2)
})
