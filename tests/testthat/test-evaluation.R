test_that("calculate_room_stats returns correct evaluation dataframe structure", {
  df <- get_dummy_data(50)
  df$raum <- rep(1:5, each = 10) # Mock 5 rooms
  
  stats_list <- calculate_room_stats(df)
  stats_df <- stats_list$room_stats
  global_df <- stats_list$global_stats
  
  expect_type(stats_list, "list")
  expect_s3_class(stats_df, "data.frame")
  expect_s3_class(global_df, "data.frame")
  expect_true(nrow(stats_df) == 5)
  expect_true(all(c("Raum", "Schuelerzahl", "MW de (+/-SD)", "MW dg (+/-SD)", "MW ds (+/-SD)", 
                    "Anzahl Grundschulen", "Groesste Grundschule", "Verhaeltnis (J:M)", 
                    "Gy (n)", "Gy/RS (n)", "RS (n)", "RS/HS (n)", "HS (n)", "Mig.-Quote",
                    "dev_de", "dev_dg", "dev_ds", "dev_mig", "dev_gender") %in% names(stats_df)))
})

test_that("create_interactive_plot produces interactive Plotly object", {
  df <- get_dummy_data(50)
  df$raum <- rep(1:5, each = 10)
  df <- merge(df, as.data.frame(table(df$abgebende_schule)), by.x="abgebende_schule", by.y="Var1")
  colnames(df)[ncol(df)] <- "n"
  
  # Create fake Must-Links leverage dataframe
  small_schools_leverage <- data.frame(
    abgebende_schule = c("Schule_Small"),
    n = c(2),
    max_leverage = c(3.0)
  )
  
  res <- create_interactive_plot(df, small_schools_leverage)
  
  expect_type(res, "list")
  expect_true(all(c("plot_static", "plot_interactive", "n_isolated") %in% names(res)))
  
  # plot_interactive should inherit from "plotly"
  expect_s3_class(res$plot_interactive, "plotly")
})
