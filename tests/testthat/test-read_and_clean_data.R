test_that("read_and_clean_data parses excel file correctly", {
  # We test this only if the test data exists
  test_file <- test_path("..", "..", "inst", "extdata", "test_data.xls")
  skip_if_not(file.exists(test_file), message = "Test data file not found at inst/extdata/test_data.xls")
  
  df <- read_and_clean_data(test_file)
  
  expect_s3_class(df, "data.frame")
  expect_true(nrow(df) > 0)
  expect_true(all(c("geschlecht", "de", "dg", "ds", "ue", "abgebende_schule", "mig") %in% names(df)))
  
  # Data type checks
  expect_true(is.numeric(df$de))
  expect_true(is.numeric(df$ue)) # mapped to 1-5 inside
})
