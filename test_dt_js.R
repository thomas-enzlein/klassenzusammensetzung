library(DT)
library(dplyr)
source("R/06_evaluation.R")
source("tests/testthat/helper-data.R")

df <- get_dummy_data()
df$raum <- sample(1:5, nrow(df), replace = TRUE)
stats_list <- calculate_room_stats(df)
rv <- list(stats = stats_list$room_stats)

dt <- datatable(rv$stats, 
                options = list(
                  pageLength = 20, 
                  dom = 't',
                  scrollX = TRUE,
                  columnDefs = list(
                    list(visible = FALSE, targets = c("dev_de", "dev_dg", "dev_ds", "dev_mig", "dev_gender", "mean_de", "mean_dg", "mean_ds", "mean_ue", "sd_ue")),
                    list(orderData = which(names(rv$stats) == "mean_de") - 1, targets = which(names(rv$stats) == "MW de (+/-SD)") - 1),
                    list(orderData = which(names(rv$stats) == "mean_dg") - 1, targets = which(names(rv$stats) == "MW dg (+/-SD)") - 1),
                    list(orderData = which(names(rv$stats) == "mean_ds") - 1, targets = which(names(rv$stats) == "MW ds (+/-SD)") - 1),
                    list(
                      targets = which(names(rv$stats) == "UE (Bar)") - 1,
                      render = JS("function(data, type, row, meta) { return data; }")
                    )
                  )
                ),
                escape = FALSE,
                rownames = FALSE)

json <- jsonlite::toJSON(dt$x$options$columnDefs, auto_unbox = TRUE)
cat(json)
