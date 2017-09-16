# lin_reg_test data production

set.seed(12345)

for (i in 1:5){
  x <- rnorm(50, 100, 15)
  y <- 200 + x * 5 + rnorm(50, 0, 30)

  data <- as.data.frame(cbind(x, y))
  file_name <- paste0("data-raw/lin_reg_test/data/lin_reg_dat_", i, ".csv")

  write.csv(data, eval(file_name), row.names = F)
}
