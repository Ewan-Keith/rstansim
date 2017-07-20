context("internal collect_* utilities return expected results")

test_that("collect_simulations functions as expected", {

  ## read in test stansim_simulation obj to use in tests
  sim1 <-
    readRDS("objects/test_stansim.rds")

  sim_renamed <- rename(sim1, new_name = "second simulation")
  sim2 <- refit(sim_renamed, "data-raw/data/schoolsdat3.rds")


})
