#-----------------------------------------------------------------
#### simulate_data reproducability ####

simulate_model <- "parameters {
real scale;
}
generated quantities {
vector[100] x;
vector[100] y;

for(i in 1:100) x[i] = normal_rng(100, 15);
for(i in 1:100) y[i] = scale * x[i];
}"

### READ THIS
## in order for stans rng to be truly reproducable the R environment must
## be maintained, this requires several specifically placed restarts
## these are called with .rs.restartR()

# restart R
.rs.restartR()

library(rstansim)

simulated_data <- small_simulation <- simulate_data(
  file = simulate_model,
  param_values = list("scale" = 2),
  vars = c("x", "y"),
  data_name = "repro_test",
  nsim = 2,
  path = "repro_test/",
  seed = 1234
)


# restart R
.rs.restartR()

library(rstansim)

# simulate the new data
simulated_data2 <- small_simulation <- simulate_data(
  file = simulate_model,
  param_values = list("scale" = 2),
  vars = c("x", "y"),
  data_name = "repro_test",
  nsim = 2,
  path = "repro_test2/",
  seed = 1234
)

# restart R
.rs.restartR()

library(rstansim)

# simulate the new data
simulated_data3 <- small_simulation <- simulate_data(
  file = simulate_model,
  param_values = list("scale" = 2),
  vars = c("x", "y"),
  data_name = "repro_test",
  nsim = 2,
  path = "repro_test3/",
  seed = 999
)


# read in datasets
orig_1 <- readRDS("repro_test/repro_test_1.rds")
orig_2 <- readRDS("repro_test/repro_test_2.rds")

rep_1 <- readRDS("repro_test2/repro_test_1.rds")
rep_2 <- readRDS("repro_test2/repro_test_2.rds")

diff_1 <- readRDS("repro_test3/repro_test_1.rds")
diff_2 <- readRDS("repro_test3/repro_test_2.rds")

# test if identical
identical(orig_1, rep_1)
identical(orig_2, rep_2)
!identical(orig_1, diff_1)
!identical(orig_2, diff_2)

#-----------------------------------------------------------------
#### fit_models reproducability ####

fit_model <- "data{
vector[100] x;
vector[100] y;
}
parameters {
real scale;
}
model {
y ~ normal(scale * x, 0.001);
}"


repro_1 <- fit_models(
  sim_name = "repro1",
  sim_data = simulated_data,
  stan_args = list(file = fit_model),
  seed = 1234
)

repro_2 <- fit_models(
  sim_name = "repro1",
  sim_data = simulated_data,
  stan_args = list(file = fit_model),
  seed = 1234
)

repro_3 <- fit_models(
  sim_name = "repro1",
  sim_data = simulated_data,
  stan_args = list(file = fit_model),
  seed = 9999
)

# test if identical
temp1 <- extract_data(repro_1)
temp2 <- extract_data(repro_2)
temp3 <- extract_data(repro_3)

identical(temp1, temp2)
!identical(temp1, temp3)

# dont forget to delete the repro_test folder
