context("testing all print methods")

test_that(paste(
  "print.stansim_simulation function should print",
  "expected output"), {

    ## read in test stansim_simulation obj to print
    extract_test_data <- readRDS("objects/test_stansim.rds")

    caught_print <- utils::capture.output(print(extract_test_data))

    expect_true(grepl("Stan Simulation Title: .*", caught_print[1]))

    expect_true(grepl("Model Name: .*", caught_print[2]))

    expect_true(grepl("", caught_print[3]))

    expect_true(grepl("Started Running at: .*", caught_print[4]))

    expect_true(grepl("Finished Running at: .*", caught_print[5]))

    expect_true(grepl("", caught_print[6]))

    expect_true(grepl("Number of Models Fitted: \\d*", caught_print[7]))

    expect_true(grepl("", caught_print[8]))

    expect_true(grepl("Parameters Recorded: \\d*", caught_print[9]))

    expect_true(grepl(
      "eta\\[1\\]   eta\\[2\\]   eta\\[3\\]   eta\\[4\\]   eta\\[5\\]  ",
      caught_print[10]))

    expect_true(grepl(
      "eta\\[6\\]   eta\\[7\\]   eta\\[8\\]   lp__     mu      ",
      caught_print[11]))

    expect_true(grepl(
      "tau      theta\\[1\\] theta\\[2\\] theta\\[3\\] theta\\[4\\]",
      caught_print[12]))

    expect_true(grepl(
      "theta\\[5\\] theta\\[6\\] theta\\[7\\] theta\\[8\\]         ",
      caught_print[13]))

    expect_true(grepl("", caught_print[14]))

    expect_true(grepl("Estimates Recorded: \\d*", caught_print[15]))

    expect_true(grepl(
      "2.5%    25%     50%     75%     97.5%  ",
      caught_print[16]))

    expect_true(grepl(
      "mean    n_eff   Rhat    sd      se_mean",
      caught_print[17]))
})

test_that(
  "print.stansim_simulation works with > 50 estimates and parameters", {

    ## read in test stansim_simulation obj to print
    extract_test_data <- readRDS("objects/test_stansim.rds")

    # add > 50 estimates to data
    temp_data <- extract_test_data$data
    temp_data <- rbind(temp_data,
                       cbind("datafile" = NA,
                             "parameter" = 1:50,
                             "estimate" = 1:50,
                             "value" = NA))

    # put back in to stansim_simulation object
    extract_test_data$data <- temp_data

    caught_print <-
      utils::capture.output(print(extract_test_data))

    expect_true(grepl("Stan Simulation Title: .*", caught_print[1]))

    expect_true(grepl("Model Name: .*", caught_print[2]))

    expect_true(grepl("", caught_print[3]))

    expect_true(grepl("Started Running at: .*", caught_print[4]))

    expect_true(grepl("Finished Running at: .*", caught_print[5]))

    expect_true(grepl("", caught_print[6]))

    expect_true(grepl("Number of Models Fitted: \\d*", caught_print[7]))

    expect_true(grepl("", caught_print[8]))

    expect_true(grepl("Parameters Recorded: \\d* \\(first 50 shown\\)",
                      caught_print[9]))

    expect_true(grepl(
      "eta\\[1\\]   eta\\[2\\]   eta\\[3\\]   eta\\[4\\]   eta\\[5\\]  ",
      caught_print[10]))

    expect_true(grepl(
      "eta\\[6\\]   eta\\[7\\]   eta\\[8\\]   lp__     mu      ",
      caught_print[11]))

    expect_true(grepl(
      "tau      theta\\[1\\] theta\\[2\\] theta\\[3\\] theta\\[4\\]",
      caught_print[12]))

    expect_true(grepl(
      "theta\\[5\\] theta\\[6\\] theta\\[7\\] theta\\[8\\] 1       ",
      caught_print[13]))

    expect_true(grepl(
      "2        3        4        5        6       ",
      caught_print[14]))

    expect_true(grepl(
      "7        8        9        10       11      ",
      caught_print[15]))

    expect_true(grepl(
      "12       13       14       15       16      ",
      caught_print[16]))

    expect_true(grepl(
      "17       18       19       20       21      ",
      caught_print[17]))

    expect_true(grepl(
      "22       23       24       25       26      ",
      caught_print[18]))

    expect_true(grepl(
      "27       28       29       30       31      ",
      caught_print[19]))

    expect_true(grepl("", caught_print[20]))

    expect_true(grepl("Estimates Recorded: \\d* \\(first 50 shown\\)",
                      caught_print[21]))

    expect_true(grepl(
      "2.5%    25%     50%     75%     97.5%  ",
      caught_print[22]))

    expect_true(grepl(
      "mean    n_eff   Rhat    sd      se_mean",
      caught_print[23]))

    expect_true(grepl(
      "1       2       3       4       5      ",
      caught_print[24]))

    expect_true(grepl(
      "6       7       8       9       10     ",
      caught_print[25]))

    expect_true(grepl(
      "11      12      13      14      15     ",
      caught_print[26]))

    expect_true(grepl(
      "16      17      18      19      20     ",
      caught_print[27]))

    expect_true(grepl(
      "21      22      23      24      25     ",
      caught_print[28]))

    expect_true(grepl(
      "26      27      28      29      30     ",
      caught_print[29]))

    expect_true(grepl(
      "31      32      33      34      35     ",
      caught_print[30]))

    expect_true(grepl(
      "36      37      38      39      40     ",
      caught_print[31]))

    })
