

testStanArgs <- list(file = 'devtest/8schools.stan', data = list(),
                    chains = 4)

testSimArgs <- list(simData = dir("devtest/data", full.names = TRUE), maxRhat = 1.05)

stanSim(stanArgs = testStanArgs, simArgs = testSimArgs)


testFull <- modifyList(testStanArgs, list(data = readRDS("devtest/data/schoolsdat1.rds")))
