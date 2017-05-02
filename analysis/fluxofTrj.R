# calculate flux with para change
source("R/functions.R")
source("stateSimulation.R")
source("data/simStart.R")

gList <- list()
for(i in seq(0.01, 0.03, 0.01)){
  gList[i*100] <- DecomposeTrj(7, 5, i, matrixA, 1e4, results) %>%
    summarise(gamma = i, flux = max(flux))

  return(gList)
}


