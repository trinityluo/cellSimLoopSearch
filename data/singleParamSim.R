#NAME: simStart.R
#FUNC: Run this script to start simulating and decomposing for all parameters, save the
#results into disk periodically
#VER.: 1.0
#HIST: XL initial 03/27/2017

library(dtplyr)

source("R/functions.R")
source("stateSimulation.R")



# function to decompose trajectories
DecomposeTrj <- function(alpha, beta, gamma, matrixA, simTime, output, totalSteps) {
  for(i in 1:round(simTime/1024, 0)) {
    # simulate a trajectory with length of 1024, this can be changed
    # larger number makes the decompsition more accuracy but consume more memory
    trj <- SimRun(alpha, beta, gamma, matrixA, 1024)
    
    if(!exists('output')) {
      output <- data.table(count = numeric(), loops = character(), steps = numeric(),
                           flux = numeric(), weight = numeric())
    }
    
    # make sure output is DT
    setDT(output)
    
    repeat{
      loop <- RemoveLoop(trj)[['loop']] %>% ReorderLoop()
      steps <- RemoveLoop(trj)[['steps']]
      trj <- RemoveLoop(trj)[['newtrj']]
      # cat('totalsteps before:', totalSteps)
      # RemoveLoop() will return NULL at the last step
      totalSteps <- ifelse(!is.null(steps), totalSteps + steps, totalSteps)
      # cat(', loop:', loop, ', steps:', steps, ', totalsteps:', totalSteps, '\n')
      if(!is.null(loop)){
        output <- UniqueLoop(loop, output, steps)
        output[, flux := count/totalSteps]
        output[, weight := count/sum(count)]
        
        
      } else {
        break
      }
    }
    cat('chunk', i, 'miu=', beta, 'RR=', RobustRatio(output$flux), '\n')
  }
  arrange(output, desc(count))
  return(output)
}

runSinglePara <- function() {
  alpha <- 7
  gamma <- 0.6
  beta <- 5
  
  
  fileName <- 'data/processed/singleloop.a7.b5.c0.6.Rds'
  results <- readRDS(fileName)
  totalSteps <- sum(results$steps * results$count)
  
  results <- DecomposeTrj(alpha, beta, gamma, matrixA, 1e5, results, totalSteps)
  
  saveRDS(results, fileName)
    
  # create rr and sim time
  
  simTime <- sum(results$steps * results$count)
  rr <- RobustRatio(results$weight)
  if(!file.exists('data/processed/singleRRInfo.txt')) {
    cat('RR', 'simTime', '\n', file = 'data/processed/singleRRInfo.txt')
  }
  cat(rr, simTime, '\n', file = 'data/processed/singleRRInfo.txt', append = T)
  
  
}

repeat(runSinglePara())
