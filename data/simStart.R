#NAME: simStart.R
#FUNC: Run this script to start simulating and decomposing for all parameters, save the
#results into disk periodically
#VER.: 1.0
#HIST: XL initial 03/27/2017

library(dtplyr)

source("R/functions.R")
source("stateSimulation.R")

# empty data table for future results
results <- data.table(count = numeric(), loops = character(), steps = numeric(),
                       flux = numeric(), weight = numeric())

# sum of states for all loops
totalSteps <- 0

# function to decompose trajectories
DecomposeTrj <- function(alpha, beta, gamma, matrixA, simTime, output) {
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

chgParam <- function(whichParam, results) {
  switch(whichParam,
    chgAlpha = {
      beta <- 5
      gamma <- 0.6
    },
    chgBeta = {
      alpha <- 7
      gamma <- 0.6
      vecBeta <- c(seq(0.1,0.9,0.1), 1:9)
      rr <- lapply(vecBeta, DecomposeTrj, 
                   alpha = 7, gamma = 0.6, matrixA, 1024, results) %>% 
        lapply(select, weight) %>% 
        lapply(unlist) %>% 
        lapply(RobustRatio) %>% 
        unlist()
      return(rr)
    },
    chgGamma = {
      alpha <- 7
      beta <- 5
    }
  )
  
}

runAllPara <- function() {
  alpha <- 7
  gamma <- 0.6
  vecBeta <- c(seq(0.1,0.9,0.1), 1:9)
  
  for(i in vecBeta) {
    fileName <- paste('data/processed/loop.a7.b', i, '.c0.6.Rds', sep = '')
    if(file.exists(fileName)) {
      results <- readRDS(fileName)
    }
    results <- DecomposeTrj(alpha, i, gamma, matrixA, 1e5, results)
    saveRDS(results, fileName)
    
    # create rr and sim time
    
    simTime <- results[, 'steps'] %>% sum()
    rr <- RobustRatio(results$weight)
    if(!file.exists('data/processed/rrInfo.txt')) {
      cat('miu', 'RR', 'simTime', '\n', file = 'data/processed/rrInfo.txt')
    }
    cat(i, rr, simTime, '\n', file = 'data/processed/rrInfo.txt', append = T)
  }
  
}

#repeat(runAllPara())




