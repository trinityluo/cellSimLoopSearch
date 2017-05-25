# calculate flux with para change
library(dtplyr)
library(readr)

source("R/functions.R")
source("stateSimulation.R")

# empty data table for future results
results <- data.table(count = numeric(), loops = character(), steps = numeric(),
                      flux = numeric(), weight = numeric())

# sum of states for all loops
totalSteps <- 0

# function to decompose trajectories
CalFlux <- function(fileNames, chunk, output) {
  dfList <- read_csv(fileNames, col_names = FALSE) %>% 
    split(., (seq(nrow(.))-1) %/% chunk)
  
  for(i in 1:round(length(dfList), 0)) {
    
    trj <- dfList[[i]][[1]]
    #cat('trj', class(trj), '\n')
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
      # cat('totalsteps before:', totalSteps, '\n')
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
    cat('chunk', i, '/', 1e7/5000, 'file=', fileNames, '\n')
  }
  arrange(output, desc(count))
  saveRDS(output, paste0('data/processed/gamma.b0.8/result.', substring(fileNames, 35, 39), '.Rds'))
  return(NULL)
}

#results <- CalFlux("data/raw/trj/trj.a7.0.b5.0.c0.01.out", 1024, results)

# first run 1-3 5-7 9-11 13-15 17-19 21-23 25-27 29-30
lapply(paste0(list.files('data/raw/trj.b0.8.g', full.names = T)[c(29:30)]), CalFlux, 5000, results)
