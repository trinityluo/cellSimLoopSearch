# main

library(data.table)

source("R/functions.R")

# inital
results <- data.table(count = numeric(), loops = character(), 
                      flux = numeric(), weight = numeric())
totalSteps <- 0



for (i in 1:length(chnks)) {
  trj <- trj.1e8.ff[chnks[[i]],]
  totalSteps <- totalSteps + length(trj)
  cat(i, '/', length(trj))
  repeat{
    loop <- RemoveLoop(trj)[['loop']] %>% ReorderLoop()
    trj <- RemoveLoop(trj)[['newtrj']]
    if(!is.null(loop)){
       results <- UniqueLoop(loop, results)
       results[, flux := count/totalSteps]
       results[, weight := count/sum(count)]
       
    } else {
      break
    }
  }
  cat(' RR=', RobustRatio(results$flux), '\n')
  write.csv(results, 'data/processed/results.csv', row.names = F)
}