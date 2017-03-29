# load packages

# source file
source('R/functions.R')

# import matrix
matrixA <- read.table('data/raw/Matrix.txt', sep = '\t') %>% as.matrix()
nodeNames <- c('Start', 'SK', 'Cdc2/Cdc13', 'Ste9', 'Rum1', 'Slp1', 'Cdc2/Cdc13*',
               'Wee1 Milk1', 'Cdc25', 'PP')
rownames(matrixA) <- nodeNames
colnames(matrixA) <- nodeNames

nodeNum <- ncol(matrixA)
stateNum <- 2^nodeNum


set.seed(0)

initState <- sample(1:1024, 1, replace = T)

SimState <- function(alpha, beta, gamma, sNow, matrixA) {
  sNowBinary <- dec2bin(sNow)
  sNextBinary <- array(dim = nodeNum)
  
  pr <- matrix(nrow = 1, ncol = nodeNum)
  
  for(i in 1:nodeNum){
    totalIn <- TotalInput(sNowBinary, i, matrixA)
    rn <- runif(1)
    if(totalIn != 0){
      nodepr <- exp(beta * totalIn)/(exp(beta * totalIn) + exp(-beta * totalIn))
      if(rn < nodepr) {sNextBinary[i] <-  1}
      else sNextBinary[i] <-  0
    }
    else if(sNow == 100 & i == 1){
      nodepr <- gamma
      if(rn < nodepr) {sNextBinary[i] <-  1}
      else sNextBinary[i] <-  0
    }
    else {
      nodepr <- 1 / (1 + exp(-alpha))
      if(rn < nodepr) {sNextBinary[i] <-  sNowBinary[i]}
      else sNextBinary[i] <- 1 - sNowBinary[i]
    }
    
  }
  sNext <- bin2dec(sNextBinary)
  return(sNext)
}

SimRun <- function(alpha, beta, gamma, matrixA, simTime){
  sNext <- SimState(alpha, beta, gamma, initState, matrixA)
  trj <- integer()
  trj[1] <- initState
  for(i in 2:simTime) {
    trj[i] <- sNext
    sNext <- SimState(alpha, beta, gamma, sNext, matrixA)
    simTime <- simTime -1
  }
  return(trj)
}

#test <- SimRun(7, 5, 0.6, matrixA, 1024)

