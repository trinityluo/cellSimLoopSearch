# load packages

# source file
source('R/functions.R')

# import matrix
matrixA <- read.table('data/raw/Matrix.txt', sep = '\t') %>% as.matrix()

nodeNum <- ncol(matrixA)
stateNum <- 2^nodeNum


TransProb <- function(alpha, beta, gamma, sNow, sNext, matrixA) {
  sNowBinary <- dec2bin(sNow)
  sNextBinary <- dec2bin(sNext)
  
  pr <- matrix(nrow = 1, ncol = nodeNum)
  totalIn <- TotalInput(sNowBinary)
  for(i in 1:nodeNum){
    if(totalIn[i] != 0){
      pr[i] <- exp(beta * (2 * sNextBinary[i] - 1) * totalIn[i])/(exp(beta * totalIn[i]) +exp(-beta * totalIn[i]))
    }
    else if(sNow == 100){
      pr[i] <- 1 - gamma
      if(sNowBinary[i] != sNextBinary[i]) {pr[i] <- 1 - pr[i]}
    }
    else {
      pr[i] <- 1 / (1 + exp(-alpha))
      if(sNowBinary[i] != sNextBinary[i]) {pr[i] <- 1 - pr[i]}
    }
  }
  prob <- apply(pr, 1, prod)
  return(prob)
}

GetTransMatrix <- function(alpha, beta, gamma, matrixA ) {
  
  tMatrix <- matrix(nrow = stateNum, ncol = stateNum)
  for(i in 1:stateNum){
    for(j in 1:stateNum){
      tMatrix[i, j] <- TransProb(alpha, beta, gamma, i, j, matrixA)
    }
  }
  return(tMatrix)
}

library(markovchain)
