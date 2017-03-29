# functions.R

library(data.table)
library(dplyr)

# # initialize var
# InitVar <- function() {
#   # empty data table for future results
#   results <<- data.table(count = numeric(), loops = character(), 
#                         flux = numeric(), weight = numeric())
#   
#   # number of total simulated steps, begin from 1
#   totalSteps <<- 1
#   
#   return(NULL)
# }


# find loop in trjactory, cut the loop off, output the loop and cutted trjactory in a list
RemoveLoop <- function(trj) {
  if (length(trj) == 0)
    return(NULL)
  
  ht <- new.env()
  for(j in 1:length(trj)) {
    i <- trj[j]
    key <- as.character(i)
    if(exists(key,envir=ht)) {
      lastind <- ht[[key]]
      loop <- trj[lastind:(j-1)] %>% as.character()
      if(length(unique(loop)) > 2) {
        return(list(loop = loop, newtrj = trj[-(lastind:j)], 
                    steps = length(unique(loop))))
      }
    }
    ht[[key]] <- j
  }
}

# reorder the loop, find minimum number in the loop, move min and all number after min to the beginning
ReorderLoop <- function(loop) {
  if (is.null(loop))
    return(NULL) 
  loop <- unique(loop)
  ind <- which.min(loop)
  if(ind != 1) loop <- c(loop[ind:length(loop)],loop[1:(ind-1)])
  paste0(loop, collapse="-")
}


UniqueLoop <- function(loop, resultsDT, steps) {
  DT <- resultsDT
  ind <- DT[loops == loop, which = TRUE]
  if (length(ind) != 0) {
    DT[ind, count := count + 1]
    
    return(DT)
    
  }
  else {
    DT <- rbindlist(list(DT, data.table(1, loop, steps = steps,
                                        flux = NA_real_, weight = NA_real_)))
    
    return(DT)
  }
  
}


RobustRatio <- function(x) {
  logx <- -log(x)
  rr <- abs(mean(logx) - min(logx))/sd(logx)
  return(rr)
}

# dec to bin
dec2bin <- function(x, nBits = 10){
  tail(rev(as.numeric(intToBits(x))),nBits)
}

bin2dec <- function(x, base = 2) {
  x <- paste(x, collapse = '')
  split_base = strsplit(as.character(x), split = "")
  return(sapply(split_base, function(x) sum(as.numeric(x) * base^(rev(seq_along(x) - 1)))))
}

# TotalInput <- function(sNowBinary) {
#   totalIn <- sNowBinary * matrixA %>% colSums()
#   totalIn[3] <- 0.9
#   totalIn[7] <- -0.9
#   
#   return(totalIn)
# }

TotalInput <- function(sNowBinary, i, matrixA) {
  sum <- 0
  for(j in 1:nodeNum) {
    sum <- sum + matrixA[j, i] * sNowBinary[j]
  }
  if(i == 3) sum = sum + 0.9
  if(i == 7) sum = sum - 0.9
  
  return(sum)
}

