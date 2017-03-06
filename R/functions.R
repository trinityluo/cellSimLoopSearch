# functions.R

library(data.table)
library(dplyr)

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
        return(list(loop=loop,newtrj=trj[-(lastind:j)]))
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


UniqueLoop <- function(loop, resultsDT) {
  DT <- resultsDT
  ind <- DT[loops == loop, which = TRUE]
  if (length(ind) != 0) {
    DT[ind, count := count + 1]
    
    return(DT)
    
  }
  else {
    DT <- rbindlist(list(DT, data.table(1, loop, flux = NA_real_, weight = NA_real_)))
    
    return(DT)
  }
  
}


RobustRatio <- function(x) {
  logx <- -log(x)
  rr <- abs(mean(logx) - min(logx))/sd(logx)
  return(rr)
}

