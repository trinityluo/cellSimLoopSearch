# import data

library(ff)

url.1e8 <- 'data/raw/trj.a7.0.b5.0.c0.60.1e8.out'
#url.1e9 <- '../data/simulation/1e9/trj.a7.0.b3.0.c0.60.1e9.out'




trj.1e8.ff <- read.table.ffdf(file = url.1e8)
chnks <- chunk(from = 1, to = nrow(trj.1e8.ff), by = 10000, method = 'seq')


#trj.1e9.ff <- read.table.ffdf(file = url.1e9)
