#NAME: global.R
#FUNC: Store the global functions in Win/Opp report that needed by server.R, ui.R
#VER.: 1.0
#HIST: XL initial 03/27/2017


# calculate RR
RobustRatio <- function(x) {
  logx <- -log(x)
  rr <- abs(mean(logx) - min(logx))/sd(logx)
  return(rr)
}

# dec to bin
dec2bin <- function(x, nBits = 10){
  tail(rev(as.numeric(intToBits(x))),nBits)
}

# binary to decimal
bin2dec <- function(x, base = 2) {
  x <- paste(x, collapse = '')
  split_base = strsplit(as.character(x), split = "")
  return(sapply(split_base, function(x) sum(as.numeric(x) * base^(rev(seq_along(x) - 1)))))
}