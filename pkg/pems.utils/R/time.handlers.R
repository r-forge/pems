###################################
#regularize
###################################

#kr vr 0.1.2 17/11/2018

#originial written for 3datx/ce-cert work 
#tested msc Adrian 
#refined as part of work for Daisy's phd

#converts irregular time-series into regular time-series 
#can also hole-file 

regularize <- function (data, Hz=1, ...) 
{
  data <- rebuildPEMS(data, "old")
  test <- comment(isPEMS(data))
  if (!test %in% c("pems", "data.frame")) 
    stop("can't touch this")
  temp <- if (test == "pems") 
    pemsData(data)
  else data
  if (!"local.time" %in% names(temp)) 
    stop("missing required reference 'local.time'")
  x <- temp$local.time
  new.x <- (0:ceiling(x[length(x)])) 
  new.x <- (1:(length(new.x)*Hz)-1)/Hz
  new.df <- temp[1, ]
  new.df[length(new.x), 1] <- NA
  for (i in names(temp)) {
    if (is.numeric(temp[, i])) 
      new.df[, i] <- approx(x, temp[, i], new.x)$y
    if (is.factor(temp[, i]) || is.character(temp[, i])) {
      new.df[, i][which(new.x %in% x)] <- temp[, i]
      for (j in 2:length(new.df[, i])) 
        new.df[j, i] <- new.df[j - 1, i]
    }
    if (any(c("POSIXct", "POSIXt") %in% class(temp[, i]))) {
      new.df[, i] <- temp[1, i] + new.x
    }
  }
  if (test == "pems") {
    class(data) = "not.pems"
    data$data <- new.df
    class(data) <- "pems"
  }
  else data <- new.df
  data <- rebuildPEMS(data)
  return(data)
}
