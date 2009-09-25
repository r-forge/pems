calc.accel <- 
function(pems,
  speed = velocity, time = local.time,
  fun.method = "standard.accel",
  fun.args = NULL,
  vis.fun = TRUE,
  fun.output = "pems"
){

###############
#calculate accel 
#version 0.0.1
#source
#karl (15/03/2009)

###############
#NOTES
###############
#currently only has a standard.accel method
#assumes time is a count in seconds
#currently allows only fun.args$dt which forces time interval 
#currently creates accel.numeric if generated accel not unique

###############
#TO DO
###########
#rationalise vis.fun using lattice

require(lattice)

#for non-pems use
if(is(pems)[1]=="pems") {
  data <- pems$data
} else {
  data <- pems
}

#check inputs
if(fun.method!="standard.accel") {
  stop(paste(" mis-match in calc.accel \n\tcalculation method '", fun.method, "' not recognised", sep = ""), call. = FALSE)
} 
if(is.na(match(deparse(substitute(speed)), names(data)))) {
  stop(paste(" mis-match in calc.accel \n\tspeed parameter '", substitute(speed), "' not found in '", substitute(pems), "'", sep = ""), call. = FALSE)
} 
if(is.na(match(deparse(substitute(time)), names(data))) & is.null(fun.args$dt)) {
  stop(paste(" mis-match in calc.accel \n\ttime parameter '", substitute(time), "' not found in '", substitute(pems), "'", sep = ""), call. = FALSE)
} 

#setup variables
if(is.null(fun.args$dt)){
  this.time <- c(NA, diff(data[, deparse(substitute(time))])) 
}else {
  this.time <- fun.args$dt 
}
this.speed <- data[, deparse(substitute(speed))]
this.speed.name <- as.character(substitute(speed))

#if pems, reset using units
this.accel.units <- "unknown"
if(is(pems)[1]=="pems") {
  this.unit <- as.character(pems$units[, this.speed.name])	
  this.correction <- "not found"
  if(this.unit=="km/h") { this.correction = 0.27777777778 }
  if(this.unit=="m/s") { this.correction = 1 }
  if(this.correction=="not found") {
    stop(paste(" mis-match in calc.accel \n\t", this.speed.name, " units not recognised", sep=""), call. = FALSE, domain = NA)
  }
  this.accel.units <- "m/s/s"
  this.speed <- this.speed * this.correction
}

this.accel <- c(0, diff(this.speed)) / this.time
this.accel <- data.frame(accel = this.accel)
data <- cbind(data, this.accel)
names(data) <- make.unique(names(data))

#setup pems info
if(is(pems)[1]=="pems") {
  this.history <- paste("calc.accel: ", names(data)[length(data)], " generated using '", this.speed.name, "'", sep = "")
  if(!is.null(fun.args$dt)) { 
    this.history <- paste(this.history, " and fun.args$dt = ", fun.args$dt, sep = "") 
  } else {
    this.history <- paste(this.history, " and '", substitute(time), "'", sep = "") 
  }
  pems$history <- c(pems$history, this.history)
  pems$units[length(pems$units) + 1] <- this.accel.units
  names(pems$units)[length(pems$units)] <- names(data)[length(data)] 
}
        
#plot if required
if(vis.fun){
  plot(this.speed,type="l")
  lines(this.accel,col="blue")
}

#repack and exit
if(is(pems)[1]=="pems") {
  pems$data <- data
  pems
} else {
  pems <- data
}
if(fun.output=="pems") { pems } else { this.accel }
}
