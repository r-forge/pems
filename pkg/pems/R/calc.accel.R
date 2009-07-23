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
#TO DO
###########
#currently makes no use of fun.method, fun.args
#assumes time is a count in seconds
#

require(lattice)


#for non-pems use
if(is(pems)[1]=="pems") {
	if(is.na(match(deparse(substitute(speed)), names(pems$data)))) {
		stop(paste("\t calc.accel: '", as.character(substitute(speed)), "' not found in pems", sep = ""), call. = FALSE, domain = NA)
	} 	
	data <- pems$data
} else {
	if(is.na(match(deparse(substitute(speed)), names(pems)))) {
		stop(paste("\t calc.accel: '", as.character(substitute(speed)), "' not found in (non-)pems", sep = ""), call. = FALSE, domain = NA)
	} 	
	data <- pems
}

this.time <- data[, deparse(substitute(time))]
this.speed <- data[, deparse(substitute(speed))]
this.speed.name <- as.character(substitute(speed))

#####################################################
#main routines

#reset units
# if it is a pems
this.accel.units <- "unknown"
if(is(pems)[1]=="pems") {
	this.unit <- as.character(pems$units[, this.speed.name])	
	this.correction <- "not found"
	if(this.unit=="km/h") { this.correction = 0.27777777778 }
	if(this.unit=="m/s") { this.correction = 1 }
	if(this.correction=="not found") {
		stop(paste("\t ca: ", this.speed.name, " units not recognised", sep=""), call. = FALSE, domain = NA)
	}
	this.accel.units <- "m/s/s"
	this.speed <- this.speed * this.correction
}

this.time <- c(NA, diff(this.time))
	#this.time <- c(this.time[1], this.time)
this.accel <- c(0, diff(this.speed)) / this.time
this.accel <- data.frame(this.accel)
names(this.accel) <- "accel"
data <- cbind(data, this.accel)
        
#setup pems info
if(is(pems)[1]=="pems") {
	pems$history <- c(pems$history, paste("calc.accel: accel generated using '", this.speed.name, "'", sep = ""))
	pems$units[length(pems$units) + 1] <- this.accel.units
	names(pems$units)[length(pems$units)] <- "accel"
}
        
#plot if required
if(vis.fun){
	plot(this.speed,type="l")
	lines(this.accel,col="blue")
}

if(is(pems)[1]=="pems") {
        pems$data <- data
        pems
} else {
        pems <- data
}

if(fun.output=="pems") { pems } else { this.accel }
}

