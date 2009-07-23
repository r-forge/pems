calc.em <-
function(pems,
		calc.method = "pems.obs",
		exflow = exh.flow.rate, extemp = exh.temp, express = exh.press,
		x.delay = NULL, x.mwt = NULL, x.unit.correct = NULL
		){

###############
#calculate emissions 
#version 0.0.1
#based on OBS manual method
#karl (12/03/2009)
#

###############
#TO DO
###########
#this currently only uses a pems.obs method
#non-pems halts at x.delay, x.mt, x.unit.correct = NULL
#non-pems halts at calc

if(is(pems)[1]=="pems") {
	data <- pems$data
} else {
	#needs to know the other constants
	if(is.null(x.delay) | is.null(x.mwt) | is.null(x.unit.correct)) {
		stop("\t calc.em: non-pems requires x.delay, x.mwt and x.unit.correct set", call. = FALSE, domain = NA)
	}
	data <- pems #to use with non-pems objects
}

exflow <- data[, deparse(substitute(exflow))]
extemp <- data[, deparse(substitute(extemp))]
express <- data[, deparse(substitute(express))]

conc.names <- grep("conc.", names(data), value = TRUE)
x.names <- gsub("conc.", "", conc.names)

for(i in 1:length(conc.names)) {
	this <- data[, conc.names[i]]
	if(is(pems)[1]=="pems") {
		this.delay <- pems$constants[, paste("delay.", x.names[i], sep = "")]
		this.delay <- round(this.delay)
		if(x.names[i]=="hc"){

	#check this
	#hc mwt
			
			this.mwt <- pems$constants$mm.h * pems$constants$alpha.exhaust.hc + pems$constants$mm.c
			this.mwt <- this.mwt * pems$constants$thc.c6 			

		} else {
			this.mwt <- pems$constants[, paste("mm.", x.names[i], sep = "")]
		}
		this.unit <- as.character(pems$units[, conc.names[i]])
		this.correction <- "not found"
		if(this.unit=="vol%") { this.correction = (1/60) * (1/100) * (1/22.415)  * (273.15/293.15) }
		if(this.unit=="ppm") {  this.correction = (1/60) * (1 / 1000000) * (1/22.415) * (273.15/293.15) }
		if(this.unit=="ppmC6") {  this.correction = (1/60) * (1 / 1000000) * (1/22.415) * (273.15/293.15) }
		if(this.correction=="not found") {
			stop(paste("\t calc.em: ", conc.names[i], " units not recognised", sep=""), call. = FALSE, domain = NA)
		}
	#need to do corrects for sampling time interval
	##############################
	#to do

	} else {
	#non-pems emission calculation
	##############################
	#to do
		stop("\t calc.em:  method not written for non-pems", call. = FALSE, domain = NA)	
	}
	#check this
	#delay
	if(this.delay > 0) { this <- c(this[(this.delay + 1):length(this)], rep(NA, (this.delay))) }
	
	#check this
	#equation
	this <- this * this.mwt * exflow * this.correction
	this <- data.frame(this)
	names(this) <- paste("em.", x.names[i], sep = "")

	#if pems write to history

#if pems write unit entries

	data <- cbind(data, this)
	
	if(is(pems)[1]=="pems") {
		pems$history <- c(pems$history, paste("calc.em: em.", x.names[i], " generated using 'conc.", x.names[i], "' and '", calc.method, "' method", sep = ""))
		pems$units[length(pems$units) + 1] <- "g/sec"
		names(pems$units)[length(pems$units)] <- paste("em.", x.names[i], sep = "")
	}
	
}

if(is(pems)[1]=="pems") {
	pems$data <- data
	pems
} else {
	data
}

}

