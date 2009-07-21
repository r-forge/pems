correct.drift <-
function(x, pems,
		calc.method = "zero.ends",
		calc.args = NULL
		){

###############
#correct drift generic 
#version 0.0.1
#based on my previous method
#karl (12/03/2009)
#

###############
#TO DO
###########
#this currently only uses a zero.ends method
#

if(is(pems)[1]=="pems") {
	data <- pems$data
} else {
	#needs to know the other constants
	#if(is.null(x.delay) | is.null(x.mwt) | is.null(x.unit.correct)) {
	#	stop("\t ce: non-pems requires x.delay, x.mwt and x.unit.correct set", call. = FALSE, domain = NA)
	#}
	data <- pems #to use with non-pems objects
}

this.x <- data[, deparse(substitute(x))]


#exflow <- data[, deparse(substitute(exflow))]
#extemp <- data[, deparse(substitute(extemp))]
#express <- data[, deparse(substitute(express))]

#conc.names <- grep("conc.", names(data), value = TRUE)
#x.names <- gsub("conc.", "", conc.names)

#for(i in 1:length(conc.names)) {
#	this <- data[, conc.names[i]]
#	if(is(pems)[1]=="pems") {
#		this.delay <- pems$constants[, paste("delay.", x.names[i], sep = "")]
#		this.delay <- round(this.delay)
#		if(x.names[i]=="hc"){
#
#	#check this
#	#hc mwt
#
#			this.mwt <- 7
#		} else {
#			this.mwt <- pems$constants[, paste("mm.", x.names[i], sep = "")]
#		}
#	#need to do corrects for units
#	#
#
#	}
#	#check this
#	#delay
#	if(this.delay > 1) { this <- c(this[(this.delay + 1):length(this)], rep(NA, (this.delay))) }
#	
#	#check this
#	#equation
#	this <- this * exflow * this.mwt
#	this <- data.frame(this)
#	names(this) <- paste("em.", x.names[i], sep = "")
#
#	#if pems write to history
#
#
#if pems write unit entries
#
#	data <- cbind(data, this)
#	
#	if(is(pems)[1]=="pems") {
#		pems$history <- c(pems$history, paste("hello: ", x.names[i], sep = ""))
#		pems$units[length(pems$units) + 1] <- "g/sec"
#		names(pems$units)[length(pems$units)] <- paste("em.", x.names[i], sep = "")
#	}
#	
#
#}


if(is(pems)[1]=="pems") {
	pems$data <- data
	pems
} else {
	data
}

}

