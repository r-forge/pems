`cut.and.stack` <-
function(y, x = local.time, pems = NULL, 
		number = 6, overlap = 0.1, 
		type= "l", aspect ="xy", 
		xlab = NULL, ylab = NULL,
		...
		){

###############
#cut and stack 
#version 0.0.1
#based on `lattice' page 184 by Deepayan Sarkar
#modfications karl (02/03/2009)
#

###############
#TO DO
###########
#so many things
#add type tester to x and y
#NULL test to be reconfigures works or test <- NULL the ..,test, but not ..,NULL,

#y handling
if(exists(as.character(substitute(y)), mode = "NULL")) {
	stop("\t cas: null 'y' set", call. = FALSE, domain = NA)
}
#assume x and y are in pems if this is declared
if(!is.null(pems)) {
	if(is(pems)[1]=="pems") {
		if(!is.na(match(deparse(substitute(y)), names(pems$data)))) {
			if(is.null(ylab)) { ylab <- as.character(substitute(y)) }
			y <- pems$data[, deparse(substitute(y))]
		}
	} else {
		if(!is.na(match(deparse(substitute(y)), names(pems)))) {
			if(is.null(ylab)) { ylab <- as.character(substitute(y)) }
			y <- pems[, deparse(substitute(y))]
		}
	}	
} 
#if not declared must be local
if(is.null(ylab)) { ylab <- deparse(substitute(y)) }

#x handling
if(exists(as.character(substitute(x)), mode = "NULL")) {
	if(is.ts(y)) {
		x <- is.ts(y) 	
		if(is.null(xlab)) { xlab <- "Time" }
	} else {
		x <- seq_along(y) 	
		if(is.null(xlab)) { xlab <- "Count" }
	}
}
#assume x (and y) are in pems if this is declared
if(!is.null(pems)) {
	if(is(pems)[1]=="pems") {
		if(!is.na(match(deparse(substitute(x)), names(pems$data)))) {
			if(is.null(xlab)) { xlab <- as.character(substitute(x)) }
			x <- pems$data[, deparse(substitute(x))]
		}
	} else {
		if(!is.na(match(deparse(substitute(x)), names(pems)))) {
			if(is.null(xlab)) { xlab <- as.character(substitute(x)) }
			x <- pems[, deparse(substitute(x))]
		}
	}	
} 
#if not declared must be local
if(is.null(xlab)) { xlab <- deparse(substitute(x)) }

require(lattice)

Time <- equal.count(as.numeric(x), 
		number = number, overlap = overlap)
xyplot(as.numeric(y) ~ x | Time,
		type = type, aspect = aspect, 
		xlab = xlab, ylab = ylab,
		default.scales = list(x = list(relation = "free"),
						y = list(relation ="free")), ...)

}

