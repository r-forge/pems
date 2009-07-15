\name{cut.and.stack}
\alias{cut.and.stack}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cut.and.stack(y, x = local.time, pems = NULL, number = 6, overlap = 0.1, type = "l", aspect = "xy", xlab = NULL, ylab = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{ ~~Describe \code{y} here~~ }
  \item{x}{ ~~Describe \code{x} here~~ }
  \item{pems}{ ~~Describe \code{pems} here~~ }
  \item{number}{ ~~Describe \code{number} here~~ }
  \item{overlap}{ ~~Describe \code{overlap} here~~ }
  \item{type}{ ~~Describe \code{type} here~~ }
  \item{aspect}{ ~~Describe \code{aspect} here~~ }
  \item{xlab}{ ~~Describe \code{xlab} here~~ }
  \item{ylab}{ ~~Describe \code{ylab} here~~ }
  \item{\dots}{ ~~Describe \code{\dots} here~~ }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
