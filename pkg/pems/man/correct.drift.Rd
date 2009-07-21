\name{correct.drift}
\Rdversion{1.1}
\alias{correct.drift}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
correct.drift(x, pems, calc.method = "zero.ends", calc.args = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{pems}{
%%     ~~Describe \code{pems} here~~
}
  \item{calc.method}{
%%     ~~Describe \code{calc.method} here~~
}
  \item{calc.args}{
%%     ~~Describe \code{calc.args} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ correct.drift }