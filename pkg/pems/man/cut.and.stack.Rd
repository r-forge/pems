\name{cut.and.stack}
\Rdversion{1.1}
\alias{cut.and.stack}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cut.and.stack(y, x = local.time, pems = NULL, cuts = 5, overlap = 0.1, flush = c("both", "none", "max", "min"), type = "l", xlab = NULL, ylab = NULL, hist.breaks = 50, boxplots = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{pems}{
%%     ~~Describe \code{pems} here~~
}
  \item{cuts}{
%%     ~~Describe \code{cuts} here~~
}
  \item{overlap}{
%%     ~~Describe \code{overlap} here~~
}
  \item{flush}{
%%     ~~Describe \code{flush} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
}
  \item{xlab}{
%%     ~~Describe \code{xlab} here~~
}
  \item{ylab}{
%%     ~~Describe \code{ylab} here~~
}
  \item{hist.breaks}{
%%     ~~Describe \code{hist.breaks} here~~
}
  \item{boxplots}{
%%     ~~Describe \code{boxplots} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function(y, x = local.time, pems = NULL, 
		cuts = 5, overlap = 0.1, flush = c("both", "none", "max", "min"),
		type= "l", #aspect =c(0.2), 
		xlab = NULL, ylab = NULL,
		hist.breaks = 50,
		boxplots = TRUE,
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

#calc subdivision
if(length(cuts)==1) {
	subset <- equal.count(as.numeric(x), number = cuts, overlap = overlap)
  } else {
	if(is.null(nrow(cuts))) {
		#for a vector series
		cuts <- sort(cuts)
		if(flush[1] == "both" | flush[1] == "min") {
			if(min(cuts) > min(as.numeric(x))) { cuts <- c(min(as.numeric(x)) , cuts) }
		}
		if(flush[1] == "both" | flush[1] == "max") {
			if(max(cuts) < max(as.numeric(x))) { cuts <- c(cuts, max(as.numeric(x))) }
		}
		cuts <- cbind(cuts[1: (length(cuts)-1)], cuts[2: (length(cuts))])
	} else {
		#for a matrix
		if(flush[1] == "both" | flush[1] == "min") {
			if(cuts[1,1] > min(as.numeric(x))) { cuts <- rbind(c(min(as.numeric(x)), cuts[1,1]), cuts[1:(nrow(cuts)), 1:2]) }
		}
		if(flush[1] == "both" | flush[1] == "max") {
			if(cuts[nrow(cuts),2] < max(as.numeric(x))) { cuts <- rbind(cuts[1:(nrow(cuts)), 1:2], c(cuts[nrow(cuts), 2], max(as.numeric(x)))) }
		}
	}
	if(overlap > 0) {
		x3 <- median(cuts[1:nrow(cuts), 2] - cuts[1:nrow(cuts), 1], na.rm = TRUE) * overlap
		cuts[1:nrow(cuts), 1] <- cuts[1:nrow(cuts), 1] - x3
		cuts[1:nrow(cuts), 2] <- cuts[1:nrow(cuts), 2] + x3
	}
	subset <- shingle(as.numeric(x), intervals = cuts)
	cuts <- nrow(cuts)
  }

plot1 <- xyplot(as.numeric(y) ~ x | subset, 
		type = type, layout = c(1, cuts), #aspect = aspect, 
		xlab = xlab, ylab = ylab,
		default.scales = list(x = list(relation = "free")),
		#strip = strip.custom(strip.names=c(FALSE,FALSE), style = 3, var.name =c("a","b","c","d","e")),
		...)

#######################################
if(boxplots==FALSE) {
#######################################

plot2 <- histogram( ~ as.numeric(y) | subset, 		
		breaks = hist.breaks,
		layout = c(1, cuts),
		xlim = c(min(as.numeric(y)),max(as.numeric(y))),
		#aspect = aspect, 
		xlab = ylab, #ylab = ylab,
		default.scales = list(x = list(relation = "free"))
		)

#########################################
  } else {

	plot2 <- bwplot( as.numeric(y) ~ as.factor(c(rep(" ",length(y))))  | subset, 		
		panel = function (x,y, ...) {
			panel.violin( x= as.factor(rep(" ",length(y))),y,...)
			panel.bwplot( x= as.factor(rep(" ",length(y))),y,... )
		}, 
		box.ratio = 3,
		horizontal = FALSE,
		layout = c(1, cuts), 
		#xlim = c(min(as.numeric(y)),max(as.numeric(y))),
		#aspect = aspect, 
		xlab = "Distributions", ylab = ylab,
		default.scales = list(x = list(relation = "free")),
		#strip = strip.custom(strip.names=c(FALSE,FALSE), style = 3, var.name =c("a","b","c","d","e"))
		, ...)



  }
#########################################


plot(plot1, position = c(0,0,0.7,1))
plot(plot2, position = c(0.65,0,1,1), newpage = FALSE)

  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ cut.and.stack }
