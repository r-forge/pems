\name{summary.pems}
\Rdversion{1.1}
\alias{summary.pems}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
summary.pems(pems, short.hand = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pems}{
%%     ~~Describe \code{pems} here~~
}
  \item{short.hand}{
%%     ~~Describe \code{short.hand} here~~
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
function (pems, short.hand = TRUE) {
########################
#pems summary over-ride
#version 0.0.1
#karl (28/02/2009)

########################
#TO DO
########################
#test (data and unit names same)
##
#comprehensive summary to do

message(paste("pems object: ", deparse(substitute(pems)), sep = ""))

if(short.hand==TRUE) { 
	
	#shorthand summary
	message("index:")
	print(names(pems))
	message(paste("data: ", ncol(pems$data), " by ", nrow(pems$data), " (", ncol(pems$data)*nrow(pems$data),") entries", sep="")) 
	print(names(pems$data))
	message(paste("units: ", length(pems$units), " entries", sep=""))
	if(identical(names(pems$data),names(pems$units))) {
		message("   data/names checksum GOOD")
	} else {
		message("   data/names checksum BAD")
	}
	message(paste("pems: ", pems$pems, sep = ""))
	message(paste("vehicle: ", pems$vehicle, sep = ""))
	message(paste("fuel: ", pems$fuel, sep = ""))
	message(paste("constants: ", ncol(pems$constants), " entries", sep = ""))
	tidy <- paste("history: ", length(pems$history), " logged action", sep = "")
		if(!length(pems$history)==1) { tidy <- paste(tidy, "s", sep = "") } 
	message(tidy)

  } else {

	#comprehensive summary
	message("index:")
	print(names(pems))
	message(paste("data: ", ncol(pems$data), " by ", nrow(pems$data), " (", ncol(pems$data)*nrow(pems$data),") entries", sep="")) 
	print(summary(pems$data))
	message(paste("units: ", length(pems$units), " entries", sep=""))
	if(identical(names(pems$data),names(pems$units))) {
		message("   data/names checksum GOOD")
	} else {
		message("   data/names checksum BAD")
	}
	print(pems$units)
	message(paste("pems: ", pems$pems, sep = ""))
	message(paste("vehicle: ", pems$vehicle, sep = ""))
	message(paste("fuel: ", pems$fuel, sep = ""))
	message(paste("constants: ", ncol(pems$constants), " entries", sep = ""))
	print(pems$constants)
	tidy <- paste("history: ", length(pems$history), " logged action", sep = "")
		if(!length(pems$history)==1) { tidy <- paste(tidy, "s", sep = "") } 
	message(tidy)
	print(pems$history)

  }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ screen.pems }
