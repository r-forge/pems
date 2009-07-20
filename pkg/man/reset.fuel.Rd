\name{reset.fuel}
\Rdversion{1.1}
\alias{reset.fuel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
reset.fuel(data, fuel = "unknown", fuel.args = NULL, use.defaults = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{fuel}{
%%     ~~Describe \code{fuel} here~~
}
  \item{fuel.args}{
%%     ~~Describe \code{fuel.args} here~~
}
  \item{use.defaults}{
%%     ~~Describe \code{use.defaults} here~~
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
function(data, fuel="unknown", fuel.args=NULL, use.defaults=TRUE) {

#to reset the fuel type of a pems object

#pems objects only
if(!class(data)=="pems") return("kicked out: not a pems object")

#default values
data$fuel <- fuel

if(use.defaults==TRUE) {
	data$constants$alpha.hc <- 1.85 
	data$constants$alpha.exhaust.hc <- 1.85 
	data$constants$beta.oc <- 0 
	data$constants$density.fuel <- 0.725
	data$constants$density.exhaust <- 1.2
  }

ans<-paste("pems fuel reset as ", fuel, sep="")

#do nothing for petrol

#diesel
if(fuel=="diesel" & use.defaults ==TRUE) {
	data$constants$alpha.hc <- 1.90 
	data$constants$alpha.exhaust.hc <- 1.90 
	data$constants$beta.oc <- 0 
	data$constants$density.fuel <- 0.825
	data$constants$density.exhaust <- 1.2 
  }

#other classes
if(!fuel=="unknown" & !fuel=="petrol" & !fuel=="diesel") {
	ans <- paste(ans, " [user class]", sep="")
  }

#further arguments
if(is.null(fuel.args)==FALSE) {
	fuel.args <- fuel.args[c("alpha.hc", "alpha.exhaust.hc", "beta.oc", "density.fuel", "density.exhaust")]
		fuel.args <- fuel.args[na.exclude(names(fuel.args))] # stop user changing non-fuel constants
	data$constants[names(fuel.args)] <- fuel.args[names(fuel.args)]
	ans.2 <- paste(names(fuel.args)[1:(length(fuel.args))],"=",as.character(fuel.args[1:(length(fuel.args))]),sep="",collapse=" ")
	ans <- paste(ans, " [forced constants ",ans.2,"]",sep="")
  }

#export result
data$history <- c(data$history,ans)
output <- data
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ reset.fuel }
