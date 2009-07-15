\name{edit.pems}
\alias{edit.pems}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
edit.pems(pems)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pems}{ ~~Describe \code{pems} here~~ }
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
function(pems) {
#pems editor
## restricts access/editoring

############
##to be completed but basics there

##for function arg like records=c(... list of pems directories to be edited...)
##		default all allowed, e.g. pems, vehicle, fuel, constants
##		wrap each editor call in an if to stop user calling illegals, accessing data, etc.  
##for each edit, edir method?, restricts, resets & tidy up of editor layouts?
##for ans$history entry to say editor call made or changes made?

pems$constants <- edit(pems$constants)
pems$pems <- edit(pems$pems)

output <- pems
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
