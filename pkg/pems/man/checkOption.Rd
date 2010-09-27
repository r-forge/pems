\name{checkOption}
\alias{checkOption}

\title{
checkOption 
}

\description{
Option selection/selection checking for pems functions.
}

\usage{
checkOption(option = NULL, allowed.options = NULL, 
    option.name = "option", allowed.options.name = "allowed.options", 
    partial.match = TRUE, fun.name = "checkOption", 
    output = c("option", "test.result"),
    ...)
}


\arguments{
  \item{option}{
  (Character, numeric, etc, vector) The supplied option. Note: Only the first entry of 
  \code{option} is currently considered by this function.  
}
  \item{allowed.options}{
  (Character, numeric, etc, vector) The series of available options to test \code{option} 
  against.
}
  \item{option.name, allowed.options.name}{ (Character vectors) How \code{option} and 
  \code{allowed.options} are to be identified in any associated error messaging. 
}
  \item{partial.match}{
  (Boolean) Option to use partial matching (via \link{\code{pmatch}}). Default \code{TRUE}, 
  otherwise, applies exact matching (via\link{\code{match}}).  
}
  \item{fun.name}{
  (Optional character vector) If \code{checkOption} is used as a workhorse routine within another function, 
  \code{fun.name} to assign any associated error messaging to the parent function rather 
  than the workhorse. 
}
  \item{output}{
  (Character vector) Output mode for \code{checkOption} results. Current options: \code{"option"} (default), 
  and \code{"test.result"}. If the supplied \code{option} object is valid, \code{"option"} 
  returns the full option term as defined in \code{allowed.options}, and 
  \code{"test.result"} returns TRUE/FALSE if \code{option} can/can not be matched against 
  \code{allowed.options}.     
}
  \item{\dots}{
  (Optional) Other arguments, currently ignored.
}
}

\details{
  None currently.
}
\value{
  None currently.
}
\references{
  None currently
}
\author{
  Karl Ropkins
}
\note{
  None currently
}

\section{Warning }{
  None currently
}

\seealso{
  None currently
}
\examples{
##---- To be confirmed
}

\keyword{ methods }
