\name{checkPems}
\alias{checkPems}

\title{
checkPems 
}

\description{
pems objects check function for the pems package.
}

\usage{
checkPems(pems = NULL, fun.name = NULL, 
    output = c("data", "pems", "test.result"), 
    ...)
}


\arguments{
  \item{pems}{
  (pems or data.frame object) Data supplied for analysis/testing for use with the \code{pems} package. 
  Note: The package is currently set up to run with either pems objects or data.frames.  
}
  \item{fun.name}{
  (Optional character vector) If \code{checkPems} is used as a workhorse routine within another function, 
  \code{fun.name} to assign any associated error messaging to the parent function rather 
  than the workhorse. 
}
  \item{output}{
  (Character vector) Output mode for \code{checkPems} results. Current options: \code{"data"} (default), 
  \code{"pems"} and \code{"test.result"}. If the supplied \code{pems} object is valid, \code{"data"} extracts 
  and returns the core data.frame for analysis, \code{"pems"} returns \code{pems} unchanged, and 
  \code{"test.result"} returns a character vector description of the object: \code{"pems"}, \code{"data.frame"} 
  or \code{"other"}.     
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
