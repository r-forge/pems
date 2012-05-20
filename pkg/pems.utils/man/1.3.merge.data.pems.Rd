\name{1.3.merge.data.pems}
\alias{1.3.merge.data.pems}
\alias{merge.pems}
\alias{bindPEMS}
\alias{cAlign}
\alias{findLinearOffset}



\title{
  Merging data and pems objects
}

\description{
  Various pems.utils functions to merge data of different types.
}

\usage{

#main function(s)

bindPEMS(x = NULL, y = NULL, ..., by = NULL)

#associated

cAlign(x = NULL, y = NULL, y.offset = 0, all = TRUE,
           suffixes = FALSE)

findLinearOffset(x = NULL, y = NULL, offset.range = NULL)

}



\arguments{

  \item{x, y}{
  (Required objects, typically pems objects, data.frames or vectors) For \code{bindPEMS} 
  and \code{cAlign}, two objects to be bound together. For \code{findLinearOffset}, two 
  objects to be aligned.
} 

  \item{\dots}{
  (Optional) Other arguments, currently passed on to \code{cAlign} and \code{findLinearOffset}.
}

  \item{by}{
  (Optional numeric or character) When \code{bindPEMS} is supplied \code{data.frames} or 
  \code{pems} objects as \code{x} and \code{y} and is asked to use \code{findLinearOffset} 
  to find \code{y.offset}, which elements of \code{x} and \code{y} should be used? (If this 
  is not supplied and required, the default assumption is the first element of each.)
}

  \item{y.offset}{
  (Function or numeric) The number of rows to offset data in \code{y} by relative to data in \code{x} 
  when combining them. For \code{bindPEMS}, this can be either a function that deteremines an 
  offset, e.g. \code{findLinearOffset}, or a numeric value to be used directly as an offset. For \code{cAlign}, 
  only a numeric is allowed.  
} 

  \item{all}{
  (Currently disabled) Argument in revision. 
}

  \item{suffixes}{
  (Logical, or character) Any suffixes to be added to \code{x} and \code{y}. If one 
  character terms is supplied this is used as a suffix for all names in \code{y}. If 
  two character terms are supplied these are used as suffixes for all names in \code{x} 
  and \code{y}, respectively. (The default \code{FALSE} does not add suffixes to either 
  object.) 
}

  \item{offset.range}{
  (Numeric) For \code{findLinearOffset}, the 'lag window' to compare \code{x} and \code{x} 
  across. By default, the function applies the widest possible window.  
} 
    
}


\details{

  \code{bindPEMS} is a column binding function that binds various objects types (e.g. 
  \code{pems} objects, \code{data.frames} and \code{vectors}) and returns results as 
  \code{pems} objects. It uses \code{cAlign} to bind associated data. Also, if requested, 
  it uses \code{findLinearOffset} to handle alignments.   

  \code{cAlign} is a modification of the standard \code{R} column binding function 
  \code{\link{cbind}}. \code{cAlign} binds \code{data.frames} (or \code{vectors}) subject 
  to an applied offset, \code{y.offset}. The row displacement of the second 
  \code{data.frame} relative to the first. Unlike \code{cbind}, \code{cAlign} does not 
  require \code{x} and \code{y} to have the same number of columns. See NOte below.  

  \code{findLinearOffset} is a wrapper for the \code{R} function \code{\link{ccf}}. 
  It is a lag function to find the best linear offset between two data series. 
 
}


\value{

  \code{bindPEMS} returns supplied objects, \code{x} and \code{y}, as a single \code{pems} object, 
  subject to requested alignment and naming modifications.

  \code{cAlign} returns supplied objects, \code{x} and \code{y}, as a single \code{data.frame}, 
  subject to requested alignment and naming modifications. 

  \code{findLinearOffset} returns the best fit offset for \code{y} relative to \code{x}. 

}

\references{
  References in preparation.
}
\author{
  Karl Ropkins
}
\note{

  \code{cAlign} generates offsets and pads out \code{data.frames} of different column lengths by 
  the addition of \code{NAs}. So, \code{data.frames} do not need to be the same column length to 
  be bound, and alignment is subject \code{y.offset}, a numeric giving the starting row for 
  \code{y} relative to \code{x}. \code{data.frame} names are handled using \code{\link{make.names}}, 
  by the \code{suffixes} argument to manage names directly.         
 
}

\section{Warning }{
  No warnings
}

\seealso{
  See \code{\link{cbind}} for standard column binding in R. 
}
\examples{

###########
##example 1 
###########

#some data
temp <- rnorm(500)

#get two offset ranges

x <- temp[25:300]
y <- temp[10:200]

plot(x, type="l"); lines(y, col="blue", lty=2)

#estimated offset

findLinearOffset(x,y)
#[1] -15

#applying linear offset

ans <- cAlign(x,y, findLinearOffset(x,y))

plot(ans$x, type="l"); lines(ans$y, col="blue", lty=2)

}

\keyword{ methods }
