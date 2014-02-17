\name{generic.pems.element.handlers}

\alias{generic.pems.element.handlers}
\alias{pems.element.generics}
\alias{print.pems.element}
\alias{plot.pems.element}


\title{ Generic handling of pems.element objects }

\description{
  pems elements objects can be manipulated using generic functions like print, plot and 
  summary in a similar fashion to objects of other R classes. 
}

\usage{

\method{print}{pems.element}(x, ...)

\method{plot}{pems.element}(x, y = NULL, xlab = NULL, ylab = NULL, ...)

}

\arguments{

  \item{x}{ 
  (An Object of pems.element class). For direct use with \code{print}, \code{plot}, \code{summary}, 
  etc. NOTE: Object naming (i.e., \code{x} or \code{object}) is determined in parent or 
  base function in R, so naming can vary by method.
}

  \item{...}{ 
  Addition options, typically passed to associated default method(s).
}

  \item{y, xlab, ylab}{ 
  (other plot arguments). As with the default \code{\link{plot}} method, \code{y} is an optional 
  second data verctor, typically numeric, and \code{xlab} and \code{ylab} are labels to use on 
  x and y axes.
}

}

\value{
  Generic functions provide appropriate (conventional) handling of objects of 
  \code{'pems.elements'} class: 

  \code{print(pems.element)} provides a (to console) description of the 
  supplied \code{pems.element} object. 

  \code{plot(pems.element)} generates a standard R plot of the supplied 
  \code{pems.element}.  
 
}

\author{ Karl Ropkins }

\examples{

\dontrun{

#the velocity pems.element in pems.1
pems.1$velocity

} 

}

\keyword{methods}
