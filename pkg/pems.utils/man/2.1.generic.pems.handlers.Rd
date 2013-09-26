\name{generic.pems.handlers}

\alias{generic.pems.handlers}
\alias{pems.generics}
\alias{as.data.frame.pems}
\alias{$.pems}
\alias{[.pems}
\alias{names.pems}
\alias{print.pems}
\alias{plot.pems}
\alias{summary.pems}
\alias{units.pems}


\title{ Generic handling of pems objects }

\description{
  pems objects can be manipulated using generic functions like print, plot and 
  summary in a similar fashion to objects of other R classes. 
}

\usage{


\method{as.data.frame}{pems}(x,...)

\method{$}{pems} (x, i, ...)

\method{[}{pems} (x, i, j, ..., force = FALSE, simplify = TRUE)

\method{names}{pems}(x, ...)

\method{print}{pems}(x, verbose = FALSE, ...)

\method{plot}{pems}(x, id = NULL, ignore = "time.stamp", n = 3, ...)

\method{summary}{pems}(object, ...)

\method{units}{pems}(x)

}

\arguments{

  \item{x, object}{ 
  (An Object of pems class). For direct use with \code{print}, \code{plot}, \code{summary}, 
  etc. NOTE: Object naming (i.e., \code{x} or \code{object}) is determined in parent or 
  base function in R, so naming can vary by method.
}

  \item{i, j}{ 
  Column and row indices, which operate in a similar fashion to \code{data.frame} indices.
}

  \item{...}{ 
  Addition options, typically passed to associated default method(s).
}

  \item{force, simplify}{ 
  (Logicals) Data element handling options: \code{force} forces data to fit it destination; 
  \code{simplify} minimised \code{pems} objects to \code{pems.elements}.
}

  \item{verbose}{ 
  (Logical, TRUE/FALSE). Should the longer form of the output be returned? By default, functions 
  with short and long output versions return the short form.
}
   
  \item{id, ignore, n}{ 
  (local plot parameters). \code{id} identifies which data series to plot; \code{ignore} identifies 
  which data series to ignore when leaving the choice of \code{id} to the function; and, \code{n} gives 
  the maximum number of data series to plot when leaving the choice of \code{id} to the function.
}

}

\value{
  Generic functions provide appropriate (conventional) handling of objects of 
  \code{'pems'} class: 

  \code{as.data.frame(pem.object)} extracts the \code{data.frame} component of a 
  pems object. 

  \code{pems.object$element.name} extracts the named element from the pems objects 
  in a similar fashion to \code{data.frame$element.name}.   

  \code{pems.object[i, j]} extracts the [i.j] elements of the data held in the 
  \code{pems} object. This is returned as either a \code{pems} or \code{pems.element} 
  object depending on the dimension of the extracted data and the \code{simplify} 
  setting.  

  \code{print(pems.object)} provides a (to console) description of that \code{pems} 
  object. 

  \code{plot(pems.object)} generates a standard R plot using selected data series in 
  that \code{pems} object.
 
  \code{names(pems.object)} returns a vector of the names of data series held in a 
  \code{pems} object. 

  \code{summary(pems.object)} generates a summary report for data series held in a 
  \code{pems} object. 

  \code{units(pems.object)} extracts the units from the supplied \code{pems} object. 

}

\author{ Karl Ropkins }

\examples{

\dontrun{

#make object
print(pems.object)
names(pems.object)

} 

}

\keyword{methods}
