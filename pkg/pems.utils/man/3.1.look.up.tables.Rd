\name{3.1.look-up.tables}
\alias{3.1.look-up.tables}
\alias{ref.unit.conversions}


\docType{data}

\title{
   reference data for use with pems.utils
}

\description{
   Various reference and example datasets intended for use with functions in 
   \code{pems.utils}. 
}

\usage{ref.unit.conversions}

\format{
  \code{ref.unit.conversions}: Unit conversion methods stored as a list of lists. See Details 
}

\details{
  \code{unit.conversions} is basically a 'look-up' for unit conversion methods. Each element of 
  the list is another list. These lists are each individual conversion methods comprising four 
  elements: \code{to} and \code{from}, character vectors given the unit ids and alias of the unit 
  types that can be converted using the method; \code{conversion}, a function for the associated 
  conversion method; and (possibly) \code{tag}, a more detailed description of the conversion 
  intended for use in documentation.      
}

\note{
  \code{ref.unit.conversions} can be updated locally. See \code{\link{convertUnits}}, 
  \code{\link{addUnitConversion}}, etc.
}

\source{
  TO BE COMPLETED
}

%\references{
%%  ~~ possibly secondary sources and usages ~~
%website link for .csv and database versions?
%}

\examples{

#basic structure
ref.unit.conversions[[1]]

}

\keyword{datasets}
