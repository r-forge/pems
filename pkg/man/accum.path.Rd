\name{accum.path}
\Rdversion{1.1}
\alias{accum.path}
\title{
%%  function to generate accum.path plot 
}
\description{
%%  work in progress
}
\usage{
accum.path(pems)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pems}{
%%     pems. A pems object or other dataframe
}
}
\details{
%%  work in progress
}
\value{
%%  work in progress
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% work in progress
}
\author{
%%  karl ropkins
}
\note{
%%  this is a work in progress
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
function(pems) {
#temp function
require(lattice)

data <- pems$data
y <- data$em.co2
x <- data$velocity
x2 <- data$local.time

plot2 <- xyplot(y~x2, type="l")


x <- cumsum(x * 0.27777777778)
y <- cumsum(y)


#s <- c(rep(1, length(y)), rep(2,length(y)))
#y <- rep(y,2)
#x <- c(x,x2)

#xyplot(y~x | s)

text1<-"accum CO2 (grams)"
plot1 <- xyplot(y~x, ylab =text1, xlab = "Distance travelled (meters)")
plot2 <- xyplot(y~x2, ylab =text1, xlab = "Time take (seconds)")

plot(plot1, position = c(0,0,1,0.525))
plot(plot2, position = c(0,0.475,1,1), newpage = FALSE)




  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ accum.plot }
\keyword{ pems }
