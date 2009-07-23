\name{accum.path}
\Rdversion{1.1}
\alias{accum.path}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
accum.path
}
\description{
to do
}
\usage{
accum.path(pems)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pems}{
%%     ~~Describe \code{pems} here~~
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
\keyword{ methods }
