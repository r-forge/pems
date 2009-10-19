\name{summary.pems}
\Rdversion{1.1}
\alias{summary.pems}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
summary.pems
}
\description{
class-specific version of the summary function
}
\usage{
summary(pems, short.hand = TRUE)
summary.pems(pems, short.hand = TRUE)
}

\arguments{
  \item{pems}{
Object of pems class
}
  \item{short.hand}{
Option for shorthand version of summary output. Default TRUE.
}
}
\details{
The standard call summary(object) is handled for an object of class pems by summary.pems(). The function includes one 
additional option, short.hand = TRUE/FALSE, which provides short- and long-hand versions of the summary report, 
respectively. Both include an object integrity checksum based of the comparison of pems$data and pems$units. 
}
\value{

}
\references{
N/A...
}
\author{
Karl
}
\note{
This is a working version
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%%}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ methods }