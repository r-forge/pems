\name{edit.pems}
\Rdversion{1.1}
\alias{edit.pems}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
to do
}
\description{
to do
}
\usage{
edit.pems(pems)
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
#pems editor
####################
#karl 06/03/2009
#version 0.0.1
## restricts access/editoring

############
##to be completed but basics there

##for function arg like records=c(... list of pems directories to be edited...)
##		default all allowed, e.g. pems, vehicle, fuel, constants
##		wrap each editor call in an if to stop user calling illegals, accessing data, etc.  
##for each edit, edir method?, restricts, resets & tidy up of editor layouts?
##for ans$history entry to say editor call made or changes made?

##pems$constants <- edit(pems$constants)
##pems$pems <- edit(pems$pems)

message("Currently, edit access to pems objects is restricted.")
message("Use reset functions to admend pems attributes.")

output <- pems
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ methods }