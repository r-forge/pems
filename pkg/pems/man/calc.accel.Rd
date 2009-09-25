\name{calc.accel}
\Rdversion{1.1}
\alias{calc.accel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate Acceleration
}
\description{
Generic function for the calculation of vehicle acceleration. The function can be used with both pems 
objects and standard data frames, but only considers data series units when used with pems objects. 
}
\usage{
calc.accel(pems, speed = velocity, time = local.time, fun.method = "standard.accel", fun.args = NULL, vis.fun = TRUE, fun.output = "pems")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pems}{
Object containing data to be used in calculation. If this is a pems object it uses pems$units to check units 
and reports acceleration in m/s/s. Otherwise, the object is handled as a standard data frame and inputs should 
be top level(e.g. data.frame$speed and data.frame$time).   
}
  \item{speed}{
Name of data series containing vehicle speed profile. Default 'velocity'.
}
  \item{time}{
Name of time data series. Default 'local.time'.
}
  \item{fun.method}{
Method to be used to calculate acceleration. Currently, the function only has the default method 'standard.accel' 
enabled.
}
  \item{fun.args}{
List of parameters to be used to calculate acceleration. Currently, the only recognised option is dt, which fixes the 
acceleration time interval. If set, dt supercedes time. Not employed in default calculation. 
}
  \item{vis.fun}{
Option to visualise function outputs (TRUE/FALSE). Default, TRUE. 
}
  \item{fun.output}{
Calculation output mode. Default 'pems' returns the orginal pems object with acceleration calculations added as 'accel'. 
Otherwise, it returns a single column data frame 'accel' containing the acceleration calculations. See value below.
}
}
\details{
Method 'standard.accel' calculates acceleration using d(speed)/d(time) (or d(speed)/dt if dt set in fun.args. 
}
\value{
If fun.output set to 'pems', the original pems object is returned with acceleration calculations added as 'accel'. If that 
object is of pems class, it also updates pems$units and logs the operation to pems$history.  
}
\references{
N/A - conventional method.
}
\author{
Karl.
}

%%\note{...}
%% ~Make other sections like Warning with \section{Warning }{....} ~

%%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~}

\examples{
##-- To be written
##-- To be written
##-- To be written

\keyword{ methods }