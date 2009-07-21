\name{import.obs}
\Rdversion{1.1}
\alias{import.obs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
import.obs(filename, pems = "Horiba OBS-1300", analytes = c("co", "co2", "nox", "hc"), vehicle = "unknown", fuel = "unknown", fuel.args = NULL, use.defaults = TRUE, history = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
%%     ~~Describe \code{filename} here~~
}
  \item{pems}{
%%     ~~Describe \code{pems} here~~
}
  \item{analytes}{
%%     ~~Describe \code{analytes} here~~
}
  \item{vehicle}{
%%     ~~Describe \code{vehicle} here~~
}
  \item{fuel}{
%%     ~~Describe \code{fuel} here~~
}
  \item{fuel.args}{
%%     ~~Describe \code{fuel.args} here~~
}
  \item{use.defaults}{
%%     ~~Describe \code{use.defaults} here~~
}
  \item{history}{
%%     ~~Describe \code{history} here~~
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
function(filename, 
		pems = "Horiba OBS-1300", 
		analytes = c("co", "co2", "nox", "hc"), 
		vehicle = "unknown",
		fuel = "unknown",
		fuel.args = NULL,
		use.defaults = TRUE, 
		history = "")
	{
#import a standard OBS-1300 tab delimited file

####
#to do
####
#rethink use.defualts: name, use, etc.
#if changed update call and arg transfer in reset.fuel
#add a journey record?

#time and date stamps
time.stamp <- scan(filename, nlines = 1, what = character(), quiet=TRUE)
	time.stamp <- paste(time.stamp[2], time.stamp[4], sep=" ")
	time.stamp <- as.POSIXct(strptime(time.stamp, format = "\%Y/\%m/\%d \%H:\%M:\%S", "GMT"))

#read headers
data.names <- scan(filename, skip=1, what = character(), nlines = 1, quiet=TRUE, sep="\t")
	data.names <- gsub(" ", ".", data.names) #replace space with "." in header names
	data.names <- gsub("/", ".", data.names) #replace "/" with "." in n/s and e/w header names
	data.names <- tolower(data.names) #simplify naming
	data.names[1] <- "local.time"
	f00 <- function(ans, analyte) #make analyte identifier conc.analyte so emission names are unique 
			{if(ans==analyte) ans <- paste("conc.",ans,sep="") else ans}
	for(i in 1:length(analytes)){ data.names <- sapply(data.names, f00, USE.NAMES=FALSE, analyte=analytes[i]) }

#read units
data.units <- scan(filename, skip=2, what = character(), nlines = 1, quiet=TRUE, sep="\t")
	data.units <- c("Y-M-D H:M:S GMT",data.units)
	f00<- function(ans) #strip brackets from strings
		{if(!ans=="")  
			{if(substr(ans,1,1)=="(" & substr(ans,nchar(ans),nchar(ans))==")" ) (substr(ans,2,(nchar(ans)-1))) else ans} 
		else ans}
	data.units <- sapply(data.units, f00, USE.NAMES=FALSE)
	#names(data.units) <- data.names

#read data
data <- read.delim(filename, header=FALSE, skip=3)
	names(data) <- data.names
	data<-cbind(time.stamp = (time.stamp + data$local.time), data)

	data.units <- as.data.frame(t(data.units))
	names(data.units) <- names(data)

#create history
if(history=="") 
	{history <- "initial file created using import.obs"} else
	{history <- c("initial file created using import.obs", history)}

#create constants
constants <- data.frame(
		log.rate = 1000,
		delay.co = 3.2, delay.co2 = 3.3, delay.hc = 3.9, delay.nox = 1.6, delay.afr = 1.6,
		mm.c = 12.011, mm.h = 1.0079, mm.0 = 15.999,
		mm.co = 28.01, mm.co2 = 44.01, mm.nox = 46.01,
		conc.o2 = 20.6, 
 		alpha.hc = 1.85, alpha.exhaust.hc = 1.85, beta.oc = 0, density.fuel = 0.725, density.exhaust = 1.2, 
		thc.c6 = 10,
		pitot.k = 2537.6, pitot.z = 0.015026,
		setting.gps.port = 0, k.wgec = 3.5, setting.velocity = 1, setting.720nox = 1, setting.gps = 1, setting.hc = 0, 
		setting.option = "", setting.coco2hc = "", setting.afr = ""
	) 

#default constants assume petrol vehicle, hc by ndir, etc.

#set up PEMS object

ans<-list(data = data, units = data.units, pems = pems, vehicle = vehicle, fuel = fuel, constants = constants, history = history)
	class(ans) <- "pems"	

if(!fuel=="unknown") {
	ans <- reset.fuel(ans, fuel, fuel.args, use.defaults)
  }

output <- ans
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
\keyword{ import.obs }
