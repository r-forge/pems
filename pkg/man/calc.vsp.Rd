\name{calc.vsp}
\Rdversion{1.1}
\alias{calc.vsp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
calc.vsp(pems, speed = velocity, accel = accel, fun.method = "jimenez.palacios", fun.args = list(a = 1.1, b = 0.132, c = 0.001208, g = 9.81), vis.fun = TRUE, fun.output = "pems")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pems}{
%%     ~~Describe \code{pems} here~~
}
  \item{speed}{
%%     ~~Describe \code{speed} here~~
}
  \item{accel}{
%%     ~~Describe \code{accel} here~~
}
  \item{fun.method}{
%%     ~~Describe \code{fun.method} here~~
}
  \item{fun.args}{
%%     ~~Describe \code{fun.args} here~~
}
  \item{vis.fun}{
%%     ~~Describe \code{vis.fun} here~~
}
  \item{fun.output}{
%%     ~~Describe \code{fun.output} here~~
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
function(pems,
		speed = velocity, 
		accel = accel,
		fun.method = "jimenez.palacios",
		fun.args = list(a = 1.1, b = 0.132, c = 0.001208, g = 9.81),
		vis.fun = TRUE,
		fun.output = "pems"
		){

###############
#calculate vehicle specific power 
#version 0.0.1
#source
#karl (16/03/2009)
#
#currently uses
#jimenez.palacios as default
#VSP (kW/metric Ton) = v(m/s) * (1.1*a(m/s/s) + 9.81*grade(\%) + 0.132) + 0.001208*v^3
#
#assumes
#wind speed and grade are negligible

###############
#TO DO
###########
#complete this current calc.accel
#add accel option to calc accel if not found
#


#VSP = v * (a*(1+?) + g*grade + g*CR) + 2?*CD*A*v3/m
#where:
#v: is vehicle speed (assuming no headwind) in m/s
#a: is vehicle acceleration in m/s2
#?: is mass factor accounting for the rotational masses (~0.1)
#g: is acceleration due to gravity
#grade: is road grade
#CR: is rolling resistance (~0.0135)
#Younglove/Scora/Barth 5
#?: is air density (1.2)
#CD: is aerodynamic drag coefficient
#A: is the frontal area
#m: is vehicle mass in metric tonnes.
#Using typical values of coefficients, in SI units the equation becomes (CDA/m ~ 0.0005):
#VSP (kW/metric Ton) = v * (1.1*a + 9.81*grade(\%) + 0.132) + 0.001208*v^3
#If we examine the same example vehicle activity sets described above, we can plot histograms of VSP values as shown in Figure 2. Again, the FTP is seen to be fairly mild and the MEC01 cycle is very aggressive with a maximum value near 400 kW/metric ton. Vehicle #14 actually had higher VSP values than the MEC01, extending beyond 400.

#Jimenez-Palacios, J. (1999) Understanding and Quantifying Motor Vehicle Emissions and Vehicle Specific Power with TILDAS Remote Sensing, MIT Doctoral Thesis.

require(lattice)

#for non-pems use
if(is(pems)[1]=="pems") {
	if(is.na(match(deparse(substitute(speed)), names(pems$data)))) {
		stop(paste("\t calc.vsp: '", as.character(substitute(speed)), "' not found in pems", sep = ""), call. = FALSE, domain = NA)
	} 	
	if(is.na(match(deparse(substitute(accel)), names(pems$data)))) {
		stop(paste("\t calc.vsp: '", as.character(substitute(accel)), "' not found in pems", sep = ""), call. = FALSE, domain = NA)
	} 	
	data <- pems$data
  } else {
	if(is.na(match(deparse(substitute(speed)), names(pems)))) {
		stop(paste("\t calc.vsp: '", as.character(substitute(speed)), "' not found in (non-)pems", sep = ""), call. = FALSE, domain = NA)
	} 	
	if(is.na(match(deparse(substitute(accel)), names(pems)))) {
		stop(paste("\t calc.vsp: '", as.character(substitute(accel)), "' not found in (non-)pems", sep = ""), call. = FALSE, domain = NA)
	} 	
	data <- pems
  }

this.speed <- data[, deparse(substitute(speed))]
this.speed.name <- as.character(substitute(speed))

this.accel <- data[, deparse(substitute(accel))]
this.accel.name <- as.character(substitute(accel))

#####################################################
#main routines

#reset/check units units
# if it is a pems
this.vsp.units <- "unknown"
if(is(pems)[1]=="pems") {
	this.unit <- as.character(pems$units[, this.speed.name])	
	this.correction <- "not found"
	if(this.unit=="km/h") { this.correction = 0.27777777778 }
	if(this.unit=="m/s") { this.correction = 1 }
	if(this.correction=="not found") {
		stop(paste("\t calc.vsp: ", this.speed.name, " units not recognised", sep=""), call. = FALSE, domain = NA)
	}
	this.speed <- this.speed * this.correction
	
	this.unit <- as.character(pems$units[, this.accel.name])
	if(!this.unit=="m/s/s") { 
		stop(paste("\t calc.vsp: ", this.speed.accel, " units not recognised", sep=""), call. = FALSE, domain = NA)
 	}
  }

#################################
#vsp calculates

this.method <- "unknown"

if(fun.method=="jimenez.palacios"){
	#VSP (kW/metric Ton) = v(m/s) * (1.1*a(m/s/s) + 9.81*grade(\%) + 0.132) + 0.001208*v^3
	this.grade <- 0
	this.vsp <- this.speed * (fun.args$a * this.accel + (fun.args$g * this.grade) + fun.args$b) + (fun.args$c * this.speed^3)
	this.method <- "jimenez.palacios"
	this.vsp.units <- "kW/metric Ton"
  }

##################################
#reject if no vsp
if(this.method=="unknown"){
	stop(paste("\t calc.vsp: ", fun.method, " not recognised", sep=""), call. = FALSE, domain = NA)
  }

####################################
#package data
data <- cbind(data, vsp = this.vsp)
#setup pems info
if(is(pems)[1]=="pems") {
	pems$history <- c(pems$history, paste("calc.vsp: vsp generated using '", this.speed.name, "', '", this.accel.name, "' and '", this.method, "' method", sep = ""))
	pems$units[length(pems$units) + 1] <- this.vsp.units
	names(pems$units)[length(pems$units)] <- "vsp"
  }
        
#plot if required
if(vis.fun){
	plot(this.speed,type="l")
	lines(this.vsp,col="blue")
  }

if(is(pems)[1]=="pems") {
        pems$data <- data
        pems
  } else {
        pems <- data
  }

if(fun.output=="pems") { pems } else { this.vsp }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ calc.vsp }
