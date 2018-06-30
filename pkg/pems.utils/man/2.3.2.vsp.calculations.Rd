\name{2.3.2.vsp.calculations}
\alias{2.3.2.vsp.calculations}
\alias{vsp.calculations}
\alias{vsp}
\alias{calcVSP}
\alias{calcVSP_JimenezPalaciosCMEM}
\alias{binVSP}
\alias{binVSP_NCSU.14}
\alias{binVSP_MOVES.23}



\title{
  Vehicle Specific Power (VSP) calculations
}

\description{
  Functions associated with VSP calculations.
}

\usage{

calcVSP(speed = NULL, accel = NULL, slope = NULL, 
          time = NULL, distance = NULL, data = NULL,
          calc.method = calcVSP_JimenezPalaciosCMEM,
          ..., fun.name = "calcVSP", this.call = NULL)

calcVSP_JimenezPalaciosCMEM(speed = NULL, accel = NULL, 
          slope = NULL, vehicle.weight = NULL, vsp.a = NULL, 
          vsp.b = NULL, vsp.c = NULL, vsp.g = NULL, ..., data = NULL,  
          fun.name = "calcVSP_JimenezPalaciosCMEM", 
          this.call = NULL)

binVSP(..., bin.method="ncsu.14")

#associated methods
binVSP_NCSU.14(vsp = NULL, data = NULL, 
          ..., fun.name="binVSP_NSCU.14")
binVSP_MOVES.23(vsp = NULL, speed = NULL, data = NULL, 
          ..., fun.name="binVSP_MOVES.23")

}




\arguments{

  \item{speed, accel, slope, time, distance, vsp}{
  (Typically pems.element vectors) \code{speed}, \code{accel}, \code{slope}, 
  \code{time} and \code{distance} are possible inputs for VSP calculation. 
  \code{vsp} and \code{speed} are possible inputs for VSP binning methods. 
  (See Notes about inputs and methods.)
} 

  \item{data}{
  (Optional, typically \code{pems}) The data source for inputs.
} 

  \item{calc.method, bin.method}{
  (Required functions) \code{calc.method} is the function used to calculate VSP (default 
  \code{calcVSP_JimenezPalaciosCMEM}). \code{bin.method} is the methods used when binning 
  VSP measurements. (See Notes.)
}

  \item{\dots}{
  (Optional) Other arguments, currently passed on as supplied to e.g.   
  \code{calc.method} (default \code{calcVSP_JimenezPalaciosCMEM}) and \code{pems.utils}  
  functions as appropriate.
}

  \item{fun.name}{
  (Optional character) The name of the parent function, to be used in error messaging.
}

  \item{this.call}{
  (Optional) Initial call, should generally be ignored. See code{\link{common.calculations}} 
  for further details.
}

  \item{vehicle.weight, vsp.a, vsp.b, vsp.c, vsp.g}{
  (Numerics) VSP constants. If not supplied, defaults are applied. See Below.
}
    
}


\details{

  \code{calcVSP...} functions calculate VSP using:

  \code{calcVSP} is a wrapper function which allows users to supply different combinations 
  of inputs. VSP calculations typically require speed, acceleration and slope inputs. This 
  wrapper allows different input combinations, e.g.:

  time and distance (time and distance -> speed, time and speed -> accel)
 
  time and speed (time and speed -> accel)

  speed and accel

  This then passes on \code{speed}, \code{accel} and (if supplied) \code{slope} to the method 
  defined by \code{calc.method}. (This means other VSP functions run via 
  \code{calcVSP(..., calc.method = function)} share this option without needed dedicated code.)

  \code{calcVSP_JimenezPalaciosCMEM} calculates VSP according to Jimenez Palacios and CMEM methods. 
  See References and Note below.

  \code{binVSP} generates VSP Mode bins depending on method applied. 

  \code{method} 'ncsu.14' uses supplied \code{vsp} and the 14 bin method described in Frey et al 2002; 
  \code{method} 'moves.23' uses supplied \code{vsp} and \code{speed} and the 23 bin methods from 
   MOVES (See Note).   
 
}


\value{

  \code{calcVSP_JimenezPalaciosCMEM} and \code{calcVSP} by default use Jimenez Palacios and CMEM 
  methods to calculate VSP (in kW/metric ton).

  \code{binVSP} generates a \code{pems.element factor} vector of VSP Mode Bins. 

}


\section{Warning }{
  \code{calcVSP_JimenezPalaciosCMEM} does not currently have special case for buses as of 
  Giannelli et al (2005). (Please let me know if you need to use them.)  
}

\note{

  \code{calcVSP...} constants can be set/modified in the calculation call, e.g. 
  \code{calcVSP(..., vsp.a = [new.value])}. If not supplied, defaults are used (see 
  References). 

  \code{binVSP_MOVES.23} is in-development. Do not use without independent confirmation 
  of values.  

%This option under review
%If VSP constants are to be added to a \code{pems} object, these should 
%  have the prefix 'vsp.', so for, e.g., \code{a} is stored in pems constants are 
%  \code{vsp.a}. This is because the common VSP designations (\code{a}, \code{b}, \code{c}, 
%  etc.) can be very easily wrongly assigned. 
  
  Unit handling in \code{pems.utils} is via \code{\link{checkUnits}}, \code{\link{getUnits}}, 
  \code{\link{setUnits}} and \code{\link{convertUnits}}. See \code{\link{common.calculations}} 
  for details.
 
}



\seealso{
  See \code{\link{common.calculations}} (and \code{\link{checkInput}}, \code{\link{checkUnits}} and 
  \code{\link{convertUnits}}) for details of data management.
}

\author{
  Karl Ropkins
}

\references{
  
 \code{calcVSP_JimenezPalaciosCMEM} uses methods described in:

 Jimenez-Palacios, J.L. (1999) Understanding and Quantifying Motor Vehicle Emissions with 
 Vehicle Specific Power and TILDAS Remote Sensing. PhD Thesis, Massachusetts Institute of 
 Technology, Cambridge, MA.

 Giannelli, R.A., Nam, E.K., Helmer, K., Younglove, T., Scora, G., and Barth, M. (2005) 
 Heavy-Duty Diesel Vehicle Fuel Consumption Modelling Based on Road Load and Power Train 
 Parameters. SAE Technical Papers, No, 05CV-3.

 \code{vehicle.weight} is the vehicle mass (in metric tons), and \code{vsp.a}, \code{vsp.b}, 
 \code{vsp.c} and \code{vsp.g} are the calculations constants for:

 \code{vsp = speed * (a * accel + (g * slope) + b) + (c * speed^3)}

 In all cases, by default b = 0.132, g = 9.81, and if not supplied \code{slope} is assumed to be 
 zero and \code{vehicle.weight} is assumed to 1.5 metric tons.

 For vehicles < 3.855 metric tons; a = 1.1, and c = 0.000302 (as of Jimenez-Palacios, 1999).

 For vehicles 3.855 to 6.350 metric tone; a = 0.0996m/2204.6, c = 1.47 + 5.22e-5m/2205 (as of 
 Giannelli et al, 2005).

 For vehicles 6.350 to 14.968 metric tone; a = 0.0875m/2204.6, c = 1.93 + 5.90e-5m/2205 (as of 
 Giannelli et al, 2005).

 For vehicles > 14.968 metric tone; a = 0.0661m/2204.6, c = 2.89 + 4.21e-5m/2205 (as of 
 Giannelli et al, 2005).

 \code{binVSP} method \code{ncsu.14} uses methods described in:
 
 Frey, H.C., Unal, A., Chen, J., Li, S. and Xuan, C., 2002. Methodology for developing modal 
 emission rates for EPA's multi-scale motor vehicle & equipment emission system. Ann Arbor, 
 Michigan: US Environmental Protection Agency. (EPA420-R-02-027)

}


\examples{

###########
##example 1 
###########

#basic usage

vsp <- calcVSP(velocity, time = local.time, data = pems.1)
#where the returned object, vsp, is vsp values as pems.element

ncsu.14 <- binVSP(vsp)
#where the returned object, nscu.14, is the associated modal bin 
# assignments based on the Frey et al (2002) 14 bin method. 

}

\keyword{ methods }
