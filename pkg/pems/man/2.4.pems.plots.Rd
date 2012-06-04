\name{2.4.pems.plots}
\alias{2.4.pems.plots}
\alias{pems.plots}
\alias{WatsonPlot}
\alias{latticePlot}
\alias{panel.PEMSXYPlot}
\alias{XYZPlot}


\title{
  Various plots for pems.utils
}

\description{
  Various plot functions and visualization tools for \code{pems} objects.
}

\usage{

#defined plots

WatsonPlot(speed = NULL, accel = NULL, time = NULL, distance = NULL, 
          drop.idle.time = TRUE, ..., data = NULL,
          fun.name = "WatsonPlot", hijack= FALSE)

#flexible plots

latticePlot(x = NULL, data = NULL, plot = xyplot, panel = NULL, 
          ..., greyscale = FALSE, fun.name = "latticePlot", 
          hijack = FALSE)

panel.PEMSXYPlot(..., grid=NULL)

XYZPlot (x = NULL, ..., data = NULL, statistic = NULL, 
          x.res = 10, y.res = 20, plot = levelplot,
          fun.name = "XYZPlot", hijack = FALSE)

}




\arguments{

  \item{speed, accel, time, distance}{
  (data series, typically vectors) For defined plots, data series to be used to generate a 
  given plot. Only some of these may be required. See Notes.

}

  \item{drop.idle.time}{
  (logical) For \code{WatsonPlot}, should the time the vehicle is idling be discarded before 
  generating the \code{WatsonPlot}.

}

  \item{\dots}{
  (Optional) Other arguments, currently passed on to \code{plot} and \code{panel}.
}


  \item{data}{
  (Optional \code{data.frame} or \code{pems} object) The data source elements in \code{x} 
  if not the current environment or a parent.
} 

  \item{fun.name, hijack}{
  (Various) \code{pems.utils} management settings, can typically be ignored by most users.
}

  \item{x}{
  (Required formula) For flexible plots. \code{pems.utils} uses the formula method of 
  the \code{\link{lattice}} package. For \code{latticePlot} plot calls take the 
  form \code{latticePlot(y ~ x | cond, ...)}. For \code{XYZPlot} plot calls take the 
  form \code{XYZPlot(Z~ y * x | cond, ...)}. See Details, Notes, Warnings and Examples.  
} 

  \item{plot, panel}{
  (Optional functions) The functions to use to generate the plot framework and the individual 
  plot panels. For \code{latticePlot}, these are by default the \code{\link{lattice}} functions 
  \code{\link{xyplot}} and \code{\link{panel.xyplot}}. For \code{XYZPlot}, currently only 
  \code{plot} is forced, and this is by default the \code{\link{lattice}} function 
  \code{\link{levelplot}} 
}

  \item{greyscale}{
  (Logical) For \code{latticePlot}, should the plot be greyscale by default? This option 
  resets the \code{lattice} color themes to greyscale while the plot is beging generated. 
  So: (1) This only effects the plot itself, not subsequent plots; and, (2) any user resets 
  overwrite this, e.g. \code{latticePlot(..., greyscale=TRUE, col="red"} will place red symbols 
  on an overwise greyscale plot. See Warning.
}

  \item{grid}{
  (List) If supplied, a list of plot parameters to be used to control the appearance of the 
  grid component of the plot. See Below.
}

  \item{statistic}{
   (Function) when binning data with \code{XYZPlot}, the function to use when evaluation the 
   elements of each data bin.
}

  \item{x.res, y.res}{
   (Numerics) when binning data with \code{XYZPlot}, the number of x- and y-axis bins to 
   generate.
}
    
}


\details{

  Defined plots operate like the main R plot function, \code{plot}. \code{x} and \code{y} 
  data series are supplied as individual arguments: in the form \code{plot(x, y)}. 

  The \code{WatsonPlot} uses speed and acceleration data series as \code{x} and \code{y} 
  inputs to generate a standard speed/acceleration frequency plot, but can accept a range of 
  different input combinations to generate this information, e.g.:

  time and distance (time and distance -> speed, time and speed -> accel)
 
  time and speed (time and speed -> accel)

  speed and accel

  Flexible plots operate like plots in the \code{\link{lattice}} package, and define the 
  plot structure using a formula format/structure:

  For \code{latticePlot} the formula structure is \code{y ~ x | cond}, where 
  \code{y} is the data series to use as the y-axis, \code{x} is the data series to 
  as the x-axis and \code{cond} is an addition 'conditioning' data series which is 
  used to separate the data into different subplots. 

  For \code{XYZPlot} the formula structure is \code{Z~ y * x | cond}, 
  \code{z} is the data series to use as the z-axis or z element of the plot,  
  \code{y} is the data series to use as the y-axis, \code{x} is the data series to 
  as the x-axis and \code{cond} is an addition 'conditioning' data series which is 
  used to separate the data into different subplots. \code{z} is optional, but When 
  it is not supplied \code{z} it is treated as the bin count. 

  \code{latticePlot} is a wrapper for a number of \code{\link{lattice}} and 
  \code{latticeExtra} plot function modifications that simplify routine 
  handling of plotted data. See Examples. 

  \code{panel.PEMSXYPlot} is a simple gridded panel function intended for use 
  with the \code{panel} argument of \code{latticePlot} or \code{\link{lattice}} 
  plot functions directly. 

  \code{XYZPlot} is a wrapper for a number of \code{\link{lattice}} plot functions 
  that provide 'xyz' data visualisations. 

  See Examples, Warnings and Note. 
 
}


\value{

  \code{WatsonPlot} generates a speed/acceleration frequency plot.

  \code{latticePlot} is a wrapper for various \code{\link{lattice}} and 
  \code{latticeExtra} functions that make nice graphs very quickly. It 
  generates trellis-style graphical outputs based on 'xy' data sets.

  \code{XYZPlot} generates trellis-style graphical outputs based on 'xyz' data 
  sets.

}

\references{
  
  Watson plots:

  Watson, H.C., Effects of a wide range of drive cycles on the emissions from vehicles 
  of three levels of technology. SAE Technical Paper Series. Paper No. 950221, 1995.

  lattice:

  Sarkar, Deepayan (2008) Lattice: Multivariate Data Visualization with
  R. Springer, New York. ISBN 978-0-387-75968-5

  latticeExtra:

  Deepayan Sarkar and Felix Andrews (2011). latticeExtra: Extra Graphical Utilities
  Based on Lattice. R package version 0.6-18. 
  http://CRAN.R-project.org/package=latticeExtra

  \code{\link{lattice}} is one of number of really nice graphical tools in \code{R}. 
  Others, like \code{ggplot2}, \code{hexplot} and \code{iplot}, help you to very 
  quickly explore your data. But, for me the trellis framework of \code{\link{lattice}} 
  has always been the most flexible.

}
\author{
  Karl Ropkins
}
\note{
         
  \code{plot} options for \code{latticePlot}: The default option is \code{xyplot}.
 
  \code{panel} options for \code{latticePlot}: The default option is \code{panel.xyplot}. The 
  addition panel, \code{panel.PEMSXYPlot} supplied as part of this package adds a grid layer 
  to a standard xy panel. It is simply made using two panels, \code{panel.grid} and 
  \code{panel.xyplot}, both in \code{lattice}. \code{edit{panel.PEMSXYPlot}} to have a look at 
  it. The extra code just allows you to pass specific plot parameters to the grid panel using 
  the argument \code{grid}. You can build almost any plot layout using these and other panels 
  in \code{lattice} as building blocks. 

  \code{plot} options for \code{XYZPlot}: The default option is \code{levelplot}.

  Other arguments: Like most other plot functions in \code{R}, \code{\link{lattice}} functions 
  use a number of common parameter terms. For example, \code{xlab} and \code{ylab} reset the x and 
  y labels of a grpah; \code{xlim} and \code{ylim} set the x- and y-scales of a graph; \code{col} 
  sets the color of a plot element; \code{type} sets the type ('p' for points, 'l' for lines, etc); 
  \code{pch} and \code{cex} set plot symbol type and size, respectively; and, \code{lty} and 
  \code{lwd} set plot line type and thickness, respectively; etc. These terms are passed onto and 
  evaluated by all these plot functions to provide standard plot think control.    

  The reason for \code{latticePlot}: \code{latticePlot} combines a number of \code{lattice} and 
  \code{latticeExtra} functions of modifications I regularly use when plotting data. So, it is 
  basically a short cut to save having to write out a lot of code I regularly use. I would encourage 
  anyone to at the very least have a look at \code{lattice}. I also hope those learning 
  \code{lattice}, find \code{latticePlot} a helpful introduction and handy 'stop gap' while they 
  are getting to grips with the code behind trellis and panel structures.

}

\section{Warning }{
  
  IMPORTANT: Conditioning is currently disabled on \code{XYZPlot}.

  \code{XYZPlot} is a short-term replace for previous function \code{quickPlot}. It will most likely 
  be replaced when \code{pems.utils.0.3} is released.
  
  The \code{greyscale} argument is a recent addition to latticePlot. I think I have reset all 
  default colors, but may have missing something. Please let me know if you spot anything still 
  colored and I'll get it fixed as soon as possible. Thanks.
}

\seealso{
  See \code{\link{lattice}}.

}
\examples{

###########
##example 1 
###########

#basic usage of defined plots

WatsonPlot(velocity, time=local.time, data=pems.1)



###########
##example 2 
###########

#basic usage of flexible plots

#latticePlot

latticePlot(velocity~local.time, data = pems.1, type = "l")

#in lattice, xyplot(velocity~local.time, data = getData(pems.1), type = "l")
#Note: to use lattice functions directly with pems objects
#      just pass data component with data = getData(pems) 
#      (NOT data = pems)

latticePlot(velocity~local.time, data = pems.1, col = "red", 
            pch = 20, panel = panel.PEMSXYPlot, 
            grid = list(col ="black", lty=2))


#XYZPlot

a <- calcAccel(velocity, local.time, data =pems.1)

#like WatsonPlot
#(But with idle data included in plot)
XYZPlot(~accel*velocity, data=a)

XYZPlot(~accel*velocity, data=a, plot = wireframe, shade=TRUE)



}

\keyword{ methods }
