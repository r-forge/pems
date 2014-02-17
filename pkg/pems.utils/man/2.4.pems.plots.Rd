\name{2.4.pems.plots}
\alias{2.4.pems.plots}
\alias{pems.plots}
\alias{latticePlot}
\alias{panel.PEMSXYPlot}
\alias{XYZPlot}
\alias{pemsPlot}
\alias{panel.pemsPlot}
\alias{preprocess.pemsPlot}
\alias{pemsXYZCondUnitsHandler}


\title{
  Various plots for pems.utils
}

\description{
  Various plot functions and visualization tools for \code{pems} objects.
}

\usage{

pemsPlot(x, y = NULL, z = NULL, ..., data = NULL, cond = NULL, 
         units = TRUE, fun.name="pemsPlot", panel = panel.pemsPlot, 
         scheme = pems.scheme)

#elements

pemsXYZCondUnitsHandler(x, y = NULL, z = NULL, cond = NULL, 
         data = NULL, units = TRUE, ..., fun.name = "pemsXYZCondHandler", 
         hijack= FALSE)

preprocess.pemsPlot(lattice.like = lattice.like, units = units, ...)

panel.pemsPlot(..., plot = TRUE, process = TRUE, loa.settings = FALSE)


#old plots

latticePlot(x = NULL, data = NULL, plot = xyplot, panel = NULL, 
          ..., greyscale = FALSE, fun.name = "latticePlot", 
          hijack = FALSE)

panel.PEMSXYPlot(..., grid=NULL)

XYZPlot(x = NULL, ..., data = NULL, statistic = NULL, 
          x.res = 10, y.res = 20, plot = levelplot,
          fun.name = "XYZPlot", hijack = FALSE)

}




\arguments{

  \item{x, y, z, cond}{
  (Various) The main plot elements. \code{x} and \code{y} are typically plot 
  coordinates. \code{z} is any additional information that could be used , e.g. 
  to modify points plotted at \code{(x,y)} coordinates or generate a third axis 
  for a surface plot. \code{cond} is a conditioning terms that can be used to 
  subset the supplied data and generate multiple plot panels. See Note Below.

  For (pemsPlot), these can be supplied either individually or as a formula, e.g.

  \code{pemsPlot(x, y, z, cond)} or \cr
  \code{pemsPlot(z~x*y|cond)}

  For other earlier plots, these must be formulae.         

  For \code{latticePlot} the basic formula structure is \code{y ~ x | cond}, where 
  \code{y} is the data series to use as the y-axis, \code{x} is the data series to 
  as the x-axis and \code{cond} is an addition 'conditioning' data series which is 
  used to separate the data into different subplots. 

  For \code{XYZPlot} the basic formula structure is \code{z ~ y * x | cond}, 
  \code{z} is the data series to use as the z-axis or z element of the plot,  
  \code{y} is the data series to use as the y-axis, \code{x} is the data series to 
  as the x-axis and \code{cond} is an addition 'conditioning' data series which is 
  used to separate the data into different subplots. \code{z} is optional, but When 
  it is not supplied \code{z} it is treated as the bin count. 
  
  See Notes, Warnings and Examples.  
} 

  \item{data}{
  (Optional \code{data.frame} or \code{pems} object) The data source elements in \code{x} 
  if not the current environment or a parent.
} 

  \item{units}{
  (Optional \code{logical} or \code{list}) for \code{pemsPlot} only Unit handling information. 
  By default, \code{pems.utils} adds any known units to plot labels and allows in-plot unit 
  management. Unit management is handled by \code{convertUnits}, and requested conversions 
  need to assigned to an axis. So, for example, the call \code{pemsPlot(..., x.to="m/s")} 
  would generate a plot with the x-axis in units of m/s (assuming \code{pems.utils} knows 
  the unit conversion). \code{units.add.to.labels = FALSE} removes label unit suffixes but 
  retains unit management. \code{units = FALSE} disables all unit-related plot handling.  
  }

  \item{\dots}{
  (Optional) Other arguments, typically passed on.
  }

  \item{fun.name, hijack}{
  (Various) \code{pems.utils} management settings, can typically be ignored by most users.
  }

  \item{panel}{
  (Function) The function to use to generate the individual plot panels. 
  }

  \item{scheme}{
  (\code{character} or \code{list}) The default coloring scheme for the plot. See Note 
  below.
  }
  
  \item{lattice.like, plot, process, loa.settings}{
  (Various) Plot management elements. These can typically be ignored by most users, but 
  allow plot developers to fine-tune the appear of plots. See Note below.
  }

  \item{greyscale}{
  (Logical) For older plots. Should the plot be greyscale by default? This option resets 
  the \code{lattice} color themes to greyscale while the plot is beging generated. So: (1) 
  This only effects the plot itself, not subsequent plots; and, (2) any user resets overwrite 
  this, e.g. \code{latticePlot(..., greyscale=TRUE, col="red"} will place red symbols on an 
  overwise greyscale plot. Newer plots use the alternative \code{plot(..., scheme = "greyscale")}.
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

  \code{pems.utils} includes conventional \code{plot} methods for \code{pems} and 
  \code{pems.element} objects. 

  However, it also includes a range of higher level plotting functions developed using 
  the \code{\link{lattice}} and \code{\link{loa}} packages. 

  Early plots, e.g. \code{latticePlot}, only allowed plot arguments using the 
  \code{\link{lattice}} formula format. While this is flexible and very powerful 
  system, some users preferred the more conventional \code{plot(x,y,..)} call format. 
  So, newer plots, e.g. \code{pemsPlot}, allow both conventional plot and 
  \code{\link{lattice}}-style formula calls. 

  \code{pemsXYZCondUnitsHandler} handles the two plot call styles by converting 
  conventional plot calls to formula-based calls. It is typically included in a function 
  function like \code{pemsPlot} at an early stage. After it is run, the returned \code{x} 
  can be treated as a formula and \code{y}, \code{z} and \code{cond} can be ignored. This 
  routine is included as a discrete function within this package and others are welcome to 
  use elsewhere for similar purposes. \code{edit(pemsPlot)} to see.  

  Newer plots also use a combination of \code{\link{lattice}} and \code{\link{loa}} 
  functions to provide a range of additional plotting options, such as integrated panel and key 
  management. See \code{\link{loa}} documentation for further details.   

  \code{preprocess...} and \code{panel...} functions handle pre-plot and in-plot elements 
  of plot generation. These use the \code{\link{loa}} modification of the \code{\link{lattice}} 
  plotting framework.    

  See Examples, Notes and Warnings. 
 
}


\value{

  \code{pemsPlot} generates a bubble plot, so it plots \code{(x,y)} points, and 
  by default color-grades and size-scales them according to \code{z}.  

  \code{latticePlot} and \code{XYZPlot} are general purpose 'xy' and 'xyz' data 
  plotting functions.  

}

\references{
  
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
         
  General, other arguments: 

  Like most other plot functions in \code{R}, \code{\link{lattice}} functions 
  use a number of common parameter terms. For example, \code{xlab} and \code{ylab} reset the x and 
  y labels of a graph; \code{xlim} and \code{ylim} set the x- and y-scales of a graph; \code{col} 
  sets the color of a plot element; \code{type} sets the type ('p' for points, 'l' for lines, etc); 
  \code{pch} and \code{cex} set plot symbol type and size, respectively; and, \code{lty} and 
  \code{lwd} set plot line type and thickness, respectively; etc. These terms are passed onto and 
  evaluated by all these plot functions to provide standard plot control. 

  \code{latticePlot}:
  
  The default \code{plot} option for \code{latticePlot} is \code{xyplot}. 

  \code{panel} options for \code{latticePlot}: Default \code{panel.xyplot}. The 
  alternative panel, \code{panel.PEMSXYPlot} supplied as part of this package, adds a 
  grid layer to a standard xy panel. It is simply made using two panels, \code{panel.grid} 
  and \code{panel.xyplot}, both in \code{lattice}. \code{edit{panel.PEMSXYPlot}} 
  to have a look at it. The extra code just allows you to pass specific plot parameters to the grid panel 
  using the argument \code{grid}. You can build almost any plot layout using these and other panels 
  in \code{lattice} as building blocks. 


  \code{XYZPlot}:

  The default \code{plot} option for \code{latticePlot} is \code{levelplot}. 
   
  \code{pemsPlot}:

  \code{pemsPlot} and subsequent plot functions use an alternative convention. Here, \code{plot} is 
  a logical, which is used with another logical, \code{process}, to tell the plot function that 
  data processing and plotting steps need to be handled separately. This option is particularly useful 
  if a calculation is carried in-plot panel that affects the plot appearance and this needs to be 
  integrated with other panels and keys. The handling mechanism is part of the \code{\link{loa}}
  package. 


  The reason for \code{latticePlot}, etc: 

  \code{latticePlot} combines a number of \code{lattice} and \code{latticeExtra} functions modifications 
  I regularly use when plotting data. So, it is basically a short cut to save having to write out a lot 
  of code I regularly use. I would encourage anyone to at the very least have a look at \code{lattice}. 
  I also hope those learning \code{lattice}, find these functions a helpful introduction and handy 'stop 
  gap' while they are getting to grips with the code behind trellis and panel structures.

}

\section{Warning }{
  
  IMPORTANT: Conditioning is currently disabled on \code{XYZPlot}.

  \code{XYZPlot} is a short-term replace for previous function \code{quickPlot}. It will most likely 
  be replaced when \code{pems.utils.0.3} is released.
  
  With all these functions I have tried to make the default plotting options as robust as possible. 
  However, it is impossible to test all the plot combines that users might try. So, please let me know 
  if anything is not working nicely for you. Thanks.

}


\seealso{
  See \code{\link{lattice}}, \code{latticeExtra}, \code{\link{loa}}.
  
}
\examples{

###########
##example 1 
###########

#basic usage of latticePlot

latticePlot(velocity~local.time, data = pems.1, type = "l")

#in lattice, xyplot(velocity~local.time, data = getData(pems.1), type = "l")
#Note: to use lattice functions directly with pems objects
#      just pass data component with data = getData(pems) 
#      (NOT data = pems)

latticePlot(velocity~local.time, data = pems.1, col = "red", 
            pch = 20, panel = panel.PEMSXYPlot, 
            grid = list(col ="black", lty=2))


###########
##example 2 
###########

#basic usage of XYZPlot

a <- calcAccel(velocity, local.time, data = pems.1)

XYZPlot(~accel*velocity, data=a)

XYZPlot(~accel*velocity, data=a, plot = wireframe, shade=TRUE)


###########
##example 3 
###########

#basic usage of pemsPlot

#like latticePlot example above 
pemsPlot(~local.time*velocity, data=pems.1, type="l")

#note formula format z~x*y, with z ignored for stanard scatter plot

#or in conventional plot(x, y, ...) format
#pemsPlot(local.time, velocity, data=pems.1, type="l")


}

\keyword{ methods }
