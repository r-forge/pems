\name{1.1.make.import.data}
\alias{1.1.make.import.data}
\alias{pems.objects}
\alias{isPEMS}
\alias{makePEMS}
\alias{makePEMSElement}
\alias{import2PEMS}
\alias{importTAB2PEMS}
\alias{importCSV2PEMS}
\alias{importOBS2PEMS}
\alias{importOB12PEMS}
\alias{importRoyalTek2PEMS}



\title{
  making and importing data
}

\description{
  Various pems.utils functions to make and import data as pems objects.
}

\usage{

#making pems objects
 
isPEMS(x, full.test = TRUE, ...)

makePEMS(x, units = NULL, constants = NULL, history = NULL,  
          ...)

makePEMSElement(x, name = NULL, units = NULL,   
          ...)


#importing data as pems objects

#general

import2PEMS(file.name = file.choose(), time.stamp = NULL, local.time = NULL,
          time.format = NULL, units = NULL, constants = NULL, history = NULL, 
          ..., file.type = NULL, file.reader = read.delim)

importTAB2PEMS(..., file.reader = read.delim)

importCSV2PEMS(..., file.reader = read.csv)


#Horiba OBS

importOBS2PEMS(file.name = file.choose(), pems = "Horiba OBS", 
          constants = NULL, history = NULL, 
          analytes = c("co", "co2", "nox", "hc"),  
          fuel = c("petrol", "diesel", "gasoline"), ...)

importOB12PEMS(file.name = file.choose(), pems = "Horiba OBS", 
          constants = NULL, history = NULL, 
          analytes = c("co", "co2", "nox", "hc"),  
          fuel = c("petrol", "diesel", "gasoline"), ...)

#RoyalTek GPS

importRoyalTek2PEMS(file.name = file.choose(), 
          file.type = c("special", "txt", "nmea"),
          vbox = "RoyalTEk", history = NULL, constants = NULL, ...) 

}


\arguments{

  \item{x}{
  (A required object) For \code{isPEMS}, any object to be tested as a \code{pems} object. 
  For \code{makePEMS}, an object to be used as the starting point to make a \code{pems} 
  objects, so typically a \code{data.frame} or another \code{pems} object. 
} 

  \item{full.test}{
  (Logical) For \code{isPEMS}, should the full pems test be applied and the \code{pems} 
  structure confirmed?  
} 

  \item{\dots}{
  (Optional) Other arguments, handling varies. For \code{isPEMS} these are ignored. For 
  \code{makePEMS} these are added to the pems object unmodified. For \code{import...} 
  functions, these are passed on and added to the \code{constants} component of the 
  \code{pems} object. Note: This different handling is experimental and may be subject to 
  change in future. 
}

  \item{units, constants, history}{
  (Default pems arguments) These are arguments that are routinely generated for 
  \code{pems} objects. \code{units} holds unit ids for unit management, \code{constants} holds 
  constants that should used specifically with data in the \code{pems} object, and \code{history} 
  holds the \code{pems} object modification history.   
}

  \item{name}{
  (Default pems.element argument) \code{name} (and \code{units}) are arguments that are routinely 
  generated for \code{pems.element} objects.    
}



  \item{file.name}{
  (file connection, etc.) For \code{import...} functions, the file/source to be imported. Note: the 
  default, \code{file.name = file.choose()}, automatically opens a file browser if this argument is 
  not supplied.   
}

  \item{time.stamp, local.time, time.format}{
   Relatively crude \code{import...} functions are useful for importing data from the clipboard or simple 
   file types. However, these sometimes need careful handling of time data. If supplied, \code{time.stamp} 
   and \code{local.time} are used as indices or ids (column numbers or names) for data series that the 
   user would like to use as the data time stamp and local time records, respectively. If supplied, 
   \code{time.format} sets the format in which the \code{time.stamp} should be imported if present/idenified.    
}

  \item{file.type, file.reader}{
  Data reader parameters for some \code{import...} functions. \code{file.type} is the type of file 
  to be imported. Note: Some \code{import...} functions can handle more than one file type, and 
  \code{file.type = "[option]"} should be used to identify these. (Note: \code{file.type} options 
  are typically file type identifiers, such as the file extensions, and a default \code{'special'}, 
  which leaves the choice to the function. This way this option can typically be ignored unless, e.g. 
  the function does not recognise the file type but the user knows it and wants to force the method.) 
  \code{file.reader} identifies the R method/function that should be used to read data from the supplied 
  file. For example, for \code{importTAB2PEMS} and \code{importCSV2PEMS}, by default, these are the 
  standard \code{R} \code{read...} functions \code{\link{read.delim}} and \code{\link{read.csv}}, 
  respectively.   
}

  \item{pems, vbox}{
  (Character vectors) For some \code{import...} functions, data source descriptions may be automatically 
  added to the \code{pems} object. \code{pems} and \code{vbox} are two examples, but others, such as 
  vehicle and fuel descritpions can also be added in a similar fashion. Note: These are for user-reference, 
  so can say whatever you want.
}

  \item{analytes}{
  (Character vector) For \code{import...} functions, the names of any pems elements to be tagged as 
  analyte concentrations. Note: If the PEMS unit reports concentrations rather than emissions it is 
  often useful to identify these at import to avoid confusion, and to simplify later handling. So, if 
  encountered, analyte names are prefixed with the term \code{'conc.'}.    
}

  \item{fuel}{
  Some \code{import...} functions that handle exhaust monitoring system data may assume fuel types when 
  calibrating inputs or calculating constants. In such cases the \code{fuel} argument is also included 
  to identify which fuel was used. 
}
    
}


\details{

  \code{isPEMS} tests if an object is/is not a \code{pems} object.

  \code{makePEMS} makes a \code{pems} object using supplied data and information. 

  Crude \code{import...} functions import simple file structures, and are useful for getting 
  data quickly into \code{R:pems.utils}. \code{importTAB2PEMS} imports tab delimited files and 
  clipboard content. \code{importCSV2PEMS} imports comma delimited files. Both assume a simple 
  file structure (i.e. data series in columns with names as headers), by require some time data 
  management by the user. Note: These are wrappers for \code{import2PEMS}. 

  Other \code{import...} import specific file types.

  \code{importOBS2PEMS} imports standard Horiba OBS files and converts them to \code{pems} 
  objects. See Notes below. 

  \code{importOB12PEMS} imports \code{.OB1} files and converts them to \code{pems} objects. \code{OB1} 
  files are generated by a Visual Basic PEMS data handler used during the RETEMM project. Notes below. 

  \code{importRoyalTek2PEMS} imports \code{.txt} and \code{.nmea} format Royal Tek GPS files and 
  converts them to \code{pems} objects. See Notes below. 
 
}


\value{

  \code{isPEMS} return a logical, \code{TRUE} if the supplied object is \code{pems} class, 
  otherwise \code{FALSE}. If the argument \code{full.test = TRUE} is also supplied, additional 
  information about the object is returned as \code{comment(output)}. 

  \code{makePEMS} returns a \code{pems} object, made using the supplied data and any additional 
  information also supplied in the same call. 
 
  \code{import...} functions return a \code{pems} object, made using the supplied file and any 
  additional information also supplied in the same call. 

}

\references{
  References in preparation.
}
\author{
  Karl Ropkins
}
\note{

  With the crude \code{import...} functions (\code{import2PEMS}, \code{importTAB2PEMS}, 
  \code{importCSV2PEMS}) modifications are minimal. Unless any additional changes are requested 
  in the \code{import...(...)} call, the data is simply read in as a \code{data.frame} and 
  converted to a \code{pems} object.  

  With \code{importOBS2PEMS}, OBS data is also modified as follows: data series names are simplified and 
  converted to lower case to simplify use in R; the data series \code{time.stamp} and \code{local.time} 
  are added (generated using the file time stamp, the row counter and the log.rate constant); 
  data series \code{latitude} and \code{longitude} are resigned according to set N/S and E/W values, 
  if these are present/valid; \code{latitude} and \code{longitude} units are also reset to \code{'d.degLat'} 
  and \code{'d.degLon'}. Any data series names in \code{analytes} is renamed \code{'conc.[analyte name]'}. 
  If not supplied in the \code{importOBS2PEMS}, typical OBS constants are currently assumed. Several of these 
  are based on emission source fuel. Defaults for these are generated according to \code{fuel} (default 
  \code{'petrol'}).

  With \code{importOB12PEMS}, handling is similar to that with \code{importOBS2PEMS}. 

  With \code{importRoyalTek2PEMS}, the Royal Tek data modifications are currently being documented.   
 
}

\section{Warning }{
  Currently, \code{makePEMS} and \code{import...} functions handle extra arguments differently. (See 
  Arguments above for details.) This may be subject to change.
}

\seealso{
  See \code{\link{ref.unit.conversions}} and \code{\link{convertUnits}} for general unit handling. 
}
\examples{

###########
##example 1 
###########

#make little pems
data <- data.frame(speed=1:10, emissions=1:10)
units <- c("m/s", "g/s")
pems <- makePEMS(x = data, units=units, example="my record") 
 
pems                       #the pems object
summary(pems)              #summary of held data
checkInput(speed, pems)    #speed element

}

\keyword{ methods }
