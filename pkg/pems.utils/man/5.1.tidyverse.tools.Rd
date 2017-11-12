\name{5.1.pems.tidyverse.tools}
\alias{5.1.pems.tidyverse.tools}
\alias{pems.tidyverse}
\alias{select_.pems}
\alias{select.pems}
\alias{select}
\alias{rename_.pems}
\alias{rename.pems}
\alias{rename}
\alias{filter_.pems}
\alias{filter.pems}
\alias{filter}
\alias{arrange_.pems}
\alias{arrange.pems}
\alias{arrange}
\alias{slice_.pems}
\alias{slice.pems}
\alias{slice}
\alias{mutate_.pems}
\alias{mutate.pems}
\alias{mutate}
\alias{group_by.pems}
\alias{group_by_.pems}
\alias{group_by}
\alias{groups.pems}
\alias{groups}
\alias{ungroup.pems}
\alias{ungroup}
\alias{group_size.pems}
\alias{group_size}
\alias{n_groups.pems}
\alias{n_groups}
\alias{summarise_.pems}
\alias{summarise.pems}
\alias{summarise}


\alias{fortify.pems}
\alias{fortify}


\title{
Functions to use tidyverse code with pems.utils outputs  
}

\description{
Various codes and methods.
}

\usage{

#pems object methods

#dplyr (1) standard methods
\method{select}{pems}(.data, ...)
\method{rename}{pems}(.data, ...)
\method{filter}{pems}(.data, ...)
\method{arrange}{pems}(.data, ...)
\method{slice}{pems}(.data, ...)
\method{mutate}{pems}(.data, ..., units=NULL, warn=TRUE)
\method{group_by}{pems}(.data, ..., add=FALSE)
\method{groups}{pems}(x)
\method{ungroup}{pems}(x, ...)
\method{group_size}{pems}(x)
\method{n_groups}{pems}(x)
\method{summarise}{pems}(.data, ...)


#dplyr (2) underscore methods
\method{select_}{pems}(.data, ..., warn=TRUE)
\method{rename_}{pems}(.data, ..., warn=TRUE)
\method{filter_}{pems}(.data, ..., warn=TRUE)
\method{arrange_}{pems}(.data, ..., warn=TRUE)
\method{slice_}{pems}(.data, ..., warn=TRUE)
\method{mutate_}{pems}(.data, ..., units=NULL, warn=TRUE)
\method{group_by_}{pems}(.data, ..., add=FALSE, warn=TRUE)             
\method{summarise_}{pems}(.data, ..., warn=TRUE)

#ggplot
\method{fortify}{pems}(model, data, ...)

}


\arguments{
  \item{.data}{
  (pems.object) The pems object to be used with, e.g. dplyr code. 
} 
  \item{\dots}{
  (Optional) Other arguments, currently all passed on to equivalent tridyverse function or method.
}
  \item{warn}{
  (Optional) Give warnings? For an underscore methods: a warning that an underscore method was used 
  (See Below). For mutate: if new elements are generated within unit assignments.
}
  \item{units}{
  (Character) In mutate, the units to assign to new elements created by call. See Below.
}
  \item{x}{
   (grouped_df) Object class used by group_by and related dplyr grouping functions. 
}
  \item{add}{
   (Optional) Argument used by group_by and related dplyr grouping functions. 
}
  \item{model}{
  (pems.object) The pems object to be used as a data source when plotting using ggplot2 code. See 
  below.
}
  \item{data}{
  (Optional) Currently ignored by fortify.pems. See below.
}


}

\details{

  The pems object methods \code{select}, \code{rename}, \code{filter}, 
  \code{arrange}, \code{slice}, \code{mutate}, \code{group_by}, 
  \code{summarise} ...
  are similar to data.frame methods in \code{dplyr}, but (hopefully) they 
  also track units, etc, like a pems object. Work in progress. See below, 
  especially Note. 

  Equivalent underscore methods (\code{select_}, etc) are also provided, although 
  it should be noted that their future in dplyr itself is not certain.

  The \code{fortify} method was developed by Hadley Wickham to simplify the integration of 
  \code{ggplot2} functions and special object classes. It is a really nice idea for multiple 
  reasons, the main one being that package users will probably never have to worry about it. 

}


\value{
  \code{select} returns the requested part of the supplied pems object, e.g.:
  \code{select(pems.1, velocity)} returns the velocity element of pems.1 as 
  a single column pems.object, consistent with the data.frame handling of 
  \code{select.data.frame}.

  \code{rename} returns the supplied pems object with the requested name change, 
  e.g.: \code{rename(pems.1, speed=velocity)} returns pems.1 with the velocity 
  column renamed speed.

  \code{filter} returns the supplied pems object after the requested filter 
  operation, e.g.: \code{filter(pems.1, velocity>0.5)} returns pems.1 after 
  excluding all rows where the velocity value was less than or equal to 0.5.

  \code{arrange} returns the supplied pems object reordered based on order 
  of values in an identified element, e.g.: \code{arrange(pems.1, velocity)} 
  returns pems.1 with its row reordered lowest to highest velocity entry.

  \code{slice} returns requested rows of the supplied pems object, e.g.: 
  \code{slice(pems.1, 1:10)} returns rows 1 to 10 of pems.1 as a new pems 
  object.

  \code{mutate} returns the supplied pems object with extra elements calculated 
  as requested, e.g.: \code{mutate(pems.1, new=velocity*2)} returns the pems object 
  with additional column, called new, which is twice the values in the velocity 
  column. The units of the new column can be set using the additional argument 
  units, e.g. \code{mutate(pems.1, new=velocity*2, units="ick")}.

  \code{group_by} returns a \code{grouped_df} object. [to document]

  \code{summarise} [to document]  

  \code{fortify} is intended for used with ggplot2 code when both pems.utils and 
  ggplot2 are loaded.

}
\references{
  Generics in general:

  H. Wickham. Advanced R. CRC Press, 2014.

  (Not yet fully implemented within this package.)

  ggplot2:

  H. Wickham. ggplot2: elegant graphics for data analysis. Springer New
  York, 2009.

  (See Chapter 9, section 9.3, pages 169-175, for discussion of fortify)
}

\author{
  Karl Ropkins
}
\note{
  Currently not sure what I think about tidyverse, but it is always 
  interesting, and ideas like fortify are really nice.  
}

\section{Warning }{
  This currently work in progress - handle with care
}

\seealso{
  [to do] 
}
\examples{

###########
##example 1 
###########

#TO DO

}

\keyword{ methods }
