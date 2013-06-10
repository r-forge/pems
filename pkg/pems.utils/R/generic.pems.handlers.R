##########################
##########################
##generic pems handlers
##########################
##########################

#kr 01/02/2012 v 0.3.1

#includes 
#(functions/code below) 
##########################
#print.pems
#names.pems
#summary.pems
#

#to do
##########################
#so much
#see individual methods 
#all need work


#comments
##########################
#












##########################
##########################
##plot.pems
##########################
##########################

#kr 07/12/2011 v 0.2.0

#what it does
##########################
#generates simple plot
#

#to do
##########################
#make simple plot
#


#comments
##########################
#to tidy
#are id and n good/standard terms
#


##' @S3method plot pems
plot.pems <- function(x, id = NULL, ignore = "time.stamp", n = 3, ...) {

   temp <- x
   class(temp) <- "no.class"

   reply <- temp$data

   if(is.null(reply)){
      message("\npems object [suspect]")
      return(invisible(NULL))
   }

   if(is.null(id)){
       id <- 1:ncol(reply)
       if(length(ignore)>0)
           id <- id[!names(reply) %in% ignore]
       if(n>0 && length(id)>n)
           id <- id[1:n]
   }

   reply <- reply[id]      

   plot(reply, ...)

}
















##########################
##########################
##print.pems
##########################
##########################

#kr 06/06/2013 v 0.3.0

#what it does
##########################
#handles pems console appearance 
#etc
#

#to do
##########################
#confirm happy with this

#comments
##########################
#


print.pems <- function (x, verbose = TRUE, ...) {

    temp <- x
    class(temp) <- "not.pems"

    #show data frame

    if(verbose){
        print.data.frame(temp$data)
    }

    #pems report line 1

    reply <- names(temp$data)
    if (is.null(reply)) 
        message("\npems object: no named data [suspect]")
    else message("\npems object: ", ncol(temp$data), " data series (each ", 
        nrow(temp$data), " cases)")

    #pems report line 2 structure

    reply <- names(temp)[names(temp) %in% c("units", "constants", 
        "history")]
    if (length(reply) < 1) 
        message("\twith no supporting structure [suspect]")
    else message("\twith supporting structures: ", paste(reply, 
        collapse = ", ", sep = ""))

    #pems report line 3 extra tags

    reply <- names(temp)[!names(temp) %in% c("data", "units", 
        "constants", "history", "dem")]
    if (length(reply) > 0) 
        message("\t[and unique tags: ", paste(reply, collapse = ", ", 
            sep = ""), "]\n")

    #output
    invisible(x)

}





##########################
##########################
##names.pems
##########################
##########################

#kr 07/12/2011 v 0.2.0

#what it does
##########################
#returns data series names from pems
#

#to do
##########################
#names<- handling
#

#comments
##########################
#to tidy


##' @S3method print pems
names.pems <- function(x, ...) names(x$data)








##########################
##########################
##summary.pems
##########################
##########################

#kr 07/12/2011 v 0.2.0

#what it does
##########################
#generates summary reports 
#

#to do
##########################
#make dedicated summary 
#and option for as current as alternative
#


#comments
##########################
#to tidy


##' @S3method print pems
summary.pems <- function(object, ...) {

   temp <- object
   class(temp) <- "no.class"

   reply <- temp$data

   if(is.null(reply)){
      message("\npems object [suspect]")
      return(invisible(NULL))
   }

   return(summary(reply))
                       
}










