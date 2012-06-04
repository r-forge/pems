##########################
##########################
##make pems
##########################
##########################

#kr

#description
##########################
#functions for making pems object


#includes 
##########################
#isPEMS
#makePEMS

#to do
##########################

#comments
##########################






##########################
##########################
##isPEMS
##########################
##########################

#kr 23/10/2010 v 0.0.1

#what it does
##########################
#is.pems -two level tester

#to do
##########################
#make test more robust?

#comments
##########################
#widely used. 
#think carefully before changing name or argument ordering


isPEMS <- function(x, full.test = TRUE, ...){

   #standard test
   output <- if(is(x)[1]=="pems") TRUE else FALSE
   #full.test
   if(full.test){
       if(is.null(x)) comment(output) <- "NULL" else 
           if(is(x)[1]=="pems") comment(output) <- "pems" else
               if(is.data.frame(x)) comment(output) <- "data.frame" else
                    comment(output) <- "other"
   }
   #output
   output
}






##########################
##########################
##makePEMS
##########################
##########################

#kr 23/10/2010 ver 0.0.1

#what it does
##########################
#make pems objects from parts

#to do
##########################
#is.null handling for args
#is.wrong handling for args
#defaults for constants
##########################
#

#comments
##########################
#widely used. 
#think carefully before changing name or argument ordering


makePEMS <- function(x, units = NULL, constants = NULL,  
                     history = NULL, ...){

#################
#currently assuming 
# x = data.frame
#################

#reported issue if data.frame[1,n]


    #data has dimension to work with
    if(!is.null(ncol(x)) && ncol(x)>0){

        #units
        if(is.null(units))
            units <- rep(NA, ncol(x))
        if(!is.data.frame(units)){
            units <- as.data.frame(t(units), stringsAsFactors = FALSE)
            units <- if(ncol(units)<ncol(x))
                         cbind(units, as.data.frame(t(rep(NA, ncol(x)-ncol(units))), stringsAsFactors = FALSE)) else
                             units[1:ncol(x)] 
            names(units) <- c(names(x), names(units), rep(NA, ncol(x)))[1:ncol(x)]
        }

    }

#to do
####################
#update constants

    #history
    history <- if(is.null(history))
                   c(match.call()) else c(history, match.call())    

    #output
    output <- list(data = x, units = units, constants = constants, 
                   history = history)

    #add in ... args
    temp <- list(...)
    output[names(temp)] <- temp

    class(output) <- "pems"
    invisible(output)
}
