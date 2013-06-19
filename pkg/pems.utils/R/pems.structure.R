########################
########################
##pems.structure
########################
########################

#in place
#################
#pemsElement
#pemsData
#



#TO DO
################
#tidy
#document
#



#questions
###################
#is this better than check...
#





########################
########################
##pemsElement
########################
########################

#version 0.2.0
#karl 17/09/2010

pemsElement <- function(input, pems=NULL, ..., 
         fun.name = "pemsElement", if.missing = "stop",
         input.name = deparse(substitute(input))){

   #reorder this later

    if(is.null(pems)) {
       ans <- try(input, silent = TRUE)
       if(is(ans)[1] == "try-error" || is.function(ans)){ 
           checkIfMissing(if.missing = if.missing,
                          reply = paste("element '", input.name[1], "' not found", sep=""),
                          suggest = "checking call arguments", 
                          if.warning = NULL, 
                          fun.name = fun.name)
           ans <- NULL
       }
       if(!is.null(ans))
           if(is.null(attributes(ans)$name)) attr(ans, "name") <- input.name
       return(ans)   
    }

    pems <- checkPEMS(pems)
    ans <- try(pems$data[,input.name], silent = TRUE)
    units <- try(pems$units[1,input.name], silent = TRUE)

    if(is(ans)[1] == "try-error" || is.function(ans)){ 
        ans <- try(input, silent = TRUE)
        if(is(ans)[1] == "try-error" || is.function(ans)){ 
             checkIfMissing(if.missing = if.missing,
                          reply = paste("element '", input.name[1], "' not found", sep=""),
                          suggest = "checking call arguments", 
                          if.warning = NULL, 
                          fun.name = fun.name)
             units <- NULL
             ans <- NULL
         }        
    }

    if(!is.null(ans))
        attr(ans, "name") <- input.name
    if(!is.null(ans) && !is.null(units))
        if(is.null(attributes(ans)$units)) attr(ans, "units") <- units
    class(ans) <- "pems.element"

    return(ans)

}






##############################
##############################
##pemsData
##############################
##############################


pemsData <- function(pems=NULL, ..., 
         fun.name = "pemsData", if.missing = "stop",
         pems.name = deparse(substitute(pems))){

    if(is.null(pems)){
          checkIfMissing(if.missing = if.missing,
               reply = paste("pems '", pems.name[1], "' not found", sep=""),
               suggest = "checking call arguments", 
               if.warning = NULL, 
               fun.name = fun.name)
    }

    pems$data

}