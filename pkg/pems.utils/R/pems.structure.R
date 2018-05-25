########################
########################
##pems.structure
########################
########################

#in place
#################
#pemsElement
#pemsData
#pemsConstants
#pemsHistory
#pemsDim

#testing
################
#pemsin



#TO DO
################

#pemsConstants
#tidy
#document
#namespace





#questions
###################
#is this better than check...
#do functions need a test that first element is pems, etc?
#




########################
########################
##pemsElement
########################
########################

#version 0.2.0
#karl 17/09/2010

pemsElement <- function(element, pems=NULL, ..., 
         fun.name = "pemsElement", if.missing = "stop",
         element.name = deparse(substitute(element))){

   #reorder this later

    if(is.null(pems)) {
       ans <- try(element, silent = TRUE)
       if(is(ans)[1] == "try-error" || is.function(ans)){ 
           checkIfMissing(if.missing = if.missing,
                          reply = paste("element '", element.name[1], "' not found", sep=""),
                          suggest = "checking call arguments", 
                          if.warning = NULL, 
                          fun.name = fun.name)
           ans <- NULL
       }
       if(!is.null(ans))
           if(is.null(attributes(ans)$name)) attr(ans, "name") <- element.name
       return(ans)   
    }

    pems <- checkPEMS(rebuildPEMS(pems, "old"))
    class(pems) <- "not.pems"

    ans <- try(pems$data[,element.name], silent = TRUE)
    units <- try(pems$units[1,element.name], silent = TRUE)

    if(is(ans)[1] == "try-error" || is.function(ans)){ 
        ans <- try(element, silent = TRUE)
        if(is(ans)[1] == "try-error" || is.function(ans)){ 
             checkIfMissing(if.missing = if.missing,
                          reply = paste("element '", element.name[1], "' not found", sep=""),
                          suggest = "checking call arguments", 
                          if.warning = NULL, 
                          fun.name = fun.name)
             units <- NULL
             ans <- NULL
         }        
    }

    if(!is.null(ans))
        attr(ans, "name") <- element.name
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
    #class(pems) <- "not.pems"
    #pems$data

#new build
#might want to strip out units, etc...?

    pems <- rebuildPEMS(pems)
    as.data.frame(pems)

}




##############################
##############################
##pemsConstants
##############################
##############################


pemsConstants <- function(pems=NULL, ..., 
         fun.name = "pemsConstants", if.missing = "stop",
         pems.name = deparse(substitute(pems))){

    if(is.null(pems)){
          checkIfMissing(if.missing = if.missing,
               reply = paste("pems '", pems.name[1], "' not found", sep=""),
               suggest = "checking call arguments", 
               if.warning = NULL, 
               fun.name = fun.name)
    }
    pems <- rebuildPEMS(pems, "old")
    class(pems) <- "not.pems"
    pems$constants

}





##############################
##############################
##pemsHistory
##############################
##############################


pemsHistory <- function(pems=NULL, ..., 
         fun.name = "pemsHistory", if.missing = "stop",
         pems.name = deparse(substitute(pems))){

    if(is.null(pems)){
          checkIfMissing(if.missing = if.missing,
               reply = paste("pems '", pems.name[1], "' not found", sep=""),
               suggest = "checking call arguments", 
               if.warning = NULL, 
               fun.name = fun.name)
    }
    pems <- rebuildPEMS(pems, "old")
    class(pems) <- "not.pems"
    pems$history

}








##############################
##############################
##pemsDim   
##############################
##############################


pemsDim <- function(pems=NULL, ..., 
         fun.name = "pemsDim", if.missing = "stop",
         pems.name = deparse(substitute(pems))){

    if(is.null(pems)){
          checkIfMissing(if.missing = if.missing,
               reply = paste("pems '", pems.name[1], "' not found", sep=""),
               suggest = "checking call arguments", 
               if.warning = NULL, 
               fun.name = fun.name)
    }
    pems <- rebuildPEMS(pems, "old")
    class(pems) <- "not.pems"
    dim(pems$data)

}





##########################
##########################
##pemsin
##########################
##########################

#kr v.0.1 07/06/2017 (from sleeper.service)

#pemsin
#pems.utils

#inputCheck alternative based on lazyeval package
##uses 
##lazy and lazyeval

#comments
##this is a test
##this needs work
##needs care 

#currently import all of lazy eval 
#might only need lazyeval and lazy

pemsin <- function(x, data=NULL){

######################
#to do
######################

#local error catcher

    x2 <- lazyeval::lazy(x)
    if(!is.null(data))
         if(!is.data.frame(data)) data <- as.data.frame(data)
    out <- try(lazyeval::lazy_eval(x, data=data), silent=TRUE)
    if(class(out)[1]=="try-error"){
         out <- try(x, silent=TRUE) 
         if(class(out)[1]=="try-error") stop("unknown arg") 
    }
    out
}


pemsin2.old <- function (x, data = NULL) 
{
    x <- enquo(x)
    data %>% as.data.frame() %>% pull(!!x)
}

pemsin2 <- function (x, data = NULL, units = NULL, .x = enquo(x)){

    ans <- if(is.null(data)) NULL else 
         try(data[quo_name(.x)], silent = TRUE)
    if (is.null(ans) | class(ans)[1] == "try-error") {
        ans <- try(x, silent = TRUE)
        if (class(ans)[1] == "try-error") 
            return(NULL)
    }
    if (!is.null(units)) 
        ans <- convertUnits(ans, to = units)
    ans
}



calc.dist <- function(speed = NULL, time = NULL, data = NULL,
                     ...){
    #setup
################
#think I can simplify setup
#maybe merge with pemsin 
#     so we don't rename data? 
################
    if (!is.null(data) && quo_name(enquo(speed)) %in% names(data)) 
        names(data)[names(data)==quo_name(enquo(speed))] <- "speed"
    if (!is.null(data) && quo_name(enquo(time)) %in% names(data)) 
        names(data)[names(data)==quo_name(enquo(time))] <- "time"

    #get inputs
    speed <- pemsin2(speed, data, units="m/s")
    time <- pemsin2(time, data, units="s")

    #my assumption
    #first unit resolution is average of rest
    #rest are time.now - time.last

    temp <- diff(time)
    temp <- c(mean(temp, na.rm=TRUE), temp)

    #my calculation
    distance <- speed * temp

    #my structure
################
#look into this 
#    makePEMSElement versus pems.element
#    also does it keep historical data types...
################
    distance <- makePEMSElement(distance, name="distance", units="m")
    distance
      
}


calc.new <- function (speed = NULL, time = NULL, data = NULL, ...) 
{
    if (!is.null(data) && quo_name(enquo(speed)) %in% names(data)) 
        names(data)[names(data)==quo_name(enquo(speed))] <- "speed"
    if (!is.null(data) && quo_name(enquo(time)) %in% names(data)) 
        names(data)[names(data)==quo_name(enquo(time))] <- "time"
    speed <- pemsin2(speed, data)
    time <- pemsin2(time, data)
    distance <- calc.dist(speed, time)
    distance
}

