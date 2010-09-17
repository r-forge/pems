checkInput <- function (
  input=NULL,
  data=NULL,
  input.name = NULL, 
  fun.name=NULL,
  output = c("data", "test.result"),
  ... 
) 
{

#check input 
#################
#version 0.0.5
#karl 17/09/2010

#notes
#################
#data comes via checkPems 
#so we know what it is

#to do
#################
#finish this monster
#

#requested test?
if(!is.character(fun.name)) fun.name <- "checkInput"
if(!is.character(input.name)) input.name <- "input"

#check ... for higher.input as pass down from above


#if missing
if (is.null(input))
   stop(paste("\t In ", fun.name,"(...) no ", input.name, " set", sep=""), call. = FALSE, domain = NA)
if (is.null(data))
   stop(paste("\t In ", fun.name,"(...) no data set", sep=""), call. = FALSE, domain = NA)



#checkInputs output
#this can not be passed back
#not a parent(..)$output issue
output <- checkOptions(output[1], eval(formals(checkInput)$output), 
                       "output", "allowed outputs", 
                       fun.name="checkInput")

#only first cases considered for
fun.name <- fun.name[1]


#numeric
if(is.numeric(input)){
  if(any(input != as.integer(input))){
     message(paste("Warning: In ", fun.name, "(...) non-integer numeric ", input.name, " set", 
        "\n\t[handled robustly (as.integer)]", sep=""))
     input <- as.integer(input)
  }

  #try as in check.pollutant

  data <- data[,input]
}

#character


return(head(data))









#checkPems output
#this can not be passed back
#not a parent(..)$output issue
output <- checkOptions(output[1], eval(formals(checkPems)$output), 
                       "output", "allowed outputs", 
                       fun.name="checkPems")

#allowed pems
allowed.pems <- c("pems", "data.frame")

#if missing
if (is.null(pems))
   stop(paste("\t In ", fun.name,"(...) no pems set", sep=""), call. = FALSE, domain = NA)

#if allowed make data
pems.is <- "other"
data <- NULL
if("pems" %in% allowed.pems & is(pems)[1]=="pems"){
   data <- pems$data
   pems.is <- "pems"
}
if("data.frame" %in% allowed.pems & is.data.frame(pems)){
   data <- pems
   pems.is <- "data.frame"
}

#output
if(output=="test.result") return(pems.is)
if(is.null(data)){
   stop(paste("\t In ", fun.name,"(...) set pems not allowed class", sep=""),
      "\n\t [suggest object of class: ", paste(allowed.pems, sep=" or ", collapse=" or "),"]", 
      call. = FALSE, domain = NA)
} else {
  if(output=="data") invisible(data) else
     if(output=="pems") invisible(pems) else
       stop(paste("\t In ", fun.name,"(...) unexpected error", sep=""),
         "\n\t [please report to pems admin]", 
         call. = FALSE, domain = NA)
}

}
