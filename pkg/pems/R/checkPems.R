checkPems <- function (
  pems = NULL, 
  fun.name = "checkPems",
  output = c("data", "pems", "test.result"),
  ... 
) 
{

#check pems/data
#################
#version 0.0.1
#karl 16/09/2010

#notes
#################

#to do
#################


#requested test?
if(!is.character(fun.name)) fun.name <- eval(formals(checkPems)$fun.name)[1]

#only first cases considered for
fun.name <- fun.name[1]

#checkPems output
#this can not be passed back
#not a parent(..)$output issue
output <- checkOption(output[1], eval(formals(checkPems)$output), 
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
