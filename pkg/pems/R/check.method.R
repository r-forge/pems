check.method <-
function (method, method.options,
  fun.name,
  method.matching = "partial",
  if.null = "stop",
  if.missing = "stop",
  if.valid = "return.valid",
  source = NULL, pems = NULL 
) 
{

#check pems method
#################
#version 0.0.1
#karl 01/11/2009
#################
#notes
#currently missing.methods/args only enabled as stop
#################
#to do
#################

rep.1 <- paste(" ", fun.name, " methods conflict", sep = "")

if(!is.null(method)){
  if(method.matching=="partial"){
    ans <- pmatch(method, method.options)
  } else {
    ans <- match(method, method.options)
  }
  if(is.na(ans)){
    if(if.missing=="stop"){
      rep.2 <- paste("could not unambiguously match fun.method '", method, "'", sep="")
      rep.3 <- paste(method.options, sep="", collapse = ", ")
      rep.3 <- paste("[valid option(s): ", rep.3, "]", sep="")
      stop(paste(rep.1, rep.2, rep.3, sep = "\n\t"), call. = FALSE, domain = NA)
    }
    rep.2 <- paste("unmatched method handling option '", if.missing , "' unrecognised", sep="")
    stop(paste(rep.1, rep.2, sep = "\n\t"), call. = FALSE, domain = NA)
  } else {
    if(if.valid=="return.valid"){
      method <- method.options[ans]
    } else {
      rep.2 <- paste("valid case method handling option '", if.valid , "' unrecognised", sep="")
      stop(paste(rep.1, rep.2, sep = "\n\t"), call. = FALSE, domain = NA)
    }
  }
} else {
  if(if.null=="stop"){
    rep.2 <- "NULL fun.method not allowed"
    rep.3 <- paste(method.options, sep="", collapse = ", ")
    rep.3 <- paste("[valid option(s): ", rep.3, "]", sep="")
    stop(paste(rep.1, rep.2, rep.3, sep = "\n\t"), call. = FALSE, domain = NA)
  }
  rep.2 <- paste("NULL case method handling option '", if.null , "' unrecognised", sep="")
  stop(paste(rep.1, rep.2, sep = "\n\t"), call. = FALSE, domain = NA)
}

return(method)

}
