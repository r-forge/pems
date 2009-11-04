check.method.args <- 
function (method, method.args, required.method.args,
  fun.name, default.args = NULL,
  method.matching = "exact",
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
#currently most if.n only enabled as stop
#need to add some conditional testing for args
#################
#to do
#################

rep.1 <- paste(" ", fun.name, " args conflict", sep = "")

if(!is.null(method.args)){
  if(method.matching=="partial"){
    ans <- pmatch(required.method.args, names(method.args))
  } else {
    ans <- match(required.method.args, names(method.args))
  }
  if(any(is.na(ans))){
    if(if.missing=="stop"){
      rep.2 <- paste("could not unambiguously match all required args for method '", method, "'", sep="")
      rep.3 <- paste(required.method.args[is.na(required.method.args[ans])], sep="", collapse = ", ")
      rep.3 <- paste("[valid arg(s): ", rep.3, "]", sep="")
      stop(paste(rep.1, rep.2, rep.3, sep = "\n\t"), call. = FALSE, domain = NA)
    }
    rep.2 <- paste("unmatched method.args handling option '", if.missing , "' unrecognised", sep="")
    stop(paste(rep.1, rep.2, sep = "\n\t"), call. = FALSE, domain = NA)
  } else {
    if(if.valid=="return.valid"){
      method.args <- method.args
    } else {
      rep.2 <- paste("valid case method.args handling option '", if.valid , "' unrecognised", sep="")
      stop(paste(rep.1, rep.2, sep = "\n\t"), call. = FALSE, domain = NA)
    }
  }
} else {
  if(if.null=="stop"){
    rep.2 <- "NULL fun.args not allowed"
    rep.3 <- paste(required.method.args, sep="", collapse = ", ")
    rep.3 <- paste("[required arg(s): ", rep.3, "]", sep="")
    stop(paste(rep.1, rep.2, rep.3, sep = "\n\t"), call. = FALSE, domain = NA)
  }
  rep.2 <- paste("NULL case method.args handling option '", if.null , "' unrecognised", sep="")
  stop(paste(rep.1, rep.2, sep = "\n\t"), call. = FALSE, domain = NA)
}

return(method.args)

}
