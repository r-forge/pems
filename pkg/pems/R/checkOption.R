checkOption <- function (
  option=NULL,
  allowed.options=NULL,
  option.name = "option",
  allowed.options.name = "allowed.options",
  partial.match=TRUE, 
  fun.name = "checkOption",
  output = c("option", "test.result"),
  ... 
) 
{

#check options
#################
#version 0.0.1
#karl 16/09/2010

#notes
#################

#to do
#################
#checkList inclusion?

#requested test?
if(!is.character(fun.name)) fun.name <- eval(formals(checkOption)$fun.name)[1]
if(!is.character(fun.name)) option.name <- eval(formals(checkOption)$option.name)[1]
if(is.null(allowed.options.name) | !is.character(fun.name)) allowed.options.name <- eval(formals(checkOption)$allowed.options.name)[1]

#if missing
if (is.null(option))
   stop(paste("\t In ", fun.name,"(...) no option set", sep=""), call. = FALSE, domain = NA)
if (is.null(allowed.options))
   stop(paste("\t In ", fun.name,"(...) no ", allowed.options.name, " set", sep=""), call. = FALSE, domain = NA)
if (is.call(allowed.options)) 
   allowed.options <- eval(allowed.options)

#only first cases considered for:
option <- option[1]
fun.name <- fun.name[1]

#recursive hell for output if we don't do this
foo <- function(a,b){
   ans <- if(partial.match) pmatch(a,b) else match(a,b)
}

#test outputs
ans <- foo(output[1], eval(formals(checkOption)$output))
if(is.na(ans))
   stop(paste("\t In checkOption(...) set output '", output[1], "' not known", sep=""),
      "\n\t [suggest one of: ", paste(eval(formals(checkOption)$output), sep=", ", collapse=", "), "]", 
      call. = FALSE, domain = NA)
output <- eval(formals(checkOption)$output)[ans]

#test options
ans <- foo(option[1], allowed.options)

#outputs
if(output=="test.result") 
   if(is.na(ans)) return(FALSE) else return(TRUE)
if(output=="option")
   if(is.na(ans))
      stop(paste("\t In ", fun.name,"(...) set ", option.name, " '", option, "' not known", sep=""),
         "\n\t [suggest one of: ", paste(allowed.options, sep=", ", collapse=", "),"]", 
         call. = FALSE, domain = NA) else invisible(allowed.options[ans])
   else stop(paste("\t In ", fun.name,"(...) unexpected error", sep=""),
      "\n\t [please report to pems admin]", 
      call. = FALSE, domain = NA)

}
