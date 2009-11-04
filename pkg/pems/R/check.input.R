check.input <- 
function (input, input.class, fun.name,
  if.null = "stop", 
  if.missing = "stop",
  if.valid = "return.valid",
  source, pems 
) 
{

#check pems input
#################
#version 0.0.2
#karl 17/10/2009

#notes
#################

#to do
#################

#test tree
if(try(eval(input), silent=TRUE)[1]=="NULL"){
  test.1 <- c("null", "none")
} else {
  if(is(try(source[, eval(input)], silent = TRUE))[1]=="try-error") {
    if(is(try(get(input), silent = TRUE))[1]=="try-error") {
      test.1 <- c("missing", "none")
    } else {
      test.1 <- c("valid", "parent")
    }
  } else {
    test.1 <- c("valid", "pems")
  }
}

#general
rep.1 <- paste(" ", fun.name, " input conflict", sep = "")
for(i in 1:3){
  if(i==1) { this.if <- if.null; this.case = "null" }
  if(i==2) { this.if <- if.missing; this.case = "missing" }
  if(i==3) { this.if <- if.valid; this.case = "valid" }

  #the stop option
  if(this.if[1]=="stop" & this.case==test.1[1]){
    rep.2 <- paste(input.class, " input '", as.character(eval(input)), "' not found", sep = "")
    if(this.case=="null"){
      rep.2 <- paste(as.character(eval(input)), " ", input.class, " input not allowed", sep = "")
    }
    if(this.case=="valid"){
      rep.2 <- paste("suspect request halted (check.input called 'stop' for valid ", input.class, " input)", sep = "")
    }
    if(is(this.if)[1]=="list"){
      if(!is.null(this.if$comment)){
        rep.2 <- paste(rep.2, this.if$comment, sep="\n\t")
      }
    } else {
      if(length(this.if)>1) {
        rep.2 <- paste(rep.2, this.if[2], sep="\n\t")
      }
    }
    stop(paste(rep.1, rep.2, sep = "\n\t"), call. = FALSE, domain = NA)
  }

  #the return.valid option
  if(this.if[1]=="return.valid" & this.case==test.1[1]){
    if(test.1[1]=="missing"){
      rep.2 <- paste(input.class, " input '", as.character(eval(input)), "' not found", sep = "")
      rep.3 <- paste("suspect (check.input called 'return.valid' for missing input)", sep = "")
    if(is(this.if)[1]=="list"){
      if(!is.null(this.if$comment)){
        rep.3 <- paste(rep.3, this.if$comment, sep="\n\t")
      }
    } else {
      if(length(this.if)>1) {
        rep.3 <- paste(rep.3, this.if[2], sep="\n\t")
      }
    }
    stop(paste(rep.1, rep.2, rep.3, sep = "\n\t"), call. = FALSE, domain = NA)
    }
    if(test.1[1]=="null"){
      return(NULL)
    }
    if(test.1[1]=="valid" & test.1[2]=="pems"){
      return(source[, eval(input)])
    }
    if(test.1[1]=="valid" & test.1[2]=="parent"){
      return(get(input))
    }
  }

  #the return.alt option
  if(this.if[1]=="return.alt" & this.case==test.1[1]){
    #what to check
    #what to do
    if(is(this.if)[1]=="list"){
      if(!is.null(this.if$alt)){
        if(is.null(this.if$alt.check)){
          return(this.if$alt)
        } else {
          if(is(this.if$alt)[1]==this.if$alt.check){
            return(this.if$alt)
          } else {
            rep.2 <- paste("suspect request halted (check.input 'return.alt' alt options failed alt.check)", sep = "")
            stop(paste(rep.1, rep.2, sep = "\n\t"), call. = FALSE, domain = NA)
          }
        }
      } else {
        rep.2 <- paste("suspect request halted (check.input 'return.alt' alt option missing)", sep = "")
        stop(paste(rep.1, rep.2, sep = "\n\t"), call. = FALSE, domain = NA)
      }
    } else {
      rep.2 <- paste("suspect request halted (check.input 'return.alt' not correctly structured)", sep = "")
      stop(paste(rep.1, rep.2, sep = "\n\t"), call. = FALSE, domain = NA)
    }
  }
}

#no error handling
rep.2 <- paste("suspect request halted (check.input 'if.", test.1[1], "' handling not recognised for '", input.class, "' input)", sep = "")
stop(paste(rep.1, rep.2, sep = "\n\t"), call. = FALSE, domain = NA)

}
