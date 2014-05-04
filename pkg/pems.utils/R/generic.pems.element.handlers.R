##########################
##########################
##generic pems.element.handlers
##########################
##########################

#kr 01/02/2012 v 0.3.1

#includes 
#(functions/code below) 
##########################
#print.pems.element
#plot.pems.element
#units.pems.element
#[.pems.element


#to do
#########################
#


as.data.frame.pems.element <- function(x, ...){

    temp <- data.frame(as.vector(x))
    names(temp) <- if(is.null(attr(x, "name")))
                       "x" else 
                           as.character(attr(x, "name")[1])
    names(temp) <- make.names(names(temp))
    temp    

}


print.pems.element <- function (x, ...){


#to do
###################
#tidy code
#

#to think about
###################
#do print and cat statements need to be merged
#as single output?
###################
#

    #data 
    #print with default method and attributes stripped
    ans <- x

#############
#test
#############
#    class(ans) <- "default"
#    attributes(ans) <- NULL
#    print.default(ans, ...)

    class(ans) <- class(ans)[class(ans)!="pems.element"]
    attributes(ans) <- attributes(ans)[names(attributes(ans))!=c("name", "units")]

#allows element to print as prior class
#############

    print(ans, ...)


    #attr
    #local report
    temp2 <- if(is.null(attributes(x)$name))
                 " [unamed]" else paste(" ", attributes(x)$name, sep = "")
    if(!is.null(attributes(x)$units))  
        temp2 <- paste(temp2, " [", attributes(x)$units, "]", sep = "")

    temp2 <- paste(temp2, " [n = ", length(x), "]", sep = "")
    
    cat("pems.element;", temp2, "\n")

}





##################
#plot.pems.element
##################

plot.pems.element <- function (x, y = NULL, xlab = NULL, ylab = NULL, ...){


#to this is lattice?

#rethink order
#think about error handling for mismatching cases
#bad plots, etc
#output styles?

    #x reset
    class(x) <- "default"

    #get x name
    if(is.null(y)){ 
        if(is.null(ylab)){
            ylab <- if(is.null(attributes(x)$name))
                        deparse(substitute(x)) else 
                            attributes(x)$name
            if(!is.null(attributes(x)$units))
                ylab <- paste(ylab, " [", attributes(x)$units, "]", sep = "")
        }
    } else {
        if(is.null(xlab)){
            xlab <- if(is.null(attributes(x)$name))
                        deparse(substitute(x)) else 
                            attributes(x)$name
            if(!is.null(attributes(x)$units))
                xlab <- paste(xlab, " [", attributes(x)$units, "]", sep = "")
        }
        if(is.null(ylab)){
            ylab <- if(is.null(attributes(y)$name))
                        deparse(substitute(y)) else 
                            attributes(y)$name
            if(!is.null(attributes(y)$units))
                ylab <- paste(ylab, " [", attributes(y)$units, "]", sep = "")
        }
    }

    plot(x = x, y = y, xlab = xlab, ylab = ylab,...)    

}




##################
#units.pems.element
##################

#need to think about this some more
units.pems.element <- function(x) attr(x, "units")




##########################
##########################
##[.pems.element
##########################
##########################

#kr 31/04/2014 v 0.2.1

#what it does
##########################
#handles pems.element[] calls 
#etc
#

#to do
##########################
#tidy
#think about force, simplify

`[.pems.element` <- function(x, i, ..., force=TRUE, wrap=FALSE){

    #pems.element handling
    #x[1] element 1 of x, etc 
    
    #output 
    #x[i] with pems.element attributes retained

    att <- attributes(x)
    class(x) <- class(x)[class(x)!="pems.element"]

    if(!force){
        i <- if(is.character(i)) 
                 i[i %in% names(x)] else i[i %in% 1:length(x)]
    }
    if(wrap && is.numeric(i)){
        if(length(x) < max(i, na.rm=TRUE)) x <- rep(x, length.out=max(i, na.rm=TRUE))
        
    }
     
    x <- try(x[i], silent = TRUE)
    if(is(x)[1] == "try-error") 
      stop("In pems.element[i] 'i' unknown/unfound", 
          call. = FALSE)
    attributes(x) <- att
    
    x

}