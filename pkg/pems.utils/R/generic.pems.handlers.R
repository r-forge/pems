##########################
##########################
##generic pems handlers
##########################
##########################

#kr 01/02/2012 v 0.3.1

#includes 
#(functions/code below) 
##########################
#as.data.frame.pems
#[.pems
#$.pems
#print.pems
#plot.pems
#names.pems
#summary.pems
#head.pems
#tail.pems
#units.pems
#

#to do
##########################
#so much
#see individual methods 
#all need work

# verbose printing? 
# but needs to be hidden when dumped 
# to object
#
# head.pems <- function(x, n =6,...) x[1:n,]
# tail
#


#comments
##########################
#



##########################
##########################
##as.data.frame.pems
##########################
##########################

#pull data.frame out of pems
#for lattice, lm, etc.

as.data.frame.pems <- function(x, ...){

    class(x) <- "not.pems"
    x$data    

}



##########################
##########################
##[.pems
##########################
##########################

#kr 06/06/2013 v 0.3.0

#what it does
##########################
#handles pems[] calls 
#etc
#

#to do
##########################
#tidy
#think about force, simplify

`[.pems` <- function(x, i, j, ..., force = TRUE, simplify = TRUE){

    #pems handling
    #x[1] element 1 as 1 col data.frame
    #x[1,] row 1
    #x[,1] element 1 
    #x[1,1] row, element 1,1 
    
    #output 
    #(simplify=TRUE) as pems.element if possible else pems
    #(simplify=FALSE) as pems 


######################
#to do
######################
#rationalise a lot of this
######################


    check.i <- if(missing(i)) FALSE else TRUE
    check.j <- if(missing(j)) FALSE else TRUE

    ###############################
    #x[]
    #x[,]
    #as is - no changes
    #        no history update
    ###############################
    if(!check.i & !check.j) return(x)

    call1 <- sys.call()
    call2 <- match.call()
    old.class <- unique(c(is(x), class(x)))
    class(x) <- "not.pems"

    ###############################
    #x[1]
    #x[1,force=T]
    #by col as pems 
    ###############################
    if(check.i && !check.j && length(as.character(call1))==length(as.character(call2))){
         if(!force){
             i <- if(is.character(i)) 
                       i[i %in% names(x$data)] else i[i %in% 1:ncol(x$data)]
         }
         d1 <- try(x$data[i], silent = TRUE)
         if(is(d1)[1] == "try-error") 
             stop("In pems[i] 'i' unknown/unfound", 
                  call. = FALSE)
         x$data <- d1
         if("units" %in% names(x)){
             d1 <- try(x$units[i], silent = TRUE)
             if(is(d1)[1] == "try-error") 
                 stop("In pems[j] units('j') unknown/unfound", 
                      call. = FALSE)
             x$units <- d1
         }
    }

    ###############################
    #x[1,]
    #x[1,,force=T]
    #by col as pems 
    ###############################
    if(check.i && !check.j && length(as.character(call1))!=length(as.character(call2))){
         if(!force){
             i <- if(is.character(i)) 
                       i[i %in% row.names(x$data)] else i[i %in% 1:nrow(x$data)]
         }
         d1 <- try(x$data[i,], silent = TRUE)
         if(is(d1)[1] == "try-error") 
             stop("In pems[i,] 'i' unknown/unfound", 
                  call. = FALSE)
         x$data <- d1
    }

    ###############################
    #x[,1]
    #x[,1,force=T]
    #by col as pems 
    ###############################
    if(!check.i && check.j){
         if(!force){
             j <- if(is.character(j)) 
                       j[j %in% names(x$data)] else j[j %in% 1:ncol(x$data)]
         }
         d1 <- try(x$data[j], silent = TRUE)
         if(is(d1)[1] == "try-error") 
             stop("In pems[,j] 'j' unknown/unfound", 
                  call. = FALSE)
         x$data <- d1
         if("units" %in% names(x)){
             d1 <- try(x$units[j], silent = TRUE)
             if(is(d1)[1] == "try-error") 
                 stop("In pems[,j] units('j') unknown/unfound", 
                      call. = FALSE)
             x$units <- d1
         }
    }

    ###############################
    #x[1,1]
    #x[1,1,force=T]
    #by col as pems 
    ###############################
    if(check.i && check.j){
         if(!force){
             i <- if(is.character(i)) 
                       i[i %in% row.names(x$data)] else i[i %in% 1:nrow(x$data)]
             j <- if(is.character(j)) 
                       j[j %in% names(x$data)] else j[j %in% 1:ncol(x$data)]
         }
         d1 <- try(x$data[i,], silent = TRUE)
         if(is(d1)[1] == "try-error") 
             stop("In x[i,j] 'i' unknown/unfound/mismatched", 
                  call. = FALSE)
         d1 <- try(d1[j], silent = TRUE)
         if(is(d1)[1] == "try-error") 
             stop("In pems[i,j] 'j' unknown/unfound/mismatched", 
                  call. = FALSE)
         x$data <- d1
         if("units" %in% names(x)){
             d1 <- try(x$units[j], silent = TRUE)
             if(is(d1)[1] == "try-error") 
                 stop("In pems[i,j] units('j') unknown/unfound", 
                      call. = FALSE)
             x$units <- d1
         }
    }

    ########################
    #simplify
    ########################
    if(simplify && ncol(x$data)==1){

############
#new test
###########
#replacing
#        d1 <- as.vector(t(x$data))
        d1 <- x$data[,1] 
###########

        attr(d1, "row.names") <- NULL
        attr(d1, "name") <- names(x$data[1])
        attr(d1, "units") <- as.character(x$units[1,1])

################
#new test
################
#used to be just pems.element
#class(d1) <- "pems.element" 
        class(d1) <- unique(c("pems.element", class(d1)))

################
#think about makePEMSElement
################

        return(d1)
    }

    ########################
    #history
    ########################
    if("history" %in% names(x$data))
         x$history <- c(x$history, call2)
    class(x)<- old.class
    return(x)    
}




##########################
#########################
##$.pems
#########################
#########################

`$.pems` <- function(x, i, ...){


#####################
#old version
#####################
#    class(x) <- "not.pems"
#    ans <- try(x$data[, i], silent = TRUE)
#    if(class(ans)[1] == "try-error"){
#        warning("Element '", i, "' not found in pems", call. = FALSE)
#        return(NULL)
#    }
#    if (!is.null(ans)) 
#        attr(ans, "name") <- i
#    if (!is.null(ans) && !is.null(units)) 
#        if (is.null(attributes(ans)$units)) 
#            attr(ans, "units") <- x$units[1,i]
#        class(ans) <- "pems.element"
#    ans

#think about x[,i, simplify=TRUE]
#might not be need because

    ans <- try(x[, i], silent = TRUE)
    if(class(ans)[1] == "try-error"){
        warning("Element '", i, "' not found in pems", call. = FALSE)
        return(NULL)
    }

    if (!is.null(ans)) 
        attr(ans, "name") <- i
    if (!is.null(ans) && !is.null(units)) 
        if (is.null(attributes(ans)$units)) 
            attr(ans, "units") <- x$units[1,i]

    ans

}










##########################
##########################
##print.pems
##########################
##########################

#kr 06/06/2013 v 0.3.0

#what it does
##########################
#handles pems console appearance 
#etc
#

#to do
##########################
#remove temp?


print.pems <- function (x, verbose = FALSE, ...) {

    temp <- x
    class(temp) <- "not.pems"

    #show data frame

    if(verbose){
        print.data.frame(temp$data)
    }

    #pems report line 1

    reply <- names(temp$data)
    if (is.null(reply)) 
        message("\npems object: no named data [suspect]")
    else message("\npems object: ", ncol(temp$data), " data series (each ", 
        nrow(temp$data), " cases)")

    #pems report line 2 structure

    reply <- names(temp)[names(temp) %in% c("units", "constants", 
        "history")]
    if (length(reply) < 1) 
        message("\twith no supporting structure [suspect]")
    else message("\twith supporting structures: ", paste(reply, 
        collapse = ", ", sep = ""))

    #pems report line 3 extra tags

    reply <- names(temp)[!names(temp) %in% c("data", "units", 
        "constants", "history", "dem")]
    if (length(reply) > 0) 
        message("\t[and unique tags: ", paste(reply, collapse = ", ", 
            sep = ""), "]\n")

    #output
    invisible(x)

}



##########################
##########################
##plot.pems
##########################
##########################

#kr 07/12/2011 v 0.2.0

#what it does
##########################
#generates simple plot
#does not keep units
#

#to do
##############################
#remove temp


##' @S3method plot pems
plot.pems <- function(x, id = NULL, ignore = "time.stamp", n = 3, ...) {

   temp <- x
   class(temp) <- "no.class"

   reply <- temp$data

   if(is.null(reply)){
      message("\npems object [suspect]")
      return(invisible(NULL))
   }

   if(is.null(id)){
       id <- 1:ncol(reply)
       if(length(ignore)>0)
           id <- id[!names(reply) %in% ignore]
       if(n>0 && length(id)>n)
           id <- id[1:n]
   }

   reply <- reply[id]      

   plot(reply, ...)

}



##########################
##########################
##names.pems
##########################
##########################

#kr 07/12/2011 v 0.2.0

#what it does
##########################
#returns data series names from pems

#to do
##########################
#names<- handling
#



##' @S3method print pems
names.pems <- function(x, ...) {

    class(x) <- "no.class"
    x <- x$data

    if(is.null(x)){
       message("\npems object [suspect]")
       return(invisible(NULL))
    }

    names(x)
}


##########################
##########################
##summary.pems
##########################
##########################

#kr 07/12/2011 v 0.2.0

#what it does
##########################
#generates summary reports 
#

#to do
##########################
#make dedicated summary 
#and option for as current as alternative
#


#comments
##########################
#to tidy


##' @S3method print pems
summary.pems <- function(object, ...) {

   class(object) <- "no.class"

   object <- object$data

   if(is.null(object)){
      message("\npems object [suspect]")
      return(invisible(NULL))
   }

   return(summary(object))
                       
}





##########################
##########################
##units.pems
##########################
##########################

#kr 07/12/2011 v 0.2.0

#what it does
##########################
#extracts units from pems 
#

#to do
##########################
#make dedicated summary 
#and option for as current as alternative
#


#comments
##########################
#to tidy



##' @S3method units pems
units.pems <- function(x) {

    class(x) <- "no.class"
    x <- x$units

    if(is.null(x)){
       message("\npems object unitless [suspect]")
       return(invisible(NULL))
    }

    return(x)

}





##########################
##########################
##head.pems
##tail.pems
##########################
##########################

#kr 31/04/2014 v 0.2.4


head.pems <- function(x, n=6, ...) x[1:n,]

tail.pems <- function(x, n=6, ...) {
    temp <- dim(as.data.frame(x))
    x[(temp-n+1):temp,]
}







############################################################
############################################################


































