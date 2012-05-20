##########################
##########################
##merge pems
##########################
##########################

#kr

#description
##########################
#functions for merging data and pems


#includes 
##########################
#bindPEMS
#cAlign
#findLinearOffset


#to do
##########################

#comments
##########################





##########################
##########################
##bindPEMS
##########################
##########################

#kr 23/01/2012 v 0.0.6

#what it does
##########################
#sticks things together 
#as pems objects
# 

#to do
##########################
#make test more robust?

#comments
##########################
#currently uses 
#cAlign and findLinearOffset
#(both below) 



bindPEMS <- function(x = NULL, y = NULL, ..., by = NULL){

    #setup
    this.call <- match.call()
    fun.name <- "bindPEMS"
    extra.args <- list(...)

    if(is.null(by))
        by <- 1

    if(length(by)<2)
        by <- c(by, by)

    if(is.null(x) & is.null(y)){
         warning(paste("\t In ", fun.name,"(...) two expected objects not supplied", sep=""),
                 paste("\n\t [suggest confirming call]", sep=""), 
                 call. = FALSE, domain = NA)
         return(NULL)
    }

    if(!isPEMS(x) & !is.null(x)){
        if(is.vector(x) & !is.data.frame(x))
            x <- data.frame(x)
        if(is.data.frame(x))
            x <- makePEMS(x)
    }

    if(!isPEMS(y) & !is.null(y)){
        if(is.vector(y) & !is.data.frame(y))
            y <- data.frame(y)
        if(is.data.frame(y))
            y <- makePEMS(y)
    }

    if(isPEMS(x) & !isPEMS(y)){
         warning(paste("\t In ", fun.name,"(...) only one of two expected objects supplied", sep=""),
                 paste("\n\t or other not PEMS-able", sep=""),
                 paste("\n\t returning valid PEMS only", sep=""),
                 paste("\n\t [suggest confirming call]", sep=""), 
                 call. = FALSE, domain = NA)
         return(x)
    }

    if(!isPEMS(x) & isPEMS(y)){
         warning(paste("\t In ", fun.name,"(...) only one of two expected objects supplied", sep=""),
                 paste("\n\t or other not PEMS-able", sep=""),
                 paste("\n\t returning valid PEMS only", sep=""),
                 paste("\n\t [suggest confirming call]", sep=""), 
                 call. = FALSE, domain = NA)
         return(y)
    }

    d1 <- x$data
    d2 <- y$data

    y.offset <- extra.args$y.offset

    offset <- 0

    if(!is.null(y.offset)){
        if(is.numeric(offset))
            offset <- y.offset
        if(is.function(y.offset)){
            temp <- list(x = d1[,by[1]], y = d2[,by[2]])
            temp[names(extra.args) != "y.offset"] <- extra.args[names(extra.args) != "y.offset"]
            offset <- do.call(y.offset, temp)
        }
    }

    data <- cAlign(d1, d2, y.offset=offset)

    units <- cbind(x$units, y$units)
    names(units) <- names(data)

    constants <- x$constants
    constants[names(y$constants)] <- y$constants

    history <- list(list(x.history = x$history), list(y.history = y$history))

    temp <- list(x = data, units =units, constants = constants, history = history)

    class(x) <- "not.pems"
    d1 <- x[!names(x) %in% c("data", "units", "constants", "history")]
    temp[names(d1)] <- d1

    class(y) <- "not.pems"
    d2 <- y[!names(y) %in% c("data", "units", "constants", "history")]
    temp[names(d2)] <- d2

    output <- do.call(makePEMS, temp)

    #reset history?
    output$history[[length(output$history)]] <- this.call 

    output

}







##########################
##########################
##cAlign
##########################
##########################

#kr 23/01/2012 v 0.0.6

#what it does
##########################
#aligns supplied data frames
#applying any offset 

#to do
##########################
#make test more robust?

#comments
##########################
#widely used by merge operations
#check updates carefully
# 

cAlign <- function(x = NULL, y = NULL, y.offset = 0, all = TRUE,
                   suffixes = FALSE){

    #setup
    fun.name <- "cAlign"

    if(is.vector(x) & !is.data.frame(x))
        x <- as.data.frame(x)

    if(is.vector(y) & !is.data.frame(y))
        y <- as.data.frame(y)

    if(!is.data.frame(x) | !is.data.frame(y))
            stop(paste("\t In ", fun.name,"(...) two expected data.frames not supplied", sep=""),
                 paste("\n\t [suggest confirming call]", sep=""), 
                 call. = FALSE, domain = NA)

    if(y.offset>0){
        temp <- as.data.frame(matrix(NA, y.offset, ncol(y)))
        names(temp) <- names(y)
        y <- rbind(temp, y)
    }    

    if(y.offset<0){
        temp <- as.data.frame(matrix(NA, -y.offset, ncol(x)))
        names(temp) <- names(x)
        x <- rbind(temp, x)
    }    

    if(nrow(y)>nrow(x)){
        temp <- as.data.frame(matrix(NA, nrow(y)-nrow(x), ncol(x)))
        names(temp) <- names(x)
        x <- rbind(x,temp)
    }

    if(nrow(x)>nrow(y)){
        temp <- as.data.frame(matrix(NA, nrow(x)-nrow(y), ncol(y)))
        names(temp) <- names(y)
        y <- rbind(y,temp)
    }

    if(is.character(suffixes)){

        if(length(suffixes)==1){
            names(y) <- paste(names(y), suffixes[1], sep="")
        }
        if(length(suffixes)>1){
            names(x) <- paste(names(x), suffixes[1], sep="")
            names(y) <- paste(names(y), suffixes[2], sep="")
        }
    }

    output <- cbind(x,y)
    names(output) <- make.names(names(output), unique=TRUE)

    output

}





##########################
##########################
##findLinearOffset
##########################
##########################

#kr 23/01/2012 v 0.0.6

#what it does
##########################
#Finds the linear offset between
#two vectors 

#to do
##########################
#make test more robust?
#make it handle data.frames

#comments
##########################
# 

findLinearOffset <- function(x = NULL, y = NULL, offset.range = NULL){


    if(is.null(offset.range)){
        offset.range <- ceiling(max(c(length(x), length(y))))
    }

    ans <- ccf(x, y, lag.max=offset.range, plot=FALSE)

    ans$lag[which(ans$acf == max(ans$acf))[1]]

}