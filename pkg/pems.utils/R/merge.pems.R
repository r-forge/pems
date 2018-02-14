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
#align
#cAlign
#cAlign2
#tAlign
#findLinearOffset

#removed
############################
#bindPEMS

#to do
##########################
#decide on bindPEMS status
#check vector issue with align
#

#comments
##########################
#think about bindPEMS
#




##########################
##########################
##align
##########################
##########################

#kr v.0.4 31/11/2015

#align
#plyr, loa
#from sleeper.service

#aligns data in two objects
##uses 
##full_join in dplyr
##pems.utils structure


#comments
##this is quick version of alignment

align <- function(data1, data2, n=0, ...){

#data alignment function
#this currently returns a pems object
#could return it in simplest of supplied cases 
#or most complex???

#to look into 
#this does not track names of vectors
#starting with example align(x,y,-15)
#makes frame with names x and x.1...

   extra.args <- list(...)
   this.call <- if("this.call" %in% names(extra.args))
                     extra.args$this.call else match.call()

   data1 <- rebuildPEMS(makePEMS(data1), "old")
   data2 <- rebuildPEMS(makePEMS(data2), "old")

   new.names <- make.names(c(names(data1), names(data2)), unique=TRUE)
   names(data1) <- new.names[1:ncol(data1)]
   names(data2) <- new.names[(ncol(data1)+1):length(new.names)] 

   #calculate refs including offset
   data1$align.temp.ref <- 1:nrow(data1)
   data2$align.temp.ref <- (1:nrow(data2)) + n

   #merge data 
   new.data <- full_join(pemsData(data1), pemsData(data2), by="align.temp.ref")
   new.data <- new.data[order(new.data$align.temp.ref),]
   row.names(new.data) <- 1:nrow(new.data)

   #use this to order elsewhere
   ##if(order)
   ##   temp <- arrange(temp, order(temp[,t1]))

   #remove the align.temp.ref
   new.data <- new.data[names(new.data)[names(new.data) %in% new.names]]

   #get associated units
   new.units <- cbind(units(data1), units(data2))
   new.units <- new.units[new.names[new.names %in% names(new.units)]]

   #smash pems
   class(data1) <- "list"
   class(data2) <- "list"
   data1 <- listUpdate(data2, data1, ignore=c("data", "units"))
   data1 <- listUpdate(list(data=new.data, units=new.units), data1)
#this should keep all tags
#but needs testing
   data1$history <- this.call
   class(data1) <- "pems" 

   return(rebuildPEMS(data1))
}





######################################
######################################
##cAlign
######################################
######################################

#kr 26/12/2015 v 0.1.0
#version update from sleeper.service

#currently not exported

cAlign.old <- function(form, data1=NULL, data2 = NULL, ...){

#function for the time alignment of data in two datasets
#using a common time-series or similar time-series.

#form formula 
#data1 first data source
#data2 optional second data source

#uses 
#ccf in  base package?
#align in sleeper.service

#to do
#tidy error messages
#need to get this working with no data
#cAlign(x~y)
#need to sort out replacement for find offset command

    #set up
    data1 <- makePEMS(data1)
    vars <- as.character(form)[-1]
    #vars[1] is the "~"

#tidy the next bit later
#but don't want it as formal
    extra.args <- list(...)
    output <- if(is.null(extra.args$output)) c("plot", "pems") else extra.args$output
    if("plot" %in% names(extra.args))
         output <- if(extra.args$plot) 
             unique(c(output, "plot")) else output[output != "plot"] 
    if("pems" %in% names(extra.args))
         output <- if(extra.args$pems) 
             unique(c(output, "pems")) else output[output != "pems"]
    if("offset" %in% names(extra.args))
         output <- if(extra.args$offset) 
             unique(c(output, "offset")) else output[output != "offset"] 
    extra.args <- extra.args[!names(extra.args) %in% c("plot", "pems", "offset", "output")]

#not tested
    if(is.null(data2)){
        if(length(vars)<2) stop("need two cases if only one data set provided")
        if(nrow(data1)<1){
            data2 <- data1
        } else {
            data2 <- makePEMS(data1[all.vars(form)[2]])
            data1 <- data1[names(data1)[names(data1) != all.vars(form)[2]]]
        }
    }

    #this first variable with be vars[1]
    #note: x and y remain data.frames here
    #(see below about making this work with no data)
    #(backward compat...)
    x <- try(model.frame(as.formula(paste("~", vars[1], sep="")), data1, na.action = na.pass),
             silent = TRUE)
    if(class(x)[1]=="try-error") stop("cAlign() conflict, '", vars[1], "' not found where expected", 
                                      call. = FALSE)
    
    #get next term
    temp <- if(length(vars)>1) vars[2] else vars[1]    
    y <- try(model.frame(as.formula(paste("~", temp, sep="")), data2, na.action = na.pass),
             silent = TRUE)
    if(class(y)[1]=="try-error") stop("cAlign() conflict, '", temp, "' not found where expected", 
                                      call. = FALSE)

    #to make above work with no data sources
    if(nrow(data1)<1) data1 <- makePEMS(x)
    if(nrow(data2)<1) data2 <- makePEMS(y)
    x <- x[,1]
    y <- y[,1]

#might rethink error messages
#so only message if both args are missing

    #align using ccf
#might still need to do more work on what we pass
#and make it more robust? ccf formals only???

    ans <- do.call(ccf, listUpdate(list(x=x, y=y, na.action=na.pass, plot=FALSE), 
                                   extra.args))
    ##ans <- ccf(x, y, na.action=na.pass, plot=FALSE, ...)
    fit <- ans$lag[which(ans$acf==max(ans$acf, na.rm=T))]

#might want to do a prettier plot
#this is before offset check 

    if("plot" %in% output){
       plot(ans, main="cAlign ACF")
       abline(v=0, col="pink", lty=3)
       abline(v=fit, col="red", lty=3)
       if(fit!=0)
           arrows(0, max(ans$acf, na.rm=T) ,fit, max(ans$acf, na.rm=T), 
                  col="red", 0.1)
    }

    if("offset" %in% output)
       if(!"pems" %in% output) return(fit) else 
           print(paste("offset = ", fit, sep=""))

    return(align(data1, data2, fit))

}




#kr 06/01/2018 v 0.2.0
#version update from SGS L&L

#replaced previous cAlign  
#add a method option

#this needs tidying


cAlign <- function(form, data1=NULL, data2 = NULL, ...){

#function for the time alignment of data in two datasets
#using a common time-series or similar time-series.

#form formula 
#data1 first data source
#data2 optional second data source

#uses 
#ccf in  base package?
#align in sleeper.service

#changes relative to previous
#added lag.start = "middle"
#added default for lag.max
#added min.overlap

#to do
#tidy error messages
#need to get this working with no data
#cAlign(x~y)
#need to sort out replacement for find offset command

    #set up
    data1 <- makePEMS(data1)
    vars <- as.character(form)[-1]
    #vars[1] is the "~"

#tidy the next bit later
#but don't want it as formal
#output = all needed...

    extra.args <- list(...)
    output <- if(is.null(extra.args$output)) c("plot", "pems") else extra.args$output
    if("plot" %in% names(extra.args))
         output <- if(extra.args$plot) 
             unique(c(output, "plot")) else output[output != "plot"] 
    if("pems" %in% names(extra.args))
         output <- if(extra.args$pems) 
             unique(c(output, "pems")) else output[output != "pems"]
    if("offset" %in% names(extra.args))
         output <- if(extra.args$offset) 
             unique(c(output, "offset")) else output[output != "offset"] 
    extra.args <- extra.args[!names(extra.args) %in% c("plot", "pems", "offset", "output")]

#not tested
    if(is.null(data2)){
        if(length(vars)<2) stop("need two cases if only one data set provided")
        if(nrow(data1)<1){
            data2 <- data1
        } else {
            data2 <- makePEMS(data1[all.vars(form)[2]])
            data1 <- data1[names(data1)[names(data1) != all.vars(form)[2]]]
        }
    }

    #this first variable with be vars[1]
    #note: x and y remain data.frames here
    #(see below about making this work with no data)
    #(backward compat...)
    x <- try(model.frame(as.formula(paste("~", vars[1], sep="")), data1, na.action = na.pass),
             silent = TRUE)
    if(class(x)[1]=="try-error") stop("cAlign() conflict, '", vars[1], "' not found where expected", 
                                      call. = FALSE)
    
    #get next term
    temp <- if(length(vars)>1) vars[2] else vars[1]    
    y <- try(model.frame(as.formula(paste("~", temp, sep="")), data2, na.action = na.pass),
             silent = TRUE)
    if(class(y)[1]=="try-error") stop("cAlign() conflict, '", temp, "' not found where expected", 
                                      call. = FALSE)

    #to make above work with no data sources
    if(nrow(data1)<1) data1 <- makePEMS(x)
    if(nrow(data2)<1) data2 <- makePEMS(y)
    x <- x[,1]
    y <- y[,1]

#to think about
#################################################
#would add mask here
#could use something like mask.1=1:30 to hide first 30 points of x, etc...

    #######################
    #for lag.start
    len.x <- length(x)
    len.y <- length(y)
    lag.tidy <- FALSE
    if(!"lag.start" %in% names(extra.args) || extra.args$lag.start=="middle"){
           lag.tidy <- TRUE
           extra.args$lag.start <- floor((0.5* len.x)-(0.5*len.y))
    }

    if(extra.args$lag.start>0) y <- c(rep(NA,extra.args$lag.start), y)
    if(extra.args$lag.start<0) x <- c(rep(NA,-extra.args$lag.start), x)

#lag.tidy<-TRUE 

#print(length(x))
#print(length(y))
    if(lag.tidy){
       if(length(x)>length(y)) y <- c(y, rep(NA, length(x)-length(y)))
       if(length(y)>length(x)) x <- c(x, rep(NA, length(y)-length(x)))
    }

##################################


#might rethink error messages
#so only message if both args are missing

    #align using ccf
#might still need to do more work on what we pass
#and make it more robust? ccf formals only???


###################################
#new bits

#need to sort out min.overlap
#        lag.max
#        min.overlap
#        method control

#need to think about best 10, etc...

    if(!"min.overlap" %in% names(extra.args))
         extra.args$min.overlap <- 0

#think about limiting
#    if(extra.args$min.overlap>min(c(len.x, len.y))) extra.args$min.overlap <- min(c(len.x, len.y))

    if(!"lag.max" %in% names(extra.args)) 
        extra.args$lag.max <- if(lag.tidy) ceiling((0.5*len.x)+(0.5*len.y)- extra.args$min.overlap) else 
                                           min(c(length(x), length(y))) 

    if(length(x) < extra.args$lag.max) x <- c(x, rep(NA, extra.args$lag.max -length(x)))
    if(length(y) < extra.args$lag.max) y <- c(y, rep(NA, extra.args$lag.max -length(y)))

#default lag.max to 30 for old case...

    ans <- do.call(ccf, listUpdate(list(x=x, y=y, na.action=na.pass, plot=FALSE), 
                                   extra.args))
    ans$lag <- ans$lag + extra.args$lag.start

##########################

    ##ans <- ccf(x, y, na.action=na.pass, plot=FALSE, ...)
    fit <- ans$lag[which(ans$acf==max(ans$acf, na.rm=T))]

#might want to do a prettier plot
#this is before offset check 

    if("plot" %in% output){
       plot(ans, main="cAlign ACF")
       abline(v=0, col="pink", lty=3)
       abline(v=fit, col="red", lty=3)
       if(fit!=0)
           arrows(0, max(ans$acf, na.rm=T) ,fit, max(ans$acf, na.rm=T), 
                  col="red", 0.1)
    }

    if("offset" %in% output)
       if(!"pems" %in% output) return(fit) else 
           print(paste("offset = ", fit, sep=""))

    return(align(data1, data2, fit))

}


##########################
##########################
##findLinearOffset
##########################
##########################

#kr 26/12/2015 v 0.1.0
#update using sleeper.service revision

#what it does
##########################
#Finds the linear offset between
#two vectors 

#to do
##########################
#make test more robust?
#make it handle data.frames, etc

#comments
##########################
#might not be keeping this 

findLinearOffset <- function(x = NULL, y = NULL, offset.range = NULL){

    if(is.null(offset.range)){
        offset.range <- ceiling(max(c(length(x), length(y))))
    }

    ans <- ccf(x, y, lag.max=offset.range, plot=FALSE)
    ans$lag[which(ans$acf == max(ans$acf))[1]]

}










######################################
######################################
##tAlign
######################################
######################################


#kr 26/12/2015 v 0.0.3
#using sleeper.service version

tAlign <- function(form, data1, data2 = NULL, order = TRUE, ...){

#function for the time alignment of data in two datasets
#using a common time.stamp, local.time, etc.

#form formula 
#data1 first data source
#data2 optional second data source

#not sure this makes sense

#uses 
#full_join in dplyr

#urgent to do
#order does not seem to be working

#to do
#think about data2 being optional
#tidy error messages
#think about information lost on merging
#(look at how align handles this)

    #set up
    data1 <- makePEMS(data1)
    vars <- as.character(form)[-1]
    #as.character(form)[1] is the "~"

#not sure this is wanted/needed
#would time aligning a single case be useful
    if(is.null(data2)){
        if(length(vars)<2) stop("need two cases if only one data set provided")
        data2 <- makePEMS(data1[all.vars(form)[2]])
        data1 <- data1[names(data1)[names(data1) != all.vars(form)[2]]]
    }

#if a ~ b then b needs to be renamed as a
    if(length(vars)>1)
        names(data2)[names(data2)==vars[2]] <- vars[1]

     t1 <- vars[1]

#make names unique 
#merging does not drop cases
    temp1 <- names(data1)
    temp2 <- names(data2)
    temp <- make.names(c(temp1, temp2), unique=TRUE)
    names(data1) <- temp[1:length(temp1)]
    names(data2) <- temp[(length(temp1)+1):length(temp)] 
    names(data2)[temp2 == t1] <- t1

#pass data to join
    temp1 <- pemsData(data1)
    temp2 <- pemsData(data2)
    temp <- full_join(temp1, temp2, by=t1)

    #names in new case
    temp.names <- names(temp)

    #set up units
    units <- units(data1)[names(units(data1)) %in% temp.names]
    temp.names <- temp.names[!temp.names %in% names(units(data1))]
    units <- cbind(units, units(data2)[names(units(data2)) %in% temp.names])

    #reorder based on time case
#arrange seems a little unreliable???
#could replace with temp[order(...),]
#could be something else going on here???

    if(order)
        temp <- temp[order(temp[,t1]),]

    #tidy
    row.names(temp) <- 1:nrow(temp)

#this does not seem to work with POSIX classes
#    if(order)
#        temp <- arrange(temp, order(temp[,t1]))

    #might rethink this to get more info out of data 1, 2
    #makePEMS() & pems() return invisible()
    out <- makePEMS(temp, units)
    out

}



