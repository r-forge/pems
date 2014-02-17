##########################
##########################
##various plots 
##########################
##########################

#kr

#description
##########################
#different plot functions


#includes 
##########################
#pemsPlot
#(preprocess, panel and plot)
#latticePlot
#panel.PEMSXYPlot
#XYZPlot
#


#to do
##########################

#comments
##########################
#XYZPlot is not staying
#


###########################
###########################
##pemsPlot
###########################
###########################

####################
#pemsPlot
####################

#kr 17/02/2014
#version 0.0.1


pemsPlot <- function(x, y = NULL, z = NULL, ..., data = NULL, 
         cond = NULL, units = TRUE, fun.name="pemsPlot",
         panel = panel.pemsPlot, scheme = pems.scheme){

    #passes settings on to convert units

    #setup
    extra.args <- list(...)
    settings <- calcChecks(fun.name, ..., data = data)

    #standardise formula or x,y,z,cond input as formula
    #and grab units
    extra.args$units <- listLoad(listUpdate(extra.args, list(load="units")))
    extra.args <- pemsXYZCondUnitsHandler(x, y, z, cond, data = data,
                              units = units, settings = settings, ...)

    extra.args <- listUpdate(extra.args, list(panel = panel, scheme = scheme, 
                                              data = data))

    do.call(loaPlot, extra.args)

}


##############################
#pemsXYZCondUnitsHandler
##############################

pemsXYZCondUnitsHandler <- function(x, y = NULL, z = NULL, cond = NULL, data = NULL, units = TRUE, 
         ..., fun.name = "pemsXYZCondHandler", hijack= FALSE){

#think about error messaging and 
#visible message sources

#think about returning redundant data with not.formula

    #allows user to use both 
    #plot(x,y,z,...) and plot(formula,...) type calls
    #returns plot(formula) and updates units

    #setup
    extra.args <- list(...)
    settings <- calcChecks(fun.name, ..., data = data)

    #units handling 
    #note: this could be less complex
    extra.args <- do.call(listLoad, listUpdate(extra.args, 
                                               list(load="units", units=units)))
    extra.args$units <- listUpdate(list(add.to.labels=TRUE), extra.args$units)
    extra.args$units$units <- if(isGood4LOA(units)) TRUE else FALSE

    temp <- if(hijack) x else 
                checkInput(x, data=data, if.missing="return")
    if(as.character(temp)[1]=="~"){
        #this is a formula
        is.formula <- TRUE
    } else {
        is.formula <- FALSE
        x <- temp
    }

    #if formula I assume you have put everything in formula
    if(is.formula){

         #add x and y units to units
         temp <- formulaHandler(x, data, output="lattice.like")
         extra.args$units$x.units <- eval(parse(text=paste("getUnits(", temp$xlab,", ", "data", ", if.missing='return')", sep="")))
         extra.args$units$y.units <- eval(parse(text=paste("getUnits(", temp$ylab,", ", "data", ", if.missing='return')", sep="")))
         if("zlab" %in% names(temp))
             extra.args$units$z.units <- eval(parse(text=paste("getUnits(", temp$zlab,", ", "data", ", if.missing='return')", sep="")))
         #return ans
         #note units already in extra.args
         return(listUpdate(extra.args, list(x=x, data=data)))
        
    }

    #if formula you cant get to here
    #so this is none formula handling

    if(!hijack){   
        y <- checkInput(y, data=data, if.missing="return")
        z <- checkInput(z, data=data, if.missing="return")
    }

    if(is.null(x))
        checkIfMissing(settings$if.missing, reply = "argument 'x' not supplied or null")
    extra.args$units$x.units <- getUnits(x, unit.conversions = settings$unit.conversions, hijack = TRUE)
    if(is.null(y))
        checkIfMissing(settings$if.missing, reply = "argument 'y' not supplied or null")
    extra.args$units$y.units <- getUnits(y, unit.conversions = settings$unit.conversions, hijack = TRUE)
    if(!is.null(z))
        extra.args$units$z.units <- getUnits(z, unit.conversions = settings$unit.conversions, hijack = TRUE)

    if(is.null(z) & is.null(cond)) extra.args$x <- ~x*y
    if(is.null(z) & !is.null(cond)) extra.args$x <- ~x*y|cond
    if(!is.null(z) & is.null(cond)) extra.args$x <- z~x*y
    if(!is.null(z) & !is.null(cond)) extra.args$x <- z~x*y|cond

#    lab.fun <- function(x, def, units, units2){
#        temp <- if(is.null(attr(x, "name"))) def else attr(x, "name")
#        if(isGood4LOA(units$units))
#            if(units$add.to.label)
#                if(!is.null(units2))
#                    temp <- paste(temp, " [", units2, "]", sep="") 
#        temp
#    } 
    if(!"xlab" %in% names(extra.args)) 
       extra.args$xlab <- if(is.null(attr(x, "name"))) "x" else attr(x, "name")
    if(!"ylab" %in% names(extra.args)) 
       extra.args$ylab <- if(is.null(attr(y, "name"))) "y" else attr(y, "name")
    if(!is.null(z) && !"zlab" %in% names(extra.args)) 
       extra.args$zlab <- if(is.null(attr(z, "name"))) "z" else attr(z, "name")



#        extra.args$xlab <- lab.fun(x, "x", extra.args$units, extra.args$units$x.units)

    #extra.args$x must exist and data not needed
    #extra.args$units updated
    return(extra.args)


}



#######################
#preprocess.pemsPlot
#######################

preprocess.pemsPlot <- function(lattice.like=lattice.like, units=units,...){

#this is a bit messy
#might rethink how stuff goes in 

        extra.args <- list(...)
        settings <- calcChecks(...) #don't set fun.name or data here or put back in later

        xlab <- if(is.null(extra.args$xlab)) lattice.like$xlab else extra.args$xlab
        ylab <- if(is.null(extra.args$ylab)) lattice.like$ylab else extra.args$ylab
        zlab <- if(is.null(extra.args$zlab)) lattice.like$zlab else extra.args$zlab

#think about conversion.ref
#settings should pass down

        if(is.null(lattice.like$z)) lattice.like$z <- 1
        if(isGood4LOA(units$units)){

            if("x.to" %in% names(extra.args)){
                from <- if("x.from" %in% names(extra.args)) extra.args$x.from else units$x.units     
                lattice.like$x <- convertUnits(lattice.like$x, to=extra.args$x.to, from=from, 
                                               unit.conversions = settings$unit.conversions, 
                                               hijack=TRUE, force=TRUE)
                units$x.units <- extra.args$x.to
            }
            if("y.to" %in% names(extra.args)){
                from <- if("y.from" %in% names(extra.args)) extra.args$y.from else units$y.units     
                lattice.like$y <- convertUnits(lattice.like$y, to=extra.args$y.to, from=from, 
                                               unit.conversions = settings$unit.conversions,
                                               hijack=TRUE, force=TRUE)
                units$y.units <- extra.args$y.to
            }
            if("z.to" %in% names(extra.args)){
                from <- if("z.from" %in% names(extra.args)) extra.args$z.from else units$z.units     
                lattice.like$z <- convertUnits(lattice.like$z, to=extra.args$z.to, from=from, 
                                               unit.conversions = settings$unit.conversions,
                                               hijack=TRUE, force=TRUE)
                units$z.units <- extra.args$z.to
            }

            if("add.to.labels" %in% names(units) && units$add.to.labels){
                if(!is.null(xlab) && "x.units" %in% names(units))
                    lattice.like$xlab <- paste(xlab, " [", units$x.units, "]", sep="")
                xlab <- lattice.like$xlab
                if(!is.null(ylab) && "y.units" %in% names(units))
                    lattice.like$ylab <- paste(ylab, " [", units$y.units, "]", sep="")
                ylab <- lattice.like$ylab
                if(!is.null(zlab) && "z.units" %in% names(units))
                    lattice.like$zlab <- paste(zlab, " [", units$z.units, "]", sep="")
                zlab <- lattice.like$zlab

            }

        list(lattice.like=lattice.like, units=units, xlab=xlab, ylab=ylab, zlab=zlab)
        }
     }


############################
#panel.pemsPlot
############################

panel.pemsPlot <- function(..., plot=TRUE, process=TRUE, loa.settings = FALSE){

#could probably loss a lot of these arguments
#but then I would not document them anywhere

        extra.args <- list(...)

        if(loa.settings){
            temp <- loaHandler(panel.loaPlot)
            temp$common.args <- unique(c(temp$common.args, "units"))
            temp$default.settings <- listUpdate(temp$default.settings, 
                                                list(loa.preprocess = preprocess.pemsPlot))
            return(temp)
        }

        if(process){
               #stuff to do if processing
               #when processing and not plotting what to return
               #what you want to save or reference in all plots
        if(!plot) return(list())
        }
        if(plot){
            panel.loaPlot(...)
        }

    }

















##########################
##########################
##latticePlot
##########################
##########################

#kr 23/01/2012 v 0.0.6

#what it does
##########################
#wrapper for various nice bits 
#of lattice and latticeExtra
#

#to do
##########################
#make test more robust?

#panel... function to fix 
#re conditioning bwplots?



#comments
##########################
#



latticePlot <- function(x = NULL, data = NULL, plot = xyplot, panel = NULL, ..., 
                   greyscale = FALSE, fun.name = "latticePlot", hijack = FALSE){

    this.call <- match.call()
    extra.args <- list(...)
    settings <- calcChecks(fun.name, ..., data = data)

    #if greyscale
    if(greyscale){
         symbol <- gray(1:8 / 8)
         fill <- "grey"
         region <- gray(11:1 / 11)
         reference <- "black"
         bg <- "transparent"
         fg <- "black"

##################
#tidy code
#track down strip fore colour
##################

         temp <-list(plot.polygon = list(col = fill[1], border = fg[1]),
                     box.rectangle = list(col= symbol[1]),
             box.umbrella      = list(col= symbol[1]),
             dot.line          = list(col = reference),
             dot.symbol        = list(col = symbol[1]),
             plot.line         = list(col = symbol[1]),
             plot.symbol       = list(col= symbol[1]),
             regions           = list(col = colorRampPalette(region)(100)),
             reference.line    = list(col = reference),
             superpose.line    = list(col = symbol),
             superpose.symbol  = list(col = symbol),
             superpose.polygon = list(col = fill, border = fg),

             background        = list(col = bg),
             add.line          = list(col = fg),
             add.text          = list(col = fg),
             box.dot           = list(col = fg),
             axis.line         = list(col = fg),
             axis.text         = list(col = fg),
             strip.border      = list(col = fg),
             strip.background = list(col = "white"),
             strip.shingle = list(col="grey"),
             box.3d            = list(col = fg),
             par.xlab.text     = list(col = fg),
             par.ylab.text     = list(col = fg),
             par.zlab.text     = list(col = fg),
             par.main.text     = list(col = fg),
             par.sub.text      = list(col = fg))

       if(is.null(extra.args$par.settings))
           extra.args$par.settings <- temp else
           extra.args$par.settings[!names(temp) %in% names(extra.args$par.settings)] <- temp[!names(temp) %in% names(extra.args$par.settings)]
       
       #need to talk to ds?
    }

    #if x is NULL/not formula catcher
    if(is.null(x) || !is(x)[1] == "formula"){
         checkIfMissing(if.missing = settings$if.missing, 
             reply = "need a formula to work with", 
             suggest = "see ?latticePlot if unclear", if.warning = "resetting x to NULL", 
             fun.name = fun.name)
         x <- NULL
    }

    #work out dimensions of conditioning
    cond.dim <- as.character(x)
    temp <- grep("[|]", cond.dim[length(cond.dim)])
    if(length(temp)<1) temp <- 0
    if(temp > 0){
        temp2 <- grep("[+]", cond.dim[length(cond.dim)])
        if(length(temp2)<1) temp2 <- 0
        temp3 <- grep("[*]", cond.dim[length(cond.dim)])
        if(length(temp3)<1) temp3 <- 0
        cond.dim <- temp2 + temp3 + 1
    }

    #set up data
    if(isPEMS(data)) data <- pemsData(data)

    #set up inputs
    temp <- list(x = x, data = data)
    if(!is.null(panel)) temp$panel <- panel
    temp[names(extra.args)] <- extra.args
    
    ans <- do.call(plot, temp)

#    latter
#    if (cond.dim != 2) plot(ans) else plot(useOuterStrips(ans, strip = strip, strip.left = strip.left))

    ans

}





###############################
###############################
##panel.PEMSXYPlot
###############################
###############################



panel.PEMSXYPlot <- function(..., grid=NULL){

    temp <- list(h = -1, v = -1)
    if(is.list(grid))
        temp[names(grid)] <- grid

    do.call(panel.grid, temp)
    panel.xyplot(...)

} 










################################
################################
##XYZPlot
################################
################################

#notes
###################
#may supercede this with something from loa
#



XYZPlot <- function(x = NULL, ..., data = NULL, statistic = NULL, 
                    x.res = 10, y.res = 20, plot = levelplot,
                    fun.name = "XYZPlot", hijack = FALSE){


    ####################
    #setups
    ####################

    #extra.args

    this.call <- match.call()
    extra.args <- list(...)
    settings <- calcChecks(fun.name, ..., data = data)

    if(isPEMS(data)) data <- pemsData(data)
    
#    if(is.null(subset))
#        subset <- TRUE


    #get structure formula
    d1 <- try(latticeParseFormula(x, data, dimension = 3, 
                                  multiple = TRUE),
              silent = TRUE)
    if(is(d1)[1] == "try-error")
        checkIfMissing(if.missing = settings$if.missing, reply = "mismatched 'x/data' data combination", 
                       suggest = "see ?XYZPlot for help", fun.name = fun.name)

    ##################
    #statistcs handling 
    ##################

    #if missing 
    #if no z count of cases
    # if z mean
    if(is.null(statistic)){
        if(is.null(d1$left)){
            d1$left <- rep(1, length(d1$right.x))
            d1$left.name <- "count"
            statistic <- function(x) length(na.omit(x))
        } else {
            statistic <- function(x) mean(x, na.rm=TRUE)
        }
    }


    ##################
    #range settings 
    ##################

    #note axis are flipped 
    #relative to lattice

    ylim <- if("ylim" %in% names(extra.args))
                extra.args$ylim else range(d1$right.x, na.rm=TRUE)

    xlim <- if("xlim" %in% names(extra.args))
                extra.args$xlim else range(d1$right.y, na.rm=TRUE)

#temp disabled

    ylim <- range(d1$right.x, na.rm=TRUE)

    xlim <- range(d1$right.y, na.rm=TRUE)



    #################
    #make grids
    #################

#tidy

    if(!is.numeric(x.res))    
          x.res <- 10
    if(!is.numeric(y.res))    
          y.res <- 10

    x.res <- do.breaks(xlim, x.res)
    y.res <- do.breaks(ylim, y.res)

    x <- cut(d1$right.y, x.res)
    y <- cut(d1$right.x, y.res)

#add in here conditioning

    temp <- data.frame(x=x, y=y)

    ans <- aggregate(d1$left, temp, statistic) 
    names(ans)[ncol(ans)] <- "z"

    #tidy names

#    temp <- gsub("[(]|[)]|[[]|[]]", "", levels(ans[,1]))

    temp.fun <- function(x){
                    temp <- gsub("[(]|[)]|[[]|[]]", "", x)
                    t1 <- as.numeric(unlist(strsplit(temp, ","))[seq(1, 40, 2)])
                    t2 <- as.numeric(unlist(strsplit(temp, ","))[seq(2, 40, 2)])
                    temp <- signif(((t2 - t1) / 2) + t1, 2)
    }
    levels(ans[,1]) <- temp.fun(levels(ans[,1]))
    levels(ans[,2]) <- temp.fun(levels(ans[,2]))
     
    #tidy the ...
    #or it will fall over
 #   map.axis <- function(components, ...) 
 #                  axis.default(components = list(check.overlap=TRUE) ...)

    if(!"ylab" %in% names(extra.args))
        extra.args$ylab <- d1$right.x.name
    if(!"xlab" %in% names(extra.args))
        extra.args$xlab <- d1$right.y.name
    if(!"zlab" %in% names(extra.args))
        extra.args$zlab <- d1$left.name

    temp <- list(x = z~x*y, data=ans)
    temp[names(extra.args)] <- extra.args

    do.call(plot, temp)

}    
    

#return(ans)


#############
#cond currently 
#dissabled
#############

#    cond.res <- 0
#    if(length(d1$condition)>0){
#        cond.res <- levels(d1$condition[[1]])
#        cond <- d1$condition[[1]]
#    }
    

#    grid <- expand.grid(x = x.res, y = y.res)

        

    

#return(d1)

    ###################
    #



#}