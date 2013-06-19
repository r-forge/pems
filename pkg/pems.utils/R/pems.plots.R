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