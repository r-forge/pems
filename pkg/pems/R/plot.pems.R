plot.pems <-
function (pems = NULL, plot = NULL, fun.method = "obs.default", 
          groups = 1:length(y.n), cols = NULL,
          ...) 
{

#to think about
##############
#rescaling and normalisation
#put extra conditioning back into end plot
#put factor calls into x and y options

#simplify - lot of extra stuff in there

    require(lattice)
    require(RColorBrewer)

    if (is.null(pems)) {
        stop("\t plot.pems: no pems", call. = FALSE, domain = NA)
    }
    if (is(pems)[1] == "pems") {
        data <- pems$data
    }
    else {
        data <- pems #for direct calls and standard data frames
    }

    this.method <- check.method(fun.method, c("obs.default"), 
        "plot.pems", if.missing = "return.false")

    if ((try(is(plot), silent = TRUE))[1] == "NULL") {
        if (this.method != FALSE) {
            if(this.method == "obs.default"){
                plot <- "conc.co + conc.co2 + conc.hc + conc.nox ~ local.time"
                
            }
            message(paste("comment: plot.pems: applied default plot using method '", this.method, "'\n\t [",
                plot, "]", sep=""))
            plot <- as.formula(plot)
        } else {
            stop(paste("\t plot.pems: fun.method '", fun.method, "' not recognised \n\t [and no valid 'plot' formula]", 
                sep = ""), call. = FALSE, domain = NA)
        }
    }

    if ((try(is(plot), silent = TRUE))[1] != "formula") {
            stop(paste("\t plot.pems: plot '", substitute(plot), "' not recognised formula \n\t [plot in form y1 + y2 + ... yn ~ x | (optional)]",
                sep = ""), call. = FALSE, domain = NA)
    }

    plot.args <- all.vars(plot)
    valid.args <- plot.args[plot.args %in% names(data)]

    if(!identical(plot.args, valid.args)){
            stop(paste("\t plot.pems: plot parameter(s) missing \n\t [check: ", paste (plot.args[!plot.args %in% names(data)], sep="", collapse = ", "), "]",
                sep = ""), call. = FALSE, domain = NA)
    }

    #sep conditioning
    x.names <- strsplit(as.character(plot[3]), " | ", fixed = TRUE)[[1]]
    if(length(x.names)>1){
        cond <- x.names[2]
        cond <- gsub(" ", "", cond)
        cond <- strsplit(cond, "+", fixed = TRUE)[[1]]
        cond <- unlist(lapply(cond, strsplit, "*", fixed = TRUE))
    } else {
        cond <- NULL
    }

    #remake plot
    plot <- as.formula(paste(plot[2], plot[1], x.names[1], sep=" "))

    x.names <- gsub(" ", "", x.names[1])
    x.names <- strsplit(x.names, "+", fixed = TRUE)[[1]]
    x.names <- unlist(lapply(x.names, strsplit, "*", fixed = TRUE))

    if (length(x.names)>1) {
        message(paste("warning: plot.pems: plots to common 'x' axis \n\t only x = '", x.names[1], "' applied", sep="")) 
        message(paste("\t [discarded: ",paste(x.names[!x.names %in% x.names[1]],sep="", collapse=", "), "]",sep=""))
        x.names <- x.names[1]
    }

    plot <- as.formula(paste(plot[2], plot[1], x.names, sep=" "))

    y.names <- all.names(plot[2])
    y.names <- y.names[y.names %in% names(data)] 

         if (length(y.names)<1) {
             stop("\t plot.pems: no valid 'y' axis"
                 , call. = FALSE, domain = NA)
         } #should never happen

    if(length(cols)>length(y.names)){
        cols <- cols[1:length(y.names)]
        message(paste("warning: plot.pems: cols appears overlong by comparison to y cases", sep="")) 
        message(paste("\t cols foreshortened",sep=""))
    }

######################
#handle not enough cols better?

    if(length(cols)<length(y.names) & !is.null(cols)){
        cols <- NULL
        message(paste("warning: plot.pems: cols appears short by comparison to y cases", sep="")) 
        message(paste("\t default cols applied",sep=""))
    }

    if(is.null(cols)){
         cols <- suppressWarnings(brewer.pal(length(y.names), "Set1"))
    }


    y.n <- 1:length(y.names)

    d <- cbind(data[y.names])

####################
#rescaling????

#    y.names <- paste(letters[1:length(y.names)], y.names, sep=":")
#    names(d) <- y.names

    d2 <- stack(d)
    d2 <- cbind(x = rep(data[,x.names], length(y.names)), d2)

    if(length(groups)>length(y.names)){
        groups <- groups[1:length(y.names)]
        message(paste("warning: plot.pems: groups appears overlong by comparison to y cases", sep="")) 
        message(paste("\t only groups = c(",paste(groups,sep="", collapse=", "), ") applied by foreshortening",sep=""))
    }
    if(length(groups)<length(y.names)){
        groups <- rep(groups, length.out = length(y.names))
        message(paste("warning: plot.pems: groups appears short by comparison to y cases", sep="")) 
        message(paste("\t groups = c(",paste(groups,sep="", collapse=", "), ") applied by extrapolation",sep=""))
    }

    final.cond <- as.numeric(factor(rank(groups)))

    final.cols <- lapply(1:max(final.cond),
                       function(x){
                           cols[final.cond==x]
                       })

    final.cond <- sapply(1:length(y.n),
                       function(x){
                           paste(names(d)[y.n[final.cond==final.cond[x]]], collapse =", ")
                       })

    final.groups <- sapply(1:length(final.cond), 
                       function(x, final.cond){
                            length(final.cond[1:x][final.cond[1:x]==final.cond[x]])
                       }, final.cond)

    final.cond <- rep(final.cond, each = nrow(d))
    final.groups <- rep(final.groups, each = nrow(d))

    d2 <- cbind(d2, final.cond, final.groups)

    final.scales <- lapply(levels(d2$final.cond), 
                        function(x){ 
                            range(subset(d2$values, d2$final.cond==x)
                                  , na.rm = TRUE, finite = TRUE) 
                        }
                     )
    
    if(!is.null(cond)){
         cond.2 <- data[cond]
         cond.2 <- data.frame(lapply(cond.2, rep, length(y.names)))
         d2 <- cbind(d2, cond.2)
    }

    no.col <- 2^length(cond)

    if(is.null(cond)) {
         extra.cond <- "as.character(final.cond)"
    } else {
         extra.cond <- paste(cond, collapse="+")
         extra.cond <- paste(extra.cond, "as.character(final.cond)", sep ="+")
    }

    extra.cond <- paste("values ~ x|", extra.cond, sep="")
    extra.cond <- as.formula(extra.cond)

    layout <- c(no.col,length(unique(final.cond)))

print(no.col)
print(final.scales)
print(layout)

    xyplot(extra.cond, groups = final.groups, data = d2, 
#         layout=c(no.col,length(unique(final.cond))), 

         layout = layout,

         #type="l",

      scales =
       list(y =
            list(relation="free", rot = 0,
                 limits = rep(final.scales, 
                              each = no.col),
                  at = rep(rep(list(TRUE, NULL), 
                           c(1, no.col-1)), length(unique(final.cond)))
                          )),

#      auto.key = list(space="right", points = FALSE, lines = FALSE
#                      , text = rev(y.names)
#                      , col = rev(cols)),

       par.settings =
       list(layout.widths = 
            list(axis.panel = c(1,rep(0,no.col-1)))),



#       scales =
#             list(y =
#                 list(relation="free", rot = 0,
#                     limits = rep(final.scales, 
#                                 c(length(unique(final.cond)),length(unique(final.cond)))),
#                     at = rep(list( 1:5, NULL, 1:20, NULL ), 
#                            c(length(unique(final.cond)), length(unique(final.cond)), length(unique(final.cond)), length(unique(final.cond))) ))),
#       par.settings =
#           list(layout.widths = 
#               list(axis.panel = rep(c(1, 0), c(1, 5)))),


#         scales = list(y = list(relation = "free"), rot=0,
#                           limits = rep(list(c(0,3), c(0,20)), c(0,1000))
#                       ),
         ...,
         #prepanel = function(x,y,...){
         #    list(ylim = final.scales[[ceiling(panel.number()/no.col)]])
         #}, 

         panel = function(x, y, ...) {

         #mycols <- cols[c(1,1,1)]
         #if(panel.number()==2) {mycols <- cols[c(4,4,4)]}


   #if(current.column()==no.col) panel.axis("right") else panel.axis("left")

         panel.grid(-1, -1)
         #panel.xyplot(x = x[1:2], y = final.scales[[ceiling(panel.number()/no.col)]],
         #    ...)
         panel.xyplot(x, y, 
             col.line = final.cols[[current.row()]],
             #ylim=final.scales[[current.row()]],
             type="l", 
             ...)
           
        }
    )    
}

