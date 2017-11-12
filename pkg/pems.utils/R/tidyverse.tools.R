##########################
##########################
##tidyverse stuff
##########################
##########################

#package
#(new/old structure)
#(old in, code, out)
#(in either, rebuild new->old, code old, out old)
#(in either, rebuild old->new, code new, rebuil new->old, out old)
#(in either, rebuild old->new, code new, out new)


#ggplot2 
#(in either, rebuild old->new, code new, out to ggplot)
#        - fortify.pems

#dplyr   
#(in either, rebuild old->new, code new, rebuil new->old, out old)
#        - select, select_.pems
#        - rename, rename_.pems
#        - filter, filter_.pems
#        - arrange, arrange_.pems
#        - slice, slice_.pems        
#(in either, rebuild new->old, code old, out old)
#        - mutate.pems, mutate_.pems
#(in development)
#        - group_by, group_by_.pems, groups.pems, ungroup.pems, 
#          group_size.pems, n_groups.pems


#(identified to do)
#        - summarise, summarise_
#        - 




###########################
###########################
##ggplot2
###########################
###########################

####################
#fortify.pems
####################

#kr 13/08/2015
#version 0.0.1

#what it does
###########################################
#allows users to work directly with ggplot2


#to do
############################
#decide if we are keeping it

#if keeping it 

#like to
#########################################
#would like to pass pems units to ggplot2 
#via fortify

fortify.pems <- function (model, data, ...) {

#problem with this and group_by

    #transistioning pems build.type
    model <- rebuildPEMS(model, "new")

    ############################
    #as.data.frame(model)
    ##################
    #above now replaced with below 
    #because ggplot now tripping on 
    #pems.element class
    
    #might be a tidier way to do this...

    x <- model
    class(x) <- class(x) [class(x) != "pems"]
    if(length(class(x))==1) class(x) <- "data.frame"
    for(i in names(x)) class(x[,i]) <- class(x[,i])[class(x[,i])!="pems.element"]
    x
}



########################
########################
##misc
########################
########################

#unexported
## deactived because too much unexported....

#this is compat_lazy_dots from dplyr
#        and compat_lazy from rlang
#included because all the *_.data.frame methods use them
#and pems methods are basically wrappers for these

#might build on other non-exported functions

##compat_lazy_dots <- function (dots, env, ..., .named = FALSE){
##
##    if (missing(dots)) { 
##        dots <- list()
##    }
##    if (inherits(dots, c("lazy", "formula"))) {
##        dots <- list(dots)
##    } else {
##        dots <- unclass(dots)
##    }
##    dots <- c(dots, list(...))
##    warn <- TRUE
##    for (i in seq_along(dots)) {
##        dots[[i]] <- compat_lazy(dots[[i]], env, warn)
##        warn <- FALSE
##    }
##    named <- have_name(dots)
##    if (.named && any(!named)) {
##        nms <- map_chr(dots[!named], f_text)
##        names(dots)[!named] <- nms
##    }
##    names(dots) <- names2(dots)
##    dots
##}

##compat_lazy <- function (lazy, env = caller_env(), warn = TRUE) 
##{
##    if (warn) 
##        warn_underscored()
##    if (missing(lazy)) {
##        return(quo())
##    }
##    coerce_type(lazy, "a quosure", formula = as_quosure(lazy, 
##        env), symbol = , language = new_quosure(lazy, env), string = , 
##        character = {
##            if (warn) 
##                warn_text_se()
##            parse_quosure(lazy[[1]], env)
##        }, logical = , integer = , double = {
##            if (length(lazy) > 1) {
##                warn("Truncating vector to length 1")
##                lazy <- lazy[[1]]
##            }
##            new_quosure(lazy, env)
##        }, list = coerce_class(lazy, "a quosure", lazy = new_quosure(lazy$expr, 
##            lazy$env)))
##}







#########################
#########################
##dplyr
#########################
#########################



#########################
#########################
##select
#########################
#########################

#kr v.0.1 31/10/2017


#might be able to simplify these in future
#might leave as is because robust


#select and select_
#pems.utils


select.pems <- function (.data, ...){

############################################
#this is overkill because I am currently running two versions of the pems object
#should be able to simplify this when new version is in place.
#############################################
#this rebuilds as new to work with
#then exports as old
#this slows things down but 
#    means rest of system still running
#############################################



#select should change data dimensions and 
#       could change names
#select(pems, speed=velocity)

   #for new build only
   .data <- rebuildPEMS(.data, "new")



    bare.bones <- attributes(.data)[names(attributes(.data)) %in% c("units", "pems.tags")]
    attributes(.data)$units <- NULL
    attributes(.data)$pems.tags <- NULL
    class(.data) <- if(length(class(.data))==1) 
          "data.frame" else
               class(.data)[class(.data) != "pems"] 

    vars <- select_vars(names(.data), !!! quos(...))
    .data <- select(.data, vars)
    bare.bones$units <- select(bare.bones$units, vars)

    #rename
    names(.data) <- names(vars)
    for(i in names(vars))
          attributes(.data[,i])$name <- i 
    names(bare.bones$units) <- names(vars)

    attributes(.data)$units <- bare.bones$units
    attributes(.data)$pems.tags <- bare.bones$pems.tags

    class(.data) <- if(length(class(.data))==1) 
          "pems" else
              unique(c("pems", class(.data)))

#  return as old pems
   rebuildPEMS(.data, "old")

}

select_.pems <- function (.data, ..., warn=TRUE){

   if(warn)
        warning(paste("In select_.pems: dplyr underscores deprecated;", 
        "see rlang `quo()` documentation", sep=" "), call. = FALSE)
   #########################
   #this is from select.pems
   #wrapping *_.data.frame needs 
   #  too much that is not exported
   #########################

   .data <- rebuildPEMS(.data, "new")

#this is select.pems above

    bare.bones <- attributes(.data)[names(attributes(.data)) %in% c("units", "pems.tags")]
    attributes(.data)$units <- NULL
    attributes(.data)$pems.tags <- NULL
    class(.data) <- if(length(class(.data))==1) 
          "data.frame" else
               class(.data)[class(.data) != "pems"] 
    vars <- select_vars(names(.data), !!! quos(...))
    .data <- select(.data, vars)
    bare.bones$units <- select(bare.bones$units, vars)
    names(.data) <- names(vars)
    for(i in names(vars))
          attributes(.data[,i])$name <- i 
    names(bare.bones$units) <- names(vars)
    attributes(.data)$units <- bare.bones$units
    attributes(.data)$pems.tags <- bare.bones$pems.tags
    class(.data) <- if(length(class(.data))==1) 
          "pems" else
              unique(c("pems", class(.data)))
   rebuildPEMS(.data, "old")

}


#testing this

#can i go...
#select_.pems <- function(.data, ..., .dots = list()) {
#  dots <- compat_lazy_dots(.dots, caller_env(), ...)
#  select(.data, !!! dots)
#}

#select_.pems <- function(.data, ..., .dots = list()) {
#  dots <- compat_lazy_dots(.dots, caller_env(), ...)
#   .data[["data"]] <- select(.data[["data"]], !!! dots)
#   .data[["units"]] <- select(.data[["units"]], !!! dots)
#   #################################
#   # as with select.pems 
#   #################################
#   .data
#}







#########################
#########################
##rename
#########################
#########################

#kr v.0.1 31/11/2017

#rename and rename_
#pems.utils


rename.pems <- function (.data, ...){

#rename should change names but not dimensions
#select(pems, speed=velocity)

   .data <- rebuildPEMS(.data, "new")

    bare.bones <- attributes(.data)[names(attributes(.data)) %in% c("units", "pems.tags")]
    attributes(.data)$units <- NULL
    attributes(.data)$pems.tags <- NULL
    class(.data) <- if(length(class(.data))==1) 
          "data.frame" else
               class(.data)[class(.data) != "pems"] 
################################
    vars <- rename_vars(names(.data), !!! quos(...))
#this is select.pems above
#with select_vars replaced with rename_vars in above
################################   
    .data <- select(.data, vars)
    bare.bones$units <- select(bare.bones$units, vars)
    names(.data) <- names(vars)
    for(i in names(vars))
          attributes(.data[,i])$name <- i 
    names(bare.bones$units) <- names(vars)
    attributes(.data)$units <- bare.bones$units
    attributes(.data)$pems.tags <- bare.bones$pems.tags
    class(.data) <- if(length(class(.data))==1) 
          "pems" else
              unique(c("pems", class(.data)))
   rebuildPEMS(.data, "old")

}

#as rename.pems with warning
rename_.pems <- function (.data, ..., warn = TRUE){

   if(warn)
        warning(paste("In rename_.pems: dplyr underscores deprecated;", 
        "see rlang `quo()` documentation", sep=" "), call. = FALSE)

   .data <- rebuildPEMS(.data, "new")

    bare.bones <- attributes(.data)[names(attributes(.data)) %in% c("units", "pems.tags")]
    attributes(.data)$units <- NULL
    attributes(.data)$pems.tags <- NULL
    class(.data) <- if(length(class(.data))==1) 
          "data.frame" else
               class(.data)[class(.data) != "pems"] 
    vars <- rename_vars(names(.data), !!! quos(...))
    .data <- select(.data, vars)
    bare.bones$units <- select(bare.bones$units, vars)
    names(.data) <- names(vars)
    for(i in names(vars))
          attributes(.data[,i])$name <- i 
    names(bare.bones$units) <- names(vars)
    attributes(.data)$units <- bare.bones$units
    attributes(.data)$pems.tags <- bare.bones$pems.tags
    class(.data) <- if(length(class(.data))==1) 
          "pems" else
              unique(c("pems", class(.data)))
   rebuildPEMS(.data, "old")

}


#write code in form... 
#function(d, a) {
#a <- enquo(a)
#select(d, !!a)
#}

#is there a enquos to enquo like quos to quos...
#plans to export function for *_.* methods???








#########################
#########################
##filter
#########################
#########################

#kr v.0.1 04/11/2017

#filter and filter_
#pems.utils

filter.pems <- function(.data, ...){

#filter can change dimensions (row number)
#but not number of columns or names

    #based on filter.data.frame
    ##.data[["data"]] <- as.data.frame(filter(tbl_df(.data[["data"]]), ...))
    ##.data

    #new structure
    .data <- rebuildPEMS(.data, "new")

    ####################
    #break
    bare.bones <- attributes(.data)[names(attributes(.data)) %in% c("units", "pems.tags")]
    attributes(.data)$units <- NULL
    attributes(.data)$pems.tags <- NULL
    class(.data) <- if(length(class(.data))==1) 
          "data.frame" else
               class(.data)[class(.data) != "pems"] 
    ######################

    .data <- as.data.frame(filter(tbl_df(.data), ...))
    
    #######################
    #rebuild
    attributes(.data)$units <- bare.bones$units
    attributes(.data)$pems.tags <- bare.bones$pems.tags
    class(.data) <- if(length(class(.data))==1) 
          "pems" else
              unique(c("pems", class(.data)))
    #######################
    rebuildPEMS(.data, "old")

}

#as filter.pems with warning
filter_.pems <- function(.data, ..., warn=TRUE){

    #testing something
    if(warn)
        warning(paste("In filter_.pems: dplyr underscores deprecated;", 
        "see rlang `quo()` documentation", sep=" "), call. = FALSE)

    .data <- rebuildPEMS(.data, "new")
    bare.bones <- attributes(.data)[names(attributes(.data)) %in% c("units", "pems.tags")]
    attributes(.data)$units <- NULL
    attributes(.data)$pems.tags <- NULL
    class(.data) <- if(length(class(.data))==1) 
          "data.frame" else
               class(.data)[class(.data) != "pems"] 
    .data <- as.data.frame(filter(tbl_df(.data), ...)) 
    attributes(.data)$units <- bare.bones$units
    attributes(.data)$pems.tags <- bare.bones$pems.tags
    class(.data) <- if(length(class(.data))==1) 
          "pems" else
              unique(c("pems", class(.data)))
   rebuildPEMS(.data, "old")

}









#########################
#########################
##arrange
#########################
#########################

#kr v.0.1 04/11/2017

#arrange and arrange_
#pems.utils

arrange.pems <- function(.data, ...){

#arrange re orders but names, n.cols and n.rows
#    have to stay the same - I think

#    #based on arange.data.frame
#    .data[["data"]] <- as.data.frame(arrange(tbl_df(.data[["data"]]), ...))
#    .data

    #new structure
    .data <- rebuildPEMS(.data, "new")

    ####################
    #break
    bare.bones <- attributes(.data)[names(attributes(.data)) %in% c("units", "pems.tags")]
    attributes(.data)$units <- NULL
    attributes(.data)$pems.tags <- NULL
    class(.data) <- if(length(class(.data))==1) 
          "data.frame" else
               class(.data)[class(.data) != "pems"] 
    ######################

    .data <- as.data.frame(arrange(tbl_df(.data), ...))
    
    #######################
    #rebuild
    attributes(.data)$units <- bare.bones$units
    attributes(.data)$pems.tags <- bare.bones$pems.tags
    class(.data) <- if(length(class(.data))==1) 
          "pems" else
              unique(c("pems", class(.data)))
    #######################
    rebuildPEMS(.data, "old")

}


#like arrange.pem with warning
arrange_.pems <- function(.data, ..., warn=TRUE){

    #like above
    if(warn)
        warning(paste("In arrange_.pems: dplyr underscores deprecated;", 
        "see rlang `quo()` documentation", sep=" "), call. = FALSE)
    
    .data <- rebuildPEMS(.data, "new")
    bare.bones <- attributes(.data)[names(attributes(.data)) %in% c("units", "pems.tags")]
    attributes(.data)$units <- NULL
    attributes(.data)$pems.tags <- NULL
    class(.data) <- if(length(class(.data))==1) 
          "data.frame" else
               class(.data)[class(.data) != "pems"] 
    .data <- as.data.frame(arrange(tbl_df(.data), ...))
    attributes(.data)$units <- bare.bones$units
    attributes(.data)$pems.tags <- bare.bones$pems.tags
    class(.data) <- if(length(class(.data))==1) 
          "pems" else
              unique(c("pems", class(.data)))
    rebuildPEMS(.data, "old")

}







#########################
#########################
##slice
#########################
#########################

#kr v.0.1 05/11/2017

#slice and slice_
#pems.utils


slice.pems <- function(.data, ...) {

#filter can change dimensions (row number)
#but not number of columns or names

    #this is not like ...data.frame 
    #transposed code tripped on unexported functions...

#    .data[["data"]] <- as.data.frame(slice(tbl_df(.data[["data"]]), ...))
#    .data

#new structure

    .data <- rebuildPEMS(.data, "new")

    ####################
    #break
    bare.bones <- attributes(.data)[names(attributes(.data)) %in% c("units", "pems.tags")]
    attributes(.data)$units <- NULL
    attributes(.data)$pems.tags <- NULL
    class(.data) <- if(length(class(.data))==1) 
          "data.frame" else
               class(.data)[class(.data) != "pems"] 
    ######################

    .data <- as.data.frame(slice(tbl_df(.data), ...))
    
    #######################
    #rebuild
    attributes(.data)$units <- bare.bones$units
    attributes(.data)$pems.tags <- bare.bones$pems.tags
    class(.data) <- if(length(class(.data))==1) 
          "pems" else
              unique(c("pems", class(.data)))
    #######################
    rebuildPEMS(.data, "old")



}


#like slice.pems with warning
slice_.pems <- function(.data, ..., warn=TRUE) {

    #like above
    if(warn)
        warning(paste("In slice_.pems: dplyr underscores deprecated;", 
        "see rlang `quo()` documentation", sep=" "), call. = FALSE)

    .data <- rebuildPEMS(.data, "new")
    bare.bones <- attributes(.data)[names(attributes(.data)) %in% c("units", "pems.tags")]
    attributes(.data)$units <- NULL
    attributes(.data)$pems.tags <- NULL
    class(.data) <- if(length(class(.data))==1) 
          "data.frame" else
               class(.data)[class(.data) != "pems"] 
    .data <- as.data.frame(slice(tbl_df(.data), ...))
    attributes(.data)$units <- bare.bones$units
    attributes(.data)$pems.tags <- bare.bones$pems.tags
    class(.data) <- if(length(class(.data))==1) 
          "pems" else
              unique(c("pems", class(.data)))
    rebuildPEMS(.data, "old")

}









#########################
#########################
##mutate
#########################
#########################

#kr v.0.1 05/11/2017

#mutate and mutate_
#pems.utils

# export

mutate.pems <- function(.data, ..., units=NULL, warn=TRUE) {

#this needs tidying
#but carefully it is a lot tricker than you think...

    #make .data new pems structure
    .data <- rebuildPEMS(.data, "new")

    #this might seem weird
    #see edit(dplyr:::transmute.default) 
    #this is renamed quos...
    m.vars <- exprs_auto_name(quos(...)) 
    m.vars <- gsub("~", "", names(m.vars))

    ######################################
    #break pems like in other functions

    bare.bones <- attributes(.data)[names(attributes(.data)) %in% c("units", "pems.tags")]
    attributes(.data)$units <- NULL
    attributes(.data)$pems.tags <- NULL
    class(.data) <- if(length(class(.data))==1) 
          "data.frame" else
               class(.data)[class(.data) != "pems"]
 
    #####################################
    #mutate data
    .data <- as.data.frame(mutate(tbl_df(.data), ...))

#######################
# this codes puts units in right places
# like to simply

    unit.rb <- !all(m.vars %in% names(bare.bones$units))
    if(unit.rb){
        #unit reset rebuild
        temp <- data.frame(t(rep(NA, ncol(.data))))
        names(temp) <- names(.data)
        temp[names(bare.bones$units)] <- bare.bones$units
        bare.bones$units <- temp
   }
   
   new.units <- bare.bones$units[m.vars]
   if(!is.null(units))
       if(length(units)==ncol(new.units))
           new.units <- units else {
               #fill the NAs with units
               test <- is.na(new.units)
               if(any(test))
                    units <- rep(units, length.out=length(test[test]))
                    new.units[test] <- units 
           }
    bare.bones$units[m.vars] <- new.units               
    #hard reset attributes
    for(i in m.vars){
             attributes(.data[,i])$name <- i
             attributes(.data[,i])$units <- bare.bones$units[,i]
        }
    if(warn & any(is.na(bare.bones$units)))
          warning(paste("In mutate.pems: new elements not assigned units", 
          "see `mutate.pems()` documentation", sep=" "), call. = FALSE)
#    
##################

    #######################
    #rebuild
    attributes(.data)$units <- bare.bones$units
    attributes(.data)$pems.tags <- bare.bones$pems.tags
    class(.data) <- if(length(class(.data))==1) 
          "pems" else
              unique(c("pems", class(.data)))
    #######################
    #return .data as old pems structure
    rebuildPEMS(.data, "old")

}


#mutate.pems with extra warning
mutate_.pems <- function(.data, ..., units=NULL, warn=TRUE) {

    #like above
    if(warn)
        warning(paste("In mutate_.pems: dplyr underscores deprecated;", 
        "see rlang `quo()` documentation", sep=" "), call. = FALSE)

    .data <- rebuildPEMS(.data, "new")

    m.vars <- exprs_auto_name(quos(...)) 
    m.vars <- gsub("~", "", names(m.vars))
    bare.bones <- attributes(.data)[names(attributes(.data)) %in% c("units", "pems.tags")]
    attributes(.data)$units <- NULL
    attributes(.data)$pems.tags <- NULL
    class(.data) <- if(length(class(.data))==1) 
          "data.frame" else
               class(.data)[class(.data) != "pems"]
    .data <- as.data.frame(mutate(tbl_df(.data), ...))
    unit.rb <- !all(m.vars %in% names(bare.bones$units))
    if(unit.rb){
        temp <- data.frame(t(rep(NA, ncol(.data))))
        names(temp) <- names(.data)
        temp[names(bare.bones$units)] <- bare.bones$units
        bare.bones$units <- temp
   }
   
   new.units <- bare.bones$units[m.vars]
   if(!is.null(units))
       if(length(units)==ncol(new.units))
           new.units <- units else {
               test <- is.na(new.units)
               if(any(test))
                    units <- rep(units, length.out=length(test[test]))
                    new.units[test] <- units 
           }
    bare.bones$units[m.vars] <- new.units               
    for(i in m.vars){
             attributes(.data[,i])$name <- i
             attributes(.data[,i])$units <- bare.bones$units[,i]
        }
    if(warn & any(is.na(bare.bones$units)))
          warning(paste("In mutate.pems: new elements not assigned units", 
          "see `mutate.pems()` documentation", sep=" "), call. = FALSE)
    attributes(.data)$units <- bare.bones$units
    attributes(.data)$pems.tags <- bare.bones$pems.tags
    class(.data) <- if(length(class(.data))==1) 
          "pems" else
              unique(c("pems", class(.data)))
    rebuildPEMS(.data, "old")

}










#########################
#########################
##summarise
#########################
#########################

#kr v.0.1 05/11/2017

#summarise, etc....
#pems.utils




summarise.pems <- function(.data, ...) {

#summarise changes everything
#suggest this drops whatever

##summarise.data.frame <- function(.data, ...) {
##  as.data.frame(summarise(tbl_df(.data), ...)) 
##}

#new structure

    .data <- rebuildPEMS(.data, "new")

    ####################
    #break
    bare.bones <- attributes(.data)[names(attributes(.data)) %in% c("units", "pems.tags")]
    attributes(.data)$units <- NULL
    attributes(.data)$pems.tags <- NULL
    class(.data) <- if(length(class(.data))==1) 
          "data.frame" else
               class(.data)[class(.data) != "pems"] 
    ######################

    .data <- as.data.frame(summarise(tbl_df(.data), ...))
    
    #######################
    #rebuild
    ##attributes(.data)$units <- bare.bones$units
    ##attributes(.data)$pems.tags <- bare.bones$pems.tags
    ##class(.data) <- if(length(class(.data))==1) 
    ##      "pems" else
    ##          unique(c("pems", class(.data)))
    #######################
    ##rebuildPEMS(.data, "old")

    .data

}

#like above but warning
summarise_.pems <- function(.data, ..., warn=TRUE) {

#summarise changes everything
#suggest this drops whatever

##summarise.data.frame <- function(.data, ...) {
##  as.data.frame(summarise(tbl_df(.data), ...)) 
##}

    if(warn)
        warning(paste("In summarise_.pems: dplyr underscores deprecated;", 
        "see rlang `quo()` documentation", sep=" "), call. = FALSE)


#new structure

    .data <- rebuildPEMS(.data, "new")

    ####################
    #break
    bare.bones <- attributes(.data)[names(attributes(.data)) %in% c("units", "pems.tags")]
    attributes(.data)$units <- NULL
    attributes(.data)$pems.tags <- NULL
    class(.data) <- if(length(class(.data))==1) 
          "data.frame" else
               class(.data)[class(.data) != "pems"] 
    ######################

    .data <- as.data.frame(summarise(tbl_df(.data), ...))
    
    #######################
    #rebuild
    ##attributes(.data)$units <- bare.bones$units
    ##attributes(.data)$pems.tags <- bare.bones$pems.tags
    ##class(.data) <- if(length(class(.data))==1) 
    ##      "pems" else
    ##          unique(c("pems", class(.data)))
    #######################
    ##rebuildPEMS(.data, "old")

    ##output 
    #whatever summarise gives us...
    .data

}













#####################
#yet to redo...
#####################




#########################
#########################
##group_by
#########################
#########################

#kr v.0.1 05/11/2017

#group_by, etc....
#pems.utils


#not yet working


group_by.pems <- function(.data, ..., add = FALSE) {

#this'll be fun...
  
  #make sure it is new...

    .data <- rebuildPEMS(.data, "new")

    ####################
    #break
    bare.bones <- attributes(.data)[names(attributes(.data)) %in% c("units", "pems.tags")]
    attributes(.data)$units <- NULL
    attributes(.data)$pems.tags <- NULL
    class(.data) <- if(length(class(.data))==1) 
          "data.frame" else
               class(.data)[class(.data) != "pems"] 
    ######################

    #function coding
    groups <- group_by_prepare(.data, ..., add = add)
    out <- grouped_df(groups$data, groups$group_names)

    #add group term units if not there
    for(i in attributes(out)$vars)
          if(!i %in% names(bare.bones$units))
                bare.bones$units[,i] <- ""

    #######################
    #rebuild
    attributes(out)$units <- bare.bones$units
    attributes(out)$pems.tags <- bare.bones$pems.tags
#    class(out) <- unique(c(c("grouped_df", "pems"), class(out)))

#testing
    class(out) <- c("grouped_df", "pems")
#    return(out)

    #######################
    rebuildPEMS(out, "old")

}


#as above plus warning
group_by_.pems <- function(.data, ..., add = FALSE, warn = TRUE) {

    #like above
    if(warn)
        warning(paste("In group_by_.pems: dplyr underscores deprecated;", 
        "see rlang `quo()` documentation", sep=" "), call. = FALSE)
  
    .data <- rebuildPEMS(.data, "new")

    ####################
    #break
    bare.bones <- attributes(.data)[names(attributes(.data)) %in% c("units", "pems.tags")]
    attributes(.data)$units <- NULL
    attributes(.data)$pems.tags <- NULL
    class(.data) <- if(length(class(.data))==1) 
          "data.frame" else
               class(.data)[class(.data) != "pems"] 
    ######################

    #function coding
    groups <- group_by_prepare(.data, ..., add = add)
    out <- grouped_df(groups$data, groups$group_names)

    #add group term units if not there
    for(i in attributes(out)$vars)
          if(!i %in% names(bare.bones$units))
                bare.bones$units[,i] <- ""

    #######################
    #rebuild
    attributes(out)$units <- bare.bones$units
    attributes(out)$pems.tags <- bare.bones$pems.tags
#    class(out) <- unique(c(c("grouped_df", "pems"), class(out)))

#testing
    class(out) <- c("grouped_df", "pems")
    return(out)

    #######################
    rebuildPEMS(out, "old")

}









#I think ...data.frame runs through ungroup_grouped_df
#regardless of how I think methods work...

ungroup.pems <- function(x, ...){

#this will need fixing if it works...
   
    x <- rebuildPEMS(x, "old")
    x[["grouped_df.tags"]] <- NULL
    x <- rebuildPEMS(x, "new")
    class(x) <- c("grouped_df", "data.frame")
    x <- ungroup(x)
    class(x) <- "pems"
    return(x)
    
}



#to do

#to test on new and old structure and grouped...new old???
#untested but famous last words...
#    what the heck can it do...

groups.pems <- function(x) NULL

#' @export
group_size.pems <- function(x) nrow(x)

#' @export
n_groups.pems <- function(x) 1L


