##########################
##########################
##common calculations
##########################
##########################

#kr

#description
##########################
#functions to do common calculates


#includes 
##########################
#calcDistance


#to do
##########################

#comments
##########################
#template to think about




##########################
##########################
##calcDistance
##########################
##########################

#kr 23/01/2012 v 0.0.6

#what it does
##########################
#calculates distance travelled 
#since previous measurement


#to do
##########################
#make test more robust?

#comments
##########################
#


#############################
#############################
##calcChecks
#############################
#############################

#front end management


calcChecks <- function(fun.name = "calcChecks", ..., data = data,
                   if.missing = c("stop", "warning", "return"), 
                   output = c("special", "input", "data.frame", "pems"),
                   unit.conversions = NULL, overwrite = FALSE){

    #output handling
    output <- checkOption(output[1], formals(setUnits)$output, 
                       "output", "allowed outputs", 
                       fun.name = fun.name)

    if(output == "special"){
        output <- if(is.null(data))
                      "input" else if(comment(isPEMS(data)) == "other")
                                       "input" else comment(isPEMS(data))
    }

    #if.missing handling
    if.missing <- checkOption(if.missing[1], formals(setUnits)$if.missing, 
                              "if.missing", "allowed if.missings", 
                              fun.name = fun.name)

    list(output = output, if.missing = if.missing, overwrite = overwrite, 
         unit.conversions = unit.conversions)

}








#############################
#############################
##calcDistance
#############################
#############################


calcDistance <- function(speed = NULL, time = NULL, data = NULL, units = "m",
                     ...){
    
    #setup
    this.call <- match.call()
    fun.name <- "calcDistance"
    
    #run checks
    settings <- calcChecks(fun.name, ..., data = data)

    speed <- checkInput(input = speed, data = data, if.missing = settings$if.missing, 
                       output = "input", fun.name = fun.name)
    speed <- convertUnits(speed, to = "m/s", fun.name = fun.name, 
                       unit.conversions = settings$unit.conversions)

    time <- checkInput(input = time, data = data, if.missing = settings$if.missing, 
                       output = "input", fun.name = fun.name)
    time <- convertUnits(time, to = "s", fun.name = fun.name, 
                       unit.conversions = settings$unit.conversions)

    #my assumption
    #first unit resolution is average of rest
    #rest are time.now - time.last

    temp <- diff(time)
    temp <- c(mean(temp, na.rm=TRUE), temp)

    distance <- speed * temp
    distance <- setUnits(distance, "m", force = TRUE, fun.name = fun.name)

    distance <- convertUnits(distance, to = units, fun.name = fun.name, 
                       unit.conversions = settings$unit.conversions)

    #make output
    output <- checkOutput(input = distance, data = data, if.missing = settings$if.missing, 
                          fun.name = fun.name, output = settings$output, 
                          overwrite = settings$overwrite)
    if(isPEMS(output))
        output$history[[length(output$history)+1]] <- this.call 

    output
    
}






#############################
#############################
##calcSpeed
#############################
#############################


calcSpeed <- function(distance = NULL, time = NULL, data = NULL, units = "m/s",
                     ...){
    
    #setup
    this.call <- match.call()
    fun.name <- "calcSpeed"
    
    #run checks
    settings <- calcChecks(fun.name, ..., data = data)

    distance <- checkInput(input = distance, data = data, if.missing = settings$if.missing, 
                       output = "input", fun.name = fun.name)
    distance <- convertUnits(distance, to = "m", fun.name = fun.name, 
                       unit.conversions = settings$unit.conversions)

    time <- checkInput(input = time, data = data, if.missing = settings$if.missing, 
                       output = "input", fun.name = fun.name)
    time <- convertUnits(time, to = "s", fun.name = fun.name, 
                       unit.conversions = settings$unit.conversions)

    #my assumption
    #first unit resolution is average of rest
    #rest are time.now - time.last

    temp <- diff(time)
    temp <- c(mean(temp, na.rm=TRUE), temp)

    speed <- distance / temp
    speed <- setUnits(distance, "m/s", force = TRUE, fun.name = fun.name)

    speed <- convertUnits(speed, to = units, fun.name = fun.name, 
                       unit.conversions = settings$unit.conversions)

    #make output
    output <- checkOutput(input = speed, data = data, if.missing = settings$if.missing, 
                          fun.name = fun.name, output = settings$output, 
                          overwrite = settings$overwrite)
    if(isPEMS(output))
        output$history[[length(output$history)+1]] <- this.call 

    output
    
}





###########################
###########################
##calcAccel
###########################
###########################


calcAcceleration <- function(...) calcAccel(...)


calcAccel <- function(speed = NULL, time = NULL, data = NULL, units = "m/s/s",
                     ...){
    
    #setup
    this.call <- match.call()
    fun.name <- "calcAccel"
    
    #run checks
    settings <- calcChecks(fun.name, ..., data = data)

    speed <- checkInput(input = speed, data = data, if.missing = settings$if.missing, 
                       output = "input", fun.name = fun.name)
    speed <- convertUnits(speed, to = "m/s", fun.name = fun.name, 
                       unit.conversions = settings$unit.conversions)

    time <- checkInput(input = time, data = data, if.missing = settings$if.missing, 
                       output = "input", fun.name = fun.name)
    time <- convertUnits(time, to = "s", fun.name = fun.name, 
                       unit.conversions = settings$unit.conversions)

    #my assumption
    #first d.speed/d.time is 0
    #rest are ...now - ....last

    d.speed <- diff(speed)
    d.time <- diff(time)

    accel <- c(0, d.speed / d.time)
    accel <- setUnits(accel, "m/s/s", force = TRUE, fun.name = fun.name)

    accel <- convertUnits(accel, to = units, fun.name = fun.name, 
                       unit.conversions = settings$unit.conversions)

    #make output
    output <- checkOutput(input = accel, data = data, if.missing = settings$if.missing, 
                          fun.name = fun.name, output = settings$output, 
                          overwrite = settings$overwrite)
    if(isPEMS(output))
        output$history[[length(output$history)+1]] <- this.call 

    output
    
}









############################
############################
##calcJerk
############################
############################


calcJerk <- function(accel = NULL, time = NULL, data = NULL, units = "m/s/s/s",
                     ...){
    
    #setup
    this.call <- match.call()
    fun.name <- "calcJerk"
    
    #run checks
    settings <- calcChecks(fun.name, ..., data = data)

    accel <- checkInput(input = accel, data = data, if.missing = settings$if.missing, 
                       output = "input", fun.name = fun.name)
    accel <- convertUnits(accel, to = "m/s/s", fun.name = fun.name, 
                       unit.conversions = settings$unit.conversions)

    time <- checkInput(input = time, data = data, if.missing = settings$if.missing, 
                       output = "input", fun.name = fun.name)
    time <- convertUnits(time, to = "s", fun.name = fun.name, 
                       unit.conversions = settings$unit.conversions)

    #my assumption
    #first d.accel/d.time is 0
    #rest are ...now - ....last

    d.accel <- diff(accel)
    d.time <- diff(time)

    jerk <- c(0, d.accel / d.time)
    jerk <- setUnits(jerk, "m/s/s/s", force = TRUE, fun.name = fun.name)

    jerk <- convertUnits(jerk, to = units, fun.name = fun.name, 
                       unit.conversions = settings$unit.conversions)

    #make output
    output <- checkOutput(input = jerk, data = data, if.missing = settings$if.missing, 
                          fun.name = fun.name, output = settings$output, 
                          overwrite = settings$overwrite)
    if(isPEMS(output))
        output$history[[length(output$history)+1]] <- this.call 

    output
    
}







