makePEMS <- function(data, header = NULL, 
    constants = NULL, setup = NULL, 
    history = NULL){

############################
#make pems objects from parts
############################

#kr 23/10/2010 ver 0.0.1

#to do
#######################
#is.null handling for args
#is.wrong handling for args
#defaults for constants
#######################


###################
#pems object as output
#################
output <- list(
       header = header, data = data, constants = constants, setup = setup, history = history
       )
class(output) <- "pems"
invisible(output)
}
