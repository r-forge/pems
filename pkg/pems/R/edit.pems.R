edit.pems <-
function(pems) {
#pems editor
####################
#karl 06/03/2009
#version 0.0.1
## restricts access/editoring

############
##to be completed but basics there

##for function arg like records=c(... list of pems directories to be edited...)
##		default all allowed, e.g. pems, vehicle, fuel, constants
##		wrap each editor call in an if to stop user calling illegals, accessing data, etc.  
##for each edit, edir method?, restricts, resets & tidy up of editor layouts?
##for ans$history entry to say editor call made or changes made?

##pems$constants <- edit(pems$constants)
##pems$pems <- edit(pems$pems)

message("Currently, edit access to pems objects is restricted.")
message("Use reset functions to admend pems attributes.")

output <- pems
}

