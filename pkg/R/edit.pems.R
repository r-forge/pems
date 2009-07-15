`edit.pems` <-
function(pems) {
#pems editor
## restricts access/editoring

############
##to be completed but basics there

##for function arg like records=c(... list of pems directories to be edited...)
##		default all allowed, e.g. pems, vehicle, fuel, constants
##		wrap each editor call in an if to stop user calling illegals, accessing data, etc.  
##for each edit, edir method?, restricts, resets & tidy up of editor layouts?
##for ans$history entry to say editor call made or changes made?

pems$constants <- edit(pems$constants)
pems$pems <- edit(pems$pems)

output <- pems
}

