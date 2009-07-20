summary.pems <-
function (pems, short.hand = TRUE) {
########################
#pems summary over-ride
#version 0.0.1
#karl (28/02/2009)

########################
#TO DO
########################
#test (data and unit names same)
##
#comprehensive summary to do

message(paste("pems object: ", deparse(substitute(pems)), sep = ""))

if(short.hand==TRUE) { 
	
	#shorthand summary
	message("index:")
	print(names(pems))
	message(paste("data: ", ncol(pems$data), " by ", nrow(pems$data), " (", ncol(pems$data)*nrow(pems$data),") entries", sep="")) 
	print(names(pems$data))
	message(paste("units: ", length(pems$units), " entries", sep=""))
	if(identical(names(pems$data),names(pems$units))) {
		message("   data/names checksum GOOD")
	} else {
		message("   data/names checksum BAD")
	}
	message(paste("pems: ", pems$pems, sep = ""))
	message(paste("vehicle: ", pems$vehicle, sep = ""))
	message(paste("fuel: ", pems$fuel, sep = ""))
	message(paste("constants: ", ncol(pems$constants), " entries", sep = ""))
	tidy <- paste("history: ", length(pems$history), " logged action", sep = "")
		if(!length(pems$history)==1) { tidy <- paste(tidy, "s", sep = "") } 
	message(tidy)

} else {

	#comprehensive summary
	message("index:")
	print(names(pems))
	message(paste("data: ", ncol(pems$data), " by ", nrow(pems$data), " (", ncol(pems$data)*nrow(pems$data),") entries", sep="")) 
	print(summary(pems$data))
	message(paste("units: ", length(pems$units), " entries", sep=""))
	if(identical(names(pems$data),names(pems$units))) {
		message("   data/names checksum GOOD")
	} else {
		message("   data/names checksum BAD")
	}
	print(pems$units)
	message(paste("pems: ", pems$pems, sep = ""))
	message(paste("vehicle: ", pems$vehicle, sep = ""))
	message(paste("fuel: ", pems$fuel, sep = ""))
	message(paste("constants: ", ncol(pems$constants), " entries", sep = ""))
	print(pems$constants)
	tidy <- paste("history: ", length(pems$history), " logged action", sep = "")
		if(!length(pems$history)==1) { tidy <- paste(tidy, "s", sep = "") } 
	message(tidy)
	print(pems$history)

}
}

