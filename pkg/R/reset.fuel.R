`reset.fuel` <-
function(data, fuel="unknown", fuel.args=NULL, use.defaults=TRUE) {

#to reset the fuel type of a pems object

#pems objects only
if(!class(data)=="pems") return("kicked out: not a pems object")

#default values
data$fuel <- fuel

if(use.defaults==TRUE) {
	data$constants$alpha.hc <- 1.85 
	data$constants$alpha.exhaust.hc <- 1.85 
	data$constants$beta.oc <- 0 
	data$constants$density.fuel <- 0.725
	data$constants$density.exhaust <- 1.2
}

ans<-paste("pems fuel reset as ", fuel, sep="")

#do nothing for petrol

#diesel
if(fuel=="diesel" & use.defaults ==TRUE) {
	data$constants$alpha.hc <- 1.90 
	data$constants$alpha.exhaust.hc <- 1.90 
	data$constants$beta.oc <- 0 
	data$constants$density.fuel <- 0.825
	data$constants$density.exhaust <- 1.2 
}

#other classes
if(!fuel=="unknown" & !fuel=="petrol" & !fuel=="diesel") {
	ans <- paste(ans, " [user class]", sep="")
}

#further arguments
if(is.null(fuel.args)==FALSE) {
	fuel.args <- fuel.args[c("alpha.hc", "alpha.exhaust.hc", "beta.oc", "density.fuel", "density.exhaust")]
		fuel.args <- fuel.args[na.exclude(names(fuel.args))] # stop user changing non-fuel constants
	data$constants[names(fuel.args)] <- fuel.args[names(fuel.args)]
	ans.2 <- paste(names(fuel.args)[1:(length(fuel.args))],"=",as.character(fuel.args[1:(length(fuel.args))]),sep="",collapse=" ")
	ans <- paste(ans, " [forced constants ",ans.2,"]",sep="")
}

#export result
data$history <- c(data$history,ans)
output <- data
}

