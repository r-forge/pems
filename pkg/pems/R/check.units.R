check.units <-
function (input, input.name, input.class, 
  fun.name,
  input.units = NULL, 
  fun.args =list("convert", "default"),
  source, pems 
) 
{

#check pems units
#################
#version 0.0.1
#karl 19/10/2009
#################
#notes
#currently only available for pems objects
#or workspace object with "units:[units]" as comment entry
#################
#to do
#unit handling for dataframes that are not pems
#################

rep.1 <- paste(" ", fun.name, " units conflict", sep = "")

if(is.null(input.units)){
  if(is.null(comment(input))){
    input.units <- pems$units[1, input.name]
  } else {
    if(length(grep("units:", comment(input)))>0){
      input.units <- grep("units:", comment(input), value=TRUE)[1]
      input.units <- gsub("units:", "", input.units)
      if(input.units==""){
         input.units <- NULL
      }
    }
  }
}
if(is.null(input.units)){
  rep.2 <- paste("no units assigned to ", input.class, " input '", input.name, "'", sep="")
  stop(paste(rep.1, rep.2, sep = "\n\t"), call. = FALSE, domain = NA)
}
input.units <- as.character(input.units)
this.correction <- 0

#speed convertions
if(input.class=="speed"){
  def.units <- "m/s"
  if (input.units == "m/s") { this.correction = 1 }
  if (input.units == "km/h") { this.correction = 0.27777777778 }
}  

#accel convertions
if(input.class=="accel"){
  def.units <- "m/s/s"
  if (input.units == "m/s/s") { this.correction = 1 }
}  

if(this.correction==0){
  rep.2 <- paste("assigned units of ", input.class, " input '", input.name, "' [", input.units, "] not recognised", sep="")
  stop(paste(rep.1, rep.2, sep = "\n\t"), call. = FALSE, domain = NA)
}  
  
input <- input * this.correction
comment(input) <- c(paste("units:", def.units, sep=""), comment(input))
input

}
