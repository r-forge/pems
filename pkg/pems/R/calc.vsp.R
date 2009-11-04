calc.vsp <- 
function (pems = NULL, speed = velocity, accel = accel, grade = NULL,
  fun.method = "jimenez.palacios", 
  fun.args = list(a = 1.1, b = 0.132, c = 0.001208, g = 9.81), 
  vis.fun = TRUE, 
  fun.output = "pems") 
{

##############################
#calculate vsp
##############################
#version 0.0.2 karl 17/10/2009
##############################
#notes
#currently only has methods: 
# "jimenez.palacios"
##############################
#to do
##############################
#need
#jim method to default missing args
#jim method history to include null

#set up
require(lattice)

if(is.null(pems)) { stop("\t calc.vsp: no pems", call. = FALSE, domain = NA) }
if(is(pems)[1]=="pems") { data <- pems$data } else { data <- pems }

this.speed <- check.input(deparse(substitute(speed)), "speed", "calc.vsp", 
              source = data, pems = pems) 
this.accel <- check.input(deparse(substitute(accel)), "accel", "calc.vsp",
              if.missing=list("stop", comment = "[suggest running calc.accel]"),
              source = data, pems = pems) 
this.grade <- check.input(deparse(substitute(grade)), "grade", "calc.vsp",
              if.null = list("return.alt", alt=0, alt.check="numeric"), 
              source = data, pems = pems) 

this.speed <- check.units(this.speed, deparse(substitute(speed)), "speed", "calc.vsp",
              source = data, pems = pems) 
this.accel <- check.units(this.accel, deparse(substitute(accel)), "accel", "calc.vsp",
              source = data, pems = pems) 

fun.method <- check.method(fun.method, c("jimenez.palacios"),
              "calc.vsp")

this.method <- "unknown"

if(fun.method=="jimenez.palacios"){

  fun.args <- check.method.args(fun.method, fun.args,
              c("a", "b", "c", "g"),
              "calc.vsp")

  this.vsp <- this.speed * (fun.args$a * this.accel + (fun.args$g * 
              this.grade) + fun.args$b) + (fun.args$c * this.speed^3)
  this.method <- "jimenez.palacios"
  this.vsp.units <- "kW/metric Ton"

}

#this should never happen
if (this.method == "unknown") {
  stop(paste("\t calc.vsp: ", fun.method, " not recognised", 
       sep = ""), call. = FALSE, domain = NA)
}

if (vis.fun) {
  plot(this.speed, type = "l")
  lines(this.vsp, col = "blue")
}

if (fun.output == "pems") {
  data <- cbind(data, vsp = this.vsp)
  names(data) <- make.unique(names(data))
  vsp.name <- names(data)[ncol(data)]
  if (is(pems)[1] == "pems") {
    pems$history <- c(pems$history, paste("calc.vsp: '", vsp.name, "' generated using '", 
    deparse(substitute(speed)), "', '", deparse(substitute(accel)), "' and '", 
    this.method, "' method", sep = ""))
    pems$units[length(pems$units) + 1] <- this.vsp.units
    names(pems$units)[length(pems$units)] <- vsp.name
    pems$data <- data
    pems
  } else {
    comment(data)[length(data)] <- paste("units:", this.vsp.units, sep="")
    pems <- data
  }
} else {
  comment(this.vsp) <- c(paste("units:", this.vsp.units, sep=""))
  this.vsp
}
}
