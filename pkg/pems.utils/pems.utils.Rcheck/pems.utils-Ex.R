pkgname <- "pems.utils"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('pems.utils')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("1.1.make.import.data")
### * 1.1.make.import.data

flush(stderr()); flush(stdout())

### Name: 1.1.make.import.data
### Title: making and importing data
### Aliases: 1.1.make.import.data pems.objects isPEMS makePEMS import2PEMS
###   importTAB2PEMS importCSV2PEMS importOBS2PEMS importRoyalTek2PEMS
### Keywords: methods

### ** Examples


###########
##example 1 
###########

#make little pems
data <- data.frame(speed=1:10, emissions=1:10)
units <- c("m/s", "g/s")
pems <- makePEMS(x = data, units=units, example="my record") 
 
pems                       #the pems object
summary(pems)              #summary of held data
checkInput(speed, pems)    #speed element




cleanEx()
nameEx("1.2.pems.structure")
### * 1.2.pems.structure

flush(stderr()); flush(stdout())

### Name: 1.2.pems.structure
### Title: 'pems' object structure
### Aliases: 1.2.pems.structure pems.structure getElement getData
### Keywords: methods

### ** Examples


###########
##example 1 
###########

#basic usage

getElement(velocity, pems.1)

#check... equivalent
#checkInput(veolcity, pems.1)




cleanEx()
nameEx("1.3.merge.data.pems")
### * 1.3.merge.data.pems

flush(stderr()); flush(stdout())

### Name: 1.3.merge.data.pems
### Title: Merging data and pems objects
### Aliases: 1.3.merge.data.pems merge.pems bindPEMS cAlign
###   findLinearOffset
### Keywords: methods

### ** Examples


###########
##example 1 
###########

#some data
temp <- rnorm(500)

#get two offset ranges

x <- temp[25:300]
y <- temp[10:200]

plot(x, type="l"); lines(y, col="blue", lty=2)

#estimated offset

findLinearOffset(x,y)
#[1] -15

#applying linear offset

ans <- cAlign(x,y, findLinearOffset(x,y))

plot(ans$x, type="l"); lines(ans$y, col="blue", lty=2)




cleanEx()
nameEx("1.4.export.data")
### * 1.4.export.data

flush(stderr()); flush(stdout())

### Name: 1.4.export.data
### Title: exporting data data
### Aliases: 1.4.export.data export.data exportData
### Keywords: methods

### ** Examples


###########
##example 1 
###########

#making a tab-delimited copy of pems.1

##exportData(pems.1, "pems.example.txt")
##dir()





cleanEx()
nameEx("2.1.generic.pems.handlers")
### * 2.1.generic.pems.handlers

flush(stderr()); flush(stdout())

### Name: generic.pems.handlers
### Title: Generic handling of pems objects
### Aliases: generic.pems.handlers pems.generics names.pems print.pems
###   plot.pems summary.pems
### Keywords: methods

### ** Examples


## Not run: 
##D 
##D #make object
##D print(pems.object)
##D names(pems.object)
##D 
## End(Not run) 




cleanEx()
nameEx("2.2.unit.handlers")
### * 2.2.unit.handlers

flush(stderr()); flush(stdout())

### Name: 2.2.unit.handlers
### Title: data unit handlers
### Aliases: 2.2.unit.handlers getUnits setUnits convertUnits
###   addUnitConversion addUnitAlias listUnitConversions
### Keywords: methods

### ** Examples


###########
##example 1 
###########

#work with data units

#getting units
#(where assigned)
getUnits(velocity, pems.1) #km/h

#setting units
a <- 1:10
a <- setUnits(a, "km/h")

#changing units
convertUnits(a, "mi/h")

# [1] 0.6213712 1.2427424 1.8641136 2.4854848 3.1068560 3.7282272 4.3495983
# [8] 4.9709695 5.5923407 6.2137119
# units: "mi/h"

###########
##example 2
###########

#working with local unit conversions
#adding/updating unit conversion methods

#make a local reference
ref.list <- ref.unit.conversions 

#add a miles/hour alias to mi/h
ref.list <- addUnitAlias("mi/h", "miles/hour", ref.list)

#add a new conversion
ref.list <- addUnitConversion(to = "silly", from = "km/h", 
                              conversion = function(x) 12 + (21 * x), 
                              tag = "kilometers/hour to some silly scale",
                              unit.conversions = ref.list)

#use these
convertUnits(a, "miles/hour", unit.conversions = ref.list)

# [1] 0.6213712 1.2427424 1.8641136 2.4854848 3.1068560 3.7282272 4.3495983
# [8] 4.9709695 5.5923407 6.2137119
# units: "miles/hour" (as above but using your unit abbreviations)

convertUnits(a, "silly", unit.conversions = ref.list)

# [1]  33  54  75  96 117 138 159 180 201 222
# units: "silly" (well, you get what you ask for)




cleanEx()
nameEx("2.3.1.vsp.calculations")
### * 2.3.1.vsp.calculations

flush(stderr()); flush(stdout())

### Name: 2.3.1.vsp.calculations
### Title: Vehicle Specific Power (VSP) calculations
### Aliases: 2.3.1.vsp.calculations vsp.calculations vsp calcVSP
###   calcVSPJimenezPalaciosCMEM
### Keywords: methods

### ** Examples


###########
##example 1 
###########

#basic usage

ans <- calcVSP(velocity, time = local.time, data = pems.1)

#ans <- pems.1 + calculated vsp







cleanEx()
nameEx("2.3.common.calculations")
### * 2.3.common.calculations

flush(stderr()); flush(stdout())

### Name: 2.3.common.calculations
### Title: Common calculations
### Aliases: 2.3.common.calculations common.calculations calcDistance
###   calcSpeed calcAccel calcAcceleration calcJerk calcChecks calcPack
### Keywords: methods

### ** Examples


###########
##example 1 
###########

#basic usage

ans <- calcAccel(velocity, local.time, pems.1)

#ans = pems.1 + calculated accel

ans <- calcAccel(velocity, local.time, pems.1, output = "input")

#ans = calculated accel


###########
#example 2
###########

#making wrappers for routine data processing 

my.pems <- list(pems.1, pems.1)

sapply(my.pems, function(x) 
                  calcAccel(velocity, local.time, data=x, output="input"))

#ans = accel data series for each pems in my.pems list

#             [,1]        [,2]
# [1,]  0.00000000  0.00000000
# [2,]  0.00000000  0.00000000
# [3,]  0.05555556  0.05555556
# [4,]  0.00000000  0.00000000
# [5,] -0.02777778 -0.02777778
# [6,]  0.05555556  0.05555556
# ...

#or
#lapply(my.pems, function(x) 
#                  calcAccel(velocity, local.time, data=x))
#for output as a list of pems objects with accel added to each

#note:
#sapply if you can/want to simiplify output to data.frame
#lapply if you want to keep output as a list of answers


###########
#example 3
###########

#making a function that allows third party hijack
#and pems management

my.fun <- function(speed, time, data = NULL, hijack = FALSE, 
                   ..., fun.name = "my.function"){

    #setup
    this.call <- match.call()
    
    #run checks
    settings <- calcChecks(fun.name, ..., data = data)
    
    #get pems elements if not already got
    if(!hijack){
 
        #the check handle errors and error messaging
        #checkInput 
        speed <- checkInput(speed, data, fun.name = fun.name,   
                            if.missing = settings$if.missing,
                            unit.conversions = settings$unit.conversions)

        time <- checkInput(time, data, fun.name = fun.name,   
                            if.missing = settings$if.missing,
                            unit.conversions = settings$unit.conversions)
    }

    #any extra error/missing case handling? 

    #run 'hijacked' code

    #reset units to what you want
    #... allows you to pass local unit.conversions
    speed <- convertUnits(speed, to = "km/h", hijack = TRUE, 
                          if.missing = settings$if.missing,
                          unit.conversions = settings$unit.conversions)

    #use someone else's calculation
    distance <- calcDistance(speed, time, hijack = TRUE, 
                          if.missing = settings$if.missing,
                          unit.conversions = settings$unit.conversions)

    #do other stuff?

    #reset units again?

    #output
    #calcPack handling the output type  
    #and my pems history tracking if data modified
    calcPack(output = distance, data = data, settings = settings, 
             fun.name = fun.name, this.call = this.call)     

}

ans <- my.fun(velocity, local.time, pems.1)

#seems long winded but gives you control of 
##the error handling, unit management, and output type
##and (if working in pems objects) history logging 

##and lets you do this

ans <- lapply(my.pems, function(x) 
                  my.fun(velocity, local.time, data=x))

#which will not always work if you are running 
#functions in functions, especially if those functions
#are also running functions in functions...






cleanEx()
nameEx("2.4.pems.plots")
### * 2.4.pems.plots

flush(stderr()); flush(stdout())

### Name: 2.4.pems.plots
### Title: Various plots for pems.utils
### Aliases: 2.4.pems.plots pems.plots latticePlot panel.PEMSXYPlot XYZPlot
### Keywords: methods

### ** Examples


###########
##example 1 
###########

#basic usage of latticePlot

latticePlot(velocity~local.time, data = pems.1, type = "l")

#in lattice, xyplot(velocity~local.time, data = getData(pems.1), type = "l")
#Note: to use lattice functions directly with pems objects
#      just pass data component with data = getData(pems) 
#      (NOT data = pems)


latticePlot(velocity~local.time, data = pems.1, col = "red", 
            pch = 20, panel = panel.PEMSXYPlot, 
            grid = list(col ="black", lty=2))


#basic usage of XYZPlot

a <- calcAccel(velocity, local.time, data =pems.1)

XYZPlot(~accel*velocity, data=a)

XYZPlot(~accel*velocity, data=a, plot = wireframe, shade=TRUE)






cleanEx()
nameEx("2.5.analysis.summary.reports")
### * 2.5.analysis.summary.reports

flush(stderr()); flush(stdout())

### Name: 2.5.analysis.summary.reports
### Title: Generating summary reports
### Aliases: 2.5.analysis.summary.reports summary.reports summaryReport
### Keywords: methods

### ** Examples


###########
##example 1 
###########

#basic usage

summaryReport(velocity, local.time, data=pems.1)

##  distance.travelled.km time.total.s avg.speed.km.h avg.running.speed.km.h
##1              6.186056         1000        22.2698               28.78538
##  time.idle.s time.idle.pc avg.accel.m.s.s time.accel.s time.accel.pc
##1          40            4       0.7921279          271          27.1
##  avg.decel.m.s.s time.decel.s time.decel.pc
##1      -0.9039449          238          23.8

#apply to multiple cases

my.pems <- list(pems.1, pems.1)

sapply(my.pems, function(x) 
                    summaryReport(velocity, local.time, data = x))


#                        [,1]       [,2]      
# distance.travelled.km  6.186056   6.186056  
# time.total.s           1000       1000      
# avg.speed.km.h         22.2698    22.2698   
# avg.running.speed.km.h 28.78538   28.78538  
# time.idle.s            40         40        
# time.idle.pc           4          4         
# avg.accel.m.s.s        0.7921279  0.7921279 
# time.accel.s           271        271       
# time.accel.pc          27.1       27.1      
# avg.decel.m.s.s        -0.9039449 -0.9039449
# time.decel.s           238        238       
# time.decel.pc          23.8       23.8  







cleanEx()
nameEx("2.5.conditioning.pems.data")
### * 2.5.conditioning.pems.data

flush(stderr()); flush(stdout())

### Name: 2.6.conditioning.pems.data
### Title: Data conditioning for pems data
### Aliases: 2.6.conditioning.pems.data conditioning.pems.data cutBy
###   cutByRow
### Keywords: methods

### ** Examples


###########
##example 1 
###########

#basic usage

#cut into equal subsets
a <- cutBy(velocity, n= 5, data=pems.1)

latticePlot(velocity~local.time|cut, data=a, 
             type="l", layout=c(1,5))

#cut at three points
a <- cutBy(velocity, rows=c(180,410,700), data=pems.1)

latticePlot(velocity~local.time|cut, data=a, 
             type="l", layout=c(1,4))




cleanEx()
nameEx("3.1.example.data")
### * 3.1.example.data

flush(stderr()); flush(stdout())

### Name: 3.1.example.data
### Title: example data for use with pems.utils
### Aliases: 3.1.example.data example.data pems.1
### Keywords: datasets

### ** Examples


#to be confirmed




cleanEx()
nameEx("3.2.look.up.tables")
### * 3.2.look.up.tables

flush(stderr()); flush(stdout())

### Name: 3.2.look-up.tables
### Title: reference data for use with pems.utils
### Aliases: 3.2.look-up.tables look-up.tables ref.unit.conversions
###   ref.chem ref.petrol ref.diesel
### Keywords: datasets

### ** Examples


#basic structure
ref.unit.conversions[[1]]




cleanEx()
nameEx("4.1.check.functions")
### * 4.1.check.functions

flush(stderr()); flush(stdout())

### Name: 4.1.common.check.functions
### Title: common check... functions
### Aliases: 4.1.common.check.functions check... checkInput checkOption
###   checkPEMS checkUnits checkOutput checkIfMissing
### Keywords: methods

### ** Examples


###########
##example 1 
###########

#some different data

a <- 1
b <- data.frame(v=10, c=1)

#using checkInput

checkInput(a)           # 1  (attr name = "a")
checkInput(v, data = b) # 10 (attr name = "v")

#checkInput(v)
#Error:   In checkInput(...) input 'v' not found

###########
##example 2
###########

#using checkInput to plot with user labels

standard.plot <- function(x, y, data = NULL){

    #getting x and y
    my.x <- checkInput(x, data = data, input.name = "x", 
                       fun.name = "standard.plot")
    my.y <- checkInput(y, data = data, input.name = "y", 
                       fun.name = "standard.plot")

    #plotting (and labelling it)
    plot(my.x, my.y, xlab = attributes(my.x)$name, 
         ylab = attributes(my.y)$name)
}

standard.plot(a,v, data=b) #etc




### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
