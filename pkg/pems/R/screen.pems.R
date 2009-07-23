screen.pems <-
function(pems, variables = 1:ncol(pems$data), time.source ="local.time"){
#data screening function for pems

#####
#to do 
#####
#exclude variables[i]==time.source then cycling through variables 

#pems objects only
if(!class(pems)=="pems") return("kicked out: not a pems objects")

#select variables to be screened
ts <- pems$data[time.source]
if(class(variables)=="numeric" | class(variables)=="integer") {
		variables <- names(pems$data[as.integer(variables)])	
	}
data <- pems$data[, sapply(pems$data, class) %in% c("numeric", "integer")]
variables <- names(data)

#use lattice?
require(lattice)

###
#for latter
i <- 2

#subset data
##replace with equal.count?

sub.set <- 5
sub.set <- ((length(data[,variables[i]]))/sub.set)
sub.set <- ceiling(((1:length(data[,variables[i]]))/sub.set))

tp1 <- xyplot(data[,variables[i]]~ts , xlab=time.source, 
		ylab= variables[i],
		panel = function(x,y,...) {
				panel.xyplot(x,y,type="l",...)
				panel.rug(x=x[!is.na(y)], y=y[!is.na(x)],col="blue",...)
				panel.rug(x=x[is.na(y)], y=y[is.na(x)],col="red",...)
			}
		)

a <- hist(data[,variables[i]], breaks = 50, plot=FALSE)
b<- c("NA", a$mids); #b<-as.character(b) #b<-factor(b, exclude="", levels = sort(unique.default(b), na.last = FALSE), ordered=FALSE)
c<-a$counts; c<-c(length(subset(data[,variables[i]], is.na(data[,variables[i]]))),c)

tp2 <- barchart(b~c, xlim= c(min(c), max(c)), 
		panel = function(x,y,...) {
				panel.barchart(x,y,col="blue",border="transparent",...)
				panel.barchart(x=x[1],y=y[1],col="red",border="transparent",...)
			}
		)

#a <- hist(subset(data[,variables[i]], ), breaks = 50, plot=FALSE)








#tp2 <- histogram(~data[,variables[i]]| sub.set, breaks =50, xlab="index", ylab= variables[i], border="transparent")
#b<- prop.table(tp2)
#tp2<-barchart(b)

print(tp1, position = c(0, 0.5, 1, 1), more=TRUE)
print(tp2, position = c(0.5, 0, 1, 0.5)) #, more=TRUE)

tp2 <- histogram(~data[,variables[i]]| sub.set, breaks =50, xlab="index", ylab= variables[i], border="transparent", plot=FALSE)





#b<-hist(data[,variables[i]], breaks=5, plot=FALSE)
#c<-data.frame(b$count)
#d<-data.frame(b$mids)

#e<-cbind(c,d)

#tp2 <- barchart(b$mids~b$counts, border="transparent")


#for(i in 1:5) {
#	a <- subset(data[,variables[i]], sub.set==i)
#	b <- hist(a, breaks=5, plot=FALSE)
#	c<-cbind(c,b$count)
#	d<-cbind(d,b$mids)
#}


#output <- sapply(data[,variables[i]],hist,100)


#tp2 <- histogram(~data[,variables[i]], breaks =100, xlab="index", ylab= variables[i], border="transparent")



#tp2 <- histogram(~data[,variables[i]], nint=100, xlab=time.source, 
#		ylab= variables[i],
#		panel = function(x,...) {
#				panel.histogram(x,type="l",...)
#				panel.rug(x=x[!is.na(y)],col="blue",...)
#				#panel.rug(x=x[is.na(y)], y=y[is.na(x)],col="red",...)
#			}
#		)



#tp2 <- bwplot(data[,variables[i]]~factor(sub.set), xlab="hello", horizontal=FALSE, 
#		ylab= variables[i],
#		panel = function(x,y,...) {
#				panel.violin(factor(1),y,...)
#				panel.bwplot(factor(2),y,...)
#				panel.rug(factor(1),y,...)
#			}
#		)

#tp2 <- bwplot(sub.set ~ data[,variables[i]], horizontal =TRUE,
#		panel = panel.violin, box.ratio =3, xlab="hello")



#tp1 <- xyplot(data[,variables[i]]~1:length(data[,variables[i]]), xlab="index", ylab= variables[i],type="l")

#tp1 <- xyplot(data[,variables[i]]~ts, groups = factor(sub.set), xlab="index", ylab= variables[i],type="l")




#tp2 <- densityplot(data[,variables[i]], kernel="rect", bw=0.2, plot.points="rug", n=200)
#tp3 <- histogram(data[,variables[i]],aspect="yx", breaks=50)


#tp2 <- histogram( ~ data[,variables[i]], data = singer, subsetbreaks=50,
#          xlab = "Height (inches)", type = "density",
#          panel = function(x, ...) {
#              panel.histogram(x, ...)
#              	#panel.mathdensity(dmath = dnorm, col = "black",
#                  #              args = list(mean=mean(x),sd=sd(x)))
#			panel.densityplot(x, kernel="rect", bw=0.2, plot.points="rug", n=200)
#          } )



#tp2 <- qqmath(~ data[,variables[i]], groups = factor(sub.set) ,type="l",)


#tp2 <- bwplot(sub.set ~ data[,variables[i]], horizontal =TRUE,
#		panel = panel.violin, box.ratio =3, xlab="hello")

#print(tp3, position = c(0.5, 0.5, 1, 1))


#    plot(density(data[,variables[1]], bw = 0.15,kernel="epanechnikov"))

    #hist(local.time,breaks=50)
#	rug(local.time)
#    rug(jitter(local.time, amount = 0.01), side = 3, col = "light blue")




#plot(data$local.time, data$conc.co,ylab=names(data["conc.co"]),type="l")
#rug(data$local.time,data$conc.co,ylab=names(data["conc.co"]))
 
#watch outputs
#names(data)
#variables
#summary(tp2)
#tp2

a
}

