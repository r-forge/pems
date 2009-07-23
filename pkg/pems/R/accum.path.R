accum.path <-
function(pems) {
#temp function
require(lattice)

data <- pems$data
y <- data$em.co2
x <- data$velocity
x2 <- data$local.time

plot2 <- xyplot(y~x2, type="l")


x <- cumsum(x * 0.27777777778)
y <- cumsum(y)


#s <- c(rep(1, length(y)), rep(2,length(y)))
#y <- rep(y,2)
#x <- c(x,x2)

#xyplot(y~x | s)

text1<-"accum CO2 (grams)"
plot1 <- xyplot(y~x, ylab =text1, xlab = "Distance travelled (meters)")
plot2 <- xyplot(y~x2, ylab =text1, xlab = "Time take (seconds)")

plot(plot1, position = c(0,0,1,0.525))
plot(plot2, position = c(0,0.475,1,1), newpage = FALSE)




}

