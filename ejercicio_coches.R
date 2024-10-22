# Mi primer script uwu
#1
dat = read.table("cars.txt",header=TRUE)
#2
dat$origin = factor(dat$origin,labels=c("USA","EUR","JAP"))
frec_abs = table(dat$origin)
# tapply(dat$mpg,dat$origin,length)
frec_rel = prop.table(frec_abs)
barplot(frec_abs, col = rainbow(3))
barplot(frec_rel, col = rainbow(3))

#3
tapply(dat$mpg,dat$origin,mean)
tapply(dat$engine,dat$origin,mean)
tapply(dat$horse,dat$origin,mean)
tapply(dat$weight,dat$origin,mean)
tapply(dat$accel,dat$origin,mean)

#4
tapply(dat$mpg,dat$origin,sd)
tapply(dat$engine,dat$origin,sd)
tapply(dat$horse,dat$origin,sd)
tapply(dat$weight,dat$origin,sd)
tapply(dat$accel,dat$origin,sd)

#5
hist(dat$mpg,col="red")

#6
dat$consumo = 235.2 / dat$mpg
hist(dat$consumo,col="green")

#7, 8, 9, 10, 11
par(mfrow=c(2,2))
boxplot(dat$mpg ~ dat$origin, col = rainbow(3),horizontal=TRUE)
boxplot(dat$consumo ~ dat$origin, col = rainbow(3),horizontal=TRUE)
boxplot(dat$weight ~ dat$origin, col = rainbow(3),horizontal=TRUE)
boxplot(dat$horse ~ dat$origin, col = rainbow(3),horizontal=TRUE)

#12
par(mfrow=c(2,2))
hist(dat$engine,col="red")
hist(dat$horse,col="green")
hist(dat$weight,col="yellow")
hist(dat$accel,col="blue")

#.__. (n.n)
#13
asim <- function(x){mean((x-mean(x))^3/sd(x)^3)}
asim(dat$mpg)
asim(dat$engine)
asim(dat$horse)
asim(dat$weight)
asim(dat$accel)
tapply(dat$mpg,dat$origin,asim)

#14
curt <- function(x){mean((x-mean(x))^4/sd(x)^4)}
curt(dat$mpg)
curt(dat$engine)
curt(dat$horse)
curt(dat$weight)
curt(dat$accel)
tapply(dat$mpg,dat$origin,curt)

