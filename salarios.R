dat2 = read.table("salary.txt",header =T)
View(dat2)
plot(dat2$year,dat$salary)
m = lm(salary ~ year, data = dat2)
summary(m)
abline(m,col= "red",lwd=3)
dat2$pred = predict(m)
dat2$resi = dat2$salary - dat2$pred
sd(dat$resi)
dat2$rank = factor(dat2$rank)
dat2$sex = factor(dat2$sex)

m2 = lm(salary ~ year + ysdeg + sex, data = dat2)
summary(m2)
