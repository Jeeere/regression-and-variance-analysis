setwd('F:/Työpöytä/Koulujutut/RegVar')

# Tehtävä 1
koulu <- c(6, 12, 10, 8, 9)
tulot <- c(10, 20, 17, 12, 11)
plot(tulot ~ koulu, pch=16, xlim=c(5,13), ylim=c(8, 22))
m1 <- lm(tulot ~ koulu) ; summary(m1)
cbind(coef(m1), round( confint(m1), 2) )
anova(m1)
	# Moodle 1
yhat <- fitted(m1) ; yhat
res <- resid(m1) ; res
abline(m1)
summary(m1) # (R^2)
	# Moodle 2

par(mfrow=c(2,1))
plot(tulot ~ yhat) ; plot(tulot ~ koulu)
cor(tulot,yhat) ; cor(tulot,koulu)
cor(tulot,yhat)^2 ; cor(tulot,koulu)^2
	# Moodle 3

# Tehtävä 2
ika <- c( 28, 40, 32, 36, 34)
m2 <- lm(tulot ~ koulu + ika)
summary(m2) # perustulostus malliobjektista
	# Moodle 4
confint(m2) # kertoimien luottamusvälit
	# Moodle 5
anova(m2) # ANOVA-taulu
library(car); vif(m2) # ladataan vif-kertoimia varten paketti car
	# Moodle 6

m2b <- lm(tulot ~ ika + koulu)
anova(m2b)
	# Moodle 7

scatter3d(tulot ~ koulu + ika, surface=FALSE, id.method="identify")

scatter3d(tulot ~ koulu + ika, surface=TRUE)

pot1 <- hatvalues(m1) ; pot2 <- hatvalues(m2)
data.frame(pot1, pot2)
	# Moodle 8
sum(pot1); sum(pot2)
cor(tulot, fitted(m2))^2

# Tehtävä 3
res1 <- resid(m1) ; stand1 <- rstandard(m1) ; stud1 <- rstudent(m1)
res2 <- resid(m2) ; stand2 <- rstandard(m2) ; stud2 <- rstudent(m2)
data.frame(res1, stand1, stud1)
	# Moodle 9
data.frame(res2, stand2, stud2)

# Tehtävä 4
par(mfrow=c(2,2))
plot(m2, which=c(1,2,3,5))

# Tehtävä 5
puut <- read.table("puut.txt", header=TRUE)
puut
attach(puut)

plot(puut)
cor(puut)
	# Moodle 10
scatter3d(tilav.m3 ~ kork.m + halk.m surface=FALSE)

par(mfrow=c(1,1))
plot(tilav.m3 ~ kork.m)
malli1 <- lm(tilav.m3 ~ kork.m)
summary(malli1)
abline(malli1)
par(mfrow=c(2,2)) ; plot(malli1, which=c(1:3, 5))
	# Moodle 11

puut$log.kork <- log(puut$kork.m)
puut$log.halk <- log(puut$halk.m)
puut$log.til <- log(puut$tilav.m3)
attach(puut)
par(mfrow=c(1,1))
plot(tilav.m3 ~ log.kork)
	# Moodle 12

plot(log.til ~ kork.m)
malli2 <- lm(log.til ~ kork.m)
summary(malli2)
abline(malli2)
par(mfrow=c(2,2)) ; plot(malli2, which=c(1:3, 5))
	# Moodle 13 14

# Tehtävä 6
malli3 <- lm(log.til ~ halk.m + kork.m)
summary(malli3)
confint(malli3)
plot(malli3)
	# Moodle 15 16

malli.final <- lm(log.til ~ log.halk + log.kork )
summary(malli.final)
confint(malli.final)
	# Moodle 17
plot(malli.final)
	# Moodle 18

data.frame(hatvalues(malli.final), rstandard(malli.final) )
	# Moodle 19 20




