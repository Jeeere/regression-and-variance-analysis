setwd('F:/Työpöytä/Koulujutut/RegVar')

koulu <- c(6, 12, 10, 8, 9)
tulot <- c(10, 20, 17, 12, 11)

par(mfrow=c(1,1))
plot( tulot ~ koulu, pch=16, xlim=c(5,13), ylim=c(8,22) )

# Moodle 1
mean(tulot)
sd(tulot)
cor(koulu,tulot)

m1 <- lm( tulot ~ koulu )

# Moodle 2
summary(m1)
anova(m1)

round( cbind( coef(m1), confint(m1) ), 2)

# Moodle 5
yhat <- fitted(m1)
res <- resid(m1)
data.frame(koulu, tulot, yhat, res)

points( yhat ~ koulu) # lisätään kuvaan sovitteet
# Moodle 6
abline(m1) # mallin m1 mukainen sovitettu regressiosuora
# Moodle 7
segments( koulu, tulot, koulu, yhat, lty=3 ) # pystysuorat etäisyydet

xnew <- data.frame( koulu = seq(6,12, by=0.5) ) ; xnew

#Moodle 8
yfit <- predict(m1, newdata= xnew, interval="confidence", level=0.95)
round(cbind(xnew, yfit), 2)

lines( xnew[, 1], yfit[, 2], lty=2)
lines( xnew[, 1], yfit[, 3], lty=2)

ypred <- predict(m1, newdata= xnew, interval="predict", level=0.95)
round(cbind(xnew, ypred), 2)
lines( xnew[, 1], ypred[, 2], lty=2)
lines( xnew[, 1], ypred[, 3], lty=2)

brain <- read.table("brain.txt", header=T)
str(brain)

brain$sukup <- factor(brain$suku, levels=c(0,1), labels = c("mies", "nainen") )
attach(brain)
source("Esanfunktiot.r") # funktio kopioitu Moodlesta R-harjoituksessa 2
tunnus.taulu(IQ, sukup, 1)
tunnus.taulu(MRI, sukup, 2)
tunnus.taulu(pituus, sukup, 1)

brain.m <- subset(brain, sukup == "mies") ; brain.m
brain.n <- subset(brain, sukup == "nainen") ; brain.n

par(mfrow=c(1,2))
with(brain.m, plot(IQ ~ MRI, pch = 16, main = "Miehet") )
with(brain.n, plot(IQ ~ MRI, pch = 16, main = "Naiset") )

with( brain.m, cor(MRI, IQ) )
with( brain.n, cor(MRI, IQ) )

malli.n <- lm( IQ ~ MRI, data = brain.n)
summary(malli.n)
round( cbind( coef(malli.n), confint(malli.n) ), 2)
anova(malli.n)

confint(malli.n, level=0.99)
# Moodle 16

par(mfrow=c(1,1))
with(brain.n, plot( IQ ~ MRI, pch = 16) )
abline(malli.n)

brain.n$MRI.kesk <- brain.n$MRI-mean(brain.n$MRI)
brain.n$MRI.kesk

mean(brain.n$MRI); mean(brain.n$MRI.kesk)
sd(brain.n$MRI); sd(brain.n$MRI.kesk)

malli2.n <- lm( IQ ~ MRI.kesk, data = brain.n)
summary(malli2.n)
round( cbind( coef(malli2.n), confint(malli2.n) ), 2)

lines( xnew[, 1], yfit[, 2], lty=2)
lines( xnew[, 1], yfit[, 3], lty=2)






