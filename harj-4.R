setwd('F:/Työpöytä/Koulujutut/RegVar')

# Tehtävä 1

syke <- read.table("sykkeet2015.txt", header=T)
str(syke)

syke$ryhma <- factor(syke$ryhma, labels = c(' vertailu', ' koe') )
ryhmasummary(syke)
attach(syke)

source("Esanfunktiot.R")
tunnus.taulu(sykemuutos, ryhma, 2)

library(beeswarm)
par(mfrow=c(1,1))
beeswarm( sykemuutos ~ ryhma, horizontal = T)
boxplot( sykemuutos ~ ryhma, horizontal = T, add = T)

# Tehtävä 2

ls.karvot <- tapply(sykemuutos, ryhma, mean)
ls.karvot
points( ls.karvot, c(1,2), pch = 16, cex = 1.5)

t.test(sykemuutos ~ ryhma, var.equal=TRUE) # Moodle 1

lm1 <- lm(sykemuutos ~ ryhma)
summary(lm1) # Moodle 2
confint(lm1)

anova(lm1) # Moodle 5
par(mfrow=c(1,2))
plot(lm1, 1:2)
(length(sykemuutos)-1) * var(sykemuutos) 

t.test( sykemuutos ~ ryhma, var.equal=F) # Moodle 7

detach(syke)

# Tehtävä 3

mat <- read.table("mat_koe.txt", header=T)
str(mat)
summary(mat) # Moodle 9
attach(mat)

par(mfrow=c(1,1))
beeswarm(pisteet ~ ryhma, method='center')

tunnus.taulu(pisteet, ryhma, 2)

# Tehtävä 4

m.a <- lm( pisteet ~ ryhma - 1)
round( cbind( summary(m.a)$coef, confint(m.a) ), 2)

m.b <- lm( pisteet ~ ryhma)
round( cbind( summary(m.b)$coef, confint(m.b) ), 2)

# Tehtävä 5

yhat <- fitted.values(m.b)
res <- residuals(m.b)
data.frame(ryhma, pisteet, yhat = round(yhat, 2), res = round(res,2)) # Moodle 15
points( as.numeric(ryhma) , yhat, pch = 16, cex = 1.5)

anova(m.b)

par(mfrow=c(1,2)) # kaksi kuvaa rinnakkain
plot(m.b, which=1:2)

# Tehtävä 6

library(gmodels)
fit.contrast(m.b, ryhma, coeff = c(-1, -1, 1, 1)/2, conf.int=0.95 )


















