setwd('F:/Työpöytä/Koulujutut/RegVar')

# Tehtävä 1
elinaika <- c(4, 5, 10, 11, 20, 29, 35, 40, 66, 70)
library(beeswarm)
beeswarm(elinaika, horizontal=TRUE)
stripchart(elinaika)
?stripchart
summary(elinaika) ; round(sd(elinaika), 1)
#t.test(elinaika, alternative=c(40), conf.level=0.95)
t.test(elinaika, mu=40)

# Tehtävä 2
malli <- lm(elinaika ~ 1)
sovitteet <- fitted(malli) ; sovitteet
residuaalit <- resid(malli) ; residuaalit
data.frame(elinaika, sovitteet, residuaalit)
mean(residuaalit)
sd(residuaalit)

mean(sovitteet)
sd(sovitteet)

# Tehtävä 3
summary(malli)
sd(elinaika)/sqrt(length(elinaika))
t.test(elinaika, mu=0, conf.level=0.95)
confint(malli, level=0.99)

# Tehtävä 4
n <- length(elinaika) ; n
i <- 1:n ; i
nu <- i/(n+1) ; nu
z.k <- qnorm(nu) ; z.k
qnorm(1/(n+1))
z.k[1]
plot(z.k, elinaika, main="QQ-kuvio")
qqnorm(elinaika)
qqline(elinaika
par(mfrow=c(2,1))
qqnorm(elinaika) ; qqline(elinaika)
qqnorm(residuaalit) ; qqline(residuaalit)


# Tehtävä 5
syke <- read.table("sykkeet2015.txt", header = TRUE)
str(syke)
syke$ryhma <- factor(syke$ryhma, labels = c(' vertailu', ' koe') )
summary(syke)
attach(syke)
source("Esanfunktiot.R")
tunnus.taulu
tunnus.taulu(loppusyke, ryhma, 2) 

par(mfrow=c(1,1))
beeswarm( loppusyke ~ ryhma, horizontal = TRUE)
boxplot( loppusyke ~ ryhma, horizontal = TRUE, add = TRUE)

ls.karvot <- tapply(loppusyke, ryhma, mean)
ls.karvot
ls.karvot[2] - ls.karvot[1]
ls.karvot[1] - ls.karvot[2] # Moodle 14
points( ls.karvot, c(1,2), pch = 16, cex = 1.5)

t.test(loppusyke ~ ryhma, var.equal=TRUE) 

lm1 <- lm(loppusyke ~ ryhma)
summary(lm1)

confint(lm1)

sov <- fitted(lm1); sov # Moodle 18
res <- resid(lm1)
data.frame(loppusyke, sov, res) # Moodle 19

par(mfrow=c(1,2))
plot(lm1, 1:2)









