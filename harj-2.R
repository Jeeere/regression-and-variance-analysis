setwd("F:/Työpöytä/Koulujutut/RegVar")
getwd()

# Tehtävä 1
pituus <- c(165.0, 166.0, 171.0, 154.0, 166.0, 159.5, 166.5, 158.5)
library(beeswarm)
beeswarm(pituus, horizontal=TRUE)
SEmean <- function(x) sd(x)/sqrt( length(x) )
n <- length(pituus)
mean.pit <- mean(pituus)
sd.pit <- sd(pituus)
se.pit <- SEmean(pituus)
mu0 <- 167
Thav <- (mean.pit - mu0)/se.pit; Thav
curve(dt(x,6), from=-4, to=4)
points(Thav, 0, pch=16)
Phav <- 2*( 1 - pt(abs(Thav), n-1) )
c(Thav, Phav)
t.95 <- qt(0.95, n-1)
ci <- mean.pit + c(-1,1) * t.95 * se.pit
ci
t.test(pituus, mu=167, conf.level=0.90) 

# Tehtävä 2
u <- seq(150, 185, by=0.1) ; u
mu <- 167; sig <- 5
plot( u, dnorm(u, mu, sig), type = "l", ylim=c(0,0.1) )

pnorm(160, 167, 5)
1 - pnorm(175, 167, 5)

qnorm(0.025, 167, 5)
qnorm(0.975, 167, 5)

# Tehtävä 3
otos <- rnorm(7, mu, sig); otos
summary(otos); sd(otos); SEmean(otos)
otos <- rnorm(1000, mu, sig); otos
summary(otos); sd(otos); SEmean(otos)

hist(otos, freq=FALSE, breaks=seq(130,210, by=2), xlim=c(150,185), add=TRUE)
source("Esanfunktiot.R")
options(digits=4)
otos10 <- normotos.sim(10, mu, sig)

summary(otos10)
otos100 <- normotos.sim(100, mu, sig, loc=F)
summary(otos100)

otos7.10k <- normotos.sim(7, mu, sig, nsim=10000, kuva=FALSE, loc=FALSE)
attach(otos7.10k)
summary(otos7.10k)

hist(keskiarvo, freq=F, br=150:185)
lines( u, dnorm(u, mu, sig/sqrt(7)) )
lines( u, dnorm(u, mu, sig), lty=3 ) 

hist(hajonta, freq=FALSE)
hist(T.suure, freq=F, br=seq(-20, 20, by=0.2), xlim=c(-6,6) )
tval <- seq(-6,6, by=0.1)
lines( tval, dnorm(tval), col="red" )
lines( tval, dt(tval, df=7-1), col="blue" )
hist(P.arvo, freq=FALSE)

length(mu.alar[ mu.alar > 167]) + length(mu.ylar[ mu.ylar < 167])
































