# Teht‰v‰ 2
pituus <- c(183, 176, 173, 177, 185)
paino <- c(70, 76, 63, 69, 80)
tdk <- c("LuTK", "OyKKK", "TSTK", "LuTK", "OyKKK")
sum(tdk == "LuTK")
opiskelijat <- data.frame(pituus, paino, tdk)
opiskelijat
opiskelijat[4, 2]
write.table(opiskelijat, quote=FALSE, file = "opiskelija.txt")

# Teht‰v‰ 3
pituus.m <- pituus/100
bmi <- paino/pituus.m^2
bmi
round(bmi,2)
sum(paino)/length(paino)
mean(paino)
summary(paino)
painoja <- c(paino, NA, 81)
painoja
sum(painoja)/length(painoja)
mean(painoja)
summary(painoja)
mean(painoja, na.rm=TRUE)

# Teht‰v‰ 4
plot(pituus, paino)
plot(pituus, paino, main="Sirontakuvio", xlab="Pituus (cm)", ylab="Paino (kg)", xlim=c(170, 190), ylim=c(60, 85), pch=15, cex=2, cex.lab=1.5)
text(170, 80, "Kappas, t‰nneh‰n voi kirjoittaa!", pos=4, col="red")

# Teht‰v‰ 5
dake <- read.table("world.txt", header=TRUE)
str(dake)

# Teht‰v‰ 6
dake$ALUE <- factor(dake$alue, labels = c("Australia", "Aasia", "Afrikka", "Eurooppa", "P-Amerikka", "E-Amerikka") )
attach(dake)
summary(dake)

# Teht‰v‰ 7
library(Epi)
stat.table( index = list( rannikko, ALUE ), contents = count(), data = dake)
dake$maanosa <- Relevel( dake$ALUE, list(1, 2, 3, 4, 5:6 ) )
stat.table( index = list( maanosa, rannikko ), contents = list( count(), percent(rannikko) ), margins = TRUE, data = dake)

# Teht‰v‰ 8
with(dake, stem(elinodote, scale=1.5))
with(dake, boxplot(elinodote, horizontal=TRUE, xlab="Vuotta"))
summary(elinodote)
sd(elinodote, na.rm=TRUE)
library(beeswarm)
par(mfrow=c(2,1))
stripchart(dake$elinodote)
beeswarm(dake$elinodote, horizontal=T, xlab="Vuotta")
which( elinodote > 85 )
dake[ c(119, 157, 211) , ]

# Teht‰v‰ 9
dake <- transform( dake, vaesttih = vaesto/pintaala)
attach(dake)
par(mfrow=c(1,1))
boxplot(vaesttih, horizontal=TRUE, xlab="Asukkaiden lkm neliˆkilometrill‰")
summary(vaesttih) # Moodle 13
sd(vaesttih, na.rm=TRUE)
dake[ which( vaesttih > 15000 ) , ]
dake[which(vaesttih > 5000) , c("Maa", "pintaala", "vaesto")]

# Teht‰v‰ 10
par(mfrow=c(1,1))
hist(elinodote, main="Elinajanodotteen histogrammi") # Moodle 15
hist(elinodote, main="Elinajanodotteen histogrammi", breaks=c(50, 60, 70, 75, 80, 85, 90) )
plot( density(elinodote[!is.na(elinodote)]), lwd=2)
par(mfrow=c(2,1))
hist(elinodote, main="Elinajanodotteen histogrammi")
plot( density(elinodote[!is.na(elinodote)]), lwd=2)

# Teht‰v‰ 11
par(mfrow=c(1,1))
boxplot(elinodote ~ maanosa, horizontal=T, las=1)

par(mfrow=c(2,2))
beeswarm( elinodote ~ maanosa, cex=0.5)
beeswarm( elinodote ~ maanosa, horizontal=T, method="center", , cex=0.5)
beeswarm( elinodote ~ maanosa, horizontal=T, method="hex", pch=16, cex=0.5)
beeswarm( elinodote ~ maanosa, horizontal=T, method="square", pch=16, cex=0.5)

par(mfrow=c(2,1))
with( subset( dake, !is.na(elinodote) & ALUE=="P-Amerikka"), plot(density(elinodote), lwd=2, col="blue", xlim=c(60,90) ) )
with( subset( dake, !is.na(elinodote) & ALUE=="E-Amerikka"), plot(density(elinodote), lwd=2, col="red", xlim=c(60,90) ) )

par(mfrow=c(1,1))
plot(ecdf(bkt[ALUE=="Aasia"]), do.points=FALSE, col = 'red', xlim=c(0,125000), verticals=T)
plot(ecdf(bkt[ALUE=="Afrikka"]), do.points=FALSE, col = 'green', add=TRUE, verticals=T)
plot(ecdf(bkt[ALUE=="Eurooppa"]), do.points=FALSE, col = 'blue', add=TRUE, verticals=T)
# nimet‰‰n k‰yr‰t
legend(100000, 0.3, legend=c("Aasia", "Afrikka", "Eurooppa"), lty=c(1,1,1), col=c("red","green","blue"))

with(dake, tapply(bkt, ALUE, summary))










