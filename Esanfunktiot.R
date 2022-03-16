# omatfunktiot.R
# --------------

tunnus.taulu <- function(Y,by,pyor=3)
############
{
   #  Funktio, joka laskee havaintojen lukumäärät, muuttujan Y keskiarvot,
   #  -hajonnat ja varianssit tekijän 'by' eri tasoille ja koko aineistolle.
   #  'pyor' = desimaalitarkkuus keskiarvoille, -hajonnoille ja variansseille.
  n<-tapply(Y,by,length)
  keskiarvo<-round(tapply(Y,by,mean),pyor)
  hajonta<-round(tapply(Y,by,sd),pyor)
  varianssi<-round(tapply(Y,by,var),pyor)
  yhteensa<-c(length(Y),round(mean(Y),pyor),round(sd(Y),pyor),round(var(Y),pyor))
  return(rbind(cbind(n,keskiarvo,hajonta,varianssi),yhteensa))
}

perc.table <-  function(row, col, margin = "col", dec=1)
##########
{   {perdir <- 2} 
    if (margin=="row") {perdir <- 1}
   pert <- round(100*prop.table(table(row,col),perdir), dec) 
   if(perdir==2)
  {N <- table(col)
   Total <- round(100*N/N,1) ;
   pert <- rbind(pert, Total, N) ; pert } 
   else
  {N <- table(row)
   Total <- round(100*N/N,1)
   pert <- cbind(pert, Total, N) ; pert } 
}


 normotos.sim <-      # simulates repeated random samples of same size n
##############        # from N(mu, sigma^2) distribution
     function( n=10,
               mu=0, 
               sigma=1, 
               level = 0.9,  # confidence level
               nsim=20,      # number of samples to be simulated
               kuva=TRUE,    # whether a figure is drawn
               loc=TRUE      # whether locator() function is used
               )
{
     xmin<- mu-4*sigma ### määritellään missä välissä plotataan
     xmax<- mu+3*sigma
       x <- seq(mu-3*sigma, mu+3*sigma, length=121)
       dmax <- max(dnorm(x, mu, sigma))
     if (kuva==1) 
        {
        plot(c(0,1),c(0,0),type="n",xlim=c(xmin,xmax), 
               ylim=c(0,nsim),xlab="x",ylab="otos n:o")
        lines( x, (nsim/(2*dmax))*dnorm(x,mu,sigma), col="green", lty=2)
        abline( v= mu, lty=2, col="green") 
        text( mu-3.5*sigma, 0, expression(paste(bar(y),", s") ) ) 
        }
     mu.hat <- NULL
     s <- NULL
     SE <- NULL
     T <- NULL
     P <- NULL 
     up <- NULL
     lo <- NULL
     for(i in 1:nsim){
        # locator(1)
        otos <- rnorm(n,mu,sigma)
        mu.hat[i] <- mean(otos)
        s[i] <- sd(otos)
        SE[i] <- s[i]/sqrt(n) 
        T[i] <- (mu.hat[i] - mu) / SE[i]
        P[i] <- 2*(1-pt(abs(T[i]), n-1) ) 
        up[i]<-mu.hat[i]+qt(1-(1-level)/2,df=n-1)*SE[i]
        lo[i]<-mu.hat[i]-qt(1-(1-level)/2,df=n-1)*SE[i]
        if (kuva) 
         {
           if (loc) 
             { locator(1)
               points(otos,rep(i-0.25,n)) 
             }
           if (loc) locator(1)
           points(c(lo[i], mu.hat[i],up[i]), rep(i+0.1,3), 
                          pch=c(124, 15, 124), col="blue")
           text(mu-3.4*sigma, i, 
              substitute( # list(bar(y),s) == 
                 list(muh,haj) , 
                 list(muh = round(mu.hat[i], 1), haj=round(s[i],1) ) ) ) 
          lines(c(lo[i],up[i]),rep(i,2),col="blue")
         }
     }
     return(data.frame(                
                 keskiarvo=mu.hat, 
                 varianssi=s^2,  
                 hajonta=s, 
                 keskivirhe=SE, 
                 T.suure = round(T,2), 
                 P.arvo = round(P,3), 
                 mu.alar=lo, 
                 mu.ylar=up ) )
}


resi.plot <- function(malli)
#########
{
     y.hat <- fitted(malli) 
     rt <- rstudent(malli)
     D <- cooks.distance(malli)
     par(mfrow=c(2,2))
     plot( y.hat, rt, main="Residuaali vs. sovite",
          xlab="vasteen sovite", ylab="studentoitu residuaali")
     abline(h=0, lty=3)
     lines(lowess(y.hat, rt) )
     plot( y.hat, sqrt(abs(rt)), main="Skaala-lokaatiokuvio" ,
          xlab="vasteen sovite", ylab="sqrt( |studentoitu residuaali| )")
     lines(lowess( y.hat, sqrt(abs(rt)) ) )
     qqnorm(rt) ; abline(a=0, b=1, lty=3) 
     plot( D, main="Cookin etäisyydet", type="h", 
         xlab="havaintoindeksi", ylab="Cookin D") ; points(D)
     par(mfrow=c(1,1))
     }
