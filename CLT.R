#q8a
rm=(list=ls())
clt <- function(type, samsize, samn){
  means <- new.env()
  for(i in 1:samn){
    if(type=='normal'){
      vals <- rnorm(samsize)
      xl=c(-1,1)
      yl=c(0,2.5)
    }
    else if(type=='uniform'){
      vals <- runif(samsize)
      xl=c(0,1)
      yl=c(0,8)
    }
    else if(type=='exponential'){
      vals <- rexp(samsize)
      xl=c(0.4,1.6)
      yl=c(0,2.5)
    }
    else if(type=='binomial'){
      vals <- rbinom(samsize, size=1, prob=0.5)
      xl=c(0,1)
      yl=c(0,5)
    }
    else if(type=='poisson'){
      vals <- rpois(samsize, lambda=1)
      xl=c(0.4,1.6)
      yl=c(0,2.5)
    }
    else if(type=='geometric'){
      vals <- rgeom(samsize, prob=0.5)
      xl=c(0,2)
      yl=c(0,2)
    }

    means$mean <- c(means$mean,mean(vals))
    means$var <- c(means$var, var(vals))
    mac <- paste(samsize," out of ", samn)
    hist(means$mean,probability=TRUE, col='red', xlim=xl, ylim=yl, xlab=mac, main='')
    points(mean(vals),col='black')
    if(type=='normal') { curve(dnorm(x,0,1/sqrt(samsize)), col='green', lwd=3, add=TRUE) }
    else { 
      curve(dnorm(x,mean(vals),sd(vals)/sqrt(samsize)), col='green', lwd=3, add=TRUE)
    }
  }
  
  abline(v=mean(means$mean), col='yellow', lwd=3)
  result<-paste(type, "- mean: ",mean(means$mean)," variance: ", mean(means$var))
  return(result)
}


norm <- clt('normal', 30, 150)
norm
unif <- clt('uniform', 30, 150)
unif
exp <- clt('exponential', 30, 150)
exp
binom <- clt('binomial', 30, 150)
binom
pois <- clt('poisson', 30, 150)
pois
geom <- clt('geometric', 30, 150)
geom



#q8b
rm=(list=ls())
sampling <- function(samsize, samn){
  means <- new.env()
  for(i in 1:samn){
    vals <- rnorm(samsize)
    means$mean <- c(means$mean,mean(vals))
    means$var <- c(means$var, var(vals))
    mac <- paste(samsize," out of ", samn)
    hist(means$mean,probability=TRUE, col='red', xlim=c(-2.5,2.5), ylim=c(0,3.5), xlab=mac, main='')
    points(mean(vals),col='black')
    curve(dnorm(x,0,1/sqrt(samsize)), col='green', lwd=3, add=TRUE)
    curve(dnorm(x,0,1), col='blue', lwd=3, add=TRUE)
  }
  
  abline(v=mean(means$mean), col='yellow', lwd=3)
  result<-paste("mean: ",mean(means$mean)," variance: ", mean(means$var))
  return(result)
}

abc<-sampling(30,150)
abc
def<-sampling(60,150)
def
ghi<-sampling(90,150)
ghi
jkl<-sampling(120,150)
jkl
