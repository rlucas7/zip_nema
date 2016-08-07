#
# code to simulate a zero-infalted poisson
# random variate and create the graphic for
# the brief research note on using 
# zero-inflated count models
#=======================================
set.seed(107986)
m <- 1000 # the number of simulations to check
n <- 100
p=1/exp(1)
true_lambda <-1
require('pscl')
xi<- matrix(0.0, nrow=m, ncol=1)

for(i in 1:m){
z <- rbinom(n,1,prob=p)
y <-ifelse(z==1, 0, rpois(n,true_lambda))	
my <- mean(y)
# get the poisson likelihood
l_pois <- sum(dpois(y, lambda=my, log=TRUE))
# now get the zip likelihood
zip <- zeroinfl(y~., data = data.frame(y), dist='pois')
zlambda <- exp(coef(zip)[1]) # zip lambda estimate
zphi <- 1/(1+exp(-coef(zip)[2]) )# zip phi estimate

zip_like0 <- sum(y==0)*( log(zphi + (1-zphi)*exp(-zlambda)) )
zip_like_plus <-sum(log(1-zphi)+ dpois(y[y>0],lambda=zlambda, log=TRUE))
zip <- zip_like0 + zip_like_plus
xi[i] <- 2*(zip-l_pois)
}
xi[xi<0] <- 0

hist(xi, breaks=25, prob=TRUE, main='LR samples', xlab=expression(xi), ylab=expression(Pr(Xi<xi)))
lines(dchisq(1:50, 1, ncp = 0), type='l')
