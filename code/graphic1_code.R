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
# get the score statistic
#l_pois <- ( sum(y==0)+ n*exp(-my))^2 / (n * ( 1 - exp(-my) ) * exp(-my))   )
# now get the zip likelihood
zip <- zeroinfl(y~., data = data.frame(y), dist='pois')
zlambda <- exp(coef(zip)[1]) # zip lambda estimate
zphi <- 1/(1+exp(-coef(zip)[2]) )# zip phi estimate
score<- (sum(y==0) - n*exp(-my) )^2 / ( n*exp(-my)*(1-exp(-my)) -n*my*exp(-2*my)) 
xi[i]<-score
}


pdf(file='hist.pdf')
hist(xi, breaks=25, prob=TRUE, main='Score statistic samples', xlab=expression(xi), ylab=expression(Pr(Xi<xi)))
seq_line <- dchisq(seq(0,35, by=1), 1, ncp = 0)/2
lines(seq_line, type='l')
dev.off()

getwd() # tells you where the machine just put the file