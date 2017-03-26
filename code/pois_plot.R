#code to make Poisson plot 
getwd()
setwd("/Users/lucasroberts/Documents/ZIP_nema/figs")
x<- 0:25
p1 <- dpois(x,1)

pdf('pois_pmf.pdf')
plot(x,p1,xlab='Y',ylab='Pr(Y=y)')
p5 <- dpois(x,5)
p10 <- dpois(x,10)
points(x,p5, col='red')
points(x,p10, col='green')
lines(x,p1)
lines(x,p5,col='red')
lines(x,p10,col='green')

legend(20,0.3,c(expression(lambda==1),expression(lambda==5), expression(lambda==10)),  lwd=c(2.5,2.5,2.5), col=c('black', 'red', 'green'))
dev.off()


setwd("/Users/lucasroberts/Documents/ZIP_nema/figs")
x<- 0:25
p1 <- dpois(x,1)
p5 <- dpois(x,5)
p10 <- dpois(x,10)
p1[-1] <- 0.75*dpois(x[-1],1)
p1[1]<- 0.75*p1[1]+0.25
pdf('zi_pois_pmf.pdf')
plot(x,p1,xlab='Y',ylab='Pr(Y=y)', main=expression(phi==0.25))
p5[-1] <- 0.75*dpois(x[-1],5)
p5[1] <- 0.75*p5[1]+0.25
p10[-1] <- 0.75*dpois(x[-1],10)
p10[1]<- 0.25 + 0.75 * dpois(x[1],10)
points(x,p5, col='red')
points(x,p10, col='green')
lines(x,p1)
lines(x,p5,col='red')
lines(x,p10,col='green')

legend(20,0.3,c(expression(lambda==1),expression(lambda==5), expression(lambda==10)),  lwd=c(2.5,2.5,2.5), col=c('black', 'red', 'green'))
dev.off()