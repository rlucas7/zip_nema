### code to analyze the nematode data for zero-inflation from Dr. Eisenbeck's paper 

## set filepath for the files 
getwd()
logname<-Sys.getenv('LOGNAME')
setwd(paste0("/Users/", logname, "/Desktop"))
getwd()

## now read in the data 

nematodes <- read.csv(list.files()[13],
                      header=TRUE, 
                      colClasses="character",
                      sep=',')
head(nematodes)

## now some basic profiling of the data 

summary(as.numeric(nematodes$Root.rating))

summary(as.numeric(nematodes$Cysts))

summary(as.numeric(nematodes$Eggs))


###3 now to perform the Jan Van der Broek score test (biometrics June 1995)

lambda_est <- mean(as.numeric(nematodes$Cysts))

p0_tilde <- exp(-lambda_est)
p0_tilde
n0 <- sum(1*(!(as.numeric(nematodes$Cysts) >0)))
n <- length(nematodes$Cysts)

# number of observtions 'expected' to be zero
n*p0_tilde

#now lets perform the JVDB score test 
numerator <- (n0 -n*p0_tilde)^2
denominator <- n*p0_tilde*(1-p0_tilde) - n*lambda_est*(p0_tilde^2)

test_stat <- numerator/denominator

pvalue <- pchisq(test_stat,df=1, ncp=0, lower.tail=FALSE)
pvalue 
### the extremely small pvalue indicates zero-inflation! 

# prepare data for zeroinfl() fitter
nm_dat<- data.frame(as.numeric(nematodes$Cysts))
names(nm_dat)<-'nm_dat'

# now fit the two parameters... 
zip <- zeroinfl(as.formula('nm_dat~1 | 1'), data=nm_dat, dist="poisson")
lambda_est
summary(zip)
## lambda estimate from poisson 
lambda_est
## lambda estimate in zip
exp(coef(zip)[1])
### difference of two estimates 
exp(coef(zip)[1])- lambda_est

### estimated zero-inflation probability 
(1+exp(-coef(zip)[2]))^(-1)

### but really the zero inflation should be measured 
### above the amount estimated by Poisson 

(1 -exp(-exp(coef(zip)[1]) ) ) / (1+exp(-coef(zip)[2]))^(-1)


