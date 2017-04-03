### code to analyze the nematode data for zero-inflation from Dr. Eisenbeck's paper 

## set filepath for the files 
getwd()
logname<-Sys.getenv('LOGNAME')
setwd(paste0("/Users/", logname, "/Desktop/nematodes"))
getwd()

## now read in the data 

nematodes <- read.csv("adamo_nematodes.csv",
                      header=TRUE, 
                      #colClasses=c("character","character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric") 
                      sep=',')
head(nematodes, n=15)
tail(nematodes)
nm_list <- names(nematodes)[3:11]
## now some basic profiling of the data 
str(nematodes)
library(pscl)

###3 now to perform the Jan Van der Broek score test (biometrics June 1995)
zip_results <- data.frame( matrix(0.0, nrow=9, ncol=12)) 
  
colnames(zip_results) <- c("species_name", "lambda_est", "p0_tilde", "n0", "n", "expected_0s", "JVDB_score_test_stat", "JVDB_pvalue", "zip_lambda_est", "diff_lambda_est", "zip_est", "zip_est_above_pois" )
zip_results

for(i in 3:11){
zip_results$species_name[i-2] <- names(nematodes)[i]
zip_results$lambda_est[i-2] <- mean(as.numeric(nematodes[,i]))

p0_tilde <- exp(-zip_results$lambda_est[i-2])
zip_results$p0_tilde[i-2] <- p0_tilde
n0 <- sum(1*(!(as.numeric(nematodes[,i]) >0)))
n <- length(nematodes[,i])
zip_results$n0[i-2] <- n0
zip_results$n[i-2] <- n
# number of observtions 'expected' to be zero
zip_results$expected_0s[i-2] <- n*p0_tilde

#now lets perform the JVDB score test 
numerator <- (n0 -n*p0_tilde)^2
denominator <- n*p0_tilde*(1-p0_tilde) - n*lambda_est*(p0_tilde^2)

test_stat <- numerator/denominator
zip_results$JVDB_score_test_stat[i-2] <- test_stat
pvalue <- pchisq(test_stat,df=1, ncp=0, lower.tail=FALSE)
zip_results$JVDB_pvalue[i-2] <- pvalue 
### the extremely small pvalue indicates zero-inflation! 

# prepare data for zeroinfl() fitter
nm_dat<- data.frame(as.numeric(nematodes[,i]))
names(nm_dat)<-'nm_dat'


# now fit the two parameters... 

### NOTE: we include an ifelse block here because the data might have no zeroes causing an error to be thrown 
### in the zeroinfl() function call 
if( n0 > 0  ){  
  
  zip <- zeroinfl(as.formula('nm_dat~1 | 1'), data=nm_dat, dist="poisson")
  ## lambda estimate in zip
  zip_results$zip_lambda_est[i-2] <- exp(coef(zip)[1])
  ### difference of two estimates 
  zip_results$diff_lambda_est[i-2] <- exp(coef(zip)[1])- lambda_est
  ### estimated zero-inflation probability 
  zip_results$zip_est[i-2] <- (1+exp(-coef(zip)[2]))^(-1)
  ### but really the zero inflation should be measured 
  ### above the amount estimated by Poisson 
  zip_results$zip_est_above_pois[i-2] <- (1 -exp(-exp(coef(zip)[1]) ) ) / (1+exp(-coef(zip)[2]))
  }else{
  ### all non-zero counts, 
    cat("all  non-zero counts for species ", eval(names(nematodes)[i]), ", therefore not fitting a ZIP model for this species")
        }
}# end bracket of for(i in ...) loop

write.csv(zip_results, file="adamo_zip_analysis_results.csv", row.names=FALSE)

