# Need to run this script first. Run Before the Analysis one.
install.packages("ggplot2")
require(lattice)
require(ggplot2)
require(tidyverse)
require(sp)
require(mgcv)
require(maps)
require(fields)
load("TB.RData")
head(TBdata)

## Log Population
tb1 <- TBdata
tb1$log.pop <- log(tb1$Population)
names(tb1)[9] <- "TBRate"

## Remove Zeros from Tb Counts
tb2 <- TBdata
for (i in seq(1:nrow(tb2))){
  if(tb2$TB[i]==0){
    tb2$TB[i] <- 1e-10
  }
}
tb2$Rate <- tb2$TB/(tb2$Population/1000)

## Data only for 2012
tb3 <- TBdata[TBdata$Year==2013,]
y <- 11001+2
plot(TBdata$Year[TBdata$Region==y], TBdata$Indigenous[TBdata$Region==y])



## Code to study any changes in Covariates: Example for Indigenous
i <- 0; diff <- 0; change_no <- 0
for (y in TBdata$Region){
  i <- i+1
  diff[i] <- max(TBdata$Indigenous[TBdata$Region==y])-
                   min(TBdata$Indigenous[TBdata$Region==y])
  if(diff[i]!=0){
    print(paste0("Region ",y," Has a change!", sep=" "))
    change_no <- change_no+1
  }
  if(diff[i]!=0){
    print(paste0("Region ",y," Has a change!", sep=" "))
    change_no <- change_no+1
  }
  if(i==200){print("Check at 50")} # Checkign we passed this point
  if(i==1400){print("Check at 1400")} # Same detail. 
} 
print(paste0("Regions Chanegd over Time: ", change_no, ",  Of ",nrow(TBdata)))
if(change_no!=0){print(paste0("Max: ",max(diff),", Min: ",min(diff)))}
# plot(density(diff)) # Uncomment to see Density.
# REPEAT for Covariates 1-8, by replacing the relevant Covariate Location.
# We Observe no changes in the values of the covariates 1-8, per year.  



#Example: 
install.packages(gamair)
load("gamair")
require(gamair)


##### LRT Test 
LRT <- as.numeric(-2*(logLik(m1)-logLik(m2)))
1 - pchisq(LRT, 1)

sim_LRT <- 1:1000
Dat <- simulate(m1,1000) 
for(i in 1:1000){ 
  Mod1 <- bam(Dat[,i] ~ 
                offset(I(log(Population/1000))) +  
                s(lon, lat, k = 100),
              data = TBdata, family = nb(link = "log"),
              method = "ML")
  Mod2 <- bam(Dat[,i] ~ 
                offset(I(log(Population/1000))) + 
                s(Indigenous) + 
                s(Illiteracy) + 
                s(Urbanisation) +
                s(Density) + 
                s(Poverty) + 
                s(Poor_Sanitation) + 
                s(Unemployment) + 
                s(Timeliness) +
                s(Year, k = 3) + 
                s(lon, lat, k = 100),
              data = TBdata, family = nb(link = "log"),
              method = "ML")
  sim_LRT[i] <- -2*(logLik(Mod1)-logLik(Mod2))
}

