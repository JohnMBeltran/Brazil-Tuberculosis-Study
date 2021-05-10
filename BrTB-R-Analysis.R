#### Model Comparison test:
  #Chi Squared:
  1-pchisq(add_m$deviance,geo_reg_m$deviance)

  ##FTest:
  p1 <- sum(model3$edf); p2 <- sum(model1$edf)
  DM1 <- model3$deviance
  DM2 <- model1$deviance
  Fstat <- ((DM1-DM2)/(p2-p1))/(DM2/(nrow(TBdata)-p2-1))
  1-pf(Fstat,p2-p1,nrow(TBdata)-p2-1)

##Model Tools: 
  qqnorm(resid(model2),pch=20, main="Residuals QQ Plot ")
  qqline(resid(model2))
  AIC(model9, model10)
  gam.check(model1)
  plot(model9)
  
  par(mfrow=c(1,1))  
#####################################
  

 
m1 <- bam(TB ~ 
               offset(I(log(Population/10000))) +
               s(lon, lat, k = 50) +
               s(Urbanisation) +
               s(Density) + 
               s(Poverty) + 
               s(Poor_Sanitation) + 
               s(Unemployment) + 
               s(Timeliness) +
               Indigenous,
             data = TBdata, 
             family = nb(link = "log"), 
             select = TRUE, method="fREML") 

  
m2 <- bam(TB ~ 
            offset(I(log(Population/1000))) +
            s(lon, lat, k = 100) +
            s(Urbanisation) +
            s(Density) + 
            s(Poverty) + 
            s(Poor_Sanitation) + 
            s(Unemployment) + 
            s(Timeliness) +
            s(Region, bs="re")+
            s(Year, k=3) + 
            s(Year,k=3, by=Region, m=1)+
            Indigenous,
          data = TBdata, 
          family = tw(link = "log"), method="REML")
AIC(m2)

m1 <- gam(TB ~ 
            offset(I(log(Population/100000))) +
            s(lon, lat, k = 100) +
            ti(lon, lat, Year, d=c(2,1), k=c(-1, 3), bs=c('tp', 'cr'))
            s(Urbanisation, bs="cr", k=-1) +
            s(Density, bs="cr", k=-1) + 
            s(Poverty, bs="cr", k=-1) + 
            s(Poor_Sanitation, bs="cr", k=-1) + 
            s(Unemployment, bs="cr", k=-1) + 
            s(Timeliness, bs="cr", k=-1) +
            s(Year, k=3, bs="cr") + 
            Indigenous,
          data = TBdata, 
          family = nb(link = "log"), method="REML")


p1 <- sum(model3$edf); p2 <- sum(model1$edf)
DM1 <- model3$deviance
DM2 <- model1$deviance
Fstat <- ((DM1-DM2)/(p2-p1))/(DM2/(nrow(TBdata)-p2-1))
1-pf(Fstat,p2-p1,nrow(TBdata)-p2-1)










