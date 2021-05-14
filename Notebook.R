#### Attemps Ru=ound 1:
model1 <- gam(TBRate ~ s(Indigenous,bs="cs")+
                s(Illiteracy,k=15,bs="cs") + s(Urbanisation,bs="cs") +
                s(Density,bs="cs") + s(Poverty,bs="cs") +
                s(Poor_Sanitation,bs="cs") + s(Unemployment,bs="cs")
              ,data=tb1,family=gaussian(link="identity"))
# bs="re" Random Effects Spline
model1 <- gam(TBRate ~ s(Indigenous,bs="cs")+
                s(Illiteracy,k=15,bs="cs") + s(Urbanisation,bs="cs") +
                s(Density,bs="cs") + s(Poverty,bs="cs") +
                s(Poor_Sanitation,bs="cs") + s(Unemployment,bs="cs")
              ,data=tb1,family=gaussian(link="identity"))
model2 <- gam(TB~offset(I(log(Population)))+
                s(Illiteracy,bs="cs") + s(Urbanisation,bs="cs") +
                s(Density,bs="cs") + s(Poverty,bs="cs") +
                s(Poor_Sanitation,bs="cs") + s(Unemployment,bs="cs") +
                s(Year,bs="re") + s(Region,bs="re"),
              data=TBdata,family=poisson(link="log"))
#### Attempts Round 2
model4 <- bam(TB ~ offset(I(log(Population))) + #Offset Pop. for Rate of TB
                s(Year,bs="cr",k=3) + #Now for the Main Covariates:  
                s(Indigenous,bs="cr",k=30)+
                s(Illiteracy,bs="cr",k=20)+s(Urbanisation,bs="cr",k=20)+
                s(Urbanisation,bs="cr",k=20)+s(Density,bs="cr",k=20)+
                s(Poverty,bs="cr",k=20)+s(Poor_Sanitation,bs="cr",k=20)+
                s(Unemployment,bs="cr",k=20)+s(Timeliness,bs="cr",k=20)+
                s(Density,bs="cr",k=20)+s(Poverty,bs="cr",k=20)+
                s(Poor_Sanitation,bs="cr",k=20)+
                s(lon,lat,k=200), data=TBdata, family=poisson(link="log"))
gam.check(model4)





### Attempt Round 3
TB ~ offset(log.pop) + Indigenous + s(Illiteracy, k = -1, bs = "cr") + 
  s(Urbanisation, k = -1, bs = "cr") + s(Density, k = -1, bs = "cr") + 
  s(Poverty, k = -1, bs = "cr") + s(Poor_Sanitation, k = -1, 
   bs = "cr") + s(Unemployment, k = -1, bs = "cr") + 
  s(Timeliness, k = -1, bs = "cr") + Year + 
  ti(lon, lat, Year, d = c(2, 1), bs = c("tp", "cr"), k = c(50, 3))
# Goodness of Fit [1] 0.04881175

