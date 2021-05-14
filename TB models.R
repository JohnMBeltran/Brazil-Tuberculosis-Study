m2<- gam(TB ~ 
            offset(I(log(Population/100000))) +
            s(Indigenous, bs="cr", k=-1) +
            s(Illiteracy, bs="cr", k=-1) +
            s(Urbanisation, bs="cr", k=-1) +
            s(Density, bs="cr", k=-1) +
            s(Poverty, bs="cr", k=-1) +
            s(Poor_Sanitation, bs="cr", k=-1) +
            s(Unemployment, bs="cr", k=-1) +
            s(Timeliness, bs="cr", k=-1) +
            s(lon, lat, bs="tp", k=100) +
            s(Year, bs="cr", k=3) +
            ti(lon, lat, Year, d=c(2,1), bs=c('tp', 'cr'), k=c(30, 3)),
          data = TBdata, family = nb(link = "log"))  
AIC(m2)
gam.check(m2)
par(mfrow=c(2,2))






p1 <- sum(m2$edf)
p2 <- sum(m1$edf)
DM1 <- m1$deviance
DM2 <- m2$deviance
Fstat <- ((DM1-DM2)/(p2-p1))/(DM2/(nrow(TBdata)-p2-1))
1-pf(Fstat,p2-p1,nrow(TBdata)-p2-1)





