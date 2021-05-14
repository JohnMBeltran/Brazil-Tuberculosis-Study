

m1 <- gam(TB ~ offset(I(log(Population/100000))) +
               s(lon, lat, bs="tp", k=-1) +
               s(Indigenous, bs="cr", k=-1) +
               s(Illiteracy, bs="cr", k=-1) +
               s(Urbanisation, bs="cr", k=-1) +
               s(Density, bs="cr", k=-1) +
               s(Poverty, bs="cr", k=-1) +
               s(Poor_Sanitation, bs="cr", k=-1) +
               s(Unemployment, bs="cr", k=-1) +
               s(Timeliness, bs="cr", k=-1)+
               s(Year, bs="cr", k=3),
          data = TBdata, family = poisson(link = "log"))  
AIC(m1); par(mfrow=c(2,2))
gam.check(m1)

m2<- gam(TB ~ offset(I(log(Population/100000))) +
             s(Indigenous, bs="cr", k=-1) +
             s(Illiteracy, bs="cr", k=-1) +
             s(Urbanisation, bs="cr", k=-1) +
             s(Density, bs="cr", k=-1) +
             s(Poverty, bs="cr", k=-1) +
             s(Poor_Sanitation, bs="cr", k=-1) +
             s(Unemployment, bs="cr", k=-1) +
             s(Timeliness, bs="cr", k=-1) +
             te(lon, lat, Year, d=c(2,1), bs=c('tp', 'cr'), k=c(30, 3)),
         data = TBdata, family = nb(link = "log"))  
AIC(m2); par(mfrow=c(2,2))
gam.check(m2)
summary(m2)

m3 <- gam(TB ~ offset(I(log(Population/100000))) +
               Indigenous +
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
AIC(m3); par(mfrow=c(2,2))
gam.check(m3)
summary(m2)


m2012 <- gam(TB ~ offset(I(log(Population/100000))) + # 2012 Model for 2014
                  Indigenous +
                  s(Urbanisation, bs="cr", k=-1) +
                  s(Density, bs="cr", k=-1) +
                  s(Poverty, bs="cr", k=-1) +
                  s(Poor_Sanitation, bs="cr", k=-1) +
                  s(Unemployment, bs="cr", k=-1) +
                  s(Timeliness, bs="cr", k=-1) +
                  s(lon, lat, bs="tp", k=100),
              data = TBdata[TBdata$Year==2012,], family = nb(link = "log")) 
#AIC(m2012); par(mfrow=c(2,2))
#gam.check(m2012)
#summary(m2012)
New2014 <- predict.bam(m2012, TBdata[TBdata$Year==2014,], type="response", se.fit=T)
par(mfrow=c(1,1))
plot(log(quantile(New2014$fit,probs=seq(0,1,0.01))),
     log(quantile(TBdata[TBdata$Year==2014,]$TB,probs=seq(0,1,0.01))),
     main="2012-Data Model predict 2014, QQ Plot",
     xlab="'New2014$fit' Log Quantiles", ylab="2014 TB Log Quantiles")
lines(seq(0,10, 0.01), seq(0,10,0.01), col ='red')
lines(log(quantile(New2014$fit+1.96*New2014$se.fit,probs=seq(0,1,0.01))),
      log(quantile(TBdata[TBdata$Year==2014,]$TB,probs=seq(0,1,0.01))), 
      col = 'purple', lty =2)
lines(log(quantile(New2014$fit-1.96*New2014$se.fit,probs=seq(0,1,0.01))),
      log(quantile(TBdata[TBdata$Year==2014,]$TB,probs=seq(0,1,0.01))), 
      col = 'purple', lty =2)
legend('bottomright', c("95% Prediction Interval", 
                        "Preductions' vs Fitted's Quantiles",
                        "Perfect Line"), 
       lty=c(2,NA,1), pch=c(NA, 1, NA), bty='c', box.col='white', bg='white',
       col=c('purple', 'black', 'red'))
grid(lwd=0.75); axis(1)



m4 <- gam(TB ~ offset(I(log(Population/100000))) + #Global Effects + Individual
            Indigenous +
            s(Urbanisation, bs="cr", k=-1) +
            s(Density, bs="cr", k=-1) +
            s(Poverty, bs="cr", k=-1) +
            s(Poor_Sanitation, bs="cr", k=-1) +
            s(Unemployment, bs="cr", k=-1) +
            s(Timeliness, bs="cr", k=-1) +
            s(lon, lat, bs="tp", k=-1) +
            s(Region, bs='re') + s(Year, k=3) + s(Year, by=Region, k=3, m=1),
          data = TBdata, family = nb(link = "log")) 
AIC(m4); par(mfrow=c(2,2))
gam.check(m4)
summary(m4)


m5 <- gam(TB ~ offset(I(log(Population/100000))) + # Individual Effects Only
            Indigenous +
            s(Urbanisation, bs="cr", k=-1) +
            s(Density, bs="cr", k=-1) +
            s(Poverty, bs="cr", k=-1) +
            s(Poor_Sanitation, bs="cr", k=-1) +
            s(Unemployment, bs="cr", k=-1) +
            s(Timeliness, bs="cr", k=-1) +
            s(lon, lat, bs="tp", k=-1) +
            s(Region, bs='re') + s(Year, by=Region, k=3),
          data = TBdata, family = nb(link = "log")) 
AIC(m5); par(mfrow=c(2,2))
gam.check(m5)
summary(m5)


mfinal <- gam(TB ~ offset(I(log(Population/100000))) + # Factor Smooths Model
            Indigenous +
            s(Urbanisation, bs="cr", k=-1) +
            s(Density, bs="cr", k=-1) +
            s(Poverty, bs="cr", k=-1) +
            s(Poor_Sanitation, bs="cr", k=-1) +
            s(Unemployment, bs="cr", k=-1) +
            s(Timeliness, bs="cr", k=-1) +
            s(lon, lat, bs="tp", k=-1) + 
            s(Year, Region, bs='fs'),
          data = TBdata, family = nb(link = "log")) 
AIC(mfinal); par(mfrow=c(2,2))
gam.check(mfinal)
summary(mfinal)


plot(mfinal, pages = 2, shade = TRUE)





