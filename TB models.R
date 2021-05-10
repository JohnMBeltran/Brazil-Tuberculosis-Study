m2012 <- bam(TB ~ 
            offset(I(log(Population/1000))) +
            s(lon, lat, k = 50) +
            s(Urbanisation) +
            s(Density) + 
            s(Poverty) + 
            s(Poor_Sanitation) + 
            s(Unemployment) + 
            s(Timeliness) +
              
            Indigenous,
          data = TBdata[TBdata$Year==2014,], 
          family = nb(link = "log"), 
          select = TRUE, method="fREML")   
          ## AIC 4747
predict.bam(m1, TBdata[TBdata$Year==2012,], type="link")

m2013 <- bam(TB ~ 
               offset(I(log(Population/1000))) +
               s(lon, lat, k = 50) +
               s(Urbanisation) +
               s(Density) + 
               s(Poverty) + 
               s(Poor_Sanitation) + 
               s(Unemployment) + 
               s(Timeliness) +
               Indigenous,
             data = TBdata[TBdata$Year==2013,], 
             family = nb(link = "log"), 
             select = TRUE, method="fREML")   
## AIC 4742



p1 <- sum(m2$edf) # Must have 
p2 <- sum(m1$edf)
DM1 <- m1$deviance
DM2 <- m2$deviance
Fstat <- ((DM1-DM2)/(p2-p1))/(DM2/(nrow(TBdata)-p2-1))
1-pf(Fstat,p2-p1,nrow(TBdata)-p2-1)


