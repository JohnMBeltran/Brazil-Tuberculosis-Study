
### Cases per Thousand: 
with(density(TBdata$TB[TBdata$Year==2012]/(TBdata$Population[TBdata$Year==2012]/1000)),
     plot(x,y, main="Brazil TB Rate, Density, 2012-2014",
          xlab = "Cases Per Thousand People, by Micro-Region",
          ylab = "Density",
          las=1, xlim=c(0, 1.18), lty=1, lwd=0.9, type="l"))
with(density(TBdata$TB[TBdata$Year==2013]/(TBdata$Population[TBdata$Year==2013]/1000)),
     lines(x, y, lty=2, col='Red', lwd=2))
with(density(TBdata$TB[TBdata$Year==2014]/(TBdata$Population[TBdata$Year==2014]/1000)),
     lines(x, y, lty=3, col='Blue', lwd=2))
legend('topright', c('2012', '2013', '2014'), lty=1:3, 
       col=c('Black', 'Red', 'Blue'), lwd = c(0.9, 2, 2), bty='n') 
grid(lwd = 0.75); axis(1)



### Population Density

## Plot.Map Micro Brazil 
plot.map2 <- function(x,n.levels=7,main="",cex=1){
        cols <- rev(heat.colors(n.levels))
        n <- 557
        Q <- quantile(x,probs=seq(0,1,len=n.levels+1))
        col <- rep(cols[1],n)
        for(i in 2:n.levels){
                col[x>=Q[i] & x<Q[i+1]] <- cols[i]
        }
        legend.names <- c()
        for(i in 1:n.levels){
                legend.names[i] <- paste("[",round(Q[i],2),", ",round(Q[i+1],2),"]",sep="")
        }
        plot(brasil_micro,col=col,main=main)
        #legend('bottomleft',legend=legend.names,fill=cols,cex=cex)
}
plot.map2(TBdata$TB[TBdata$Year==2013],main="2013")





### Prediction Densities

New2013 <- predict.bam(m2012, TBdata[TBdata$Year==2013,], type="response")
plot(log(quantile(New2013,probs=seq(0,1,0.01))),
     log(quantile(TBdata[TBdata$Year==2013,]$TB,probs=seq(0,1,0.01))),
     main="2012-Data Model predict 2013, QQ Plot")
lines(seq(0,10, 0.01), seq(0,10,0.01))

New2014 <- predict.bam(m2012, TBdata[TBdata$Year==2014,], type="response", se.fit = T)
plot(log(quantile(New2014$fit,probs=seq(0,1,0.01))),
     log(quantile(TBdata[TBdata$Year==2014,]$TB,probs=seq(0,1,0.01))),
     main="2012-Data Model predict 2014, QQ Plot")
lines(seq(0,10, 0.01), seq(0,10,0.01))

New2013 <- predict.bam(m2012, TBdata[TBdata$Year==2013,], type="response")
plot(log(quantile(New2013,probs=seq(0,1,0.01))),
     log(quantile(TBdata[TBdata$Year==2013,]$TB,probs=seq(0,1,0.01))),
     main="2012-Data Model predict 2013, QQ Plot")
lines(seq(0,10, 0.01), seq(0,10,0.01))


