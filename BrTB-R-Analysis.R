require(tidyverse)
require(sp)
require(mgcv)
require(maps)
require(fields)
load("TB.RData")
head(TBdata)


plot.map(TBdata$TB[TBdata$Year==2014],n.levels=7,main="TB counts for 2014")







