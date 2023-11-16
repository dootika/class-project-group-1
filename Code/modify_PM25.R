PM2.5=read.csv("IndiaPM25-V5GL03-Annual-REGIONAL-1998-2021-wThresFrac.csv")

PM2.5=PM2.5[PM2.5$Year==2011,c(1,3)]
PM2.5[25,1]='Delhi'
names(PM2.5)=c('State','PM2.5_ug_m3')
write.csv(PM2.5,file="PM25.csv")
