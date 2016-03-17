library(DBI)
library(RSQLite)
library(jsonlite)
library(ggplot2)

# read database file
sqlite    <- dbDriver("SQLite")
exampledb <- dbConnect(sqlite,"origin_db")
MiData <- dbGetQuery(exampledb,"select * from date_data")

#data cleanning
MiData$date<-as.Date(MiData$date,"%Y-%m-%d")
jsondata<-sapply(MiData$summary,fromJSON)
slp<-jsondata[1,]
stp<-jsondata[4,]
sleepdur<-unname( unlist( lapply(slp,"[[","lt") )  )   ##extract elements from list of list
deepsleep<-unname( unlist( lapply(slp,"[[","dp") )  )
totalstep<-unname( unlist( lapply(stp,"[[","ttl") ) )

#plot 1
png("total sleep duration.png")
p<-ggplot(data.frame(date=MiData$date,sleepdur=sleepdur/60),aes(date,sleepdur))
p<-p+geom_point()+geom_smooth()+geom_smooth(method="lm",color="red")
p<-p+labs(title="Total Sleep")+ylab("Sleep Duration (hour)")
p
dev.off()

#plot 2
png("deep sleep duration.png")
p<-ggplot(data.frame(date=MiData$date,deepsleep=deepsleep/60),aes(date,deepsleep))
p<-p+geom_point()+geom_smooth()+geom_smooth(method="lm",color="red")
p<-p+labs(title="Deep Sleep")+ylab("Deep Sleep Duration (hour)")
p
dev.off()

#plot3
png("total step.png")
p<-ggplot(data.frame(date=MiData$date,totalstep=totalstep),aes(date,totalstep))
p<-p+geom_point()+geom_smooth()+geom_smooth(method="lm",color="red")
p<-p+labs(title="Total Steps")+ylab("Step")
p
dev.off()
