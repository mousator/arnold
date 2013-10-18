require(sp)
require(lattice)
library(rworldmap)

setwd('~/Documents/projects/arnold')
file <- "file.csv"

# read basic csv data
data <- read.csv2(file=file, header=T, sep=",")
# make a nices and simpler data frame
data.s <- data.frame(
  datetime=strptime(as.character(data$Date.and.time.of.GPS.position), format="%Y-%m-%d %H:%M"),
  battery=data$Battery.state,
  gsmsignal=data$GSM.signal.level,
  gpssignal=data$GPS.interval,
  temp=as.numeric(sapply(as.character(data$Temperature),function(x){strsplit(x,split=" ")[[1]][1]})),
  activity=data$Activity,
  lat=as.numeric(as.character(data$Lat)),
  long=as.numeric(as.character(data$Long))
)
data.s <- cbind(data.s, date=as.Date(data.s$datetime), hour=as.numeric(format(data.s$date,"%H")))
# compute distances
data.c <- data.s
coordinates(data.c) <- ~ lat + long
countriesSP <- getMap(resolution='low')
proj4string(data.c) <- CRS(proj4string(countriesSP)) #CRS("+proj=longlat +datum=WGS84")
dists <- sapply(seq_along(data.c[-1,]), function(i)
  spDistsN1(pts = data.c[i, ], pt = data.c[i+1, ], longlat = TRUE))
indices = over(data.c, countriesSP)
# add distances and country name to data frame
data.s <- cbind(data.s,dist=dists[1],country=as.factor(as.character(indices$ADMIN)))
data.s$dist[-1] <- dists
data.s <- cbind(data.s,dists.int=as.integer(data.s$dist),fly=data.s$dist>3)
data.s$country <- reorder(data.s$country,1:dim(data.s)[1])
# distance per day and temp
dist.per.day <- aggregate(data.s$dist,by=list(data.s$date),FUN=sum)
temp.per.day <- aggregate(data.s$temp,by=list(data.s$date),FUN=mean)
data.agg <- merge(x=dist.per.day,y=temp.per.day,by.x="Group.1",by.y="Group.1")
colnames(data.agg) <- c("date","dist.sum","temp.avg")
data.agg.coutry <- aggregate(data.s$dist,by=list(data.s$country,data.s$date),FUN=sum)
colnames(data.agg.coutry) <- c("country","date","dist")
# distance per hour
dist.per.hour.sum <- aggregate(data.s$dist,by=list(data.s$hour),FUN=sum)
dist.per.hour.avg <- aggregate(data.s$dist,by=list(data.s$hour),FUN=mean)
data.agg.hour <- merge(x=dist.per.hour.avg,y=dist.per.hour.sum,by.x="Group.1",by.y="Group.1")
colnames(data.agg.hour) <- c("hour","dist.avg","dist.sum")
# 
data.s.fly <- data.s[data.s$fly,]
data.s.fly.agg.time <- aggregate(data.s.fly$hour,by=list(data.s.fly$date),FUN=min)
data.s.fly.agg.country <- aggregate(data.s.fly$country,by=list(data.s.fly$date),FUN=function(c){c[1]})
data.s.fly.agg <- merge(x=data.s.fly.agg.country,y=data.s.fly.agg.time,by=c("Group.1"))
colnames(data.s.fly.agg) <- c("date","country","hour")
# some helpful vars
countries.count <- length(unique(c(data.s$country[!is.na(data.s$country)])))
colors <- terrain.colors(countries.count)

# charting
barchart(data=data.agg, x=dist.sum~date,horizontal=F,scales=list(x=list(rot=45)))
barchart(data=data.agg.hour, x=dist.sum~hour,horizontal=F,scales=list(x=list(rot=45)))
barchart(data=data.agg.hour, x=dist.avg~hour,horizontal=F,scales=list(x=list(rot=45)))

barchart(data=data.s, x=dist~hour|country,horizontal=F,scales=list(x=list(rot=45)),stack=F)

xyplot(data.s$dist~data.s$datetime)

panel.smoother <- function(x, y) {
  #panel.xyplot(x, y) # show points 
  panel.lines(x, y)  # show smoothed line 
}
#attach(data.s)
#detach(data.s)
#paste0(sprintf("%02d",1:15),"haha")


xyplot(dist~datetime|cut(date,4),data=data.s,groups=country, auto.key = list(points=T,space="right"),type="o",scales=list(cex=.8,relation="free"))
barchart(dist~date,data=data.agg.coutry,groups=country, 
         auto.key = list(points=T,space="right"),horizontal=F,
         scales=list(cex=.5, x=list(rot=45)),stack=T,draw.key = TRUE,
         par.settings=list(superpose.polygon=list(col=colors)))

# temparature per day
xyplot(temp~datetime,data=data.s,groups=country, auto.key = list(points=T,space="right"),
       type="l",scales=list(cex=.8),
       par.settings=list(superpose.lines=list(col=colors)))


         #col=colors)


xyplot(dist~datetime,groups=country, auto.key = list(points=T,space="right"),type="o")
xyplot(dist~date|country, scales=list(cex=.8, col="red",relation="free",ylim=150),
       panel=panel.smoother, layout = c(length(unique(c(country[!is.na(country)]))), 1))

cloud(c(1:length(data.s$lat)) ~ long * lat, data = data.s)
xyplot(dist~date|country scales=list(cex=.8, col="red",relation="free",ylim=150),
       panel=panel.smoother, layout = c(length(unique(c(country[!is.na(country)]))), 1),
       xlab="Weight", ylab="Miles per Gallon", 
       main="MGP vs Weight by Horse Power")

xyplot(country ~ dist, data = data.s)
plot(countriesSP)