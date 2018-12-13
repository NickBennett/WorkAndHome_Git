source('D:/WorkAndHome/WorkAndHome_Git/Infographic/R/create_infographic.R')


df1 <- read.csv('D:/WorkAndHome/GPS/Trial2Data/location-2018-09-19-122824.csv',stringsAsFactors = F)
df2 <- read.csv('D:/WorkAndHome/GPS/Trial2Data/staypoint-2018-09-19-122825.csv',stringsAsFactors=F)
dir <- 'D:/WorkAndHome/WorkAndHome_Git/Infographic/data/'

setwd(dir)
ids <- unique(df1$User.ID)
for(id in ids){
  d1 <- subset(df1,User.ID == id)
  d2 <- subset(df2,User.ID == id)
  ###DO SOME DATA CHECKS HERE TO SEE IF INFOGRAPHIC IS WORTHWHILE
  ngps <- dim(d1)[1]
  nq <- length(which(d2$Staypoint.Name != ""))+length(which(d2$Staypoint.Hardcoded.Location != ""))
  if (nq > 10) { 
    graph <- TRUE
  } else {
    graph <- FALSE
  }
  print(paste(id,ngps,nq,graph,sep=' ; '))
  if (graph){
    create_infographic(d1,d2,id,dir)
  }
} 
  

  


df1 <- read.csv('D:/WorkAndHome/GPS/Trial2Data/location-2018-09-19-122824.csv',stringsAsFactors = F)
df2 <- read.csv('D:/WorkAndHome/GPS/Trial2Data/staypoint-2018-09-19-122825.csv',stringsAsFactors=F)
df3 <- df2[df2$Staypoint.Name != '',]
df3$Lat <- NA
df3$Lng <- NA
df3$d1.ID <- NA
for (i in 1:dim(df3)[1]){
  temp <- subset(df1,Staypoint.ID == df3$Staypoint.ID[i])
  if (dim(temp)[1] > 0){
    df3$Lng[i] <- mean(temp$Lng)
    df3$Lat[i] <- mean(temp$Lat)
    df3$d1.ID[i] <- temp$User.ID[1]
  }
} 
  
  
  
#   
#   
# #Make full maps for all users
# for(id in ids){
#   d1 <- subset(df1,User.ID == id)
#   d2 <- subset(df2,User.ID == id)
#   ngps <- dim(d1)[1]
#   nq <- length(which(d2$Staypoint.Name != ""))
#   print(paste(id,ngps,nq,sep=' ; '))
#   rLng <- range(d1$Lng)
#   rLat <- range(d1$Lat)
#   center <- c(mean(rLng),mean(rLat))
#   pcex <- 4
#   ### STILL NEEDS WORK ON ZOOM/BBOX
#   id_bbox <- make_bbox(lat = Lat, lon = Lng, data = d1,f=0.1)
#   zoom <- calc_zoom(id_bbox) - 1
#   
#   err <- 1
#   while (err == 1){
#     mapa <- try(get_map(location=c(lon=center[1],lat=center[2]),zoom=zoom,maptype='terrain',source='google'),silent=T)
#     if (class(mapa)[1] != 'try-error'){
#       err <- 0
#     }
#     Sys.sleep(1)
#   }
#   
#   ggmap(mapa) +
#     geom_point(data = d1, aes(x=Lng,y=Lat,colour = Activity.Type)) +
#     geom_point(size=pcex) + 
#     theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
#           plot.margin = unit(c(0, 0, -1, -1), 'lines'),legend.position=c(0,0.05),legend.title=element_blank(),
#           legend.direction='horizontal',legend.background=element_rect(fill=col1),
#           legend.key=element_rect(fill=col1,colour=col1),legend.justification = 'left') + 
#     xlab('') + 
#     ylab('')
#   ggsave(paste(id,'fullmap.png',sep=''))
# }
# 
# 
# 
