library(grid)
library(gridExtra)
library(ggplot2)
library(tmap)
library(trip)
library(png)
library(useful)
library(ggmap)

# d1 - location data
# d2 - Staypoint data
# ID - User ID number
# outdir - where to save it

create_infographic <-  function(d1,d2,ID,outdir){
  col1 <- "#E2E2E3"
  col2 <- "#458B00"
  col3 <- "#6495ED"
  col4 <- "#CA8B01"
  col5 <- "#A9A8A7"
  # Configure Theme # CANT GET FONTS TO WORK NICK
  my_theme <- function() {
    theme(
      plot.background = element_rect(fill = col1, colour = col1),
      panel.background = element_rect(fill = col1),
      axis.text = element_text(colour = col2, family = "sans"),
      plot.title = element_text(colour = col3, face = "bold", size = 18, vjust = 1, family = "sans"),
      axis.title = element_text(colour = col3, face = "bold", size = 13, family = "sans"),
      panel.grid.major.x = element_line(colour = col2),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      strip.text = element_text(family = "sans", colour = "white"),
      strip.background = element_rect(fill = col2),
      axis.ticks = element_line(colour = col2)
    )
  }
  #Table Theme
  tab_theme <- ttheme_default(
    # Text
    base_family="sans",
    # Alternate the row fill colour
    core = list(fg_params=list(col=col1),
                bg_params=list(fill=c(col3, col2),col=col1)),
    # Change column header
    colhead = list(fg_params=list(col="white"),
                   bg_params=list(fill=col4,col=col1))
  )
  
  d1 <- subset(d1, User.ID == ID)
  d2 <- subset(d2, User.ID == ID)
  #Keep original fix IDs
  d1$Loc.ID <- row.names(d1)
  #Dates
  d1$Date.Time <- as.POSIXct(strptime(d1$Recorded.Time,format='%F %T'),tz='GMT')
  start_date <- format(min(d1$Date.Time),"%F")
  end_date <- format(max(d1$Date.Time),"%F")
  
  #Tracking Data Summary
  d1 <- d1[order(d1$Date.Time),]     #order times
  ind <- duplicated(d1$Date.Time)    #remove duplicate times
  dtrack <- d1[!ind,]
  nPts <- dim(dtrack)[1]
  
  
  spP <- dtrack
  coordinates(spP) <- ~Lng+Lat
  spP@proj4string <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  tr <- trip(spP, c("Date.Time", "User.ID")) 
  spL <- as(tr,'SpatialLinesDataFrame')
  dist <- round(sum(trackDistance(tr,longlat=TRUE)))    #in Kilometers
  
  #===============================
  #P2 Histograms of Feeling Scores
  #===============================
  d2$fac.enjy <- factor(d2$Staypoint.How.Enjoy.,levels=c(0:6))
  t1 <- data.frame(table(d2$fac.enjy))
  d2$fac.happ <- factor(d2$Staypoint.How.Happy.,levels=c(0:6))
  t2 <- data.frame(table(d2$fac.happ))
  d2$fac.strs <- factor(d2$Staypoint.How.Stressed.,levels=c(0:6))
  t3 <- data.frame(table(d2$fac.strs))
  grp <- rep(c('Enjoyment','Happiness','Stress'),each=7)
  x <- rep(0:6,3)
  dat <- data.frame(grp,x,y=c(t1$Freq,t2$Freq,t3$Freq))
  p2 <- ggplot(data = dat, aes(x = x, y = y)) + geom_bar(stat = "identity", fill = col3) +
    ylab("Frequency") + xlab("Feeling Score") + facet_grid(. ~ grp) +
    ggtitle("How did you feel?")
  #p2 + my_theme()
  
  #===============================
  #P3 Question "TOP" Summaries
  #===============================
  stay <- c(d2$Staypoint.Name,d2$Staypoint.Hardcoded.Location)
  stay <- stay[stay!= ""]
  n.quest <- length(stay)
  indNoLoc <- which(stay == 'NO_LOCATION')
  stay <- stay[stay != 'NO_LOCATION']
  stay.tab <- as.data.frame(table(stay),stringsAsFactors = FALSE)
  stay.tab <- stay.tab[order(stay.tab$Freq,decreasing=TRUE),]
  names(stay.tab) <- c('Location','Freq')
  if (dim(stay.tab)[1] < 2){
    print("WARNING: LESS THAN 2 UNIQUE STAYPOINTS.")
  }
  stay.sum <- sum(stay.tab$Freq)
  stay.tab2 <- head(stay.tab,5)
  stay.tab2[6,] <- c('Other Places',stay.sum - sum(stay.tab2$Freq)+length(indNoLoc))
  stay.tab3 <- stay.tab2
  stay.tab3$Location <- strtrim(stay.tab3$Location,24)
  g3a <- tableGrob(stay.tab3,rows=NULL,vp = vplayout(4, 1),theme=tab_theme)
  
  
  act <- c(d2$Staypoint.Activity,d2$Staypoint.Second.Activity)
  act <- act[act!=""] 
  act <- act[act!="Nothing else"]                               #remove nothing else
  act.tab <- as.data.frame(table(act),stringsAsFactors = FALSE)
  act.tab <- act.tab[order(act.tab$Freq,decreasing=TRUE),]
  names(act.tab) <- c('Activity','Freq')
  act.sum <- sum(act.tab$Freq)
  act.tab2 <- head(act.tab,5)
  act.tab2[6,] <- c('Other Activities',act.sum - sum(act.tab2$Freq))
  act.tab2$Activity <- strtrim(act.tab2$Activity,24)
  g3b <- tableGrob(act.tab2,rows=NULL,vp = vplayout(4, 2),theme=tab_theme)
  
  
  with <- d2$Staypoint.People   #Change to person with
  with <- with[with!=""]
  with <- unlist(sapply(strsplit(with, ", "), unlist))  #concatenate multiple answers
  with.tab <- as.data.frame(table(with),stringsAsFactors = FALSE)
  with.tab <- with.tab[order(with.tab$Freq,decreasing=TRUE),]
  names(with.tab) <- c('People','Freq')
  with.sum <- sum(with.tab$Freq)
  with.tab2 <- head(with.tab,5)
  with.tab2[6,] <- c('Other People',with.sum - sum(with.tab2$Freq))
  with.tab2$People <- strtrim(with.tab2$People,24)
  g3c <- tableGrob(with.tab2,rows=NULL,vp = vplayout(4, 3),theme=tab_theme)
  
  
  #Final summary
  Sum_Stat <- paste(start_date,end_date,n.quest,dist,nPts, sep = "\n")
  #==============================
  #P1 Maps
  #==============================

  d1$Travel.Type <- factor(d1$Activity.Type)
  
  rLng <- range(d1$Lng)
  rLat <- range(d1$Lat)
  center <- c(mean(rLng),mean(rLat))
  
  #Plot All Data 
  
  pcex <- 4
  ### STILL NEEDS WORK ON ZOOM/BBOX
  id_bbox <- make_bbox(lat = Lat, lon = Lng, data = d1,f=0.1)
  zoom <- calc_zoom(id_bbox)
  
  err <- 1
  while (err == 1){
    mapa <- try(get_map(location=c(lon=center[1],lat=center[2]),zoom=zoom,maptype='hybrid',source='google'),silent=T)
    if (class(mapa)[1] != 'try-error'){
      err <- 0
    }
    Sys.sleep(1)
  }
  #mapa <- get_map(location=c(lon=center[1],lat=center[2]),zoom=zoom,maptype='terrain',source='google')
  
  m1a <- ggmap(mapa) +
    geom_point(data = d1, aes(x=Lng,y=Lat,colour = Activity.Type)) +
    geom_point(size=pcex) + 
    theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
          plot.margin = unit(c(0, 0, -1, -1), 'lines'),legend.position=c(0,0.05),legend.title=element_blank(),
          legend.direction='horizontal',legend.background=element_rect(fill=col1),
          legend.key=element_rect(fill=col1,colour=col1),legend.justification = 'left') + 
    xlab('') + 
    ylab('')
  
  #Get Top Staypoint ID - often home
  txtHome <- stay.tab2$Location[1]
  iH <- which(d2$Staypoint.Name == txtHome | d2$Staypoint.Hardcoded.Location == txtHome) 
  locH <- d1[which(d1$Staypoint.ID %in% d2$Staypoint.ID[iH]),]
  err <- 1
  while (err == 1){
    mapb <- try(get_map(location=c(lon=mean(locH$Lng),lat=mean(locH$Lat)),maptype='hybrid',source ='google',zoom=15),silent=T)
    if (class(mapb)[1] != 'try-error'){
      err <- 0
    }
    Sys.sleep(1)
  }
  
  m1b <- ggmap(mapb) + geom_point(data = d1, aes(x=Lng,y=Lat,colour = Activity.Type),show.legend=FALSE) + 
    geom_point(size=pcex) + 
    theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
          plot.margin = unit(c(0, 0, -1, -1), 'lines')) + xlab('') + ylab('')
  
  #Get Second Top Staypoint ID - often work
  txtAct <- stay.tab2$Location[2] 
  iA <- which(d2$Staypoint.Name == txtAct | d2$Staypoint.Hardcoded.Location == txtAct) 
  locA <- d1[which(d1$Staypoint.ID %in% d2$Staypoint.ID[iA]),]
  err <- 1
  while (err == 1){
    mapc <- try(get_map(location=c(lon=mean(locA$Lng),lat=mean(locA$Lat)),maptype='hybrid',source='google',zoom=15),silent=T)
    if (class(mapc)[1] != 'try-error'){
      err <- 0
    }
    Sys.sleep(1)
  }
  
  m1c <- ggmap(mapc) + geom_point(data = d1, aes(x=Lng,y=Lat,colour = Activity.Type),show.legend=FALSE) + 
    geom_point(size=pcex) + 
    theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
          plot.margin = unit(c(0, 0, -1, -1), 'lines')) + xlab('') + ylab('')
  
  
  #=====================================================================================================
  # Generate Infographic in PDF format
  #=====================================================================================================
  outfile <- paste(outdir,"WorkAndHome_Summary_Participant_",ID,".pdf",sep='')
  pdf(outfile, width = 8.27, height = 11.69)
  grid.newpage() 
  pushViewport(viewport(layout = grid.layout(4, 3)))
  
  #Background and Text
  #------------------------
  grid.rect(gp = gpar(fill = col1, col = col1))
  grid.text("WORKANDHOME", y = unit(0.99, "npc"), x = unit(0.5, "npc"), 
            vjust = 1, hjust = .5, gp = gpar(fontfamily = "sans", col = col5, cex = 6, alpha = 0.3))
  grid.text("Mobile Survey", y = unit(0.91, "npc"), gp = gpar(fontfamily = "sans", col = col2, cex = 3))
  grid.rect(gp = gpar(fill = col2, col = col2), x = unit(0.5, "npc"), y = unit(0.82, "npc"), 
            width = unit(1, "npc"), height = unit(0.11, "npc"))
  grid.text("SUMMARY", y = unit(0.82, "npc"), x = unit(0.5, "npc"), vjust = .5, hjust = .5, 
            gp = gpar(fontfamily = "sans", col = col4, cex = 7, alpha = 0.3))
  grid.text(paste("PARTICIPANT ID:",ID), vjust = 0, hjust = 0, x = unit(0.01, "npc"), y = unit(0.88, "npc"), 
            gp = gpar(fontfamily = "sans", col = col3, cex = 1.2))
  grid.text("DATA INFO:", vjust = 0, hjust = 0, x = unit(0.01, "npc"), y = unit(0.86, "npc"), 
            gp = gpar(fontfamily = "sans", col = "white", cex = 1.2))
  grid.text(paste(
    "Participation Start Date:",
    "Participation End Date:",
    "Questions Answered:",
    "Total Distance (km):",
    "No. Location Points: ",
    sep = "\n"), vjust = 0, hjust = 0, x = unit(0.01, "npc"), y = unit(0.78, "npc"), 
    gp = gpar(fontfamily = "sans", col = col1, cex = 0.8))
  grid.text(Sum_Stat, vjust = 0, hjust = 0, x = unit(0.22, "npc"), y = unit(0.78, "npc"), 
            gp = gpar(fontfamily = "sans", col = col1, cex = 0.8))
  grid.text('All Locations', vjust = 0, hjust = 0, x = unit(0.01, "npc"), y = unit(0.75, "npc"), 
            gp = gpar(fontfamily = "sans", col = col3, cex = 0.8))
  grid.text(txtHome, vjust = 0, hjust = 0, x = unit(0.34, "npc"), y = unit(0.75, "npc"), 
            gp = gpar(fontfamily = "sans", col = col3, cex = 0.8))
  grid.text(txtAct, vjust = 0, hjust = 0, x = unit(0.67, "npc"), y = unit(0.75, "npc"), 
            gp = gpar(fontfamily = "sans", col = col3, cex = 0.8))
  grid.text("Your Top Things!", vjust = 0, hjust = 0.5, x = unit(0.5, "npc"), y = unit(0.22, "npc"), 
            gp = gpar(fontfamily = "sans", col = col3, cex = 1.8))
  
  erc <- readPNG("D:/WorkAndHome/WorkAndHome_Git/Infographic/data/ERC.png.")
  grid.raster(erc,x=unit(0.05,'npc'),y=unit(0.02,'npc'),height=unit(0.04,"npc"),width=unit(0.1,"npc"))
  
  sta <- readPNG("D:/WorkAndHome/WorkAndHome_Git/Infographic/data/UStA.png.")
  grid.raster(sta,x=unit(0.19,'npc'),y=unit(0.02,'npc'),height=unit(0.03,"npc"),width=unit(0.15,"npc"))
  
  sou <- readPNG("D:/WorkAndHome/WorkAndHome_Git/Infographic/data/USouth2.png.")
  grid.raster(sou,x=unit(0.38,'npc'),y=unit(0.02,'npc'),height=unit(0.03,"npc"),width=unit(0.19,"npc"))
  
  wah <- readPNG("D:/WorkAndHome/WorkAndHome_Git/Infographic/data/wah.png.")
  #grid.raster(wah,x=unit(0.89,'npc'),y=unit(0.82,'npc'),height=unit(0.1,"npc"),width=unit(0.17,"npc"))
  grid.raster(wah,x=unit(0.53,'npc'),y=unit(0.02,'npc'),height=unit(0.034,"npc"),width=unit(0.056,"npc"))
  #------------------
  
  #graphics here
  #-----------------------
  print(m1b,vp=vplayout(2,2))
  print(m1c,vp=vplayout(2,3))
  print(m1a,vp=vplayout(2,1)) #plot with legend last so legend shows
  print(p2+my_theme(),vp=vplayout(3,1:3))
  
  
  grid.draw(g3a)
  grid.draw(g3b)
  grid.draw(g3c)
  #------------------------
  
  ##LOGOS
  
  #logos <- readPNG("D:/WorkAndHome/WorkAndHome_Git/Infographic/data/logos_all.png.")
  #logosg <- rasterGrob(logos,height=unit(0.35,"npc"),width=unit(0.8,"npc"),interpolate=TRUE,vp=vplayout(1,3),vjust=1.15)
  #grid.draw(logosg)
  
  #WebURL
  grid.text('Web: www.workandhome.ac.uk', vjust = 0.9, hjust = 0, x = unit(0.75, "npc"), y = unit(0.01, "npc"), 
            gp = gpar(fontfamily = "sans", col = col3, cex = 0.7))
  
  dev.off()
  #=========================================================================
}









