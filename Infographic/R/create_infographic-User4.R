# Link users
rm(list=ls())

library(grid)
library(gridExtra)
library(ggplot2)
library(tmap)
library(trip)
library(png)
library(useful)
library(ggmap)

### Load datasets
setwd("Infographic/")
d1 <- read.csv("data/locations.csv") # Location data
d2 <- read.csv("data/staypoints.csv") # Staypoint data
outdir <- ("")

### Retrieve user ID
ID <- 4

### PDF Script ###
key <- "Get from Google Maps API"
ggmap::register_google(key = key)

create_infographic <-  function(d1,d2,ID,outdir){
  #===============================
  # Aesthetics
  #===============================
  col1 <- "#E2E2E3" #Background colour
  col2 <- "#458B00" #Summary background, Mobile Survey text colour (green)
  col3 <- "#6495ED" #Graph and table title, legend, bar and row colour (keep - blue)
  col4 <- "#c172b7" #Table header background colour (purple/pink) #c691bf
  col5 <- "#A9A8A7" #WORKANDHOME text colour
  col6 <- "#d13206" #Graph header background and dividers (red)
  col7 <- "#CA8B01" #Table header background colour (golden)
  # Configure Theme # CANT GET FONTS TO WORK NICK
  
  #Graph theme
  kobe_theme <- function() {
    theme(
      plot.background = element_rect(fill = col1, colour = col1),
      panel.background = element_rect(fill = col1),
      axis.text = element_text(colour = col3, family = "sans"), #Axes label colours
      plot.title = element_text(colour = col3, face = "bold", size = 18, vjust = 1, family = "sans"),
      axis.title = element_text(colour = col3, face = "bold", size = 13, family = "sans"),
      panel.grid.major.x = element_line(colour = col6), #Dividing strips
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      strip.text = element_text(family = "sans", colour = "white"),
      strip.background = element_rect(fill = col7), #Graph title background colour
      axis.ticks = element_line(colour = col4) #Dividing colour
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
    colhead = list(fg_params=list(col="white"), #Heading text colour
                   bg_params=list(fill=col7,col=col1)) #Heading fill, line colour
  )

  d1_s <- subset(d1, User.ID == ID)
  d2_s <- subset(d2, User.ID == ID)
  #Keep original fix IDs
  d1_s$Loc.ID <- row.names(d1_s)
  #Dates
  d1_s$Date.Time <- as.POSIXct(strptime(d1_s$Recorded.Time,format='%F %T'),tz='GMT')
  start_date <- format(min(d1_s$Date.Time),"%F")
  end_date <- format(max(d1_s$Date.Time),"%F")

  #Tracking Data Summary
  d1_s <- d1_s[order(d1_s$Date.Time),]     #order times
  ind <- duplicated(d1_s$Date.Time)    #remove duplicate times
  dtrack <- d1_s[!ind,]
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
  d2_s$fac.enjy <- factor(d2_s$Staypoint.How.Enjoy.,levels=c(0:6))
  t1 <- data.frame(table(d2_s$fac.enjy))
  d2_s$fac.happ <- factor(d2_s$Staypoint.How.Happy.,levels=c(0:6))
  t2 <- data.frame(table(d2_s$fac.happ))
  d2_s$fac.strs <- factor(d2_s$Staypoint.How.Stressed.,levels=c(0:6))
  t3 <- data.frame(table(d2_s$fac.strs))
  grp <- rep(c('Enjoyment','Happiness','Stress'),each=7)
  x <- rep(0:6,3)
  dat <- data.frame(grp,x,y=c(t1$Freq,t2$Freq,t3$Freq))
  p2 <- ggplot(data = dat, aes(x = x, y = y)) + geom_bar(stat = "identity", fill = col3) +
    ylab("Frequency") + xlab("Feeling Score") + facet_grid(. ~ grp) +
    ggtitle("How did you feel?")

  #===============================
  #P3 Question "TOP" Summaries
  #===============================

  #Generate staypoint list
  stay <- d2_s[c("Staypoint.Name")]
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
  stay.tab
  stay.tab2 <- head(stay.tab,5)
  stay.tab2
  stay.tab2[6,] <- c('Other Places',stay.sum - sum(stay.tab2$Freq)+length(indNoLoc))
  stay.tab3 <- stay.tab2
  stay.tab3$Location <- strtrim(stay.tab3$Location,24)
  g3a <- tableGrob(stay.tab3,rows=NULL,vp = vplayout(4, 1),theme=tab_theme)

  #Generate activity list
  act <- d2_s[c("Staypoint.Activity")]
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

  #Generate 'with' list
  with <- d2_s$Staypoint.Activity   #Change to person with
  with <- with[with!=""]
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
  Sum_Stat #Date of start/finish, number questions answers, total distance travelled (km), number of gps points
  
  
  #==============================
  #P1 Maps
  #==============================

  d1_s$Travel.Type <- factor(d1_s$Activity.Type)
  rLng <- range(d1_s$Lng)
  rLng
  rLat <- range(d1_s$Lat)
  rLat
  center <- c(mean(rLng),mean(rLat))
  center

  #Plot All Data
  pcex <- 4

  id_bbox <- make_bbox(lat = Lat, lon = Lng, data = d1_s)
  zoom <- calc_zoom(id_bbox)
  max(d1_s$Lng) - min(d1_s$Lng)
  if ((max(d1_s$Lng) - min(d1_s$Lng) > 19)){
    zoom <- 5
  }
  if (zoom > 5 & (max(d1_s$Lat) - min(d1_s$Lat) > 7)){
    zoom <-6
  }
  if (max(d1_s$Lng) - min(d1_s$Lng) > 0.1 & max(d1_s$Lat) - min(d1_s$Lat) < 0.03){
    zoom <- 14
  }

  mapa <- get_map(location=id_bbox,zoom=zoom-2,maptype='hybrid',source='google') 
  
  m1a <- ggmap(mapa) + 
    geom_point(data = d1_s, aes(x=Lng,y=Lat,colour = Activity.Type), size=0.4) +
    theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
          plot.margin = unit(c(0, 0, -1, -1), 'lines'),legend.position=c(0,0),legend.title=element_blank(),
          legend.direction='horizontal',legend.background=element_rect(fill=col1),
          legend.key=element_rect(fill=col1,colour=col1),legend.justification = 'left') +
    xlab('') +
    ylab('')
  # plot(m1a)
  
  #Get Top Staypoint ID - often home
  txtHome <- stay.tab2$Location[1]
  iH <- which(d2_s$Staypoint.Name == txtHome | d2_s$Staypoint.Name == txtHome)
  locH <- d1_s[which(d1_s$Staypoint.ID %in% d2_s$Staypoint.ID[iH]),]
  
  #Calculate the centroid
  rLngH <- range(locH$Lng)
  rLngH
  rLatH <- range(locH$Lat)
  rLatH
  centerH <- c(mean(rLngH),mean(rLatH))
  centerH
  
  #Calculate the boundary box and zoom
  max(locH$Lat) - min(locH$Lat)
  
  #Calculate popular spot
  locH_geo <- locH[c("Lat","Lng")]
  locH_geo
  locH_geo <- round(locH_geo,3)
  res = do.call(paste,c(locH_geo))
  locH_most_common <- locH_geo[which.max(ave(seq(res),res,FUN=length)),]
  locH_most_common[[1]]
  locH_most_common[[2]]
  top_staypoint_bbox <- make_bbox(lat = Lat, lon = Lng, data=locH_geo)
  top_staypoint_zoom <- calc_zoom(top_staypoint_bbox)
  
  if (top_staypoint_zoom == "Inf"){
    top_staypoint_zoom <- 18
  }
  if (top_staypoint_zoom > 19){
    top_staypoint_zoom <- 19
  }
  top_staypoint_zoom
  
  if (max(locH_geo$Lat) - min(locH_geo$Lat) > 0.3){
    top_staypoint_zoom <- 11
  }
  
  if (txtHome == "HOME"){
    top_staypoint_zoom <- 17
  } 
  if (txtHome == "WORK"){
    mapb <- get_map(location=top_staypoint_bbox,maptype='hybrid',source ='google',zoom=top_staypoint_zoom-1)
  } else {
    mapb <- get_map(location=c(lon=locH_most_common[[2]],lat=locH_most_common[[1]]),maptype='hybrid',source ='google',zoom=top_staypoint_zoom-1)
  }

  nrow(locH_most_common)
  
  #Plot top staypoint
  m1b <- ggmap(mapb) + geom_point(data = d1_s, aes(x=Lng,y=Lat,colour = Activity.Type),show.legend=FALSE,size=0.5) +
    theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
          plot.margin = unit(c(0, 0, -1, -1), 'lines')) + xlab('') + ylab('')
  # plot(m1b)

  #Get Second Top Staypoint ID - often work
  txtAct <- stay.tab2$Location[2]
  if (is.na(stay.tab2$Location[2])) {
    txtAct <- stay.tab2$Location[1]
  }
  stay.tab2
  txtAct
  iA <- which(d2_s$Staypoint.Name == txtAct | d2_s$Staypoint.Name == txtAct)
  iA
  locA <- d1_s[which(d1_s$Staypoint.ID %in% d2_s$Staypoint.ID[iA]),]
  locA
  
  #Calculate the centroid
  rLngA <- range(locA$Lng)
  rLngA
  rLatA <- range(locA$Lat)
  rLatA
  centerA <- c(mean(rLngA),mean(rLatA))
  centerA
  
  #Calculate the boundary box and zoom
  second_staypoint_bbox <- make_bbox(lat = Lat, lon = Lng, data=locA)
  second_staypoint_bbox
  second_staypoint_zoom <- calc_zoom(second_staypoint_bbox)
  second_staypoint_zoom
  

  #Calculate popular spot
  locA_geo <- locA[c("Lat","Lng")]
  locA_geo
  locA_geo <- round(locA_geo,3)
  res = do.call(paste,c(locA_geo))
  locA_most_common <- locA_geo[which.max(ave(seq(res),res,FUN=length)),]
  locA_most_common[[1]]
  locA_most_common[[2]]
  
  if (second_staypoint_zoom == "Inf"){
    second_staypoint_zoom <- 18
  }
  if (second_staypoint_zoom >19){
    second_staypoint_zoom <- 19
  }
  
  if (txtAct == "HOME"){
    second_staypoint_zoom <- 17
  }
  if (txtAct == "WORK"){
    mapc <- get_map(location=second_staypoint_bbox,maptype='hybrid',source='google',zoom=second_staypoint_zoom-2)
  } else {
    mapc <- get_map(location=c(lon=locA_most_common[[2]],lat=locA_most_common[[1]]),maptype='hybrid',source='google',zoom=second_staypoint_zoom-1)
  }

  m1c <- ggmap(mapc) + geom_point(data = d1_s, aes(x=Lng,y=Lat,colour = Activity.Type),show.legend=FALSE,size=0.5) +
    theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
          plot.margin = unit(c(0, 0, -1, -1), 'lines')) + xlab('') + ylab('')
  # plot(m1c)

  #=====================================================================================================
  # Generate Infographic in PDF format
  #=====================================================================================================
  outfile <- paste(outdir,"WorkAndHome_Summary_Participant_",ID,".pdf",sep='')
  pdf(outfile, width = 8.27, height = 11.69)
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(4, 3)))

  #Background and Text
  #------------------------
  #Draw background
  grid.rect(gp = gpar(fill = col1, col = col1))
  #Draw title
  grid.text("WORKANDHOME", y = unit(0.98, "npc"), x = unit(0.5, "npc"),
            vjust = 1, hjust = .5, gp = gpar(fontfamily = "sans", col = col3, cex = 6, alpha = 1))
  #Draw colourful box
  grid.rect(gp = gpar(fill = col2, col = col2,alpha=0.8), x = unit(0.5, "npc"), y = unit(0.82, "npc"),
            width = unit(1, "npc"), height = unit(0.11, "npc"))
  #Draw subtitle overlay
  grid.text("Mobile Survey", y = unit(0.855, "npc"), x = unit(0.65, "npc"), gp = gpar(fontfamily = "sans", 
                                                                                     col = col1, cex = 2,alpha=1))
  #Draw Summary text overlay
  grid.text("Summary", y = unit(0.82, "npc"), x = unit(0.65, "npc"), vjust = .5, hjust = .5,
            gp = gpar(fontfamily = "sans", col = col1, cex = 6, alpha = 1))
  #Draw participant information overlay
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
  #Draw map titles
  grid.text('All Locations', vjust = 0, hjust = 0, x = unit(0.01, "npc"), y = unit(0.75, "npc"),
            gp = gpar(fontfamily = "sans", col = col3, cex = 0.8))
  grid.text(paste('Top Place: ', txtHome), vjust = 0, hjust = 0, x = unit(0.34, "npc"), y = unit(0.75, "npc"),
            gp = gpar(fontfamily = "sans", col = col3, cex = 0.8))
  grid.text(paste('2nd Place: ', txtAct), vjust = 0, hjust = 0, x = unit(0.67, "npc"), y = unit(0.75, "npc"),
            gp = gpar(fontfamily = "sans", col = col3, cex = 0.8))
  grid.text("Your Top Things!", vjust = 0, hjust = 0.5, x = unit(0.5, "npc"), y = unit(0.22, "npc"),
            gp = gpar(fontfamily = "sans", col = col3, cex = 1.8))

  #Draw logos
  erc <- readPNG("data/ERC.png.")
  grid.raster(erc,x=unit(0.05,'npc'),y=unit(0.02,'npc'),height=unit(0.04,"npc"),width=unit(0.1,"npc"))

  sta <- readPNG("data/UStA.png.")
  grid.raster(sta,x=unit(0.19,'npc'),y=unit(0.02,'npc'),height=unit(0.03,"npc"),width=unit(0.15,"npc"))

  sou <- readPNG("data/USouth2.png.")
  grid.raster(sou,x=unit(0.38,'npc'),y=unit(0.02,'npc'),height=unit(0.03,"npc"),width=unit(0.19,"npc"))

  wah <- readPNG("data/wah.png.")
  #grid.raster(wah,x=unit(0.89,'npc'),y=unit(0.82,'npc'),height=unit(0.1,"npc"),width=unit(0.17,"npc"))
  grid.raster(wah,x=unit(0.53,'npc'),y=unit(0.02,'npc'),height=unit(0.034,"npc"),width=unit(0.056,"npc"))
  #------------------

  #graphics here
  #-----------------------
  print(m1b,vp=vplayout(2,2))
  print(m1c,vp=vplayout(2,3))
  print(m1a,vp=vplayout(2,1)) #plot with legend last so legend shows
  print(p2+kobe_theme(),vp=vplayout(3,1:3))


  grid.draw(g3a)
  grid.draw(g3b)
  grid.draw(g3c)
  #------------------------

  #WebURL
  grid.text('Web: www.workandhome.ac.uk', vjust = 0.9, hjust = 0, x = unit(0.75, "npc"), y = unit(0.01, "npc"),
            gp = gpar(fontfamily = "sans", col = col3, cex = 0.7))

  dev.off()
  #=========================================================================
}


xx <- create_infographic(d1,d2,ID,outdir)
print(xx)
for (ID in new_file$User.ID){
  print(ID)
  create_infographic(d1,d2,ID,outdir)
}
