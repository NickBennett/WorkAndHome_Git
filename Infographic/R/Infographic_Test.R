library(grid)
library(gridExtra)
library(ggplot2)
#library(cowplot)
library(trip)
library(png)
library(useful)
#library(maps)
#library(mapdata)
library(ggmap)
library(extrafont)   #NOT WORKING
#font_import() # Import all fonts # Takes a while
#loadfonts()

# Configure Theme # CANT GET FONTS TO WORK NICK
kobe_theme <- function() {
  theme(
    plot.background = element_rect(fill = "#E2E2E3", colour = "#E2E2E3"),
    panel.background = element_rect(fill = "#E2E2E3"),
    #panel.background = element_rect(fill = "white"),
    axis.text = element_text(colour = "#E7A922", family = "sans"),
    plot.title = element_text(colour = "#552683", face = "bold", size = 18, vjust = 1, family = "sans"),
    axis.title = element_text(colour = "#552683", face = "bold", size = 13, family = "sans"),
    panel.grid.major.x = element_line(colour = "#E7A922"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    strip.text = element_text(family = "sans", colour = "white"),
    strip.background = element_rect(fill = "#E7A922"),
    axis.ticks = element_line(colour = "#E7A922")
  )
}

#Table Theme
tab_theme <- ttheme_default(
  # Text
  base_family="sans",
  # Alternate the row fill colour
  core = list(fg_params=list(col="#E2E2E3"),
              bg_params=list(fill=c("#552683", "#E7A922"),col="#E2E2E3")),
  # Change column header
  colhead = list(fg_params=list(col="white"),
                 bg_params=list(fill="#CA8B01",col="#E2E2E3"))
)

#Load Data
d1 <- read.csv('D:/WorkAndHome/GPS/jed-2018-07-30/location-2018-07-30-073747.csv',stringsAsFactors = F)
d2 <- read.csv('D:/WorkAndHome/GPS/jed-2018-07-30/staypoint-2018-07-30-073747.csv',stringsAsFactors = F)

#Keep original fix IDs
d1$Loc.ID <- row.names(d1)

#Dates
d1$Date.Time <- as.POSIXct(strptime(d1$Recorded.Time,format='%F %T'),tz='GMT')
start_date <- format(min(d1$Date.Time),"%F")
end_date <- format(max(d1$Date.Time),"%F")

#Tracking Data Summary
d1 <- d1[order(d1$Date.Time),]     #order times
ind <- duplicated(d1$Date.Time)    #remove duplicate times
d1a <- d1[!ind,]
nPts <- dim(d1a)[1]


spP <- d1a
coordinates(spP) <- ~Lng+Lat
spP@proj4string <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
tr <- trip(spP, c("Date.Time", "User.ID")) 
spL <- as(tr,'SpatialLinesDataFrame')
dist <- round(sum(trackDistance(tr,longlat=TRUE)))    #in Kilometers


#Survey Summary  #NEED TO FIX
n.act1 <- length(!is.na(d2$Staypoint.Activity))
n.act2 <- length(!is.na(d2$Staypoint.Activity))
n.pers <- length(!is.na(d2$Staypoint.Activity))
n.enjy <- length(!is.na(d2$Staypoint.How.Anxious.))
n.happ <- length(!is.na(d2$Staypoint.How.Happy.))
n.strs <- length(!is.na(d2$Staypoint.How.Stressed.))
n.loca <- length(!is.na(d2$Staypoint.Name))

n.quest <- median(c(n.act1,n.act2,n.pers,n.enjy,n.happ,n.strs,n.loca))

Sum_Stat <- paste(start_date,end_date,n.quest,dist,npts, sep = "\n")



#==============================
#P1 Maps
#==============================
d1a <- d1a[1884:2508,]   #subset for mapping
d1a$lon <- d1a$Lng
d1a$lat <- d1a$Lat
d1a$Travel.Type <- factor(d1a$Activity.Type)

#Plot All Data 
### STILL NEEDS WORK ON ZOOM/BBOX
id_bbox <- make_bbox(lat = lat, lon = lon, data = d1a,f=0.5)
zoom <- calc_zoom(id_bbox,f=0.2)
mapa <- get_map(location = id_bbox, maptype = "terrain", source = "google",zoom=zoom)  #needs work on size

m1a <- ggmap(mapa) + geom_point(data = d1a, aes(x=lon,y=lat,colour = Activity.Type),show.legend=FALSE) + theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),plot.margin = unit(c(0, 0, -1, -1), 'lines')) + xlab('') + ylab('')



#Get Home data
locH <- subset(d1a,Staypoint.ID == 2091)[1,]   #hard coded
mapb <- get_map(location = c(lon=locH$lon,lat=locH$lat), maptype = "hybrid", source = "google",zoom=14)  #hard coded zoom

m1b <- ggmap(mapb) + geom_point(data = d1a, aes(x=lon,y=lat,colour = Activity.Type),show.legend=FALSE) + theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),plot.margin = unit(c(0, 0, -1, -1), 'lines')) + xlab('') + ylab('')

#Get top activity location
locA <- subset(d1a,Staypoint.ID == 2119)[1,]   #hard coded
mapc <- get_map(location = c(lon=locA$lon,lat=locA$lat), maptype = "hybrid", source = "google",zoom=14)  #hard coded zoom

m1c <- ggmap(mapc) + geom_point(data = d1a, aes(x=lon,y=lat,colour = Activity.Type),show.legend=FALSE)+ theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),plot.margin = unit(c(0, 0, -1, -1), 'lines')) + xlab('') + ylab('')

#===============================
#P2 Histograms of Feeling Scores
#===============================
d2$fac.enjy <- factor(d2$Staypoint.How.Anxious.,levels=c(0:6))
t1 <- data.frame(table(d2$fac.enjy))
d2$fac.happ <- factor(d2$Staypoint.How.Happy.,levels=c(0:6))
t2 <- data.frame(table(d2$fac.happ))
d2$fac.strs <- factor(d2$Staypoint.How.Stressed.,levels=c(0:6))
t3 <- data.frame(table(d2$fac.strs))
grp <- rep(c('Enjoyment','Happiness','Stress'),each=7)
x <- rep(0:6,3)
dat <- data.frame(grp,x,y=c(t1$Freq,t2$Freq,t3$Freq))
p2 <- ggplot(data = dat, aes(x = x, y = y)) + geom_bar(stat = "identity", fill = "#552683") +
  ylab("Frequency") + xlab("Feeling Score") + facet_grid(. ~ grp) +
  ggtitle("How did you feel?")
#p2 + kobe_theme()

#===============================
#P3 Question "TOP" Summaries
#===============================
stay <- d2$Staypoint.Name
stay <- stay[stay!= ""]
stay.tab <- as.data.frame(table(stay),stringsAsFactors = FALSE)
stay.tab <- stay.tab[order(stay.tab$Freq,decreasing=TRUE),]
names(stay.tab) <- c('Location','Freq')
stay.sum <- sum(stay.tab$Freq)
stay.tab2 <- head(stay.tab,5)
stay.tab2[6,] <- c('Other Places',stay.sum - sum(stay.tab2$Freq))
#stay.tab2$Location <- factor(stay.tab2$Location,levels=stay.tab2$Location)
#levels(stay.tab2$Location) <- rev(stay.tab2$Location)
#stay.tab2$Freq <- rev(stay.tab2$Freq)
g3a <- tableGrob(stay.tab2,rows=NULL,vp = vplayout(4, 1),theme=tab_theme)
topStay <- stay.tab2[1,1]

act <- d2$Staypoint.Activity
act <- act[act!=""]
act.tab <- as.data.frame(table(act),stringsAsFactors = FALSE)
act.tab <- act.tab[order(act.tab$Freq,decreasing=TRUE),]
names(act.tab) <- c('Activity','Freq')
act.sum <- sum(act.tab$Freq)
act.tab2 <- head(act.tab,5)
act.tab2[6,] <- c('Other Activities',act.sum - sum(act.tab2$Freq))
#act.tab2$Activity <- factor(act.tab2$Activity,levels=act.tab2$Activity)
#levels(act.tab2$Activity) <- rev(act.tab2$Activity)
#act.tab2$Freq <- rev(act.tab2$Freq)
g3b <- tableGrob(act.tab2,rows=NULL,vp = vplayout(4, 2),theme=tab_theme)


with <- d2$Staypoint.Activity    #Change to person with
with <- with[with!=""]
with.tab <- as.data.frame(table(with),stringsAsFactors = FALSE)
with.tab <- with.tab[order(with.tab$Freq,decreasing=TRUE),]
names(with.tab) <- c('People','Freq')
with.sum <- sum(with.tab$Freq)
with.tab2 <- head(with.tab,5)
with.tab2[6,] <- c('Other People',with.sum - sum(with.tab2$Freq))
#with.tab2$People <- factor(with.tab2$People,levels=with.tab2$People)
#levels(with.tab2$People) <- rev(with.tab2$People)
#with.tab2$Freq <- rev(with.tab2$Freq)
g3c <- tableGrob(with.tab2,rows=NULL,vp = vplayout(4, 3),theme=tab_theme)

#grp <- rep(c('Top Places','Top Activities','Top People'),each=6)
#x <- c(stay.tab2$Location,act.tab2$Activity,with.tab2$People)
#dat <- data.frame(grp,x,y=c(stay.tab2$Freq,act.tab2$Freq,with.tab2$Freq))
#p3 <- ggplot(data = dat, aes(x = "", y = y, fill = factor(x))) +
#  geom_bar(stat = "identity",width=1) + facet_grid(. ~ grp) +
#  ylab("Y LABEL") + xlab("X LABEL") + ggtitle("TITLE OF THE FIGURE")
# 
# p3a <- ggplot(data = stay.tab2, aes(x = Location, y = as.numeric(Freq))) + geom_bar(stat = "identity", fill = "#552683") + coord_flip() + ylab("Frequency") + xlab("") +ggtitle("Top Locations")
# p3a
# p3b <- ggplot(data = act.tab2, aes(x = Activity, y = as.numeric(Freq))) + geom_bar(stat = "identity", fill = "#552683") + coord_flip() + ylab("Frequency") + xlab("")+ ggtitle("Top Activities")
# p3b
# p3c <- ggplot(data = with.tab2, aes(x = People, y = as.numeric(Freq))) + geom_bar(stat = "identity", fill = "#552683") + coord_flip() + ylab("Frequency") + xlab("")+ggtitle("Top People")
# p3c
#=====================================================================================================
# Generate Infographic in PDF format
#=====================================================================================================

pdf('D:/WorkAndHome/WorkAndHome_Git/Infographic/Infographic_test.pdf', width = 8.27, height = 11.69)
grid.newpage() 
pushViewport(viewport(layout = grid.layout(4, 3)))

#Background and Text
#------------------------
grid.rect(gp = gpar(fill = "#E2E2E3", col = "#E2E2E3"))
grid.text("WORKANDHOME", y = unit(0.99, "npc"), x = unit(0.5, "npc"), vjust = 1, hjust = .5, gp = gpar(fontfamily = "sans", col = "#A9A8A7", cex = 6, alpha = 0.3))
grid.text("Mobile Survey", y = unit(0.91, "npc"), gp = gpar(fontfamily = "sans", col = "#E7A922", cex = 3))
#grid.text("DO WE NEED THIS", vjust = 0, y = unit(0.92, "npc"), gp = gpar(fontfamily = "sans", col = "#552683", cex = 0.8))
#grid.text("OR THIS", vjust = 0, y = unit(0.913, "npc"), gp = gpar(fontfamily = "sans", col = "#552683", cex = 0.8))
#grid.text("DELETE THIS?", vjust = 0, y = unit(0.906, "npc"), gp = gpar(fontfamily = "sans", col = "#552683", cex = 0.8))
grid.rect(gp = gpar(fill = "#E7A922", col = "#E7A922"), x = unit(0.5, "npc"), y = unit(0.82, "npc"), width = unit(1, "npc"), height = unit(0.11, "npc"))
grid.text("SUMMARY", y = unit(0.82, "npc"), x = unit(0.5, "npc"), vjust = .5, hjust = .5, gp = gpar(fontfamily = "sans", col = "#CA8B01", cex = 7, alpha = 0.3))
grid.text("PARTICIPANT ID: 01234567", vjust = 0, hjust = 0, x = unit(0.01, "npc"), y = unit(0.88, "npc"), gp = gpar(fontfamily = "sans", col = "#552683", cex = 1.2))
grid.text("DATA INFO:", vjust = 0, hjust = 0, x = unit(0.01, "npc"), y = unit(0.86, "npc"), gp = gpar(fontfamily = "sans", col = "white", cex = 1.2))
grid.text(paste(
  "Participation Start Date:",
  "Participation End Date:",
  "Questions Answered:",
  "Total Distance (km):",
  "No. Location Points: ",
  sep = "\n"), vjust = 0, hjust = 0, x = unit(0.01, "npc"), y = unit(0.78, "npc"), gp = gpar(fontfamily = "sans", col = "#552683", cex = 0.8))
grid.text(Sum_Stat, vjust = 0, hjust = 0, x = unit(0.22, "npc"), y = unit(0.78, "npc"), gp = gpar(fontfamily = "sans", col = "#552683", cex = 0.8))
grid.text('All Locations', vjust = 0, hjust = 0, x = unit(0.01, "npc"), y = unit(0.75, "npc"), gp = gpar(fontfamily = "sans", col = "#552683", cex = 0.8))
grid.text('Home', vjust = 0, hjust = 0, x = unit(0.34, "npc"), y = unit(0.75, "npc"), gp = gpar(fontfamily = "sans", col = "#552683", cex = 0.8))
grid.text(topStay, vjust = 0, hjust = 0, x = unit(0.67, "npc"), y = unit(0.75, "npc"), gp = gpar(fontfamily = "sans", col = "#552683", cex = 0.8))
grid.text("Your Top Things!", vjust = 0, hjust = 0.5, x = unit(0.5, "npc"), y = unit(0.22, "npc"), gp = gpar(fontfamily = "sans", col = "#552683", cex = 1.8))
#------------------

#graphics here
#-----------------------
print(m1a,vp=vplayout(2,1))
print(m1b,vp=vplayout(2,2))
print(m1c,vp=vplayout(2,3))

print(p2+kobe_theme(),vp=vplayout(3,1:3))

grid.draw(g3a)
grid.draw(g3b)
grid.draw(g3c)
#------------------------

##LOGOS
logos <- readPNG("D:/WorkAndHome/WorkAndHome_Git/Infographic/data/logos_all.png.")
logosg <- rasterGrob(logos,height=unit(0.42,"npc"),width=unit(0.9,"npc"),interpolate=TRUE,vp=vplayout(1,3),vjust=0.9)
grid.draw(logosg)

#WebURL
grid.text('More Info: www.workandhome.ac.uk', vjust = 0.9, hjust = 0, x = unit(0.75, "npc"), y = unit(0.01, "npc"), gp = gpar(fontfamily = "sans", col = "#552683", cex = 0.7))

dev.off()
#=========================================================================

# TO DO LIST
# 1. Fix bounding box issue for the 'entire dataset' map
# 2. Add legends/north arrows to maps (make better in general)
# 3. Change colour scheme to match Work And Home Logo


