
#Movement Between Tiles Facebook Data
#Description of columns:

#Description:What specific pairs of map tiles are people moving between more or less often than we would expect based on pre-crisis levels? This dataset contains information about the number of people moving between tile pairs over a given time period. We measure this during baseline (movement between tile pairs averaged across the three weeks prior to the disaster) as well, so we can understand how many more or fewer people are moving during the disaster period compared to usual. This helps us distinguish disaster related movements from people’s normal migration patterns. The following metrics are available: * Date Time - The time period represented by the current map layer. * Starting Location - The region or tile where the movement of the group started. * Ending Location: The region or tile where the movement of the group ended. * Length (km) - The distance traveled in kilometers. * Baseline: People Moving - The total number of people who moved from Starting Location to Ending Location on average during the weeks before the disaster began. * Crisis: People Moving - The total number of people who moved from Starting Location to Ending Location during the time period specified * Difference - The difference between the number of people moving from Starting Location to Ending Location during the disaster compared to before the disaster. * Percent Change - The percentage difference between the number of people moving from Starting Location to Ending Location during the disaster compared to before the disaster. * Standard (Z) Score: The number of standard deviations by which the count of people moving during the crisis differs from the number of people moving during the baseline. Any z-value greater than 4 or smaller than -4 is clipped at 4 or -4. For more information go to the Help Page https://fburl.com/disastermaps_help



options(scipen=999)  # turn-off scientific notation like 1e+48

batch_read <- function(path, pattern, recursive = FALSE, read_fun, ...) {
  data.files <- list.files(path, pattern = pattern, recursive = recursive)
  data <- lapply(paste0(path, data.files), read_fun, ...)
  data <- do.call("rbind", data)
  data
}

#
rdb <- batch_read(
  path = "/home/memo/Documents/perso/DataPopAlliance/maldives/dir/geoinsights/BetwTiles/"
  , pattern = "\\.csv"
  , read_fun = read.csv
  , header = TRUE
)


db<-rdb[,c(1,2,4,6,7,11:14,16:20)]


db$origen <- as.factor(sapply(as.character(db$geometry), function(x) {strsplit(x, "\\,")[[1]][1]}))
db$destino <- as.factor(sapply(as.character(db$geometry), function(x) {strsplit(x, "\\,")[[1]][2]}))
db$geometry<-NULL

db$origen <- as.character(db$origen)
db$destino <- as.character(db$destino)

geom<-db[,c(14,15)]

#write.csv(geom, "/home/memo/Documents/perso/DataPopAlliance/maldives/dir/geoinsights/BetwTiles_rdb.csv", row.names=FALSE)

geom<-read.csv("/home/memo/Documents/perso/DataPopAlliance/maldives/dir/geoinsights/BetwTiles_rdb2_curated.csv")

#remove dispensable coordinates
db<-db[,c(1:9)]

#join with recent extracted and curated coordinates
dbb<-cbind(db,geom)

#add identifier
dbb$ide<-seq.int(nrow(dbb))

#write.csv(dbb, "/home/memo/Documents/perso/DataPopAlliance/maldives/dir/geoinsights/BetwTiles_rdb_geometries.csv", row.names=FALSE)


dbb<-read.csv("/home/memo/Documents/perso/DataPopAlliance/maldives/dir/geoinsights/BetwTiles_rdb_geometries.csv")

### Assign coordinates into Atolls
### using shapefiles downloaded from 
### https://data.humdata.org/dataset/maldives-administrative-boundaries-polygon-polyline
### Download the one named "mdv_devinfo_admin2b"

#install.libraries("rgdal")
library(rgdal)
setwd("/home/memo/Documents/perso/DataPopAlliance/maldives/dir/shapefiles/")
geo <- readOGR(dsn="mdv_devinfo_admin2b", layer="MDV_DevInfo_Admin2B")
geo <- geo[,c(1,2,3,9)]
plot(geo)

library(ggplot2)
geodf<-fortify(geo, region="ID_")
head(geodf)
geo$id <- row.names(geo) # allocate an id variable to the sp data

###Spatial Allocatioin 
# Use first lng
coords<-dbb[,c(10,11)]

# set as spatial object
sp <- SpatialPoints(coords)
rm(coords)

# Use the following Coordinate Reference System (CRS)

proj4string(sp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj4string(geo) <-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#assigning points into polygons
assign <- over(sp, geo) 
dim(assign)

#use rownames just after over function to use the in the following merge
assign$rous <- rownames(assign)
dbb$rous<-rownames(dbb)

assign$rous<-as.factor(as.character(assign$rous))
dbb$rous<-as.factor(as.character(dbb$rous))
names(assign)[1]<-"ID"

fdf <- merge(x=assign, y=dbb, by.x="rous", by.y="rous")

#fdf<- fdf[!is.na(fdf$ID),]
#NAs can be removed, prefer not, if necessary these will be named as "Indian Ocean", 
#which is the original name in the raw data

#renamed these columns 
names(fdf)[3]<-"origen1"
names(fdf)[4]<-"origen3"
names(fdf)[5]<-"origen0"



## Second Assign coordinates into Atolls
## Second Assign coordinates into Atolls
#  for the destination coordinates

setwd("/home/memo/Documents/perso/DataPopAlliance/maldives/dir/shapefiles/")
geo <- readOGR(dsn="mdv_devinfo_admin2b", layer="MDV_DevInfo_Admin2B")
geo <- geo[,c(1,2,3,9)]
plot(geo)

library(ggplot2)
geodf<-fortify(geo, region="ID_")
head(geodf)
geo$id <- row.names(geo) # allocate an id variable to the sp data

### second Spatial Allocatioin 
# First lng
coords<-dbb[,c(12,13)]

# set as spatial object
sp <- SpatialPoints(coords)
rm(coords)

# CRS
proj4string(sp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj4string(geo) <-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

assign <- over(sp, geo) 
dim(assign)

# get rownames just after over function
assign$rous <- rownames(assign)
dbb<-dbb[,c(3,12:14)]
dbb$rous<-rownames(dbb)

assign$rous<-as.factor(as.character(assign$rous))
dbb$rous<-as.factor(as.character(dbb$rous))
names(assign)[1]<-"ID"

fdf2 <- merge(x=assign, y=dbb, by.x="rous", by.y="rous")

#fdf2<- fdf2[!is.na(fdf2$ID),]
#NAs can be removed, prefer not, if necessary these will be named as "Indian Ocean", 
#which is the original name in the raw data

names(fdf2)[3]<-"destination1"
names(fdf2)[4]<-"destination3"
names(fdf2)[5]<-"destination0"


fin <- merge(x=fdf, y=fdf2, by.x="ide", by.y="ide")

final<-fin[,c(1,4,5,6,8:20,23,24,25)]

#write.csv(final, "/home/memo/Documents/perso/DataPopAlliance/maldives/dir/geoinsights/BetwTiles_rdb_official_polygons.csv", row.names=FALSE)



final<-read.csv("/home/memo/Documents/perso/DataPopAlliance/maldives/dir/geoinsights/BetwTiles_rdb_official_polygons.csv")

#from final object, subset only intra Atolls movement
#not between diff Atolls
#internalMobility(db0)
im<-subset(final, origen1==destination1)

library(lubridate)
im$date <- as.factor(sapply(as.character(im$date_time), function(x) {strsplit(x, "\\ ")[[1]][1]}))
im$time <- as.factor(sapply(as.character(im$date_time), function(x) {strsplit(x, "\\ ")[[1]][2]}))




median<-aggregate(percent_change~date, FUN=median, data=im, na.rm=TRUE)
median$date<-as.Date(median$date, format="%Y-%m-%d")

library(ggplot2)
png("/home/memo/Documents/perso/DataPopAlliance/maldives/dir/covid/interMobility_perc_change_median.png", width = 29, height = 8, units = 'in', res = 300)
ggplot(median, aes(x=date, y=percent_change, color=percent_change)) +
  geom_point(size=4) +
  theme(text = element_text(size=30))+
  xlab("Date") + ylab("Mobility Percent Change")+
  geom_smooth(span = 0.3)+
  scale_x_date(breaks = as.Date(c("2020-04-15", "2020-06-08","2020-07-23","2020-09-27","2020-12-07")),
               minor_breaks = as.Date(c("2020-04-15", "2020-07-23")))
  #theme_minimal()
dev.off()


#since we are interested in plotting for all the atolls, 
#we use origen1, but it could also be destination1 column


atollsMedian<-aggregate(percent_change~date+origen1, FUN=median, data=im, na.rm=TRUE)
atollsMedian$date<-as.Date(atollsMedian$date, format="%Y-%m-%d")

library(dplyr)

png("/home/memo/Documents/perso/DataPopAlliance/maldives/dir/covid/interMobility_perc_change_median_perAtoll.png", width = 9, height = 6, units = 'in', res = 300)
atollsMedian %>%
  ggplot(aes(x = date, y = percent_change)) + 
  geom_point(alpha = 0.25) +
  facet_wrap(~ origen1, ncol = 4)+
  xlab("Date") +
  ylab("Mobility Percent Change")+
  theme_minimal()+
  geom_smooth(span = 0.3)
dev.off()











