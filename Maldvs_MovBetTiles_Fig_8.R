

#continue from final object. (see Maldvs_MovBetTiles_Figs_6_and_7.R)
#or read it, if written
final<-read.csv("/home/memo/Documents/perso/DataPopAlliance/maldives/dir/geoinsights/BetwTiles_rdb_official_polygons.csv")

#internalMoblity (im), mobility intra Atolls, not between diff Atolls
im<-subset(final, origen1==destination1)


im$date <- as.factor(sapply(as.character(im$date_time), function(x) {strsplit(x, "\\ ")[[1]][1]}))
im$time <- as.factor(sapply(as.character(im$date_time), function(x) {strsplit(x, "\\ ")[[1]][2]}))

#both origen1 and destination1 are the same
imMedian<-aggregate(percent_change~origen1, FUN=median, data=im, na.rm=TRUE)

#cloropleth
#download shapefile from 
#https://drive.google.com/drive/u/0/folders/1FUpQE3G4V193xM0_z3u_6OFOSEiyCx7y
library("rgdal")
library(data.table)
library(plyr)
library(ggthemes)
library(data.table)
library(ggplot2)
library(viridis)

#open shapefile and prepare it to join with the previous "imMedian" object
#this preparation means to link the shapefile rowname ID to a dataframe (got from the same shapefile)
#to get the dataframe version of shapefile is used fortify 
#once this is done, a join is done using the corresponding NAME1 column
setwd("/home/memo/Documents/perso/DataPopAlliance/maldives/dir/shapefiles/")
shapefile <- readOGR(dsn="mdv_devinfo_admin2b", layer="MDV_DevInfo_Admin2B")
shapefile<-shapefile[,c(1,2)]
shapefile@data$id = rownames(shapefile@data)
shapefile.points = fortify(shapefile, region = "id")
shapefile.df = join(shapefile.points, shapefile@data, by = "id")
shapefile.df = subset(shapefile.df, select = c(long, lat, group, NAME1_))
names(shapefile.df) = c("long", "lat", "group", "NAME1")
names(imMedian)[1]<-"NAME1"
full.data = join(imMedian, shapefile.df, by = "NAME1", type = "full")


#print the map using png or svg
#svg is used if some final format need to add using photoshop or inkscape
#for this map you use the coordinates in x and y and the percent change column
#viridis is used to get the color palette
svg("choropleth_mobility_02c.svg", width = 8, height = 8)
ggplot(full.data, aes(x = long, y = lat, group = group,
       fill = percent_change)) +
    geom_polygon(color = "grey75", size = 0.2) +
    coord_equal() +
    #scale_fill_viridis() +
    #geom_path(colour="black", lwd=0.05) +
    #facet_wrap(~ variable ) +
    #theme_map() + facet_wrap(~ variable)+
    #scale_fill_distiller() +
    #labs(title = "2 halft(CARPETAS DE INVESTIGACIÓN 2020)",
    #     fill = NULL) +
    #theme_void() +
    theme(legend.position = "bottom",
          panel.background = element_rect(fill = NA, colour = "#cccccc"))+ 
    scale_fill_viridis(option = "cividis", direction = -1)
dev.off()
