






#this clusters file is in the DPA drive here:
#https://drive.google.com/drive/u/0/folders/1CSR_Wti1Wj35IiGq70A3OrFO_8iQirxB
clusters<-read.csv("unemployment_and_mobility.csv")
clusters<-clusters[,c(1,6)]

!!!!!!!!!!!!!!! mapa clusters !!!!!!!!!!!!!!! 
!!!!!!!!!!!!!!! mapa clusters !!!!!!!!!!!!!!! 
!!!!!!!!!!!!!!! mapa clusters !!!!!!!!!!!!!!! 
!!!!!!!!!!!!!!! mapa clusters !!!!!!!!!!!!!!! 

library(data.table)
library(plyr)
library(ggthemes)
library(rgdal)
library(dplyr)
library(ggplot2)
library(viridis)

#your shapefile files
setwd("/you_path/")
shapefile <- readOGR(dsn="sub_folder", layer="sub_subfolder")
shapefile<-shapefile[,c(1,2)]


#shapefile = readOGR(dsn = "DIRECTORY WITH SHAPEFILES", layer = "THE ACTUAL SHAPEFILE")
shapefile@data$id = rownames(shapefile@data)
shapefile.points = fortify(shapefile, region = "id")
shapefile.df = join(shapefile.points, shapefile@data, by = "id")
shapefile.df = subset(shapefile.df, select = c(long, lat, group, NAME1_))
names(shapefile.df) = c("long", "lat", "group", "NAME1")
library(data.table)
full.data = join(clusters, shapefile.df, by = "NAME1", type = "full")



#the red-blue colors were changes in inkscape-photoshop
#but bhe blue and dark-blue also shows the clusters, respectively.
svg("fig.13.svg", width = 8, height = 8)
p0 <- ggplot(data = full.data,
             mapping = aes(x = long, y = lat,
                 group = group,
                 fill = cluster))

p1 <- p0 + geom_polygon(color = "gray90", size = 0.05) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) 

p2 <- p1 #+ scale_fill_viridis_c(option = "plasma")

p2 + theme_map() + #facet_wrap(~ year, ncol = 2) +
    theme(legend.position = "bottom",
          strip.background = element_blank()) +
    #labs(fill = "Changement en pourcentage / semaine",
    #     "title = "Opiate Related Deaths by State, 2000-2014")
     labs(fill = "clusters")
dev.off()
