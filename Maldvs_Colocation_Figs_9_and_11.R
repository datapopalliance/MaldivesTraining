
#From the Colocation Maps files, Facebook calculates the probability that two different people from Atolls could meet. 
library(circlize)
library(dplyr)
library(ggplot2)


options(scipen=999)  # turn-off scientific notation like 1e+48

batch_read <- function(path, pattern, recursive = FALSE, read_fun, ...) {
  data.files <- list.files(path, pattern = pattern, recursive = recursive)
  data <- lapply(paste0(path, data.files), read_fun, ...)
  data <- do.call("rbind", data)
  data
}

rdb <- batch_read(
  path = "/home/memo/Documents/perso/DataPopAlliance/maldives/dir/geoinsights/colocation/"
  , pattern = "\\.csv"
  , read_fun = read.csv
  , header = TRUE
)


# Figure 9 script
# Figure 9 barplot 

#select columns of interest
#aggregate to get the mean by polygons
#order from high to low link_value (probability of encounter)
#add a numeric sequential identifier
db<-rdb[,c(2,8,13,15)]
dbb<-aggregate(link_value~polygon1_name+polygon2_name, FUN=mean, data=db, na.rm=TRUE)
dbb<-dbb[order(-dbb$link_value),]
dbb$id<-seq.int(nrow(dbb))

#visually inspect to get the rows only with the highest link_value (probability of encounter)
#after the row 17, probabilities show a dramatic decrease
final<-head(dbb,17)


final$Encounter<-paste(final$polygon1_name, final$polygon2_name, sep="-")
final<-final[,c(4,5,3)]
names(final)[3]<-"Probability"

#final<-final[,c(1,3)]
#names(final)[2]<-"Probability"
#names(final)[1]<-"Encounter"



png("figure_9.png", width = 20, height = 20, units = 'cm', res = 300)
final %>% 
  ggplot(aes(reorder(Encounter, Probability), Probability)) + 
  geom_col(aes(fill = Probability)) + 
  scale_fill_gradient2(low = "dodgerblue4", 
                       high = "dodgerblue4") + 
  coord_flip() + 
  labs(x = "Potential Atoll-Atoll Encounters")
dev.off()












# Figure 11 script
# Figure 11 for chordDiagram

db<-rdb[,c(2,8,13,15)]

dbb<-aggregate(link_value~polygon1_name+polygon2_name, FUN=mean, data=db, na.rm=TRUE)
dbb<-dbb[order(-dbb$link_value),]
dbb$id<-seq.int(nrow(dbb))

#those mirror polygons (aquellos que tienen mismo nombre de atoll en ambas columnas de poligonos)
mirror<-head(dbb,17)

#remove the mirrors
#los que llamo non-mirrors son los que estan justo abajo de los mirror, a partir del 18 row
nonmirror<-dbb[18:247,]
#como no nos interesa la dirección de origen destino 
#y puesto que los hay repetidos, los elimino para quedarme con los únicos
unique<-nonmirror[ c(TRUE,FALSE), ]  # rows
#finalmente los mirror como los únicos
final<-rbind(mirror, unique)

#aqui hago subset de los que tienen probabilidades digamos de importancia "media"
#los que están de rango justo por dejabo de los mirror (que son los que tienen los de mayor probalidad)
#pero si abarcar tantos rows que tengan una probabilidad digamos un orden de magnitud más baja
fini<-subset(final, id>17 & id<46)
fini$id<-NULL
names(fini)<-c("source", "target", "value")



png("figure_11.png", width = 10, height = 10, units = 'in', res = 350)
#par(mar = c(0, 0, 0, 0), mfrow = c(0, 2))
#chordDiagram(c7)
chordDiagram(fini, annotationTrack = "grid", preAllocateTracks = 1)
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1] + .1, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
  circos.axis(h = "top", labels.cex = 0.5, major.tick.percentage = 0.2, sector.index = sector.name, track.index = 2)
}, bg.border = NA)
dev.off()



#esta figura no está en el reporte
#pero aquí están todos los atolls por debajo de los mirror
fini2<-subset(final, id>17)
fini2$id<-NULL
names(fini2)<-c("source", "target", "value")



png("figure_11_B.png", width = 10, height = 10, units = 'in', res = 350)
#par(mar = c(0, 0, 0, 0), mfrow = c(0, 2))
#chordDiagram(c7)
chordDiagram(fini2, annotationTrack = "grid", preAllocateTracks = 1)
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1] + .1, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
  circos.axis(h = "top", labels.cex = 0.5, major.tick.percentage = 0.2, sector.index = sector.name, track.index = 2)
}, bg.border = NA)
dev.off()
