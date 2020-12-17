

#Colocation data from geoinsights facebook
#raw data uploaded to DPA drive link:
#https://drive.google.com/drive/u/0/folders/1vT7014CuLiwhdn9y8cgWdmUV0UWrBtxW


library(reshape)
library(dplyr)

options(scipen=999)  # turn-off scientific notation like 1e+48
#read all csv
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

db<-rdb[,c(2,8,13,15)]


library(lubridate)

#prepare columns in date format
#and get days of week and the corresponding week
db$date<-as.Date(db$ds, format="%Y-%m-%d")
db$Week_Day<- as.numeric(format(db$date, format='%w'))
db$week <- db$date + (0 - db$Week_Day)


#get the mean of the link_value (probability of encounter)
dbb<-aggregate(link_value~polygon1_name+polygon2_name+week, FUN=mean, data=db, na.rm=TRUE)


#get weeks of interest from covid curve
#these weeks were used in the figure 5
w1<-subset(dbb, week=="2020-03-29")
w2<-subset(dbb, week=="2020-05-10")
w3<-subset(dbb, week=="2020-06-07")
w4<-subset(dbb, week=="2020-08-16")

#bind the previously selected weeks
#and add a sequential numeric identifier
ws<-rbind(w1,w2,w3,w4)
ws<-ws[order(-ws$link_value),]
ws$id<-seq.int(nrow(ws))

#mirror
#we visually identified the rows in which the origen (polygon1) and destination (polygon2) atolls are the same
#these atoll_name_x == atoll_name_x  finish in row number 61
mi<-ws[1:61,]

#generate a column for encounter (polygon1-polygon2)
#and simplify dataframe
mi$Encounter<-paste(mi$polygon1_name, mi$polygon2_name, sep="-")
mi<-mi[,c(6,4,3)]

#extend table into the different weeks and simplify dataframe
#and rename columns
final<-reshape(mi, idvar="Encounter", timevar="week", direction ="wide")
final<-final[,c(1,4,2,3,5)]
names(final)<-c("Encounter", "March_29", "May_10", "June_07", "August_16")

#the table 2 shows no NA
#but it is interesting to complete table

#order table by the first week
#and get means for the selected columns
final<-final[order(-final$March_29),]
final[,-1] <-round(final[,-1],3) 
final$mean <- rowMeans(subset(final, select=c(2:5)), na.rm = TRUE)


library(gridExtra)
png("table.png", height=10, width=10, units = 'in', res = 300)
grid.table(final)
dev.off()

columnMean <- summarize_all(final[,c(2:5)], mean, na.rm=TRUE)
columnMean


