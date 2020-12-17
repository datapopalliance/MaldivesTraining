
#Facebook data dictionary

#Maldives Coronavirus Disease Prevention Map Mar 31 2020 - Facebook Population (Tile Level)
#Description:Location density maps are heat maps, which show where people are located before, during and after a disaster and where populations have increased or decreased. We can compare this information to historical records, like population estimates based on satellite images. Comparing these data sets can help response organizations understand areas impacted by a natural disaster. The following metrics are are included in the map: * Date Time - The time period represented by the current map layer. * Standard (Z) Score - The number of standard deviations by which the crisis population count in the location differs from the baseline count. * Baseline:People - The average number of people we expect to be in the area during the specified time based on pre-disaster estimates. * Crisis:People - The number of people observed in the tile during the selected time period. * Difference - The difference between the population at the time of the crisis and the population during the baseline. * Percent Change - The percentage difference between the population at the time of the crisis and the population during the baseline. For more information go to the Help Page https://fburl.com/disastermaps_help


#the Facebook population maps were downloaded from:
#https://www.facebook.com/geoinsights-portal/
#and then uploaded to the DPA drive folder: 
#https://drive.google.com/drive/u/0/folders/117og1_1L1ht0vUEhOXj19yJXlGv4sgcI


#read and bind all the csv cointained in the corresponding folder
options(scipen=999) 
batch_read <- function(path, pattern, recursive = FALSE, read_fun, ...) {
  data.files <- list.files(path, pattern = pattern, recursive = recursive)
  data <- lapply(paste0(path, data.files), read_fun, ...)
  data <- do.call("rbind", data)
  data
}

idb <- batch_read(
  path = "/home/memo/Documents/perso/DataPopAlliance/maldives/dir/populationFB/data/"
  , pattern = "\\.csv"
  , read_fun = read.csv
  , header = TRUE
)

#For Kepler to display a temporal visualization of data
#A date_time columns needs to be included in the following format:
#yyyy-mm-dd 00:00:00.0
#the following script adds a synthetic column named millisec 
#that is pasted to the date column

idb$date <- as.factor(sapply(as.character(idb$date_time), function(x) {strsplit(x, "\\ ")[[1]][1]}))
df<-idb[,c(1,2,3,5,11,14)]
df$millisec <- cumsum(c(0,as.numeric(diff(df$date))!=0))
df$time<-rep("00:00:01.", nrow(df))
df$millisec<-paste(df$time, df$millisec, sep="")
df$time<-NULL

df$date<-as.character(df$date)
df$dateTime<-paste(df$date, df$millisec, sep=" ")
df<-df[,c(1,2,3,5,8)]

write.csv(df, "Preprocessed_data_for_kepler.csv", row.names=FALSE)

#this file is being uploaded in the DPA drive:
#https://drive.google.com/drive/u/0/folders/1VJjy3jZHBYTNI9iGQnPRIBVTSwd2oI32