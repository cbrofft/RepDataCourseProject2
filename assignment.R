library(knitr)
library(ggplot2)
library(dplyr)
library(plyr)
## Staging the files and setting the directory.  
##The file will be downloaded from the source and unzipped and saved as stormdata.csv
setwd("~/RepDataCourseProject2")
fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
downloadFile <- "./repdata%2Fdata%2FStormData.csv.bz2"
download.file(fileURL, downloadFile, method = "curl")
stormdata <-read.csv("repdata%2Fdata%2FStormData.csv.bz2",header = TRUE, sep = ",")
unlink(downloadFile)
# Calculate deaths and injuries from storms
stormdata$death_injuries<-stormdata$FATALITIES+stormdata$INJURIES
