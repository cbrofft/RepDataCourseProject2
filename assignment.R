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

#Convert from factor to character for property and crop damage
stormdata$PROPDMGEXP<-as.character(stormdata$PROPDMGEXP)
stormdata$CROPDMGEXP<-as.character(stormdata$CROPDMGEXP)

## Property and crop damage units and respective column numbers
propdamage<-grep("PROPDMGEXP",colnames(stormdata))
cropdamage<-grep("CROPDMGEXP",colnames(stormdata))

#Calculating the units of damage for crop and property
stormdata[stormdata$PROPDMGEXP=="",propdamage]<-"0"
stormdata[stormdata$PROPDMGEXP=="-",propdamage]<-"0"
stormdata[stormdata$PROPDMGEXP=="?",propdamage]<-"0"
stormdata[stormdata$PROPDMGEXP=="+",propdamage]<-"0"
stormdata[stormdata$PROPDMGEXP=="1",propdamage]<-"0"
stormdata[stormdata$PROPDMGEXP=="2",propdamage]<-"0"
stormdata[stormdata$PROPDMGEXP=="3",propdamage]<-"0"
stormdata[stormdata$PROPDMGEXP=="4",propdamage]<-"0"
stormdata[stormdata$PROPDMGEXP=="5",propdamage]<-"0"
stormdata[stormdata$PROPDMGEXP=="6",propdamage]<-"0"
stormdata[stormdata$PROPDMGEXP=="7",propdamage]<-"0"
stormdata[stormdata$PROPDMGEXP=="8",propdamage]<-"0"
stormdata[stormdata$PROPDMGEXP=="h",propdamage]<-"100"
stormdata[stormdata$PROPDMGEXP=="H",propdamage]<-"100"
stormdata[stormdata$PROPDMGEXP=="K",propdamage]<-"1000"
stormdata[stormdata$PROPDMGEXP=="m",propdamage]<-"1000000"
stormdata[stormdata$PROPDMGEXP=="M",propdamage]<-"1000000"
stormdata[stormdata$PROPDMGEXP=="B",propdamage]<-"1000000000"

stormdata[stormdata$CROPDMGEXP=="",cropdamage]<-"0"
stormdata[stormdata$CROPDMGEXP=="?",cropdamage]<-"0"
stormdata[stormdata$CROPDMGEXP=="2",cropdamage]<-"0"
stormdata[stormdata$CROPDMGEXP=="k",cropdamage]<-"1000"
stormdata[stormdata$CROPDMGEXP=="K",cropdamage]<-"1000"
stormdata[stormdata$CROPDMGEXP=="m",cropdamage]<-"1000000"
stormdata[stormdata$CROPDMGEXP=="M",cropdamage]<-"1000000"
stormdata[stormdata$CROPDMGEXP=="B",cropdamage]<-"1000000000"

# Convert property and crop damages to numeric values now
stormdata$PROPDMGEXP<-as.numeric(stormdata$PROPDMGEXP)
stormdata$CROPDMGEXP<-as.numeric(stormdata$CROPDMGEXP)

#Calculate property and crop loss values
stormdata$propertyloss<-stormdata$PROPDMG*stormdata$PROPDMGEXP
stormdata$croploss<-stormdata$CROPDMG*stormdata$CROPDMGEXP

# Get the total economic loss
stormdata$totaleconomicloss<-stormdata$propertyloss+stormdata$croploss

# Get column numbers for human loss 
col1<-grep("EVTYPE",colnames(stormdata))
col2<-grep("death_injuries",colnames(stormdata))

# Create data set for human loss with omitting NA values
humanloss<-stormdata[,c(col1,col2)]
humanloss<-humanloss[complete.cases(humanloss), ]


# Get ccolumn numbers for economic loss 
col1<-grep("EVTYPE",colnames(stormdata))
col2<-grep("economicloss",colnames(stormdata))

# Create economic loss with NA Values omitted
economicloss<-stormdata[,c(col1,col2)]
economicloss<-economicloss[complete.cases(economicloss), ]

# Aggregation of  total human loss
humanlossclean<-aggregate(death_injuries~EVTYPE,humanloss,sum,na.rm=TRUE)
# Sort human losses data in decreasing order of losses
humanlossclean<-humanlossclean[order(humanlossclean$death_injuries,decreasing = TRUE),]
# Create data set of top 3 events
humanlossfinal<-humanlossclean[1:3,]

# Aggregation of  total economic losses
ecolossclean<-aggregate(totaleconomicloss~EVTYPE,economicloss,sum,na.rm=TRUE)
# Sort economic losses data in decreasing order of losses
ecolossclean<-ecolossclean[order(ecolossclean$totaleconomicloss,decreasing = TRUE),]
## Create dataset for top 3 events
ecolossfinal<-ecolossclean[1:3,]