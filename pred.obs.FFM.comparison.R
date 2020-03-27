#Comparison of observed and predicted FFMs for 3 gages in SOC: Aliso @ STP, Aliso @ Jeronimo, Lower Oso

#load library
library("devtools")
library("ffcAPIClient")
library("ggplot2")
library("scales")

#my token for FFC API Client
mytoken <- "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJmaXJzdE5hbWUiOiJLcmlzIiwibGFzdE5hbWUiOiJUYW5pZ3VjaGkgUXVhbiIsImVtYWlsIjoia3Jpc3RpbmV0cUBzY2N3cnAub3JnIiwicm9sZSI6IlVTRVIiLCJpYXQiOjE1NzM4NjgwODN9.UJhTioLNNJOxvY_PYb_GIbcMRI_qewjkfYx-usC_7ZA"


#set working directory
gage.dir <- "L:/San Juan WQIP_KTQ/Data/Working/FlowAlteration/County_Gage_Data/"
model.dir <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/200312_Updated_Calibration/"
pred.data.dir <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/200312_Updated_Calibration/WY94-Present/"
setwd(pred.data.dir)

#Gage info
gage <- read.csv("L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/200312_Updated_Calibration/WY15-19_Calibration_Assessment/WY15-19_Statistical_Summary_v22M_KTQ.csv", skip=5, header=TRUE)
#only use first 3 rows of gage info
gage <- gage[1:3,]

#Predicted model output all
mod.files <- list.files(pred.data.dir, pattern ="\\.out$")

#Loop to go through only the gage files

for (i in 1:length(gage$Gage)){
  sub <- gage[i,]
  gage.name <- sub$Gage
  subbasin <- sub$LSPC.Subbasin
  COMID <- sub$COMID
  
  #load in hourly model prediction
  ind.data <- grep(subbasin, mod.files)
  pred <- read.table(mod.files[ind.data], skip=22)
  names(pred) <- c("gage", "year", "month", "day", "hour", "min", "depth", "hyd.radius", "av.vel","flow.cfs")
  date <- paste(pred$month, pred$day, pred$year, sep="/")
  pred$date <- date
  unique.dates <- unique(date)
  
  #calc mean daily flow
  #empty mean daily flow vector
  flow <- NA

  for (j in 1:length(unique.dates)){
    sub.day <- pred[pred$date == unique.dates[j],]
    flow[j] <- mean(sub.day$flow.cfs)
  }
  #create new data frame with date and mean daily flow to go into FFC
  data <- data.frame(cbind(unique.dates, flow))
  names(data) <- c("date", "flow")
  #write daily output file
  fname <- paste0(pred.data.dir,"daily/", subbasin,"_daily.txt")
  write.table(data, fname, row.names = FALSE, sep = ",")
  
  #calc FFMs and alteration for predicted data
  #Run dataframe through FFC online with my own gage data or model data
  results2 <- ffcAPIClient::evaluate_alteration(timeseries_df = data, comid = COMID, token = mytoken)
  ref.percentiles <- results2$predicted_percentiles
  alteration.all <- results2$alteration
  mod.percentiles.all <- results2$ffc_percentiles
  results.ffm.mod.all <- results2$ffc_results
  drh.data <- results2$drh_data
  results2$predicted_wyt_percentiles
  
}



#load in gage data from County web portal
data <- read.csv("AlisoCreek_STP_DailyDischarge.csv", skip=3)
#format date and Q into dataframe
date2 <- strptime(as.character(data$Date.and.time),"%m/%d/%Y")
date <- format(date2, "%m/%d/%Y")
Q.cfs <- data$Mean
my_df <- data.frame(date,Q.cfs)
names(my_df) <- c("date","flow")
my_df$date <- as.character(my_df$date)

#COMID for that gage (may be a list and create loop to go through this list)
COMID <- 20350539

#my token, update with your token
mytoken <- "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJmaXJzdE5hbWUiOiJLcmlzIiwibGFzdE5hbWUiOiJUYW5pZ3VjaGkgUXVhbiIsImVtYWlsIjoia3Jpc3RpbmV0cUBzY2N3cnAub3JnIiwicm9sZSI6IlVTRVIiLCJpYXQiOjE1NzM4NjgwODN9.UJhTioLNNJOxvY_PYb_GIbcMRI_qewjkfYx-usC_7ZA"


#Run dataframe through FFC online with my own gage data or model data
results2 <- ffcAPIClient::evaluate_alteration(timeseries_df = my_df, comid = COMID, token = mytoken, plot_output_folder = "L:/San Juan WQIP_KTQ/Data/Working/FlowAlteration/testing_RpackageFFC")
pred.percentiles <- results2$predicted_percentiles
alteration <- results2$alteration
percentiles <- results2$ffc_percentiles
results.ffm <- results2$ffc_results
drh.data <- results2$drh_data
results2$predicted_wyt_percentiles
