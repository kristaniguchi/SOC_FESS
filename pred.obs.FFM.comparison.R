#Comparison of observed and predicted FFMs for 3 gages in SOC: Aliso @ STP, Aliso @ Jeronimo, Lower Oso

#load library
library("devtools")
library("ffcAPIClient")
library("ggplot2")
library("scales")
library("purrr")
library("tidyverse")

#my token for FFC API Client
mytoken <- "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJmaXJzdE5hbWUiOiJLcmlzIiwibGFzdE5hbWUiOiJUYW5pZ3VjaGkgUXVhbiIsImVtYWlsIjoia3Jpc3RpbmV0cUBzY2N3cnAub3JnIiwicm9sZSI6IlVTRVIiLCJpYXQiOjE1NzM4NjgwODN9.UJhTioLNNJOxvY_PYb_GIbcMRI_qewjkfYx-usC_7ZA"


#set working directory
gage.dir <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/200326_Gauge_Data/"
model.dir <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/200312_Updated_Calibration/"
pred.data.dir <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/200312_Updated_Calibration/WY94-Present/"
setwd(pred.data.dir)

#Gage info
gage <- read.csv("L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/200312_Updated_Calibration/WY15-19_Calibration_Assessment/WY15-19_Statistical_Summary_v22M_KTQ.csv", skip=5, header=TRUE)
#only use first 3 rows of gage info
gage <- gage[1:3,]
#all gage data
gage.files <- list.files(gage.dir, full.names=TRUE)


#Predicted model output all
mod.files <- list.files(pred.data.dir, pattern ="\\.out$")

#Functional flow metric names and labels for plots
filename <- ("L:/CA  E-flows framework_ES/Misc/Functional Flows metrics/functional_flow_metric_modeling/all_metric_def_list_FFMs.csv")
ffm.labels <- read.csv(filename)


#Loop to go through only the gage files

for (i in 1:length(gage$Gage)){
  #get gage data i
  sub <- gage[i,] #gage info
  gage.name <- sub$Gage
  subbasin <- sub$LSPC.Subbasin
  COMID <- sub$COMID
  ################################################
  ####PREDICTED DATA######
  
  #load in hourly model prediction
  ind.data <- grep(subbasin, mod.files)
  pred <- read.table(mod.files[ind.data], skip=22)
  names(pred) <- c("gage", "year", "month", "day", "hour", "min", "depth", "hyd.radius", "av.vel","flow.cfs")
  #format date
  date <- paste(pred$month, pred$day, pred$year, sep="/")
  pred$date <- date
  unique.dates <- unique(date)
  ################
  
  #calc mean daily flow for predicted data
  #empty mean daily flow vector
  flow.pred <- NA

  for (j in 1:length(unique.dates)){
    sub.day <- pred[pred$date == unique.dates[j],]
    flow.pred[j] <- mean(sub.day$flow.cfs)
  }
  #create new data frame with date and mean daily flow to go into FFC
  data.pred <- data.frame(cbind(unique.dates, flow.pred))
  names(data.pred) <- c("date", "flow")
  #write daily output file
  fname <- paste0(pred.data.dir,"daily/", subbasin,"_pred_daily.txt")
  write.table(data.pred, fname, row.names = FALSE, sep = ",")
  ################
  
  #calc FFMs and alteration for predicted data.pred
  #Run data.predframe through FFC online with my own gage data.pred or model data.pred
  results.pred <- ffcAPIClient::evaluate_alteration(timeseries_df = data.pred, comid = COMID, token = mytoken)
  #reference percentiles
  ref.percentiles <- results.pred$predicted_percentiles
  ref.percentiles.wyt <- results.pred$predicted_wyt_percentiles
  #predicted results, LSPC
  pred.alteration.all <- results.pred$alteration
  pred.percentiles.all <- results.pred$ffc_percentiles
  pred.results.ffm.all <- results.pred$ffc_results
    pred.results.ffm.all$type <- "pred"
  pred.drh.data <- results.pred$drh_data.pred
  ################################################
  ####OBSERVED GAGE DATA######

  #load in hourly gage data
  #if oso (403010), use truncated file ("trunc"), else use file starting with subbasin number
  if(subbasin == 403010){
    ind.gage <- grep("trunc", gage.files)
  }else{
    ind.gage <- grep(subbasin, gage.files)
  }
  #read in obs data
  obs <- read.csv(gage.files[ind.gage])
  #format date
  date1 <- strptime(obs$DateTime, format = "%m/%d/%Y %H:%M")
  date2 <- format(date1, "%m/%d/%Y")
  obs$date <- date2
  unique.dates2 <- unique(date2)
  ################
  
  #calc mean daily flow for observed gage data
  #empty mean daily flow vector
  flow.obs <- NA
  
  for (k in 1:length(unique.dates2)){
    sub.day2 <- obs[obs$date == unique.dates2[k],]
    flow.obs[k] <- mean(sub.day2$obs_flow)
  }
  #create new data frame with date and mean daily flow to go into FFC
  data.obs <- data.frame(cbind(unique.dates2, flow.obs))
  names(data.obs) <- c("date", "flow")
  #write daily output file
  fname2 <- paste0(gage.dir,"daily/", subbasin,"_obs_daily.txt")
  write.table(data.obs, fname2, row.names = FALSE, sep = ",")
  ################
  #calc FFMs and alteration for observed data
  #Run dataframe through FFC online with my own gage data or model data
  results.obs <- ffcAPIClient::evaluate_alteration(timeseries_df = data.obs, comid = COMID, token = mytoken)
  obs.alteration.all <- results.obs$alteration
  obs.percentiles.all <- results.obs$ffc_percentiles
  obs.results.ffm.all <- results.obs$ffc_results
    obs.results.ffm.all$type <- "obs"
  obs.drh.data <- results.obs$drh_data
  ################################################
  #####create plots pred vs. obs FFMs#####
  
  #subset data to show overlapping dates
  #observed gage POR
  start.date.obs <- as.character(data.obs$date[1])
  end.date.obs <- as.character(data.obs$date[length(data.obs$date)])
  obs.por <- paste0("Observed Gage POR: ", start.date.obs, " to ", end.date.obs)
  #full WY start and end
  start.wy.obs0 <- na.omit(obs.results.ffm.all$Year[obs.results.ffm.all$DS_Dur_WS>1])
  start.wy.obs <- start.wy.obs0[1]
  end.wy.obs <- start.wy.obs0[length(start.wy.obs0)]
  
  #predicted LSPC POR
  start.date.pred <- as.character(data.pred$date[1])
  end.date.pred <- as.character(data.pred$date[length(data.pred$date)])
  pred.por <- paste0("LSPC POR: ", start.date.pred, " to ", end.date.pred)
  #full WY start and end, first year that isn't NA is start WY, last year that isn't NA is end WY
  start.wy.pred0 <- na.omit(pred.results.ffm.all$Year[pred.results.ffm.all$DS_Dur_WS>1])
  start.wy.pred <- start.wy.pred0[1]
  end.wy.pred <- start.wy.pred0[length(start.wy.pred0)]
  
  #subset to overlapping years
  start.overall <- max(as.numeric(c(start.wy.obs, start.wy.pred)))
  end.overall <- min(as.numeric(c(end.wy.obs, end.wy.pred)))
  #index of start and end for pred and obs
  ind.start.obs <- grep(start.overall, obs.results.ffm.all$Year)
  ind.end.obs <- grep(end.overall, obs.results.ffm.all$Year)
  ind.start.pred <- grep(start.overall, pred.results.ffm.all$Year)
  ind.end.pred <- grep(end.overall, pred.results.ffm.all$Year)
  
  #subset ffm results
  sub.obs <- obs.results.ffm.all[ind.start.obs:ind.end.obs,]
  sub.pred <- pred.results.ffm.all[ind.start.pred:ind.end.pred,]
  ffm.names <- names(sub.obs)
  #color points based on >2015, >=2015
  pre.2015.lab <- paste0(start.overall, "-2014")
  post.2014.lab <- paste0("2015-", end.overall)
  ind.2014 <- grep("2014", sub.obs$Year)
  label.years <- c(rep(pre.2015.lab, ind.2014), rep(post.2014.lab, (length(sub.obs$Year)-ind.2014)))
  
  #loop to plot predicted vs obs FFMs
  for(l in 2:(length(ffm.names)-1)){
    #get flow metric and labels/title for plots
    ffm <- ffm.names[l]
    #find index of ffm in lookup table
    index.ffm <- grep(ffm, ffm.labels$flow_metric)
    #only plot if core FFM
    if(length(index.ffm) > 0){
      #from lookup table find title and axes labels
      x.name <- paste0("Observed", ffm.labels$title_ffm[index.ffm])
      y.name <- paste0("Predicted", ffm.labels$title_ffm[index.ffm])
      title <- ffm.labels$title_name[index.ffm]
      #plot
      plot <- ggplot(data = data.frame(x = sub.obs[,l], y=sub.pred[,l], timeframe = label.years)) + 
        geom_point(mapping = aes(x = x, y = y, col=timeframe, size=.5)) +
        labs(x = x.name, y= y.name, subtitle = gage.name, title = title) + 
        scale_size(guide=FALSE) + theme(legend.title = element_blank(), legend.position = "bottom") +
        geom_abline()
      # print plots to screen
      print(plot)
  
    }
  }
  
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
