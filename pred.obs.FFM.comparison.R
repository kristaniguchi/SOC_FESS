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
plot.output.dir.pred <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/200312_Updated_Calibration/WY94-Present/daily/FFMs/"
plot.output.dir.obs <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/200326_Gauge_Data/daily/FFMs/"

#Gage info
gage <- read.csv("L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/200312_Updated_Calibration/WY15-19_Calibration_Assessment/WY15-19_Statistical_Summary_v22M_KTQ.csv", skip=5, header=TRUE)
#only use first 3 rows of gage info
gage <- gage[1:3,]
#all gage data
gage.files <- list.files(gage.dir, full.names=TRUE)


#Predicted model output all
mod.files <- list.files(pred.data.dir, pattern ="\\.out$")

#Functional flow metric names and labels for plots
filename <- ("L:/CA  E-flows framework_ES/Misc/Functional Flows metrics/functional_flow_metric_modeling/all_metric_def_list_FFMs_v2.csv")
ffm.labels <- read.csv(filename)
  ffm.labels$metric <- ffm.labels$flow_metric


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
  #directory to save ffm outputs
  output.dir <- paste0(plot.output.dir.pred, subbasin)
  #Run data.predframe through FFC online with my own gage data.pred or model data.pred
  results.pred <- ffcAPIClient::evaluate_alteration(timeseries_df = data.pred, comid = COMID, token = mytoken, plot_output_folder = output.dir)
  #reference percentiles
  ref.percentiles <- results.pred$predicted_percentiles
    ref.percentiles$source2 <- rep("Reference", length(ref.percentiles$p10))
  ref.percentiles.wyt <- results.pred$predicted_wyt_percentiles
  #predicted results, LSPC
  pred.alteration.all <- results.pred$alteration
  pred.percentiles.all <- results.pred$ffc_percentiles
    pred.percentiles.all$source2 <- rep("LSPC", length(pred.percentiles.all$p10))
  pred.results.ffm.all <- results.pred$ffc_results
    pred.results.ffm.all$type <- "pred"
  pred.drh.data <- results.pred$drh_data.pred
  #write outputs to dir
  write.csv(ref.percentiles, file=paste0(output.dir,"/ref.percentiles.csv"), row.names=FALSE)
  write.csv(pred.alteration.all, file=paste0(output.dir,"/pred.alteration.all.csv"), row.names=FALSE)
  write.csv(pred.percentiles.all, file=paste0(output.dir,"/pred.percentiles.all.csv"), row.names=FALSE)
  write.csv(pred.results.ffm.all, file=paste0(output.dir,"/pred.results.ffm.all.csv"), row.names=FALSE)
  write.csv(pred.drh.data, file=paste0(output.dir,"/pred.drh.data.csv"), row.names=FALSE)
  
  
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
    flow.obs[k] <- mean(sub.day2$obs_flow, na.rm=TRUE)
  }
  #if NaN, replace with NA
  flow.obs<- as.numeric(sub("NaN", "NA", flow.obs))
  #create new data frame with date and mean daily flow to go into FFC
  data.obs <- data.frame(cbind(unique.dates2, flow.obs))
  names(data.obs) <- c("date", "flow")
  #write daily output file
  fname2 <- paste0(gage.dir,"daily/", subbasin,"_obs_daily.txt")
  write.table(data.obs, fname2, row.names = FALSE, sep = ",")
  ################
  #calc FFMs and alteration for observed data
  output.dir.obs <- paste0(plot.output.dir.obs, subbasin)
  #Run dataframe through FFC online with my own gage data or model data
  results.obs <- ffcAPIClient::evaluate_alteration(timeseries_df = data.obs, comid = COMID, token = mytoken, plot_output_folder = output.dir.obs)
  obs.alteration.all <- results.obs$alteration
  obs.percentiles.all <- results.obs$ffc_percentiles
    obs.percentiles.all$source2 <- rep("Gage", length(obs.percentiles.all$p10))
  obs.results.ffm.all <- results.obs$ffc_results
    obs.results.ffm.all$type <- "obs"
  obs.drh.data <- results.obs$drh_data
  #write outputs to dir
  write.csv(obs.alteration.all, file=paste0(output.dir.obs,"/obs.alteration.all.csv"), row.names=FALSE)
  write.csv(obs.percentiles.all, file=paste0(output.dir.obs,"/obs.percentiles.all.csv"), row.names=FALSE)
  write.csv(obs.results.ffm.all, file=paste0(output.dir.obs,"/obs.results.ffm.all.csv"), row.names=FALSE)
  write.csv(obs.drh.data, file=paste0(output.dir.obs,"/obs.drh.data.csv"), row.names=FALSE)
  
  ################################################
  #####create plots pred vs. obs FFMs#####
  
  #subset data to show overlapping dates
  #observed gage POR
  start.date.obs <- as.character(data.obs$date[1])
  end.date.obs <- as.character(data.obs$date[length(data.obs$date)])
  obs.por <- paste0("Observed Gage POR: ", start.date.obs, " to ", end.date.obs)
  #full WY start and end
  start.wy.obs0 <- na.omit(obs.results.ffm.all$Year[obs.results.ffm.all$Wet_Tim>1])
  start.wy.obs <- start.wy.obs0[1]
  end.wy.obs <- start.wy.obs0[length(start.wy.obs0)]
  obs.wy.por <- paste0("WY ", start.wy.obs, " to ", end.wy.obs)
  
  
  #predicted LSPC POR
  start.date.pred <- as.character(data.pred$date[1])
  end.date.pred <- as.character(data.pred$date[length(data.pred$date)])
  pred.por <- paste0("LSPC POR: ", start.date.pred, " to ", end.date.pred)
  #full WY start and end, first year that isn't NA is start WY, last year that isn't NA is end WY
  start.wy.pred0 <- na.omit(pred.results.ffm.all$Year[pred.results.ffm.all$Wet_Tim>1])
  start.wy.pred <- start.wy.pred0[1]
  end.wy.pred <- start.wy.pred0[length(start.wy.pred0)]
  pred.wy.por <- paste0("WY ", start.wy.pred, " to ", end.wy.pred)
  
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
  #color points based on ><2015, >=2015
    #if there's no point before 2015, then all should be post timeframe
  if(start.overall>2014){
    pre.2015.lab <- paste0("pre-", start.overall)
    post.2014.lab <- paste0(start.overall, "-", end.overall)
    #all years are in the post timeframe
    label.years <- rep(post.2014.lab, length(sub.obs$Year))
  }else{
    pre.2015.lab <- paste0(start.overall, "-2014")
    post.2014.lab <- paste0("2015-", end.overall)
    ind.2014 <- grep("2014", sub.obs$Year)
    label.years <- c(rep(pre.2015.lab, ind.2014), rep(post.2014.lab, (length(sub.obs$Year)-ind.2014)))
  }
  
  #loop to plot predicted vs obs for each ffm
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
      #if more than 2 points set xlim, ylim to be equal and plot
      if(length(sub.obs[,l])>1){
        #set xlim and ylim to be equal axes
        limits <- c(min(sub.obs[,l],sub.pred[,l], na.rm=TRUE), max(sub.obs[,l],sub.pred[,l], na.rm=TRUE))
        #plot
        plot <- ggplot(data = data.frame(x = sub.obs[,l], y=sub.pred[,l], timeframe = label.years)) + 
          geom_point(mapping = aes(x = x, y = y, col=timeframe, size=.5)) +
          labs(x = x.name, y= y.name, subtitle = gage.name, title = title) + 
          xlim(limits) + ylim(limits) +
          scale_size(guide=FALSE) + 
          theme(legend.title = element_blank(), legend.position = "bottom", legend.text= element_text(size=10)) +
          guides(colour=guide_legend(override.aes = list(size = 4))) + geom_abline()
        #else only one point, just plot the point
      }else{
        #plot
        plot <- ggplot(data = data.frame(x = sub.obs[,l], y=sub.pred[,l], timeframe = label.years)) + 
          geom_point(mapping = aes(x = x, y = y, col=timeframe, size=.5)) +
          labs(x = x.name, y= y.name, subtitle = gage.name, title = title) + 
          scale_size(guide=FALSE) + theme(legend.title = element_blank(), legend.position = "bottom", legend.text= element_text(size=10)) +
          guides(colour=guide_legend(override.aes = list(size = 4))) + geom_abline()
      }
      #print plots to screen
      print(plot)
    }
  }
  ############################
  ###Boxplot comparisons of entire POR LSPC (add colored points), Gage, Reference
  #create combined boxplots for each component
  unique.components <- as.character(unique(ffm.labels$title_component))
  for(m in 1:length(unique.components)){
    #subset percentiles based on component m
    ind.comp.m <- grep(unique.components[m], ffm.labels$title_component)
    ffm.names.m <- ffm.labels[ind.comp.m,]
    sub.pred.comp <- obs.percentiles.all[as.character(obs.percentiles.all$metric) %in%  as.character(ffm.names.m$flow_metric),]
    sub.obs.comp <- pred.percentiles.all[as.character(pred.percentiles.all$metric) %in%  as.character(ffm.names.m$flow_metric),]
    sub.ref.comp <- ref.percentiles[as.character(ref.percentiles$metric) %in%  as.character(ffm.names.m$flow_metric),]
    
    #subset predicted points based on component m
    sub.pred.comp.pts <- pred.results.ffm.all[,c("Year",as.character(ffm.names.m$flow_metric))]
    #timeframe for predicted points, this will be used for point colors
    ind.2014.pred <- grep("2014", sub.pred.comp.pts$Year)
    timeframe.pred <- c(rep("1994-2014", ind.2014.pred), rep("2015-present", length(sub.pred.comp.pts$Year)-ind.2014.pred))
    
    #Boxplots for components
    title <- as.character(ffm.names.m$title_component[1]) #component
    subtitle.bp <- paste0(gage.name, ": Subbasin ", subbasin)
    characteristic <- sort(as.character(ffm.names.m$flow_characteristic)) 
    metrics.title <- sort(as.character(ffm.names.m$title_ffm)) #boxplot title
    
    #combine all percentiles dataframes and merge metric label names
    mergeCols <- names(obs.percentiles.all)
    percentiles.cbind.all <- full_join(ref.percentiles, obs.percentiles.all, by=mergeCols) %>% 
      full_join(pred.percentiles.all, by=mergeCols) %>% 
      merge(ffm.labels, by="metric")
    
    #subset percentiles for component only
    percentiles.cbind.all.sub.m <- percentiles.cbind.all[percentiles.cbind.all$metric %in% as.character(ffm.names.m$flow_metric),] #%>%
    
    #if peak flow plots, create boxplots in order of increasing magnitude (not based on alphabetical order)
    if(ffm.labels$flow_component[m] == "Peak Flow"){
      #fill color based on entire POR LSPC (add colored points), Gage, Reference
      fill<- percentiles.cbind.all.sub.m$source2
      #reorder peak metric plots
      if(unique.components[m] == "Peak Flow Magnitude"){
        percentiles.cbind.all.sub.m$title_ffm <- factor(as.character(percentiles.cbind.all.sub.m$title_ffm), levels = c(" Magnitude (2-year flood, cfs)", " Magnitude (5-year flood, cfs)", " Magnitude (10-year flood, cfs)"))
      }
      if(unique.components[m] == "Peak Flow Duration"){
        percentiles.cbind.all.sub.m$title_ffm <- factor(as.character(percentiles.cbind.all.sub.m$title_ffm), levels = c(" Duration (2-year flood, days)", " Duration (5-year flood, days)", " Duration (10-year flood, days)"))
      }
      if(unique.components[m] == "Peak Flow Frequency"){
        percentiles.cbind.all.sub.m$title_ffm <- factor(as.character(percentiles.cbind.all.sub.m$title_ffm), levels = c(" Frequency (2-year flood)", " Frequency (5-year flood)", " Frequency (10-year flood)"))
      }
      #All years plots
      P<- ggplot(percentiles.cbind.all.sub.m, aes(x=source2, ymin = p10, lower = p25, middle = p50, upper = p75, ymax = p90, fill=source2)) +
        geom_boxplot(stat = "identity") +  facet_wrap(~title_ffm, scales="free") +
        labs(title=title,x ="", y = "", subtitle = subtitle.bp) 
      print(P)
      
    }else{
      #fill color based on entire POR LSPC (add colored points), Gage, Reference
      fill<- percentiles.cbind.all.sub.m$source2
      #All years plots
      P<- ggplot(percentiles.cbind.all.sub.m, aes(x=source2, ymin = p10, lower = p25, middle = p50, upper = p75, ymax = p90, fill=source2)) +
        geom_boxplot(stat = "identity") +  facet_wrap(~title_ffm, scales="free") +
        labs(title=title,x ="", y = "", subtitle = subtitle.bp) 
      print(P)
      
    }
  }
  
}



  
  
  
  
  
  
#ignore from here, old code
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
