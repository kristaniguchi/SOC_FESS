#Comparison of reference model outputs to statewide model outputs at Aliso @ STP
  #Also compare Cristianitos ref gage to statewide and ref model

#load library
#load library
install.packages("devtools")
library("devtools")
#devtools::install_github('ceff-tech/ffc_api_client/ffcAPIClient')
library("ffcAPIClient")
library("devtools")
library("ffcAPIClient")
library("ggplot2")
library("scales")
library("purrr")
library("plyr")
library("tidyverse")

#my token for FFC API Client
mytoken <- "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJmaXJzdE5hbWUiOiJLcmlzIiwibGFzdE5hbWUiOiJUYW5pZ3VjaGkgUXVhbiIsImVtYWlsIjoia3Jpc3RpbmV0cUBzY2N3cnAub3JnIiwicm9sZSI6IlVTRVIiLCJpYXQiOjE1NzM4NjgwODN9.UJhTioLNNJOxvY_PYb_GIbcMRI_qewjkfYx-usC_7ZA"


#set working directory
gage.dir <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/200326_Gauge_Data/"
#ref model directories
model.dir <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/200422_Predevelopment_Reference_Condition/"
pred.data.dir <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/200422_Predevelopment_Reference_Condition/WY94-Present/"
plot.output.dir.pred <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/200422_Predevelopment_Reference_Condition/WY94-Present/daily/FFMs/"

setwd(pred.data.dir)
plot.output.dir.obs <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/200326_Gauge_Data/daily/FFMs/"

#Gage info, saved in previous calibration folder
gage <- read.csv("L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/200312_Updated_Calibration/WY15-19_Calibration_Assessment/WY15-19_Statistical_Summary_v22M_KTQ.csv", skip=5, header=TRUE)
#only use cristianitos gage for now
gage <- gage[gage$Gage == "Cristianitos",]
#all gage data
gage.files <- list.files(gage.dir, full.names=TRUE)


#Predicted model output all for reference condition
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
  #for old calibration data
  #pred <- read.table(mod.files[ind.data], skip=22)
  #names(pred) <- c("gage", "year", "month", "day", "hour", "min", "depth", "hyd.radius", "av.vel","flow.cfs")
  #for new calibration data 
  pred <- read.table(mod.files[ind.data], skip=23)
  names(pred) <- c("gage", "year", "month", "day", "hour", "min", "precip", "depth", "hyd.radius", "av.vel","flow.cfs")
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
  fname <- paste0(pred.data.dir,"daily/", subbasin,"_pred_daily_ref.txt")
  write.table(data.pred, fname, row.names = FALSE, sep = ",")
  ################
  
  #calc FFMs and alteration for predicted data.pred ref
  #directory to save ffm outputs
  output.dir <- paste0(plot.output.dir.pred, subbasin)
  #Run data.predframe through FFC online with my own gage data.pred or model data.pred
  results.pred <- ffcAPIClient::evaluate_alteration(timeseries_df = data.pred, comid = COMID, token = mytoken, plot_output_folder = output.dir)
  #reference percentiles from statewide model
  ref.percentiles <- results.pred$predicted_percentiles
  ref.percentiles$source2 <- rep("Reference", length(ref.percentiles$p10))
  ref.percentiles.wyt <- results.pred$predicted_wyt_percentiles
  #predicted results, LSPC reference model 
  pred.alteration.all <- results.pred$alteration
  pred.percentiles.all <- results.pred$ffc_percentiles
  pred.percentiles.all$source2 <- rep("LSPC", length(pred.percentiles.all$p10))
  pred.results.ffm.all <- results.pred$ffc_results
  pred.results.ffm.all$type <- "pred"
  pred.drh.data <- results.pred$drh_data.pred
  #write outputs to dir
  write.csv(ref.percentiles, file=paste0(output.dir,"/ref.percentiles.statewide.csv"), row.names=FALSE)
  write.csv(pred.alteration.all, file=paste0(output.dir,"/pred.ref.statewide.alteration.all.csv"), row.names=FALSE)
  write.csv(pred.percentiles.all, file=paste0(output.dir,"/pred.ref.lspc.percentiles.all.csv"), row.names=FALSE)
  write.csv(pred.results.ffm.all, file=paste0(output.dir,"/pred.ref.lspc.results.ffm.all.csv"), row.names=FALSE)
  write.csv(pred.drh.data, file=paste0(output.dir,"/pred.ref.lspc.drh.data.csv"), row.names=FALSE)
  
  
  ################################################
  ####OBSERVED GAGE DATA######
  
  #load in hourly gage data----------- UPDATE THIS WITH Cristianitos Gage data!

  #read in obs data
  gage.ref.file <- paste0("L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/200326_Gauge_Data/daily/201020_CristianitosRefGageUSGS.txt")
  obs <- read.table(gage.ref.file, skip = 31, fill=TRUE)
  #format date
  date1 <- strptime(obs$V3, format = "%Y-%m-%d")
  date2 <- format(date1, "%m/%d/%Y")
  obs$date <- date2
  obs$discharge <- obs$V4
  ################
  
  #create new data frame with date and mean daily flow to go into FFC
  data.obs <- data.frame(cbind( obs$date, obs$discharge))
  names(data.obs) <- c("date", "flow")
  #write daily output file
  fname2 <- paste0(gage.dir,"daily/", subbasin,"cristianitos_ref_obs_daily.txt")
  write.table(data.obs, fname2, row.names = FALSE, sep = ",")
  ################
  #calc FFMs and alteration for observed ref data, cristianitos gage
  output.dir.obs <- paste0(plot.output.dir.obs, subbasin, "/cristianitos")
  #Run dataframe through FFC online with my own gage data or model data
  results.obs <- ffcAPIClient::evaluate_alteration(timeseries_df = data.obs, comid = COMID, token = mytoken, plot_output_folder = output.dir.obs)
  obs.alteration.all <- results.obs$alteration
  obs.percentiles.all <- results.obs$ffc_percentiles
  obs.percentiles.all$source2 <- rep("Gage", length(obs.percentiles.all$p10))
  obs.results.ffm.all <- results.obs$ffc_results
  obs.results.ffm.all$type <- "obs"
  obs.drh.data <- results.obs$drh_data
  
  #write outputs to dir
  write.csv(obs.alteration.all, file=paste0(output.dir.obs,"/refobs.cristianitos.alisostpstatewide.alteration.all.csv"), row.names=FALSE)
  write.csv(obs.percentiles.all, file=paste0(output.dir.obs,"/refobs.cristianitos.alisostpstatewide.percentiles.all.csv"), row.names=FALSE)
  write.csv(obs.results.ffm.all, file=paste0(output.dir.obs,"/refobs.cristianitos.alisostpstatewide.results.ffm.all.csv"), row.names=FALSE)
  write.csv(obs.drh.data, file=paste0(output.dir.obs,"/refobs.cristianitos.alisostpstatewidedrh.data.csv"), row.names=FALSE)
  
  #add WYT to annual results
  #read in WYT for COMID, for now will use WYT based on sulfur creek
  ind.wyt.file <- grep( paste0("COMID", COMID), gage.files)
  wyt.comid <- read.csv(gage.files[ind.wyt.file])
  #rename year col
  names(wyt.comid)[names(wyt.comid)=="year"] <- "Year"
  
  #read in WYT for sulfur creek rain gage: has current dates after 2014
  ind.sulfur <- grep( "WYT_sulfurcreek_raingage_1975_2019", gage.files)
  wyt.precip.sulfur <- read.csv(gage.files[ind.sulfur])
  #rename year col
  names(wyt.precip.sulfur)[names(wyt.precip.sulfur)=="WY"] <- "Year"
  
  #add in WYT for sulfur creek to obs gage and pred LSPC results
  obs.results.ffm.allwyt.comid <- data.frame(obs.results.ffm.all$Year)
  #add WYT comid
  obs.results.ffm.all <- merge(obs.results.ffm.all, wyt.precip.sulfur[,c("Year","WYT_Gage")], by="Year")
  pred.results.ffm.all <- merge(pred.results.ffm.all, wyt.precip.sulfur[,c("Year","WYT_Gage")], by="Year")
  
  
  ################################################
  #####create plots pred vs. obs FFMs#####
  
  
  #subset data to show overlapping dates
  #observed gage POR, cristianitos
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
  
  #loop to plot predicted vs obs for each ffm and do annual flow alteration status by WYT
  for(l in 2:(length(ffm.names)-2)){
    #get flow metric and labels/title for plots
    ffm <- ffm.names[l]
    #find index of ffm in lookup table
    index.ffm <- grep(ffm, ffm.labels$flow_metric)
    #only plot if core FFM
    if(length(index.ffm) > 0){
      #from lookup table find title and axes labels
      x.name <- paste0("Cristianitos Ref. Gage", ffm.labels$title_ffm[index.ffm])
      y.name <- paste0("Predicted Ref. LSPC", ffm.labels$title_ffm[index.ffm])
      title <- ffm.labels$title_name[index.ffm]
      #if more than 2 points set xlim, ylim to be equal and plot
      if(length(na.omit(cbind(sub.obs[,l],sub.pred[,l])))>1){
        #set xlim and ylim to be equal axes
        subset <- na.omit(cbind(sub.obs[,l],sub.pred[,l]))
        limits <- range(subset)
        #plot
        plot <- ggplot(data = data.frame(x = sub.obs[,l], y=sub.pred[,l], wyt = sub.pred$WYT_Gage, timeframe = label.years)) + 
          geom_point(mapping = aes(x = x, y = y, col=timeframe, shape = wyt, size=.5)) +
          labs(x = x.name, y= y.name, subtitle = gage.name, title = title) + 
          xlim(limits) + ylim(limits) +
          scale_size(guide=FALSE) + 
          scale_color_manual(values=c("#e66101", "#5e3c99")) + 
          theme(legend.title = element_blank(), legend.position = "bottom", legend.text= element_text(size=10)) +
          guides(colour=guide_legend(override.aes = list(size = 4))) + geom_abline()
        
        #Annual alteration status for ffm l based on WYT
        #WYT ref reformat names
        ref.percentiles.wyt$wyt[ref.percentiles.wyt$wyt =="dry"] <- "Dry"
        ref.percentiles.wyt$wyt[ref.percentiles.wyt$wyt =="wet"] <- "Wet"
        ref.percentiles.wyt$wyt[ref.percentiles.wyt$wyt =="moderate"] <- "Moderate"
        #ref percentiles for ffm i
        ref.percentiles.metric <- ref.percentiles.wyt[ref.percentiles.wyt$metric ==ffm,]
        #if there is a matching ref percentiles for that metric
        if(length(ref.percentiles.metric$wyt) > 0){
          #Gage:
          #loop to determine alteration each year
          obs.alteration.wyt <- NA
          obs.alteration.dir.wyt <- NA #direction of alteration
          
          for(n in 1:length(obs.results.ffm.all$Year)){
            wyt.n <- obs.results.ffm.all$WYT_Gage[n]
            #get ffm percentiles for wyt.n
            ref.percentiles.metric.wyt <- ref.percentiles.metric[ref.percentiles.metric$wyt == wyt.n,]
            #if current ffm is NA, alteartion NA, else if within p10-p90, no alteration, if outside likely altered
            if(is.na(obs.results.ffm.all[n,ffm])){
              obs.alteration.wyt[n] <- NA
              obs.alteration.dir.wyt[n] <- NA
            }else{
              if(obs.results.ffm.all[n,ffm] >= ref.percentiles.metric.wyt$p10 & obs.results.ffm.all[n,ffm] <= ref.percentiles.metric.wyt$p90){
                obs.alteration.wyt[n] <- "Likely unaltered"
                obs.alteration.dir.wyt[n] <- 0
              }else{
                obs.alteration.wyt[n] <- "Likely altered"
                #determine direction (-1 is depleted, 1 is augmented)
                if(obs.results.ffm.all[n,ffm] < ref.percentiles.metric.wyt$p10){
                  obs.alteration.dir.wyt[n] <- -1
                }else{
                  obs.alteration.dir.wyt[n] <- 1
                }
              }
            }
          }
          
          #alteration data.frame and 
          alt.df <- data.frame(Year = as.numeric(obs.results.ffm.all$Year), ffm = rep(ffm, length(obs.results.ffm.all$Year)), wyt = obs.results.ffm.all$WYT_Gage, metric.obs = obs.results.ffm.all[,ffm], obs.alteration.wyt = obs.alteration.wyt, obs.alteration.dir.wyt = as.factor(obs.alteration.dir.wyt))
          alt.df <- na.omit(alt.df)
          alt.plot <- ggplot(data = alt.df) + labs(subtitle = gage.name, title = title) +
            geom_point(mapping = aes(x = Year, y = obs.alteration.dir.wyt)) +
            geom_line(mapping = aes(x = Year, y = obs.alteration.dir.wyt, group= NA)) +
            scale_y_discrete(name= "Alteration Status", breaks = c(-1,0,1), labels = c("Likely Altered, Below", "Likely Unaltered", "Likely Altered, Above")) 
          plot(alt.plot)
        }
        
        #else only one point, just plot the point
      }else{
        #plot
        plot <- ggplot(data = data.frame(x = sub.obs[,l], y=sub.pred[,l], timeframe = label.years)) + 
          geom_point(mapping = aes(x = x, y = y, col=timeframe, size=.5)) +
          labs(x = x.name, y= y.name, subtitle = gage.name, title = title) + 
          scale_color_manual(values=c("#e66101", "#5e3c99")) + 
          scale_size(guide=FALSE) + theme(legend.title = element_blank(), legend.position = "bottom", legend.text= element_text(size=10)) +
          guides(colour=guide_legend(override.aes = list(size = 4))) + geom_abline()
      }
      #print plots to screen
      print(plot)
    }
  }
  
  #read in percentiles all for Aliso gage @stp, LSPC current
  gage.percentiles.all <- read.csv("L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/200326_Gauge_Data/daily/FFMs/201020/obs.percentiles.all.csv")
  lspc.current.percentiles.all <- read.csv("L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/200411_Updated_Calibration/WY94-Present/daily/FFMs/201020/pred.percentiles.all.csv")
  #change source2 to appropriate label for boxplots
  lspc.current.percentiles.all$source2 <- rep("LSPC\nCurrent", length(lspc.current.percentiles.all$source2))
  pred.percentiles.all$source2 <- rep("LSPC\nReference", length(pred.percentiles.all$source2))
  ref.percentiles$source2 <- rep("Statewide\nReference", length(ref.percentiles$source2))
  
  ############################
  ###Boxplot comparisons of entire POR LSPC current, LSPC ref, Reference statewide
  #create combined boxplots for each component
  unique.components <- as.character(unique(ffm.labels$title_component))
  for(m in 1:length(unique.components)){
    #subset percentiles based on component m
    ind.comp.m <- grep(unique.components[m], ffm.labels$title_component)
    ffm.names.m <- ffm.labels[ind.comp.m,]
    #gage Aliso @STP current
    sub.obs.cur.comp <- gage.percentiles.all[as.character(gage.percentiles.all$metric) %in%  as.character(ffm.names.m$flow_metric),]
    #lspc current
    sub.pred.current.comp <- lspc.current.percentiles.all[as.character(lspc.current.percentiles.all$metric) %in%  as.character(ffm.names.m$flow_metric),]
    #predicted reference LSPC
    sub.pred.ref.comp <- pred.percentiles.all[as.character(pred.percentiles.all$metric) %in%  as.character(ffm.names.m$flow_metric),]
    #statewide ref
    sub.ref.comp <- ref.percentiles[as.character(ref.percentiles$metric) %in%  as.character(ffm.names.m$flow_metric),]
    
    #subset predicted ref points based on component m
    #sub.pred.ref.comp.pts <- pred.results.ffm.all[,c("Year",as.character(ffm.names.m$flow_metric))]
    #timeframe for predicted points, this will be used for point colors
    #ind.2014.pred <- grep("2014", sub.pred.ref.comp.pts$Year)
    #timeframe.pred <- c(rep("1994-2014", ind.2014.pred), rep("2015-present", length(sub.pred.ref.comp.pts$Year)-ind.2014.pred))
    
    #Boxplots for components
    title <- as.character(ffm.names.m$title_component[1]) #component
    subtitle.bp <- paste0("Aliso @ STP")
    characteristic <- sort(as.character(ffm.names.m$flow_characteristic)) 
    metrics.title <- sort(as.character(ffm.names.m$title_ffm)) #boxplot title
    
    #combine all percentiles dataframes and merge metric label names
    mergeCols <- names(gage.percentiles.all)
    percentiles.cbind.all <- full_join(gage.percentiles.all, lspc.current.percentiles.all, by=mergeCols) %>% 
      full_join(pred.percentiles.all, by=mergeCols) %>% 
      full_join(ref.percentiles, by=mergeCols) %>% 
      merge(ffm.labels, by="metric")
    
    #set levels for source2
    percentiles.cbind.all$source2 <- factor(percentiles.cbind.all$source2, levels= c("Gage", "LSPC\nCurrent", "LSPC\nReference", "Statewide\nReference"))
    
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
        scale_fill_manual(values=c("#a6cee3", "#1f78b4", "#33a02c","#b2df8a")) +
        labs(title=title,x ="", y = "", subtitle = subtitle.bp) +
        theme(legend.position="bottom")
      print(P)
      
    }else{
      #fill color based on entire POR LSPC (add colored points), Gage, Reference
      fill<- percentiles.cbind.all.sub.m$source2
      #All years plots
      P<- ggplot(percentiles.cbind.all.sub.m, aes(x=source2, ymin = p10, lower = p25, middle = p50, upper = p75, ymax = p90, fill=source2)) +
        geom_boxplot(stat = "identity") +  facet_wrap(~title_ffm, scales="free") +
        scale_fill_manual(values=c("#a6cee3", "#1f78b4", "#33a02c","#b2df8a")) +
        labs(title=title,x ="", y = "", subtitle = subtitle.bp) +
        theme(legend.position="bottom", legend.title=element_blank())
      print(P)
      
      sub <- percentiles.cbind.all.sub.m[percentiles.cbind.all.sub.m$metric == "DS_Dur_WS",]
      row.names(sub) <- as.character(sub$source2)
      P<- ggplot(sub, aes(x=source2, ymin = p10, lower = p25, middle = p50, upper = p75, ymax = p90)) +
        geom_boxplot(stat="identity") 
      print(P)
      
    }
  }
  
  
}

plotData <- structure(list(Min = c(250.866197946263, 270.805621355386), `2.5%` = c(1806.60435406315, 
                                                                                   1807.31835620068), `50%` = c(7660.214866495, 7667.78371852935
                                                                                   ), mean = c(8598.13903058242, 8600.3276561399), `97.5%` = c(20678.7670144642, 
                                                                                                                                               20675.0354903488), Max = c(35526.3400756986, 36782.0082581976
                                                                                                                                               )), .Names = c("Min", "2.5%", "50%", "mean", "97.5%", "Max"), row.names = c("median", 
                                                                                                                                                                                                                           "mean"), class = "data.frame")

row.names(plotData) -> plotData$X1
ggplot(plotData, aes(x = X1, ymin=Min, lower=`2.5%`, middle = `50%`, upper = `97.5%`, ymax = Max)) + 
  geom_boxplot(stat="identity")

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






