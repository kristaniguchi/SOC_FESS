#to compare LSPC model output for daily total Q versus using hourly Q aggregated to mean daily

#load library
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
model.hrly.dir <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/200411_Updated_Calibration/WY94-Present/"
model.daily.dir <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/200507_DailyTotalQ_prelimtofuturegcm/"
plot.output.dir.pred <- model.daily.dir

#load in gage, model hrly, model daily
pred.daily <- read.table("L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/200507_DailyTotalQ_prelimtofuturegcm/201020.out", skip=23)
names(pred.daily) <- c("gage", "year", "month", "day", "hour", "min", "precip", "depth", "hyd.radius", "av.vel","flow.cfs")
#format date
date <- paste(pred.daily$month, pred.daily$day, pred.daily$year, sep="/")
pred.daily$date <- date
unique.dates <- unique(date)
#create new data frame with date and total daily flow to go into FFC
data.pred <- data.frame(cbind(unique.dates, pred.daily$flow.cfs))
names(data.pred) <- c("date", "flow")

#run through FFC
COMID <- 20350539                                                                                       
results.pred <- ffcAPIClient::evaluate_alteration(timeseries_df = data.pred, comid = COMID, token = mytoken, plot_output_folder = plot.output.dir.pred)
#predicted results, LSPC daily total q
pred.alteration.all <- results.pred$alteration
pred.percentiles.all <- results.pred$ffc_percentiles
pred.percentiles.all$source2 <- rep("LSPC daily", length(pred.percentiles.all$p10))
pred.results.ffm.all <- results.pred$ffc_results
pred.results.ffm.all$type <- "pred"
pred.drh.data <- results.pred$drh_data.pred
ref.percentiles <- results.pred$predicted_percentiles
#write outputs to dir
write.csv(ref.percentiles, file=paste0(plot.output.dir.pred,"/ref.percentiles.csv"), row.names=FALSE)
write.csv(pred.alteration.all, file=paste0(plot.output.dir.pred,"/pred.alteration.all.csv"), row.names=FALSE)
write.csv(pred.percentiles.all, file=paste0(plot.output.dir.pred,"/pred.percentiles.all.csv"), row.names=FALSE)
write.csv(pred.results.ffm.all, file=paste0(plot.output.dir.pred,"/pred.results.ffm.all.csv"), row.names=FALSE)
write.csv(pred.drh.data, file=paste0(plot.output.dir.pred,"/pred.drh.data.csv"), row.names=FALSE)

#read in ffm results from hourly timestamp
hrly.ffms <- read.csv("L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/200411_Updated_Calibration/WY94-Present/daily/FFMs/201020/pred.results.ffm.all.csv")

#read in ffm results from gage
gage.ffms <- read.csv("L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/200326_Gauge_Data/daily/FFMs/201020/gage.ffms.csv")

#read in percentiles from LSPC ref
lspc.ref.percentiles <- read.csv("L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/200422_Predevelopment_Reference_Condition/WY94-Present/daily/FFMs/201020/pred.ref.lspc.percentiles.all.csv")


#ffm names
ffm.names <- names(hrly.ffms)


#subset data to show overlapping dates
#observed gage POR
no.na.gage <- gage.ffms[-which(is.na(gage.ffms$Peak_10)),]
start.date.obs <- as.character(no.na.gage$Year[1])
end.date.obs <- as.character(no.na.gage$Year[length(no.na.gage$Year)])
obs.wy.por <- paste0("Observed Gage POR: ", start.date.obs, " to ", end.date.obs)


#predicted LSPC POR
no.na.lspc <- hrly.ffms[-which(is.na(hrly.ffms$Peak_10)),]
start.date.pred <- as.character(no.na.lspc$Year[1])
end.date.pred <- as.character(no.na.lspc$Year[length(no.na.lspc$Year)])
pred.wy.por <- paste0("LSPC POR: ", start.date.pred, " to ", end.date.pred)


#subset to overlapping years
start.overall <- max(as.numeric(c(start.date.obs, start.date.pred)))
end.overall <- min(as.numeric(c(end.date.obs, end.date.pred)))
#index of start and end for pred and obs
ind.start.obs <- grep(start.overall, gage.ffms$Year)
ind.end.obs <- grep(end.overall, gage.ffms$Year)
ind.start.pred <- grep(start.overall, pred.results.ffm.all$Year)
ind.end.pred <- grep(end.overall, pred.results.ffm.all$Year)

#subset ffm results for daily predicted results
sub.obs <- gage.ffms[ind.start.obs:ind.end.obs,]
sub.pred.daily <- pred.results.ffm.all[ind.start.pred:ind.end.pred,]
ffm.names <- names(sub.obs)

#subset ffm results for hrly predicted results
sub.pred.hrly <- hrly.ffms[ind.start.pred:ind.end.pred,]

#create new overall subset that has hrly and daily timesteps with corresponding obs ffms
sub.obs.repeat <- rbind(sub.obs, sub.obs)
sub.pred.all <- rbind(sub.pred.daily, sub.pred.hrly)
#create new timestep
timestep <- c(rep("Daily", length(sub.pred.daily$Year)) , rep("Hourly", length(sub.pred.hrly$Year)))

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

label.years.overall <- c(label.years, label.years)

#timestep comparison plot
#Create plots hourly vs obs and daily vs observed

#loop to plot predicted vs obs for each ffm 
for(l in 2:(length(ffm.names)-2)){
  #get flow metric and labels/title for plots
  ffm <- ffm.names[l]
  #find index of ffm in lookup table
  index.ffm <- grep(ffm, ffm.labels$flow_metric)
  #only plot if core FFM
  if(length(index.ffm) > 0){
    #from lookup table find title and axes labels
    x.name <- paste0("Gage Observed", ffm.labels$title_ffm[index.ffm])
    y.name <- paste0("LSPC Predicted", ffm.labels$title_ffm[index.ffm])
    title <- ffm.labels$title_name[index.ffm]
    #if more than 2 points set xlim, ylim to be equal and plot
    if(length(na.omit(cbind(sub.obs[,l], sub.pred.daily[,l])))>1){
      #set xlim and ylim to be equal axes
      subset.daily <- na.omit(cbind(sub.obs[,l],sub.pred.daily[,l]))
      subset.hrly <- na.omit(cbind(sub.obs[,l],sub.pred.hrly[,l]))
      limits <- range(c(subset.daily, subset.hrly))
      #plot
      plot <- ggplot(data = data.frame(x = sub.obs.repeat[,l], y=sub.pred.all[,l], timestep = timestep, timeframe = label.years.overall)) + 
        geom_point(mapping = aes(x = x, y = y, col=timestep, shape = timestep, size=.5)) +
        labs(x = x.name, y= y.name, subtitle = "Aliso @ STP", title = title) + 
        xlim(limits) + ylim(limits) +
        scale_size(guide=FALSE) + 
        scale_color_manual(values=c("#e66101", "#5e3c99")) + 
        theme(legend.title = element_blank(), legend.position = "bottom", legend.text= element_text(size=10)) +
        guides(colour=guide_legend(override.aes = list(size = 4))) + geom_abline()
      
      #else only one point, just plot the point
    }else{
      #plot
      plot <- ggplot(data = data.frame(x = sub.obs.repeat[,l], y=sub.pred.all[,l], timestep = timestep, timeframe = label.years)) + 
        geom_point(mapping = aes(x = x, y = y,  shape = timestep, size=.5)) +
        labs(x = x.name, y= y.name, subtitle = "Aliso @ STP", title = title) + 
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
#read in percentiles from LSPC hourly timestep
lspc.current.percentiles.all.hrly <- read.csv("L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/200411_Updated_Calibration/WY94-Present/daily/FFMs/201020/pred.percentiles.all.csv")
#other percentiles lspc current daily and statewide ref
lspc.percentiles.all.daily <- pred.percentiles.all
statewide.ref.percentiles <- ref.percentiles
#change source2 to appropriate label for boxplots
gage.percentiles.all$source2 <- rep("Gage\nCurrent", length(gage.percentiles.all$source2))
lspc.current.percentiles.all.hrly$source2 <- rep("LSPC\nCurrent\nHourly", length(lspc.current.percentiles.all.hrly$source2))
lspc.percentiles.all.daily$source2 <- rep("LSPC\nCurrent\nDaily", length(lspc.percentiles.all.daily$source2))
lspc.ref.percentiles$source2 <- rep("LSPC\nReference", length(lspc.ref.percentiles$source2))
statewide.ref.percentiles$source2 <- rep("Statewide\nReference", length(statewide.ref.percentiles$comid))



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
  #lspc current hourly
  sub.pred.current.hrly.comp <- lspc.current.percentiles.all.hrly[as.character(lspc.current.percentiles.all.hrly$metric) %in%  as.character(ffm.names.m$flow_metric),]
  #lspc current daily
  sub.pred.current.daily.comp <- lspc.percentiles.all.daily[as.character(lspc.percentiles.all.daily$metric) %in%  as.character(ffm.names.m$flow_metric),]
  #predicted reference LSPC
  sub.pred.ref.comp <- lspc.ref.percentiles[as.character(lspc.ref.percentiles$metric) %in%  as.character(ffm.names.m$flow_metric),]
  #statewide ref
  sub.ref.comp <- statewide.ref.percentiles[as.character(statewide.ref.percentiles$metric) %in%  as.character(ffm.names.m$flow_metric),]
  

  #Boxplots for components
  title <- as.character(ffm.names.m$title_component[1]) #component
  subtitle.bp <- paste0("Aliso @ STP")
  characteristic <- sort(as.character(ffm.names.m$flow_characteristic)) 
  metrics.title <- sort(as.character(ffm.names.m$title_ffm)) #boxplot title
  
  #combine all percentiles dataframes and merge metric label names
  mergeCols <- names(gage.percentiles.all)
  percentiles.cbind.all <- full_join(gage.percentiles.all, lspc.current.percentiles.all.hrly, by=mergeCols) %>% 
    full_join(lspc.percentiles.all.daily, by=mergeCols) %>% 
    full_join(lspc.ref.percentiles, by=mergeCols) %>% 
    full_join(statewide.ref.percentiles, by=mergeCols) %>% 
    merge(ffm.labels, by="metric")
  
  #set levels for source2
  percentiles.cbind.all$source2 <- factor(percentiles.cbind.all$source2, levels= c("Gage\nCurrent", "LSPC\nCurrent\nHourly", "LSPC\nCurrent\nDaily","LSPC\nReference", "Statewide\nReference"))
  
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
      scale_fill_manual(values=c("#d1e5f0", "#67a9cf", "#2166ac","#33a02c","#b2df8a")) +
      labs(title=title,x ="", y = "", subtitle = subtitle.bp) +
      theme(legend.position="bottom")
    print(P)
    
  }else{
    #fill color based on entire POR LSPC (add colored points), Gage, Reference
    fill<- percentiles.cbind.all.sub.m$source2
    #All years plots
    P<- ggplot(percentiles.cbind.all.sub.m, aes(x=source2, ymin = p10, lower = p25, middle = p50, upper = p75, ymax = p90, fill=source2)) +
      geom_boxplot(stat = "identity") +  facet_wrap(~title_ffm, scales="free") +
      scale_fill_manual(values=c("#d1e5f0", "#67a9cf", "#2166ac","#33a02c","#b2df8a")) +
      labs(title=title,x ="", y = "", subtitle = subtitle.bp) +
      theme(legend.position="bottom", legend.title=element_blank())
    print(P)
    
  }
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
        
        for(n in 1:length(gage.ffms$Year)){
          wyt.n <- gage.ffms$WYT_Gage[n]
          #get ffm percentiles for wyt.n
          ref.percentiles.metric.wyt <- ref.percentiles.metric[ref.percentiles.metric$wyt == wyt.n,]
          #if current ffm is NA, alteartion NA, else if within p10-p90, no alteration, if outside likely altered
          if(is.na(gage.ffms[n,ffm])){
            obs.alteration.wyt[n] <- NA
            obs.alteration.dir.wyt[n] <- NA
          }else{
            if(gage.ffms[n,ffm] >= ref.percentiles.metric.wyt$p10 & gage.ffms[n,ffm] <= ref.percentiles.metric.wyt$p90){
              obs.alteration.wyt[n] <- "Likely unaltered"
              obs.alteration.dir.wyt[n] <- 0
            }else{
              obs.alteration.wyt[n] <- "Likely altered"
              #determine direction (-1 is depleted, 1 is augmented)
              if(gage.ffms[n,ffm] < ref.percentiles.metric.wyt$p10){
                obs.alteration.dir.wyt[n] <- -1
              }else{
                obs.alteration.dir.wyt[n] <- 1
              }
            }
          }
        }
        
        #alteration data.frame and 
        alt.df <- data.frame(Year = as.numeric(gage.ffms$Year), ffm = rep(ffm, length(gage.ffms$Year)), wyt = gage.ffms$WYT_Gage, metric.obs = gage.ffms[,ffm], obs.alteration.wyt = obs.alteration.wyt, obs.alteration.dir.wyt = as.factor(obs.alteration.dir.wyt))
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
    
  }
}



