#Future scenario evaluation - do we see a difference at the flow metric level?
  #1. Synthesis boxplots for every reach comparing baseline condition, each gcm
  #2. Heat maps of alteration for each gcm
  #3. Alteration maps (?)

############################################################
#directories

#future climate scenario ffm outputs directory to loop through
future.dir <- "W:/SOC_FlowEcologyStudy/FutureClimateScenarios/FFMs/"

#future subdirs
future.subdirs <- list.files(future.dir, full.names = TRUE)

##Functional flow metric names and labels for plots
filename <- ("L:/CA  E-flows framework_ES/Misc/Functional Flows metrics/functional_flow_metric_modeling/all_metric_def_list_FFMs_v2.csv")
ffm.labels <- read.csv(filename)
ffm.labels$metric <- ffm.labels$flow_metric



############################################################
#1. Synthesis boxplots for every reach comparing baseline condition, each gcm and 

#loop through each reach and create boxplots of baseline and future gcm percentiles

#get the future results and baseline results filenames (all consistent for every reach)
files.all <- list.files(future.subdirs[1])
#find future results
#ind.future.results <- grep("future.results.ffm.all", files.all)
#future.results.fname <- files.all[ind.future.results]
#baseline results filename
#baseline.results.fname <- "historical.results.ffm.all.csv"

#find percentiles for future and baseline boxplot comparisons
ind.future.percentiles <- grep("future.percentiles.all.", files.all)
future.percentiles.fname <- files.all[ind.future.percentiles]
##########update this to ind and list all of the baseline names
baseline.percentile.fname <- "historical.percentiles.all.csv"
#gcn namesCNRM-CM5
gcm <- c("CanESM2", "CCSM4", "CNRM-CM5", "MIROC5", "Baseline")
#reach name
reach <- list.files(future.dir)


for (i in 1:length(future.subdirs)){
  #set working directory to reach i
  setwd(future.subdirs[i])
  
  #read in baseline and future percentiles for gcm 1 and append future percentiles to data frame
  percentiles <- read.csv(baseline.percentile.fname[1])
  #baseline name
  baseline.name <- paste0(gcm[1], "\n1975-2005")
  percentiles$Scenario <- rep(baseline.name, length(percentiles$p10))
  #read in future percentiles
  percentiles.future <- read.csv(future.percentiles.fname[1])
  future.name <- paste0(gcm[1], "\n2031-2060")
  percentiles.future$Scenario <- rep(future.name, length(percentiles.future$p10))
  #append to baseline dataframe
  percentiles <- data.frame(rbind(percentiles, percentiles.future))
  
  #read in future and baseline percentiles for remaining gcms and append to percentiles df
  for(j in 2:length(future.percentiles.fname)){
    
    #read in reference data for gcm j
    reference.j <- read.csv(reference.percentiles.fname[j])
    #scenario name
    name <- paste0(gcm[j], "\n1975-2005")
    reference.j$Scenario <- rep(name, length(percentiles.reference$p10))
    
    #read in future data for gcm j
    future.j <- read.csv(future.percentiles.fname[j])
    #scenario name
    name <- paste0(gcm[j], "\n2031-2060")
    future.j$Scenario <- rep(name, length(percentiles.future$p10))
    
    #append future and ref to percentiles df
    percentiles <- data.frame(rbind(percentiles, future.j, reference.j))
  }
  
  #write the percentiles df all
  fname.all <- paste0(reach[i], "_percentiles_ALL_gcms_baseline_future.csv")
  #create new column replaceing \n with a space
  percentiles$Scenario2 <- gsub("\n", "_", percentiles$Scenario)
  write.csv(percentiles, file=fname.all)
  
  
  #Boxplots of alteration by GCM baseline and future for every flow metric
  
  for (k in 1:length())
  
  
  
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
    x.name <- paste0("Observed", ffm.labels$title_ffm[index.ffm])
    y.name <- paste0("Predicted", ffm.labels$title_ffm[index.ffm])
    title <- ffm.labels$title_name[index.ffm]
    #if more than 2 points set xlim, ylim to be equal and plot
    if(length(sub.obs[,l])>1){
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

############################
###Boxplot comparisons of entire POR LSPC (add colored points), Gage, Reference
#create combined boxplots for each component
unique.components <- as.character(unique(ffm.labels$title_component))
for(m in 1:length(unique.components)){
  #subset percentiles based on component m
  ind.comp.m <- grep(unique.components[m], ffm.labels$title_component)
  ffm.names.m <- ffm.labels[ind.comp.m,]
  sub.obs.comp <- obs.percentiles.all[as.character(obs.percentiles.all$metric) %in%  as.character(ffm.names.m$flow_metric),]
  sub.pred.comp <- pred.percentiles.all[as.character(pred.percentiles.all$metric) %in%  as.character(ffm.names.m$flow_metric),]
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
      scale_fill_manual(values=c("#a6cee3", "#1f78b4", "#b2df8a")) +
      labs(title=title,x ="", y = "", subtitle = subtitle.bp) 
    print(P)
    
  }else{
    #fill color based on entire POR LSPC (add colored points), Gage, Reference
    fill<- percentiles.cbind.all.sub.m$source2
    #All years plots
    P<- ggplot(percentiles.cbind.all.sub.m, aes(x=source2, ymin = p10, lower = p25, middle = p50, upper = p75, ymax = p90, fill=source2)) +
      geom_boxplot(stat = "identity") +  facet_wrap(~title_ffm, scales="free") +
      scale_fill_manual(values=c("#a6cee3", "#1f78b4", "#b2df8a")) +
      labs(title=title,x ="", y = "", subtitle = subtitle.bp) 
    print(P)
    
  }
}





############################################################
#2. create overall plots of baseline and future flow metrics for Aliso, color coded by GCM


############################################################
#3. heat maps of alteration for each gcm


############################################################
#4. calc degree of alteration for those that were categorized


############################################################
