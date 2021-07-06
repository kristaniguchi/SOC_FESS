#Flow component alteration map for future scenarios!
  #Make change maps using percent change from hist to current and current alteration direction
  #if it's likely altered high and percent change increases --> worse
  #if it's likely altered low and percent change decreases --> worse
  #if it's opposite to two conditions, then conditions could improve
  #if it's likely unaltered --> could be uncertain

#install.packages("ggsn")
#install.packages("ggmap")
#install.packages("mapview")
#install.packages("geosphere")
#install.packages("rgeos")
#to install spDataLarge
#devtools::install_github("robinlovelace/geocompr")
library(spData)
#install.packages("spDataLarge", repos = "https://nowosad.github.io/drat/", type = "source")
library(spDataLarge)

library(tidyverse)
library(readxl)
library(sf)
library(ggsn)
library(ggmap)
library(mapview)
library(spData)      
library(spDataLarge)
library(geosphere)
library(rgeos)


#####
########

#load in spatial data
#subbasin polygon shapefile - updated shapefile with Bell Canyon subbasin added
basins <- st_read("data/Agg_Boundaries_v14.shp", quiet = T)
#reach polylines
reaches <- st_read('data/reaches_forSCCWRP_Aliso.shp', quiet = T)

#lookuptable to convert subbasin codes for model output subbasin names
subbasin_lookup <- read.csv(file="L:/San Juan WQIP_KTQ/Data/SpatialData/SOC_FESS_Subbasin_NHD.COMID_Lookup_Table.csv") %>% 
  select(Subbasin, new.subbasinname) %>% 
  rename(New_Name = Subbasin,
         subbasin.model = new.subbasinname)


#read in the alteration direction CSCI/ASCI df
csci.asci.alt.direction <- read.csv("C:/Users/KristineT.SCCWRP2K/SCCWRP/SOC WQIP - Flow Ecology Study - General/Tier2_analysis/09_altered_metrics_direction_current_June2021.csv") %>% 
  rename(subbasin.model = site,
         metric = Hydro_Metric) %>% 
  left_join(subbasin_lookup, by = "subbasin.model")

#alteration directory
alt.dir.name <- "Future_Climate_Aliso_ALL_lowflowbias"
alteration.dir <- paste0("L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_flowalteration_assessment/", alt.dir.name)

#set directory for output plots
dir.output.name <- "AlterationMaps"
dir.output <- paste0(alteration.dir, "/", dir.output.name)
dir.create(dir.output)
#directory for level 1 plots
dir.output.1 <- paste0(dir.output, "/Level1")
dir.create(dir.output.1)
#directory for level 2 plots
dir.output.2 <- paste0(dir.output, "/Level2")
dir.create(dir.output.2)


#Load percent change historical to mid-century csv
pct.change.data <- read.csv("L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_flowalteration_assessment/Future_Climate_Aliso_ALL_lowflowbias/pct.change.percentiles.FFM.future.climate.all.scenarios.Aliso.ALL.lowflowbias.csv")

#filter to p50 pct change only
#subbasin.model.p50 <- pct.change.data %>% 
#  filter(percentile == "p50")

#remove all peak mag metrics
pct.change.data <- pct.change.data[-grep("Peak_10", pct.change.data$metric),]
pct.change.data <- pct.change.data[-grep("Peak_2", pct.change.data$metric),]
pct.change.data <- pct.change.data[-grep("Peak_5", pct.change.data$metric),]
#remove peak timing metric
pct.change.data <- pct.change.data[-grep("Peak_Tim", pct.change.data$metric),]


#read in FFM alteration statuses under current conditions
ffm.alteration.curr <- read.csv("L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_flowalteration_assessment/Aliso_Oso_SmCk_SanJuan_all_lowflowbias/ffm_alteration.df.overall.join.Aliso.Oso.SmallCreeks.SanJuanLSPC.lowflowbias.csv")
#exclude peak mag metrics
ffm.alteration.curr <- ffm.alteration.curr[ffm.alteration.curr$title_component != "Peak Flow Magnitude" , ]
#subset to aliso watersheds only!
#take first 3 digits of subbasin.model
subbasin.digits3 <- as.numeric(substr(ffm.alteration.curr$subbasin.model, 1, 3))
#subset to subbasins that start with 201-207 (Aliso only)
ffm.alteration.curr <- ffm.alteration.curr[subbasin.digits3 >= 201 &  subbasin.digits3 <= 207, ]

#Combine the pct change p50 to the ffm.alteration.curr
ffm.alt.curr.join <- pct.change.data %>% 
  left_join(ffm.alteration.curr, by = c("subbasin.model", "metric"))


#for every subbasin.gcm.metric.percentile, if alteration direction is high and abs(pct.change < -0.1)

ffm.alt.curr.join$future.category <- case_when(
  #if alteration direction is high and percent change is less than -10%, possible improvement
  ffm.alt.curr.join$alteration.direction == "high" & ffm.alt.curr.join$pct.change < -0.1 ~ "Possible Improvement",
  #if alteration direction is low and percent change is more than 10%, possible improvement
  ffm.alt.curr.join$alteration.direction == "low" & ffm.alt.curr.join$pct.change > 0.1 ~ "Possible Improvement",
  
  #if alteration direction is high and percent change is more than 10%, possible degradation
  ffm.alt.curr.join$alteration.direction == "high" & ffm.alt.curr.join$pct.change > 0.1  ~ "Possible Degradation",
  #if alteration direction is low and percent change is less than -10%, possible degradation
  ffm.alt.curr.join$alteration.direction == "low" & ffm.alt.curr.join$pct.change < -0.1 ~ "Possible Degradation",
  
  #if alteration direction is high and percent change is less than or = 10% and >= -10%, uncertain
  ffm.alt.curr.join$alteration.direction == "high" & ffm.alt.curr.join$pct.change <= 0.1 & ffm.alt.curr.join$pct.change >= -0.1 ~ "Uncertain",
  #if alteration direction is low and percent change is less than or = 10% and >= -10%, uncertain
  ffm.alt.curr.join$alteration.direction == "low" & ffm.alt.curr.join$pct.change <= 0.1 & ffm.alt.curr.join$pct.change >= -0.1 ~ "Uncertain",
  
  
  #if alteration direction is none found (unaltered) then uncertain
  ffm.alt.curr.join$alteration.direction == "none_found"   ~ "Uncertain",
  #if alteration direction is NA, then NA
  is.na(ffm.alt.curr.join$alteration.direction)  ~ "Uncertain",
)

################################################################
#Loop to make maps for each FFM and GCM, percentile % change
#create gcm.metric.percentile column
ffm.alt.curr.join$gcm.metric.percentile <- paste(ffm.alt.curr.join$gcm, ffm.alt.curr.join$metric, ffm.alt.curr.join$percentile, sep=" ")
#find unique gcm.metric.percentile to loop through each combo will have a map
unique.gcm.metric.percentile <- unique(ffm.alt.curr.join$gcm.metric.percentile)
#filter to only p50 for now
ind.p50 <- grep("p50", unique.gcm.metric.percentile)
unique.gcm.metric.percentile <- unique.gcm.metric.percentile[ind.p50]

#join subbasin shapefile with ffm.alt.curr.join
#but first set subbasin to New_Name to match basin id
ffm.alt.curr.join <- ffm.alt.curr.join %>% 
  rename(New_Name = subbasin)
#join wtih basins based on New_Name
ffm.alt.curr.join <- ffm.alt.curr.join %>% 
  inner_join(basins, by = c('New_Name'))
ffm.alt.curr.join


#set colors for future categories
color.future <- c("#4575b4", "grey", "#a50f15", "white")
categories.future <- c("Possible Improvement", "Uncertain", "Possible Degradation", "NA")
key <- data.frame(cbind(color.future, categories.future))

#gcm lookup labels
GCM.name <- c("CanESM2", "CNRM-CM5", "CCSM4", "MIROC5")
GCM.labels <- c("CanESM2 (wetter/hotter)", "CNRM-CM5 (wetter/cooler)", "CCSM4 (moderate)", "MIROC5 (drier/other)")
gcm.lookup <- data.frame(cbind(GCM.name, GCM.labels))


for(z in 1:length(unique.gcm.metric.percentile)) {
  #subset data to unique.gcm.metric.percentile i
  subset <- ffm.alt.curr.join %>% 
    filter(gcm.metric.percentile == unique.gcm.metric.percentile[z])
  
  #metric and gcm
  metric.z <- unique(subset$metric)
  gcm.z <- unique(subset$gcm)
  #get gcm title
  gcm.title <- gcm.lookup$GCM.labels[gcm.lookup$GCM.name == gcm.z]
  
  
  #subset the color key
  key.sub <- key[key$categories.future %in% subset$future.category,]
  
  #save as factor
  key.sub$categories.future <- factor(key.sub$categories.future, levels = key.sub$categories.future)
  subset$future.category <- factor(subset$future.category, levels = key.sub$categories.future)
  
  #watershed plot for future level 1
  #filled alteration plots
  alt.plot <- ggplot() +
    geom_sf(data = subset, color= "#969696", aes(fill=future.category, geometry = geometry)) +
    scale_fill_manual(name = "Future Category", labels = key.sub$categories.future, values=key.sub$color.future) +
    geom_sf(data = reaches, color = "#67a9cf", size = 0.5) +
    labs(title=unique(subset$title_name), subtitle=gcm.title, x ="", y = "") + 
    #annotation_scale() +
    #annotation_north_arrow(pad_y = unit(0.9, "cm"),  height = unit(.8, "cm"),
                           #width = unit(.8, "cm")) +
    theme(panel.background = element_rect(fill = "white"),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.grid = element_line(color = "white", size = 0.8))
  
  #print
   #print(alt.plot)
  
  #write plot
  #save as jpg
  plot.fname <- paste0(dir.output.1, "/", metric.z, "_",gcm.z, "_alteration.map.jpg")
  ggsave(alt.plot, file=plot.fname, dpi=300, height=4, width=6)
}



########################################################################
#Level 2 - future climate effects to CSCI/ASCI metrics
#use the same definitions as above but focus on only the most important FFMS for CSCI/ASCI

#take first 3 digits of csci.asci.alt.direction
subbasin.digits3 <- as.numeric(substr(csci.asci.alt.direction$subbasin.model, 1, 3))
#subset to subbasins that start with 201-207 (Aliso only)
csci.asci.alt.direction <- csci.asci.alt.direction[subbasin.digits3 >= 201 &  subbasin.digits3 <= 207, ]

#join wtih basins based on New_Name
csci.asci.alt.direction2 <- csci.asci.alt.direction %>% 
  inner_join(basins, by = c('New_Name'))
csci.asci.alt.direction2


#loop through for CSCI and ASCI, create plots similar to above 
for(i in c("CSCI", "ASCI")){
  #subset to CSCI or ASCI
  level2.sub <- csci.asci.alt.direction2 %>% 
    filter(Biol == i)
  #unique metrics level 2 sub
  unique.metrics2 <- unique(level2.sub$metric)
  
  #subset percent change to only metrics in level2.sub
  pct.change.data.sub <- pct.change.data[pct.change.data$metric %in% unique.metrics2,]
  
  #Combine the pct change p50 to the ffm.alteration.curr
  pct.change.level2.join <- pct.change.data.sub %>% 
    left_join(level2.sub, by = c("subbasin.model", "metric"))
  

  #for every subbasin.gcm.metric.percentile, if alteration direction is high and abs(pct.change < -0.1)
  
  pct.change.level2.join$future.category <- case_when(
    #if alteration direction is high and percent change is less than -10%, possible improvement
    pct.change.level2.join$Direction == "High" & pct.change.level2.join$pct.change < -0.1 ~ "Possible Improvement",
    #if alteration direction is low and percent change is more than 10%, possible improvement
    pct.change.level2.join$Direction == "Low" & pct.change.level2.join$pct.change > 0.1 ~ "Possible Improvement",
    
    #if alteration direction is high and percent change is more than 10%, possible degradation
    pct.change.level2.join$Direction == "High" & pct.change.level2.join$pct.change > 0.1  ~ "Possible Degradation",
    #if alteration direction is low and percent change is less than -10%, possible degradation
    pct.change.level2.join$Direction == "Low" & pct.change.level2.join$pct.change < -0.1 ~ "Possible Degradation",
    
    #if alteration direction is high and percent change is less than or = 10% and >= -10%, uncertain
    pct.change.level2.join$Direction == "High" & pct.change.level2.join$pct.change <= 0.1 & pct.change.level2.join$pct.change >= -0.1 ~ "Uncertain",
    #if alteration direction is low and percent change is less than or = 10% and >= -10%, uncertain
    pct.change.level2.join$Direction == "Low" & pct.change.level2.join$pct.change <= 0.1 & pct.change.level2.join$pct.change >= -0.1 ~ "Uncertain",
    
    
    #if alteration direction is high and percent change is less than -10%, possible improvement
    pct.change.level2.join$Direction == "Uncertain"   ~ "Uncertain",
    #if alteration direction is low and percent change is more than 10%, possible improvement
    is.na(pct.change.level2.join$Direction)  ~ "NA",
  )
  
  #change future category NA to "Uncertain"
  pct.change.level2.join$future.category[is.na(pct.change.level2.join$future.category)] <- "Uncertain"

  ################
  #Loop to make maps for each FFM and GCM, percentile % change
  #create gcm.metric.percentile column
  pct.change.level2.join$gcm.metric.percentile <- paste(pct.change.level2.join$gcm, pct.change.level2.join$metric, pct.change.level2.join$percentile, sep=" ")
  #find unique gcm.metric.percentile to loop through each combo will have a map
  unique.gcm.metric.percentile <- unique(pct.change.level2.join$gcm.metric.percentile)
  #filter to only p50 for now
  ind.p50 <- grep("p50", unique.gcm.metric.percentile)
  unique.gcm.metric.percentile <- unique.gcm.metric.percentile[ind.p50]
  
  #set colors for future categories
  color.future <- c("#4575b4", "grey", "#a50f15", "white")
  categories.future <- c("Possible Improvement", "Uncertain", "Possible Degradation", "NA")
  key <- data.frame(cbind(color.future, categories.future))
  
  #gcm lookup labels
  GCM.name <- c("CanESM2", "CNRM-CM5", "CCSM4", "MIROC5")
  GCM.labels <- c("CanESM2 (wetter/hotter)", "CNRM-CM5 (wetter/cooler)", "CCSM4 (moderate)", "MIROC5 (drier/other)")
  gcm.lookup <- data.frame(cbind(GCM.name, GCM.labels))
  
  
  for(y in 1:length(unique.gcm.metric.percentile)) {
    #subset data to unique.gcm.metric.percentile i
    subset2 <- pct.change.level2.join %>% 
      filter(gcm.metric.percentile == unique.gcm.metric.percentile[y])

    #metric and gcm
    metric.z <- unique(subset2$metric)
    gcm.z <- unique(subset2$gcm)
    #get gcm title
    gcm.title <- gcm.lookup$GCM.labels[gcm.lookup$GCM.name == gcm.z]
    
    
    #subset the color key
    key.sub <- key[key$categories.future %in% subset2$future.category,]
    
    #save as factor
    key.sub$categories.future <- factor(key.sub$categories.future, levels = key.sub$categories.future)
    subset2$future.category <- factor(subset2$future.category, levels = key.sub$categories.future)
    
    #watershed plot for future level 1
    unique2 <- unique(subset2$future.category)
    test <- subset2[subset2$future.category == "Possible Degradation",]
    test2 <- subset2[subset2$future.category == "Uncertain",]
    test.all <- data.frame(subset2)
    

    #filled alteration plots
    alt.plot <- ggplot() +
      #fill in the aliso subbasins that were previously unaltered (not in level2 df) as uncertain
      geom_sf(data = subset, color= "#969696", fill="grey", aes(geometry = geometry)) +
      geom_sf(data = subset2, color= "#969696", aes(fill=future.category, geometry = geometry)) +
      scale_fill_manual(name = "Future Category", labels = key.sub$categories.future, values=key.sub$color.future) +
      geom_sf(data = reaches, color = "#67a9cf", size = 0.5) +
      labs(title=unique(subset2$metric), subtitle=gcm.title, x ="", y = "") + 
      #annotation_scale() +
      #annotation_north_arrow(pad_y = unit(0.9, "cm"),  height = unit(.8, "cm"),
      #width = unit(.8, "cm")) +
      theme(panel.background = element_rect(fill = "white"),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            panel.grid = element_line(color = "white", size = 0.8))
    
    #print
    print(alt.plot)
    
    #write plot
    #save as jpg
    plot.fname <- paste0(dir.output.2, "/", i,"_", metric.z, "_",gcm.z, "_level2_alteration.map.jpg")
    ggsave(alt.plot, file=plot.fname, dpi=300, height=4, width=6)
  }
  
  
  
}











#########OLD SCRIPT BELOW
#load in data

#component alteration data
#fname1 = "data/summary_component_alteration.csv" #update to directory
fname <- "W:/SOC_FlowEcologyStudy/FutureClimateScenarios/component.alteration.summary.Aliso.gcm.all.rcp8.5.csv"
comp_alt <- read.csv(fname)
#comp_alt <- read.csv(file = "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_flowalteration_assessment/summary_component_alteration.csv")

#create New_Name column with subbasin id to match polygon layer
comp_alt$New_Name <- comp_alt$subbasin
#omit first 5 rows - those are added subbasins, we have individual so no need
comp_alt <- comp_alt[6:length(comp_alt$COMID),]
#set levels for flow component so it goes in sequence of WY
comp_alt$flow_component <- factor(comp_alt$flow_component, levels = c("Fall pulse flow", "Wet-season base flow", "Peak flow", "Spring recession flow", "Dry-season base flow"))

#subbasin polygon shapefile
basins <- st_read("data/subbasin_boundaries_forSCCWRP.shp", quiet = T)
#basins <- st_read('L:/San Juan WQIP_KTQ/Data/SpatialData/Model_Subbasins_Reaches/New_Subbasins_forSCCWRP_12062019/New_Subbasins_forSCCWRP/subbasin_boundaries_forSCCWRP.shp', quiet = T)
basins2 <- basins %>% 
  inner_join(comp_alt, by = c('New_Name'))
basins2


#reach polylines
reaches <- st_read('data/reaches_forSCCWRP.shp', quiet = T)
#reaches <- st_read('L:/San Juan WQIP_KTQ/Data/SpatialData/Model_Subbasins_Reaches/New_Subbasins_forSCCWRP_12062019/New_Subbasins_forSCCWRP/reaches_forSCCWRP.shp', quiet = T)


#join subbasin shapefile with component alteration
comp_alt <- comp_alt %>% 
  inner_join(basins, by = c('New_Name'))
comp_alt


#find centroids of subbasins
#unique.subbasins
unique.subbasins <- unique(comp_alt$subbasin)

centroid <- st_centroid(basins) 

centroids <- 
  centroid$geom %>% 
  purrr::map(.,.f = function(x){data.frame(long = x[1],lat = x[2])}) %>% 
  bind_rows %>% data.frame(New_Name = centroid$New_Name) %>% 
  left_join(comp_alt,by = "New_Name")
#omit NA subbasins that were not analyzed
centroids <- centroids[-which(is.na(centroids$COMID)),]

#plot the subbasins

plot <- ggplot(basins2) + 
  geom_sf(color = "grey", fill="white") +
  #geom_point(data=centroids, aes(x=long, y=lat), size=1, alpha=6/10) +
  xlab("") + ylab("")

plot

##########bar plots
#Loop through each gcm and create barplots by gcm

unique.sites <- unique(centroids$New_Name)
#gcn namesCNRM-CM5
gcm <- c("CanESM2", "CCSM4", "CNRM-CM5", "MIROC5")


for(k in 1:4){
  #subset for gcm k
  centroids.sub <- centroids[centroids$gcm == gcm[k],]
  
  bar.testplot_list <- 
    lapply(1:length(unique.sites), function(i) { 
      gt_plot <- ggplotGrob(
        ggplot(centroids.sub[centroids.sub$New_Name == unique.sites[i],])+
          geom_bar(aes(factor(flow_component),n_ffm_altered,group=New_Name, fill = factor(flow_component)), 
                   position='dodge',stat='identity', color = NA) +
          scale_fill_manual(name = "Altered Flow Components", labels = c("Fall pulse flow", "Wet-season base flow", "Peak flow", "Spring recession flow", "Dry-season base flow"), values = c("#1b9e77","#d95f02","#7570b3","#e7298a","#66a61e")) +
          labs(x = NULL, y = NULL) + 
          theme(legend.position = "none", rect = element_blank(),
                line = element_blank(), text = element_blank()) 
      )
      panel_coords <- gt_plot$layout[gt_plot$layout$name == "panel",]
      gt_plot[panel_coords$t:panel_coords$b, panel_coords$l:panel_coords$r]
    })
  
  #add barplots to intial map with annotation_custom
  bar_annotation_list <- lapply(1:length(unique.sites), function(i) 
    annotation_custom(bar.testplot_list[[i]], 
                      xmin = unique(centroids$long[centroids$New_Name == unique.sites[i]]) - 1.3e3,
                      xmax = unique(centroids$long[centroids$New_Name == unique.sites[i]]) + 1.3e3,
                      ymin = unique(centroids$lat[centroids$New_Name == unique.sites[i]]) - 1.3e3,
                      ymax = unique(centroids$lat[centroids$New_Name == unique.sites[i]]) + 1.3e3) )
  
  
  #redo plot with title of gcm and legend
  plot2 <- ggplot(basins2) + 
    geom_sf(color = "grey", fill="white") +
    xlab("") + ylab("") + ggtitle(paste0("Component Alteration: ", gcm[k]))
  plot2
  
  result_plot <- Reduce(`+`, bar_annotation_list, plot2)
  result_plot
  #save plots
  file.map <- paste0("W:/SOC_FlowEcologyStudy/FutureClimateScenarios/alterationmap_", gcm[k], ".jpg")
  ggsave(result_plot, filename=file.map, dpi=300, height=6.5, width=5.5)
  
  
  #create separate bar plot for just the first item subbasin (for zoom plot)
  #y axis integers only
  # A function factory for getting integer y-axis values.
  integer_breaks <- function(n = 5, ...) {
    fxn <- function(x) {
      breaks <- floor(pretty(x, n, ...))
      names(breaks) <- attr(breaks, "labels")
      breaks
    }
    return(fxn)
  }
  
  bar <-  ggplot(centroids.sub[centroids.sub$New_Name == unique.sites[1],]) +
    geom_bar(aes(factor(flow_component),n_ffm_altered,group=New_Name, fill = factor(flow_component)), 
             position='dodge',stat='identity', color = NA) +
    scale_fill_manual(name = "Flow Components", labels = c("Fall pulse flow", "Wet-season base flow", "Peak flow", "Spring recession flow", "Dry-season base flow"), values = c("#1b9e77","#d95f02","#7570b3","#e7298a","#66a61e")) +
    labs(x = "Flow Components", y = "Number of Altered Metrics") +
    theme(panel.background = element_rect(fill = "white")) +
    scale_y_continuous(breaks= integer_breaks())
  bar
  file.bar <- paste0("W:/SOC_FlowEcologyStudy/FutureClimateScenarios/zoom_barplot_compalteration_", gcm[k], ".jpg")
  ggsave(bar, filename=file.bar, dpi=300, height=5, width=8.5)

  
}


#individual zoomed plot subbasin 1

sub1 <- ggplot(basins2[basins2$New_Name == unique.sites[1],]) + 
  geom_sf(color = "grey", fill="white") +
  xlab("") + ylab("")

sub1
ggsave(sub1, filename="W:/SOC_FlowEcologyStudy/FutureClimateScenarios/subbasin1_compalteration.jpg", dpi=300, height=8, width=8)



###################
#heatmap of alteration: component vs. flow characteristics

#install packages
#install.packages("ztable")
library(ztable)
if(!require(devtools)) install.packages("devtools")
devtools::install_github("cardiomoon/ztable")

require(moonBook)
x=table(acs$Dx,acs$smoking)
x

#read in alteration summary table
data <- read.csv(file="W:/SOC_FlowEcologyStudy/FutureClimateScenarios/ffm.alteration.summary.Aliso.gcm.all.rcp8.5.csv")
#subset to altered only
altered <- data[data$alteration.status == "likely_altered",]
#remove NA
altered <- altered[-which(is.na(altered$subbasin)),]

#loop through each gcm
for(j in 1:4){
  #subset altered df to gcm j
  altered.sub <- altered[altered$gcm == gcm[j],]
  
  #subset so if there is one altered characteristic per component (remove duplicates from one subbasin so we get number of subbasins with altered flow characteristics)
  unique.altered.sites <- unique(altered.sub$subbasin.model)
  #create empty df that will be filled
  altered.new <- altered.sub[1,]
  #fill with NA for first row to be removed later
  altered.new[1,] <- NA
  altered.new$comp.characteristic <- NA
  
  for(i in 1:length(unique.altered.sites)){
    sub1 <- altered.sub[altered.sub$subbasin.model == unique.altered.sites[i],]
    #create vector component_characteristics in sub1
    sub1$comp.characteristic <- paste0(sub1$flow_characteristic, "_", sub1$flow_component)
    #remove duplicated rows based on comp.characteristic but keep only unique/distinct rows from a data frame
    sub1 <- sub1 %>% dplyr::distinct(flow_characteristic,flow_component,  .keep_all = TRUE)
    #save sub1 into new df
    altered.new <- rbind(altered.new, sub1)
  }
  #remove first NA row
  altered.new <- altered.new[2:length(altered.new$subbasin),]
  
  #calculate number in each category
  ffm_summary <- data.frame(aggregate(altered.new, by = altered.new[c('flow_characteristic', 'flow_component')], length))
  
  
  #create table for heatmap
  #dev.off()
  mine.heatmap <- ggplot(data = ffm_summary, mapping = aes(x = flow_characteristic,
                                                           y = factor(flow_component, levels =  c("Fall pulse flow", "Wet-season base flow", "Peak flow", "Spring recession flow", "Dry-season base flow")),
                                                           fill = subbasin)) +
    geom_tile() +
    ylab(label = "Flow Component") + xlab(label="Hydrograph Element") +
    scale_fill_gradient(name = "Number of\nAltered Subbasins",
                        low = "#fef0d9",
                        high = "#b30000") +
    ggtitle(label = paste0("Altered Subbasins in Aliso: ", gcm[j])) + theme_light() +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold"))
  
  mine.heatmap
  #save heatmap
  filename.heatmap <- paste("W:/SOC_FlowEcologyStudy/FutureClimateScenarios/alteration_heatmap_", gcm[j], ".jpg")
  ggsave(mine.heatmap, file=filename.heatmap, dpi=300, height=8, width=12)
  
  
}


#simple study area plot highlighting focus for SAG presentation
study <- ggplot(basins) + 
  geom_sf(color = "grey", fill="white") +
  xlab("") + ylab("")

study + geom_sf(data = basins3, color = "yellow", fill="white", size = 1.5) +
  geom_sf(data = reaches, color = "#67a9cf", size = 0.5) 







######### Peak flow magnitude alteration assessment ############
#assess how the 2, 5, 10 yr flood magnitudes changed from baseline to future scenarios













################################################# #haven't updated this section yet
#create different alteration maps
#hydrograph element alteration - plot for each magnitude


#read in alteration summary table - all metrics
data <- read.csv(file="data/ffm_alteration.df.overall.join.csv")
#data <- read.csv(file="L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_flowalteration_assessment/ffm_alteration.df.overall.join.csv")
#create New_Name column with subbasin id to match polygon layer
data$New_Name <- data$subbasin
#omit first 25 rows - those are added subbasins, we have individual so no need
data <- data[25:length(data$COMID),]
#set levels for flow component so it goes in sequence of WY
data$flow_component <- factor(data$flow_component, levels = c("Fall pulse flow", "Wet-season base flow", "Peak flow", "Spring recession flow", "Dry-season base flow"))

#subset to the flow characteristic of interest - magnitude
mag <- data[data$flow_characteristic == "Magnitude (cfs)",] 



#subbasin polygons
basins3 <- basins %>% 
  inner_join(mag, by = c('New_Name'))
basins3


#reach polylines
reaches <- st_read('L:/San Juan WQIP_KTQ/Data/SpatialData/Model_Subbasins_Reaches/New_Subbasins_forSCCWRP_12062019/New_Subbasins_forSCCWRP/reaches_forSCCWRP.shp', quiet = T)


#join subbasin shapefile with magnitude alteration
mag <- mag %>% 
  inner_join(basins, by = c('New_Name'))
mag


#find centroids of subbasins
#unique.subbasins
unique.subbasins <- unique(mag$subbasin)

centroid <- st_centroid(basins) 

centroids <- 
  centroid$geom %>% 
  purrr::map(.,.f = function(x){data.frame(long = x[1],lat = x[2])}) %>% 
  bind_rows %>% data.frame(New_Name = centroid$New_Name) %>% 
  left_join(mag,by = "New_Name")
#omit NA subbasins that were not analyzed
if(length(which(is.na(centroids$COMID))) > 0) {
  centroids <- centroids[-which(is.na(centroids$COMID)),]
}

#plot the subbasins

plot <- ggplot(basins3) + 
  geom_sf(color = "grey", fill="white") +
  xlab("") + ylab("") + ggtitle("Magnitude Alteration")

plot

##########barplots

unique.sites <- unique(centroids$New_Name)

bar.testplot_list <- 
  lapply(1:length(unique.sites), function(i) { 
    #subset to unique site i
    sub.i <- centroids[centroids$New_Name == unique.sites[i],]
    #get number of altered mag flow metrics in each component
    mag.alt.n <- data.frame(aggregate(sub.i, by = sub.i[c('flow_component','alteration_status')], length))
    mag.alt.n$count <- mag.alt.n$long
    #find altered components, unaltered components should have 0 count
    altered.comp <- mag.alt.n$flow_component[mag.alt.n$alteration_status == "likely_altered"]
    #find rows with unaltered components
    unaltered.comp.rows <- mag.alt.n[mag.alt.n$flow_component != altered.comp,]
    if(length(unaltered.comp.rows$flow_component) > 0){
      #change count to 0 (0 altered flow metrics)
      unaltered.comp.rows$count <- rep(0, length(unaltered.comp.rows$flow_component))
      #subset to only likely altered
      mag.alt.n <- mag.alt.n[mag.alt.n$alteration_status == "likely_altered",]
      #add in unaltered comp with 0 counts
      mag.alt.n <- data.frame(rbind( mag.alt.n, unaltered.comp.rows))
    }else{
      #subset to only likely altered
      mag.alt.n <- mag.alt.n[mag.alt.n$alteration_status == "likely_altered",]
    }
    gt_plot <- ggplotGrob(
      ggplot(mag.alt.n)+
        geom_bar(aes(factor(flow_component),count, fill = factor(flow_component)), 
                 position='dodge',stat='identity', color = NA) +
        scale_fill_manual(name = "Altered Flow Components", labels = c("Fall pulse flow", "Wet-season base flow", "Peak flow", "Spring recession flow", "Dry-season base flow"), values = c("#1b9e77","#d95f02","#7570b3","#e7298a","#66a61e")) +
        labs(x = NULL, y = NULL) + 
        theme(legend.position = "none", rect = element_blank(),
              line = element_blank(), text = element_blank()) 
    )
    panel_coords <- gt_plot$layout[gt_plot$layout$name == "panel",]
    gt_plot[panel_coords$t:panel_coords$b, panel_coords$l:panel_coords$r]
  })


#add barplots to intial map with annotation_custom
bar_annotation_list <- lapply(1:length(unique.sites), function(i) 
  annotation_custom(bar.testplot_list[[i]], 
                    xmin = unique(centroids$long[centroids$New_Name == unique.sites[i]]) - 1.3e3,
                    xmax = unique(centroids$long[centroids$New_Name == unique.sites[i]]) + 1.3e3,
                    ymin = unique(centroids$lat[centroids$New_Name == unique.sites[i]]) - 1.3e3,
                    ymax = unique(centroids$lat[centroids$New_Name == unique.sites[i]]) + 1.3e3) )

result_plot <- Reduce(`+`, bar_annotation_list, plot)

plot + bar_annotation_list

result_plot


#individual zoomed plot of a basin

sub1 <- ggplot(basins3[basins3$New_Name == unique.sites[1],]) + 
  geom_sf(color = "grey", fill="white") +
  #geom_point(data=centroids, aes(x=long, y=lat), size=1, alpha=6/10) +
  xlab("") + ylab("")

sub1
#ggsave(sub1, filename="C:/Users/KristineT.SCCWRP2K/Documents/Git/SOC_FESS/subbasin1_compalteration.jpg", dpi=300, height=8, width=8)


#create separate barplot for just the first item
#subset to unique site i
sub.i <- centroids[centroids$New_Name == unique.sites[1],]
#get number of altered mag flow metrics in each component
mag.alt.n <- data.frame(aggregate(sub.i, by = sub.i[c('flow_component','alteration_status')], length))
mag.alt.n$count <- mag.alt.n$long
#find altered components, unaltered components should have 0 count
altered.comp <- mag.alt.n$flow_component[mag.alt.n$alteration_status == "likely_altered"]
#find rows with unaltered components
unaltered.comp.rows <- mag.alt.n[mag.alt.n$flow_component != altered.comp,]
if(length(unaltered.comp.rows$flow_component) > 0){
  #change count to 0 (0 altered flow metrics)
  unaltered.comp.rows$count <- rep(0, length(unaltered.comp.rows$flow_component))
  #subset to only likely altered
  mag.alt.n <- mag.alt.n[mag.alt.n$alteration_status == "likely_altered",]
  #add in unaltered comp with 0 counts
  mag.alt.n <- data.frame(rbind( mag.alt.n, unaltered.comp.rows))
}else{
  #subset to only likely altered
  mag.alt.n <- mag.alt.n[mag.alt.n$alteration_status == "likely_altered",]
}

ggplot(mag.alt.n)+
  geom_bar(aes(factor(flow_component),count, fill = factor(flow_component)), 
           position='dodge',stat='identity', color = NA) +
  scale_fill_manual(name = "Altered Flow Components", labels = c("Fall pulse flow", "Wet-season base flow", "Peak flow", "Spring recession flow", "Dry-season base flow"), values = c("#1b9e77","#d95f02","#7570b3","#e7298a","#66a61e")) +
  labs(x = NULL, y = NULL) + 
  theme(legend.position = "none", rect = element_blank(),
        line = element_blank(), text = element_blank()) 


bar <-  ggplot(mag.alt.n) +
  geom_bar(aes(factor(flow_component),count,fill = factor(flow_component)), 
           position='dodge',stat='identity', color = NA) +
  scale_fill_manual(name = "Flow Components", labels = c("Fall pulse flow", "Wet-season base flow", "Peak flow", "Spring recession flow", "Dry-season base flow"), values = c("#1b9e77","#d95f02","#7570b3","#e7298a","#66a61e")) +
  labs(x = "Flow Components", y = "Number of\nAltered Magnitude Metrics") +
  theme(panel.background = element_rect(fill = "white")) +
  theme(axis.title.y = element_text(size = rel(1.4),face="bold")) + theme(axis.title.x = element_text(size = rel(1.4),face="bold")) +
  theme(axis.text=element_text(size=11))
bar

ggsave(bar, filename="C:/Users/KristineT.SCCWRP2K/Documents/Git/SOC_FESS/zoom_barplot_mag.alteration.jpg", dpi=300, height=8, width=11)






























