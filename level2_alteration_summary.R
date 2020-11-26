#### Level 2 - alteration summary based on various thresholds

#install.packages("ggsn")
#install.packages("ggmap")
#install.packages("mapview")
#install.packages("geosphere")
#install.packages("rgeos")
#to install spDataLarge
#devtools::install_github("robinlovelace/geocompr")
library(spData)
#install.packages("spDataLarge")
#install.packages("spDataLarge", repos = "https://nowosad.github.io/drat/", type = "source")
library(spDataLarge)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(readxl)
library(sf)
library(ggsn)
library(ggmap)
library(mapview)
library(spData)      
library(spDataLarge)
library(geosphere)
library(rgeos)


#load in data
#species suitability data
level2.dir <- "C:/Users/KristineT/SCCWRP/SOC WQIP - Flow Ecology Study - General/Tier2_analysis/"
fname <- paste0(level2.dir, "09_metric_suitability_tally_condensed.csv") #update to directory

#read in suitability data 
suit_data <- read.csv(fname)

#UPDATE
#set output directory
out.dir <- paste0(level2.dir, "Suitability_Maps/", "prob50_50time/")
#out.dir <- paste0(level3.dir, "Suitability_Maps/", "All_species_suit_class_wide_option2_strict_prob_shorter_time/")

#read in information on subbasin and New_Name
basin_comid_lookup <- read.csv("L:/San Juan WQIP_KTQ/Data/SpatialData/v13_pourpoints_NHD_comids.csv") 

#read in shapefiles subbasins and reaches
#subbasin polygon shapefile
basins <- st_read("data/subbasin_boundaries_forSCCWRP.shp", quiet = T)

#reach polylines
reaches <- st_read('data/reaches_forSCCWRP.shp', quiet = T)


############################################################
#post-process Aliso suitability data

#read in information on subbasin and COMID
basin_comid_lookup <- read.csv("L:/San Juan WQIP_KTQ/Data/SpatialData/v13_pourpoints_NHD_comids.csv")

#lookuptable to convert subbasin codes for model output subbasin names
subbasin_lookup <- read.csv("L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/191220_Interim_Calibration/site_name_lookupletternumbers.csv")

#convert basin orig name to outputfile name (model subbasin name)
new.subbasinname <- basin_comid_lookup$Subbasin

for(z in 1:length(subbasin_lookup$Letter)){
  new.subbasinname <- gsub(subbasin_lookup$Letter[z], subbasin_lookup$Number[z], new.subbasinname)
}

#find and replace - in new.subbasinname with nothing, make consistent with file name
new.subbasinname <- gsub("-", "", new.subbasinname)
basin_comid_lookup$site <- as.numeric(new.subbasinname)

#join new subbasin name with 

suit_data2 <- suit_data %>% 
  inner_join(basin_comid_lookup, by = c('site')) %>% 
  select(c(names(suit_data), Subbasin)) %>% 
  rename(New_Name = Subbasin)

#####post process to get overall alteration

####################################
#if >25% time unsuitable, class it as altered
suit_data2$alteration_25pct_time[suit_data2$Unsuitable > 25] <- "Altered"
suit_data2$alteration_25pct_time[suit_data2$Unsuitable <= 25] <- "Unaltered"

#### if 1 metric is altered, then all altered
#aggregate site, Biol, Threshold, alteration_25pct_time
subset.25pct.time <- suit_data2 %>% 
  group_by(site, New_Name, Biol, Threshold, alteration_25pct_time) %>% 
  tally() %>% 
  ungroup()

#save as dataframe
subset.25pct.time <- data.frame(subset.25pct.time)

#1 metric altered, considered altered
#if number altered > 0 --> class it as overall altered
subset.25pct.time$overall.altered.1metric[which(subset.25pct.time$alteration_25pct_time == "Altered" & subset.25pct.time$n > 0)] <- "Altered"
#if 3 metrics unaltered --> class it as unaltered
subset.25pct.time$overall.altered.1metric[which(subset.25pct.time$alteration_25pct_time == "Unaltered" & subset.25pct.time$n == 3)] <- "Unaltered"

#2-3 metric altered, considered altered
#if number altered > 1 --> class it as overall altered
subset.25pct.time$overall.altered.2metric[which(subset.25pct.time$alteration_25pct_time == "Altered" & subset.25pct.time$n > 1)] <- "Altered"
#if 3 metrics unaltered --> class it as unaltered
subset.25pct.time$overall.altered.2metric[which(subset.25pct.time$alteration_25pct_time == "Unaltered" & subset.25pct.time$n > 1)] <- "Unaltered"

#3 metric altered, considered altered
#if number altered > 0 --> class it as overall altered
subset.25pct.time$overall.altered.3metric[which(subset.25pct.time$alteration_25pct_time == "Altered" & subset.25pct.time$n  == 3)] <- "Altered"
#if 3 metrics unaltered --> class it as unaltered
subset.25pct.time$overall.altered.3metric[which(subset.25pct.time$alteration_25pct_time == "Unaltered" & subset.25pct.time$n > 0)] <- "Unaltered"

#save csv
file.name <- paste0(level2.dir, "summary.25pct.time.altered.csv")
write.csv(subset.25pct.time, file=file.name, row.names = FALSE)

#summarize overall alteration
#1 metric
subset.25pct.time.summary1 <- subset.25pct.time %>% 
  group_by(Biol, Threshold, overall.altered.1metric) %>% 
  tally() %>% 
  ungroup() %>% 
  na.omit() %>% 
  mutate(pct.1metric = 100*n/19) 

#2 metric
subset.25pct.time.summary2 <- subset.25pct.time %>% 
  group_by(Biol, Threshold, overall.altered.2metric) %>% 
  tally() %>% 
  ungroup() %>% 
  na.omit() %>% 
  mutate(pct.2metric = 100*n/19) 

#3 metric
subset.25pct.time.summary3 <- subset.25pct.time %>% 
  group_by(Biol, Threshold, overall.altered.3metric) %>% 
  tally() %>% 
  ungroup() %>% 
  na.omit() %>% 
  mutate(pct.3metric = 100*n/19) 



####################################
#if >50% time unsuitable, altered
suit_data2$alteration_50pct_time[suit_data2$Unsuitable > 50] <- "Altered"
suit_data2$alteration_50pct_time[suit_data2$Unsuitable <= 50] <- "Unaltered"

#### if 1 metric is altered, then all altered
#aggregate site, Biol, Threshold, alteration_50pct_time
subset.50pct.time <- suit_data2 %>% 
  group_by(site, New_Name, Biol, Threshold, alteration_50pct_time) %>% 
  tally() %>% 
  ungroup()

#save as dataframe
subset.50pct.time <- data.frame(subset.50pct.time)

#overall alteration based on # of altered metrics
#1 metric altered, considered altered
#if number altered > 0 --> class it as overall altered
subset.50pct.time$overall.altered.1metric[which(subset.50pct.time$alteration_50pct_time == "Altered" & subset.50pct.time$n > 0)] <- "Altered"
#if 3 metrics unaltered --> class it as unaltered
subset.50pct.time$overall.altered.1metric[which(subset.50pct.time$alteration_50pct_time == "Unaltered" & subset.50pct.time$n == 3)] <- "Unaltered"

#2-3 metric altered, considered altered
#if number altered > 1 --> class it as overall altered
subset.50pct.time$overall.altered.2metric[which(subset.50pct.time$alteration_50pct_time == "Altered" & subset.50pct.time$n > 1)] <- "Altered"
#if 3 metrics unaltered --> class it as unaltered
subset.50pct.time$overall.altered.2metric[which(subset.50pct.time$alteration_50pct_time == "Unaltered" & subset.50pct.time$n > 1)] <- "Unaltered"

#3 metric altered, considered altered
#if number altered > 0 --> class it as overall altered
subset.50pct.time$overall.altered.3metric[which(subset.50pct.time$alteration_50pct_time == "Altered" & subset.50pct.time$n  == 3)] <- "Altered"
#if 3 metrics unaltered --> class it as unaltered
subset.50pct.time$overall.altered.3metric[which(subset.50pct.time$alteration_50pct_time == "Unaltered" & subset.50pct.time$n > 0)] <- "Unaltered"


#save csv
file.name <- paste0(level2.dir, "summary.50pct.time.altered.csv")
write.csv(subset.50pct.time, file=file.name, row.names = FALSE)

#summarize overall alteration
#1 metric
subset.50pct.time.summary1 <- subset.50pct.time %>% 
  group_by(Biol, Threshold, overall.altered.1metric) %>% 
  tally() %>% 
  ungroup() %>% 
  na.omit() %>% 
  mutate(pct.1metric = 100*n/19) 

#2 metric
subset.50pct.time.summary2 <- subset.50pct.time %>% 
  group_by(Biol, Threshold, overall.altered.2metric) %>% 
  tally() %>% 
  ungroup() %>% 
  na.omit() %>% 
  mutate(pct.2metric = 100*n/19) 

#3 metric
subset.50pct.time.summary3 <- subset.50pct.time %>% 
  group_by(Biol, Threshold, overall.altered.3metric) %>% 
  tally() %>% 
  ungroup() %>% 
  na.omit() %>% 
  mutate(pct.3metric = 100*n/19) 

####################################
#if >75% time unsuitable, altered
suit_data2$alteration_75pct_time[suit_data2$Unsuitable > 75] <- "Altered"
suit_data2$alteration_75pct_time[suit_data2$Unsuitable <= 75] <- "Unaltered"


#### if 1 metric is altered, then all altered
#aggregate site, Biol, Threshold, alteration_75pct_time
subset.75pct.time <- suit_data2 %>% 
  group_by(site, New_Name, Biol, Threshold, alteration_75pct_time) %>% 
  tally() %>% 
  ungroup()

#save as dataframe
subset.75pct.time <- data.frame(subset.75pct.time)

#overall alteration based on # of altered metrics
#1 metric altered, considered altered
#if number altered > 0 --> class it as overall altered
subset.75pct.time$overall.altered.1metric[which(subset.75pct.time$alteration_75pct_time == "Altered" & subset.75pct.time$n > 0)] <- "Altered"
#if 3 metrics unaltered --> class it as unaltered
subset.75pct.time$overall.altered.1metric[which(subset.75pct.time$alteration_75pct_time == "Unaltered" & subset.75pct.time$n == 3)] <- "Unaltered"

#2-3 metric altered, considered altered
#if number altered > 1 --> class it as overall altered
subset.75pct.time$overall.altered.2metric[which(subset.75pct.time$alteration_75pct_time == "Altered" & subset.75pct.time$n > 1)] <- "Altered"
#if 3 metrics unaltered --> class it as unaltered
subset.75pct.time$overall.altered.2metric[which(subset.75pct.time$alteration_75pct_time == "Unaltered" & subset.75pct.time$n > 1)] <- "Unaltered"

#3 metric altered, considered altered
#if number altered > 0 --> class it as overall altered
subset.75pct.time$overall.altered.3metric[which(subset.75pct.time$alteration_75pct_time == "Altered" & subset.75pct.time$n  == 3)] <- "Altered"
#if 3 metrics unaltered --> class it as unaltered
subset.75pct.time$overall.altered.3metric[which(subset.75pct.time$alteration_75pct_time == "Unaltered" & subset.75pct.time$n > 0)] <- "Unaltered"


#save csv
file.name <- paste0(level2.dir, "summary.75pct.time.altered.csv")
write.csv(subset.75pct.time, file=file.name, row.names = FALSE)


#summarize overall alteration
#1 metric
subset.75pct.time.summary1 <- subset.75pct.time %>% 
  group_by(Biol, Threshold, overall.altered.1metric) %>% 
  tally() %>% 
  ungroup() %>% 
  na.omit() %>% 
  mutate(pct.1metric = 100*n/19) 

#2 metric
subset.75pct.time.summary2 <- subset.75pct.time %>% 
  group_by(Biol, Threshold, overall.altered.2metric) %>% 
  tally() %>% 
  ungroup() %>% 
  na.omit() %>% 
  mutate(pct.2metric = 100*n/19) 

#3 metric
subset.75pct.time.summary3 <- subset.75pct.time %>% 
  group_by(Biol, Threshold, overall.altered.3metric) %>% 
  tally() %>% 
  ungroup() %>% 
  na.omit() %>% 
  mutate(pct.3metric = 100*n/19) 

############################################################
#create suitability maps for 50% of time threshold 

#names
cols <- names(subset.50pct.time)
col.names <- c("overall.altered.1metric", "overall.altered.2metric", "overall.altered.3metric")

#colors for suitability categories
colors <- c("#ca0020", "#0571b0", "white")
alteration <- c("Altered",  "Unaltered", NA)
categories <- c("Likely Altered", "Likely Unaltered", "Not evaluated")
lookup <- data.frame(cbind(colors, alteration, categories))


for(i in 7:9){
  #title
  col.name <- cols[i]
  metric.threshold <- gsub("overall.altered.", "", col.name)
  metric.threshold <- gsub("metric", " Metric Altered Threshold", metric.threshold)
  
  #subset 
  #subset <- subset.75pct.time[,c(2, 3,4, i)]
  subset <- subset.50pct.time[,c(2, 3,4, i)]
  #subset <- subset.25pct.time[,c(2, 3,4, i)]
  
  names(subset) <- c("New_Name", "Biol", "Probability_Threshold","Alteration - Bio")
  subset$New_Name <- as.character(subset$New_Name)
  #remove NA rows
  subset <- na.omit(subset)
  
  #loop through CSCI and ASCI thresholds
  indices <- c("ASCI", "CSCI")
  
  for(z in indices){
    #subset either csci or asci
    subset.index <- subset[subset$Biol == z,]
    
    #loop through the 3 thresholds
    unique.prob.thresholds <- na.omit(unique(subset.index$Probability_Threshold))
    for(q in unique.prob.thresholds){
      #subset based on prob threshold
      subset.index.threshold <- subset.index[subset.index$Probability_Threshold == q,]
      #prob value
      prob <- gsub("Threshold", "Probability Threshold at ", q)
      
      #merge with basins
      subset.join <- subset.index.threshold %>% 
        full_join(basins, by = c('New_Name'))
      
      #title
      title <- paste0(z, ": ", prob, "%")
      subtitle <- metric.threshold
      
      #plot
      #Set up base map 
      study <- ggplot(basins) + 
        geom_sf(color = "#969696", fill="white") +
        labs(title=title, subtitle = subtitle, x ="", y = "")  + 
        theme(panel.background = element_rect(fill = "white"),
              axis.ticks = element_blank(),
              axis.text = element_blank(),
              panel.grid = element_line(color = "white", size = 0.8),
              plot.title = element_text(size=20)) 
      study
      
      #subset lookup categories and tables
      lookup.sub <- lookup[lookup$alteration %in% unique(subset.join$`Alteration - Bio`),]
      
      #save as factor
      lookup.sub$alteration <- factor(lookup.sub$alteration, levels = unique(lookup.sub$alteration))
      subset.join$`Alteration - Bio` <- factor(subset.join$`Alteration - Bio`, levels = unique(lookup.sub$alteration))
      
      
      #synthesis map
      syn.plot <- study + geom_sf(data = subset.join, color= "#969696", aes(fill=`Alteration - Bio`, geometry = geometry)) +
        scale_fill_manual(name = "Alteration - Bio", labels = lookup.sub$categories, values=lookup.sub$colors) +
        geom_sf(data = reaches, color = "#67a9cf", size = 0.5) 
      #print
      print(syn.plot)
      
      #write plot
      #out.filename <- paste0(out.dir, col.name, "_suitability_map.jpg")
      #ggsave(syn.plot, file = out.filename, dpi=300, height=4, width=6)
      
    }
    
  }
  
}





############################################################

####Plot maps of suitability

#loop through each focal species, make maps for each

#species columns
cols <- names(suit_data)
species.cols <- 3:length(cols)

#colors for suitability categories
colors <- c("#0571b0", "#ca0020", "white")
suitability <- c("High",  "Low", NA)
categories <- c("Yes", "No", "Not evaluated")
lookup <- data.frame(cbind(colors, suitability, categories))


for(i in species.cols){
  #title
  col.name <- cols[i]
  title <- gsub("_", ": ", col.name)
  title <- gsub("\\.", "\\/", title)
  
  #subset 
  subset <- suit_data[,c(2,i)]
  names(subset)[2] <- "suitability"
  
  #merge with basins
  subset.join <- subset %>% 
    full_join(basins, by = c('New_Name'))
  
  #change Partial to Low for suitability
  subset.join$suitability <- gsub("Partial", "Low", subset.join$suitability)
  
  
  #Set up base map 
  study <- ggplot(basins) + 
    geom_sf(color = "#969696", fill="white") +
    labs(title=title, x ="", y = "")  + 
    theme(panel.background = element_rect(fill = "white"),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.grid = element_line(color = "white", size = 0.8),
          plot.title = element_text(size=20)) 
  study
  
  #subset lookup categories and tables
  lookup.sub <- lookup[lookup$suitability %in% unique(subset.join$suitability),]
  
  #save as factor
  lookup.sub$suitability <- factor(lookup.sub$suitability, levels = unique(lookup.sub$suitability))
  subset.join$suitability <- factor(subset.join$suitability, levels = unique(subset.join$suitability))
  
  
  #synthesis map
  syn.plot <- study + geom_sf(data = subset.join, color= "#969696", aes(fill=suitability, geometry = geometry)) +
    scale_fill_manual(name = "Flow conditions suitable?", labels = lookup.sub$categories, values=lookup.sub$colors) +
    geom_sf(data = reaches, color = "#67a9cf", size = 0.5) 
  #print
  print(syn.plot)
  
  #write plot
  out.filename <- paste0(out.dir, col.name, "_suitability_map.jpg")
  ggsave(syn.plot, file = out.filename, dpi=300, height=4, width=6)
  
}


