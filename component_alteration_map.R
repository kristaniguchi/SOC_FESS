#Flow component alteration map

install.packages("ggsn")
install.packages("ggmap")
install.packages("mapview")
install.packages("geosphere")
install.packages("rgeos")
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

#component alteration data

fname = "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_flowalteration_assessment/Aliso_RecalibrationUpdate/summary_component_alteration.csv" #update to directory
comp_alt <- read.csv(fname)
#comp_alt <- read.csv(file = "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_flowalteration_assessment/summary_component_alteration.csv")

#create New_Name column with subbasin id to match polygon layer
comp_alt$New_Name <- comp_alt$subbasin
#OLD: omit first 5 rows - those are added subbasins, we have individual so no need
#comp_alt <- comp_alt[6:length(comp_alt$COMID),]

#set levels for flow component so it goes in sequence of WY
comp_alt$flow_component <- factor(comp_alt$flow_component, levels = c("Fall pulse flow", "Wet-season base flow", "Peak flow", "Spring recession flow", "Dry-season base flow"))

#subbasin polygon shapefile
basins <- st_read("data/subbasin_boundaries_forSCCWRP.shp", quiet = T)
#basins <- st_read('L:/San Juan WQIP_KTQ/Data/SpatialData/Model_Subbasins_Reaches/New_Subbasins_forSCCWRP_12062019/New_Subbasins_forSCCWRP/subbasin_boundaries_forSCCWRP.shp', quiet = T)
basins2 <- basins %>% 
  inner_join(comp_alt, by = c('New_Name'))
basins2

#basins2save <- basins2



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
  geom_point(data=centroids, aes(x=long, y=lat), size=1, alpha=6/10) +
  xlab("") + ylab("")

plot

##########barplots

unique.sites <- unique(centroids$New_Name)

  
bar.testplot_list <- 
  lapply(1:length(unique.sites), function(i) { 
    gt_plot <- ggplotGrob(
      ggplot(centroids[centroids$New_Name == unique.sites[i],])+
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

result_plot <- Reduce(`+`, bar_annotation_list, plot)

plot + bar_annotation_list

result_plot



#individual zoomed plot

sub1 <- ggplot(basins2[basins2$New_Name == unique.sites[1],]) + 
  geom_sf(color = "grey", fill="white") +
  #geom_point(data=centroids, aes(x=long, y=lat), size=1, alpha=6/10) +
  xlab("") + ylab("")

sub1
ggsave(sub1, filename="C:/Users/KristineT/Documents/Git/SOC_FESS/subbasin1_compalteration.jpg", dpi=300, height=8, width=8)


#create separate barplot for just the first item

  
bar <-  ggplot(centroids[centroids$New_Name == unique.sites[1],]) +
        geom_bar(aes(factor(flow_component),n_ffm_altered,group=New_Name, fill = factor(flow_component)), 
        position='dodge',stat='identity', color = NA) +
        scale_fill_manual(name = "Flow Components", labels = c("Fall pulse flow", "Wet-season base flow", "Peak flow", "Spring recession flow", "Dry-season base flow"), values = c("#1b9e77","#d95f02","#7570b3","#e7298a","#66a61e")) +
        labs(x = "Flow Components", y = "Number of Altered Metrics") +
        theme(panel.background = element_rect(fill = "white"))
bar
ggsave(bar, filename="C:/Users/KristineT/Documents/Git/SOC_FESS/zoom_barplot_compalteration.jpg", dpi=300, height=5, width=8.5)


#################################################
#create different alteration maps
#hydrograph element alteration - plot for each magnitude


#read in alteration summary table - all metrics
#data <- read.csv(file="data/ffm_alteration.df.overall.join.csv")
data <- read.csv(file="L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_flowalteration_assessment/Aliso_RecalibrationUpdate/ffm_alteration.df.overall.join.SanJuan.Aliso.csv")


#create New_Name column with subbasin id to match polygon layer
data$New_Name <- data$subbasin
#Old: omit first 25 rows - those are added subbasins, we have individual so no need
#data <- data[25:length(data$COMID),]

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
    mag.alt.n <- data.frame(aggregate(sub.i, by = sub.i[c('flow_component','alteration.status')], length))
    mag.alt.n$count <- mag.alt.n$long
    #find altered components, unaltered components should have 0 count
    altered.comp <- mag.alt.n$flow_component[mag.alt.n$alteration.status == "likely_altered"]
    #find rows with unaltered components
    unaltered.comp.rows <- mag.alt.n[mag.alt.n$flow_component != altered.comp,]
    if(length(unaltered.comp.rows$flow_component) > 0){
      #change count to 0 (0 altered flow metrics)
      unaltered.comp.rows$count <- rep(0, length(unaltered.comp.rows$flow_component))
      #subset to only likely altered
      mag.alt.n <- mag.alt.n[mag.alt.n$alteration.status == "likely_altered",]
      #add in unaltered comp with 0 counts
      mag.alt.n <- data.frame(rbind( mag.alt.n, unaltered.comp.rows))
    }else{
      #subset to only likely altered
      mag.alt.n <- mag.alt.n[mag.alt.n$alteration.status == "likely_altered",]
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
#ggsave(sub1, filename="C:/Users/KristineT/Documents/Git/SOC_FESS/subbasin1_compalteration.jpg", dpi=300, height=8, width=8)


#create separate barplot for just the first item
#subset to unique site i
sub.i <- centroids[centroids$New_Name == unique.sites[1],]
#get number of altered mag flow metrics in each component
mag.alt.n <- data.frame(aggregate(sub.i, by = sub.i[c('flow_component','alteration.status')], length))
mag.alt.n$count <- mag.alt.n$long
#find altered components, unaltered components should have 0 count
altered.comp <- mag.alt.n$flow_component[mag.alt.n$alteration.status == "likely_altered"]
#find rows with unaltered components
unaltered.comp.rows <- mag.alt.n[mag.alt.n$flow_component != altered.comp,]
if(length(unaltered.comp.rows$flow_component) > 0){
  #change count to 0 (0 altered flow metrics)
  unaltered.comp.rows$count <- rep(0, length(unaltered.comp.rows$flow_component))
  #subset to only likely altered
  mag.alt.n <- mag.alt.n[mag.alt.n$alteration.status == "likely_altered",]
  #add in unaltered comp with 0 counts
  mag.alt.n <- data.frame(rbind( mag.alt.n, unaltered.comp.rows))
}else{
  #subset to only likely altered
  mag.alt.n <- mag.alt.n[mag.alt.n$alteration.status == "likely_altered",]
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

ggsave(bar, filename="C:/Users/KristineT/Documents/Git/SOC_FESS/zoom_barplot_mag.alteration.jpg", dpi=300, height=8, width=11)




###################
#heatmap of alteration: component vs. flow characteristics

#install packages
#install.packages("ztable")
library(ztable)
#if(!require(devtools)) install.packages("devtools")
#devtools::install_github("cardiomoon/ztable")
#install.packages("moonBook")
require(moonBook)
x=table(acs$Dx,acs$smoking)
x

#read in alteration summary table
data <- read.csv(file="L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_flowalteration_assessment/Aliso_RecalibrationUpdate/ffm_alteration.df.overall.join.SanJuan.Aliso.csv")
#exclude the additive subbasins
#data <- data[26:length(data$subbasin),]
#subset to altered only
altered <- data[data$alteration.status == "likely_altered",]
#remove NA
altered <- altered[-which(is.na(altered$subbasin)),]

#subset so if there is one altered characteristic per component (remove duplicates from one subbasin so we get number of subbasins with altered flow characteristics)
unique.altered.sites <- unique(altered$subbasin.model)
#create empty df that will be filled
altered.new <- altered[1,]
#fill with NA for first row to be removed later
altered.new[1,] <- NA
altered.new$comp.characteristic <- NA

for(i in 1:length(unique.altered.sites)){
  sub1 <- altered[altered$subbasin.model == unique.altered.sites[i],]
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
dev.off()
mine.heatmap <- ggplot(data = ffm_summary, mapping = aes(x = flow_characteristic,
                                                       y = factor(flow_component, levels =  c("Fall pulse flow", "Wet-season base flow", "Peak flow", "Spring recession flow", "Dry-season base flow")),
                                                       fill = subbasin)) +
  geom_tile() +
  ylab(label = "Flow Component") + xlab(label="Hydrograph Element") +
  scale_fill_gradient(name = "Number of\nAltered Subbasins",
                      low = "#fef0d9",
                      high = "#b30000") +
  ggtitle(label = "Altered Subbasins in Aliso and San Juan Tributaries") + theme_light() +
  theme(axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"))

mine.heatmap

ggsave(mine.heatmap, file="C:/Users/KristineT/Documents/Git/SOC_FESS/heatmap_alteration.jpg", dpi=300, height=8, width=12)

#simple study area plot highlighting focus for SAG presentation
study <- ggplot(basins) + 
  geom_sf(color = "grey", fill="white") +
  xlab("") + ylab("")

study <- ggplot(basins) + 
  geom_sf(color = "#969696", fill="#d9d9d9") +
  labs(x ="", y = "") + 
  theme(panel.background = element_rect(fill = "white"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_line(color = "white", size = 0.8))

  

study + geom_sf(data = basins3, color = "red", fill="white", size = 1.2) +
geom_sf(data = reaches, color = "#67a9cf", size = 0.5) 



#test FFM METRIC alteration maps
#subbasin polygons
data$New_Name <- data$subbasin
basins4 <- basins %>% 
  inner_join(data, by = c('New_Name'))
basins4

#replace alteration category names
basins4$alteration.status[basins4$alteration.status == "likely_altered"] <- "Likely Altered"
basins4$alteration.status[basins4$alteration.status == "likely_unaltered"] <- "Likely Unaltered"
basins4$alteration.status[basins4$alteration.status == "indeterminate"] <- "Indeterminate"
basins4$alteration.status[basins4$alteration.status == "not_enough_data"] <- "NA"

#replace alteration direction names
basins4$alteration.direction[basins4$alteration.direction == "none_found"] <- ""
basins4$alteration.direction[which(is.na(basins4$alteration.direction))] <- ""
basins4$alteration.direction[basins4$alteration.direction == "undeterminable"] <- ""
basins4$alteration.direction[basins4$alteration.direction == "low"] <- " Low"
basins4$alteration.direction[basins4$alteration.direction == "early"] <- " Low"
basins4$alteration.direction[basins4$alteration.direction == "late"] <- " High"
basins4$alteration.direction[basins4$alteration.direction == "high"] <- " High"
#create new alteration category with direction
basins4$alteration.status.new <- paste0(basins4$alteration.status, basins4$alteration.direction)
#replace indeterminate high and low
basins4$alteration.status.new <- gsub("Indeterminate High", "Indeterminate", basins4$alteration.status.new)
basins4$alteration.status.new <- gsub("Indeterminate Low", "Indeterminate", basins4$alteration.status.new)
unique(basins4$alteration.status.new)

#list of colors and alteration statuses, color current by alteration status
colors <- c("#cb181d", "#fdbe85", "#2171b5", "#f7f7f7", "#d9d9d9")
alteration.status.new <- c("Likely Altered High", "Likely Altered Low", "Likely Unaltered", "Indeterminate", "NA")
lookup <- data.frame(cbind(colors, alteration.status.new))

#UPDATE: output director for alteration maps FFMs
dir.alt <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_flowalteration_assessment/Aliso_RecalibrationUpdate/AlterationMaps/"

#loop through each metric and plot
unique.ffm <- unique(basins4$ffm)

for(j in 1:length(unique.ffm)){
  #subset basins4 to ffm j
  basins4.sub <- basins4[basins4$ffm == unique.ffm[j],]
  
  #subset colors and status
  lookup.sub <- lookup[lookup$alteration.status.new %in% basins4.sub$alteration.status.new,]
  
  #save as factor
  lookup.sub$alteration.status.new <- factor(lookup.sub$alteration.status.new, levels = lookup.sub$alteration.status.new)
  basins4.sub$alteration.status.new <- factor(basins4.sub$alteration.status.new, levels = lookup.sub$alteration.status.new)
  
  #find and replace names for timing low early, high late
  if(unique(basins4.sub$flow_characteristic) == "Timing (date)"){
    basins4.sub$alteration.status.new <- gsub("Likely Altered Low", "Likely Altered Early", basins4.sub$alteration.status.new)
    basins4.sub$alteration.status.new <- gsub("Likely Altered High", "Likely Altered Late", basins4.sub$alteration.status.new)
    lookup.sub$alteration.status.new <- gsub("Likely Altered Low", "Likely Altered Early", lookup.sub$alteration.status.new)
    lookup.sub$alteration.status.new <- gsub("Likely Altered High", "Likely Altered Late", lookup.sub$alteration.status.new)
  }
  unique(basins4.sub$alteration.status.new)
  
  #base map 
  study2 <- ggplot(basins) + 
    geom_sf(color = "#969696", fill="#d9d9d9") +
    labs(title=unique(basins4.sub$title_name),x ="", y = "") + 
    theme(panel.background = element_rect(fill = "white"),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.grid = element_line(color = "white", size = 0.8))
  
  #filled alteration plots
  alt.plot <- study2 + geom_sf(data = basins4.sub, color= "#969696", aes(fill=alteration.status.new)) +
    scale_fill_manual(name = "Alteration Status", labels = lookup.sub$alteration.status.new, values=lookup.sub$colors) +
    geom_sf(data = reaches, color = "#67a9cf", size = 0.5) 
  #print
  print(alt.plot)
  
  #write plot
  #save as jpg
  plot.fname <- paste0(dir.alt,unique(basins4.sub$ffm), "_alteration.map.jpg")
  ggsave(alt.plot, file=plot.fname, dpi=300, height=8, width=12)
  
}


#############Loop through facet maps for components

#loop through each component and plot panel plots of the metrics
uniq.comp <- unique(basins4$flow_component)

for(k in 1:length(uniq.comp)){
  #subset basins4 to ffm j
  basins4.sub <- basins4[basins4$flow_component == uniq.comp[k],]
  
  #subset colors and status
  lookup.sub <- lookup[lookup$alteration.status.new %in% basins4.sub$alteration.status.new,]
  #save as factor
  lookup.sub$alteration.status.new <- factor(lookup.sub$alteration.status.new, levels = lookup.sub$alteration.status.new)
  basins4.sub$alteration.status.new <- factor(basins4.sub$alteration.status.new, levels = lookup.sub$alteration.status.new)
  #title metric needs to be sorted, factor
  basins4.sub$title_ffm <- factor(basins4.sub$title_ffm, levels = unique(basins4.sub$title_ffm))
  
  
  #if peak flow mag, use 3 columns
  if(uniq.comp[k] == "Peak flow"){
    col.num <- 3
    font.size <- 12
  }else{
    col.num <- 2
    font.size <- 14
  }
  
  #base map 
  study2 <- ggplot(basins) + 
    #geom_sf(color = "#969696", fill="#fdbe85") +
    geom_sf(color = "#969696", fill="#d9d9d9") +
    labs(title=unique(basins4.sub$title_component),x ="", y = "")  + 
    theme(panel.background = element_rect(fill = "white"),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.grid = element_line(color = "white", size = 0.8),
          plot.title = element_text(size=20))
  
  #filled alteration plots
  alt.plot <- study2 + geom_sf(data = basins4.sub, color= "#969696", aes(fill=alteration.status.new)) +
    scale_fill_manual(name = "Alteration Status", labels = lookup.sub$alteration.status.new, values=lookup.sub$colors) +
    facet_wrap(~ title_ffm, ncol = col.num) +
    theme(strip.text.x = element_text(size = font.size)) +
    geom_sf(data = reaches, color = "#67a9cf", size = 0.5) 
  #print
  print(alt.plot)
  
  #write plot
  #save as jpg
  plot.fname <- paste0(dir.alt,unique(basins4.sub$flow_component), "_alteration.map.jpg")
  ggsave(alt.plot, file=plot.fname, dpi=300, height=8, width=10)
  
}
















