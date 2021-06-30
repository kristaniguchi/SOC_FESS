#Flow component alteration map - FFM maps, heat maps, synthesis

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
#install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
library(readxl)
library(sf)
library(ggsn)
library(ggmap)
library(mapview)
library(spData)      
library(spDataLarge)
library(ggspatial)    
library(geosphere)
library(rgeos)


#load in data
alt.dir.name <- "Aliso_Oso_SmCk_SanJuan_all_lowflowbias/" #update to directory name with data to be analyzed
#alteration directory
alteration.dir <- paste0("L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_flowalteration_assessment/", alt.dir.name)

#component alteration data
#fname = "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_flowalteration_assessment/Aliso_RecalibrationUpdate/summary_component_alteration.csv" #update to directory
fname = paste0(alteration.dir, "summary_component_alteration.csv") #update to directory
comp_alt <- read.csv(fname)
#comp_alt <- read.csv(file = "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_flowalteration_assessment/summary_component_alteration.csv")

#create New_Name column with subbasin id to match polygon layer
comp_alt$New_Name <- comp_alt$subbasin
#OLD: omit first 5 rows - those are added subbasins, we have individual so no need
#comp_alt <- comp_alt[6:length(comp_alt$COMID),]

#set levels for flow component so it goes in sequence of WY
comp_alt$flow_component <- factor(comp_alt$flow_component, levels = c("Fall pulse flow", "Wet-season baseflow", "Peak flow", "Spring recession flow", "Dry-season baseflow"))

#update component alteration to exclude peak magnitude alteration (Peak 2, 5, 10) - minus 3 altered metric for all peak components
#find the row index of peak flow magnitude alteration
ind.peak <- grep("Peak flow", comp_alt$flow_component)
#for all peak mag rows, the n_ffm_altered is n_ffm_altered-3 (excluding peak mag alteration)
comp_alt$n_ffm_altered[ind.peak] <- comp_alt$n_ffm_altered[ind.peak] - 3
#update component_alteration if n_ffm_altered >1, likely altered, else 
comp_alt$component_alteration[comp_alt$n_ffm_altered > 0] <- "likely_altered"
#update component_alteration if n_ffm_altered == 0, likely unaltered 
comp_alt$component_alteration[comp_alt$n_ffm_altered == 0] <- "likely_unaltered"

#subbasin polygon shapefile - updated shapefile with Bell Canyon subbasin added
basins <- st_read("data/Agg_Boundaries_v14.shp", quiet = T)
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

#Do not need source anymore since all from LSPC (model source LSPC or Wildermuth)
#source <- read.csv("L:/San Juan WQIP_KTQ/Data/SpatialData/Model_Subbasins_Reaches/New_Subbasins_forSCCWRP_12062019/Subbasins_inmodel_source.csv")


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
        scale_fill_manual(name = "Altered Flow Components", labels = c("Fall pulse flow", "Wet-season baseflow", "Peak flow", "Spring recession flow", "Dry-season baseflow"), values = c("#1b9e77","#d95f02","#7570b3","#e7298a","#66a61e")) +
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
#ggsave(sub1, filename="C:/Users/KristineT/Documents/Git/SOC_FESS/subbasin1_compalteration.jpg", dpi=300, height=8, width=8)


#create separate barplot for just the first item

  
bar <-  ggplot(centroids[centroids$New_Name == unique.sites[1],]) +
        geom_bar(aes(factor(flow_component),n_ffm_altered,group=New_Name, fill = factor(flow_component)), 
        position='dodge',stat='identity', color = NA) +
        scale_fill_manual(name = "Flow Components", labels = c("Fall pulse flow", "Wet-season baseflow", "Peak flow", "Spring recession flow", "Dry-season baseflow"), values = c("#1b9e77","#d95f02","#7570b3","#e7298a","#66a61e")) +
        labs(x = "Flow Components", y = "Number of Altered Metrics") +
        theme(panel.background = element_rect(fill = "white"))
bar
#ggsave(bar, filename="C:/Users/KristineT/Documents/Git/SOC_FESS/zoom_barplot_compalteration.jpg", dpi=300, height=5, width=8.5)


#################################################
#create different alteration maps
#hydrograph element alteration - plot for each magnitude


#read in alteration summary table - all metrics
#data <- read.csv(file="data/ffm_alteration.df.overall.join.csv")
data <- read.csv(file=paste0(alteration.dir, "ffm_alteration.df.overall.join.Aliso.Oso.SmallCreeks.SanJuanLSPC.lowflowbias.csv"))


#create New_Name column with subbasin id to match polygon layer
data$New_Name <- data$subbasin
#Old: omit first 25 rows - those are added subbasins, we have individual so no need
#data <- data[25:length(data$COMID),]

#set levels for flow component so it goes in sequence of WY
data$flow_component <- factor(data$flow_component, levels = c("Fall pulse flow", "Wet-season baseflow", "Peak flow", "Spring recession flow", "Dry-season baseflow"))

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
        geom_bar(aes(flow_component,count, fill = flow_component), 
                 position='dodge',stat='identity', color = NA) +
        scale_fill_manual(name = "Altered Flow Components", labels = c("Fall pulse flow", "Wet-season baseflow", "Peak flow", "Spring recession flow", "Dry-season baseflow"), values = c("#1b9e77","#d95f02","#7570b3","#e7298a","#66a61e")) +
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
ggsave(sub1, filename="C:/Users/KristineT.SCCWRP2K/Documents/Git/SOC_FESS/subbasin1_compalteration.jpg", dpi=300, height=8, width=8)


#create separate barplot for just the first item
#subset to unique site i
sub.i <- centroids[centroids$New_Name == unique.sites[1],]
#get number of altered mag flow metrics in each component
mag.alt.n <- data.frame(aggregate(sub.i, by = sub.i[c('flow_component','alteration.status')], length))
mag.alt.n$count <- mag.alt.n$long
#find altered components, unaltered components should have 0 count
altered.comp <- mag.alt.n$flow_component[mag.alt.n$alteration.status == "likely_altered"]
#find rows with unaltered components
unaltered.comp.rows <- mag.alt.n[mag.alt.n$alteration.status == "likely_unaltered",]
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


#component colors labels 
components <- c("Fall pulse flow", "Wet-season baseflow", "Peak flow", "Spring recession flow", "Dry-season baseflow")
colors <- c("#1b9e77","#d95f02","#7570b3","#e7298a","#66a61e")
labels <- data.frame(cbind(components, colors))
#subset based on what will be plotted
components.plot <- as.character(unique(mag.alt.n$flow_component))
labels.sub <- labels[labels$components %in% components.plot,]

ggplot(mag.alt.n)+
  geom_bar(aes(flow_component,count, fill = flow_component), 
           position='dodge',stat='identity', color = NA) +
  scale_fill_manual(name = "Altered Flow Components", labels = labels.sub$components, values = labels.sub$colors) +
  labs(x = NULL, y = NULL) + 
  theme(legend.position = "none", rect = element_blank(),
        line = element_blank(), text = element_blank()) 

bar <-  ggplot(mag.alt.n) +
  geom_bar(aes(flow_component,count,fill = flow_component), 
           position='dodge',stat='identity', color = NA) +
  scale_fill_manual(name = "Altered Flow Components", labels = labels.sub$components, values = labels.sub$colors) +
  labs(x = "Flow Components", y = "Number of\nAltered Magnitude Metrics") +
  theme(panel.background = element_rect(fill = "white")) +
  theme(axis.title.y = element_text(size = rel(1.4),face="bold")) + theme(axis.title.x = element_text(size = rel(1.4),face="bold")) +
  theme(axis.text=element_text(size=11))
bar

#ggsave(bar, filename="C:/Users/KristineT/Documents/Git/SOC_FESS/zoom_barplot_mag.alteration.jpg", dpi=300, height=8, width=11)
ggsave(bar, filename="C:/Users/KristineT.SCCWRP2K/Documents/Git/SOC_FESS/zoom_barplot_mag.alteration.jpg", dpi=300, height=8, width=11)




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
data <- read.csv(file=paste0(alteration.dir, "ffm_alteration.df.overall.join.Aliso.Oso.SmallCreeks.SanJuanLSPC.lowflowbias.csv"))

#####UPDATE: For peak magnitude metrics, put NA for now!
#alteration status NA for peak mag metrics
data$alteration.status[data$flow_component == "Peak flow" & data$flow_characteristic == "Magnitude (cfs)"] <- NA
#alteration direction NA for peak mag metrics
data$alteration.direction[data$flow_component == "Peak flow" & data$flow_characteristic == "Magnitude (cfs)"] <- NA


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
                                                       y = factor(flow_component, levels =  c("Fall pulse flow", "Wet-season baseflow", "Peak flow", "Spring recession flow", "Dry-season baseflow")),
                                                       fill = subbasin)) +
  geom_tile() +
  ylab(label = "Flow Component") + xlab(label="Hydrograph Element") +
  scale_fill_gradient(name = "Number of\nAltered Subbasins",
                      low = "#fef0d9",
                      high = "#b30000") +
  ggtitle(label = "Altered Subbasins in Aliso, Oso, and Smaller Coastal Tributaries") + theme_light() +
  theme(axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"))

mine.heatmap

#ggsave(mine.heatmap, file="C:/Users/KristineT/Documents/Git/SOC_FESS/heatmap_alteration_allsubbasins.jpg", dpi=300, height=8, width=12)
ggsave(mine.heatmap, file="C:/Users/KristineT.SCCWRP2K/Documents/Git/SOC_FESS/heatmap_alteration_allsubbasins.jpg", dpi=300, height=8, width=12)

#updated heatmap without frequency or ROC for annual report 2020/2021
#find Frequency and Rate of change (%)
freq.ind <- grep("Frequency", ffm_summary$flow_characteristic)
roc.ind <- grep("Rate of change", ffm_summary$flow_characteristic)
#remove freq and ROC from heatmap
ffm_summary2 <- ffm_summary[-c(freq.ind, roc.ind),]

mine.heatmap2 <- ggplot(data = ffm_summary2, mapping = aes(x = flow_characteristic,
                                                         y = factor(flow_component, levels =  c("Fall pulse flow", "Wet-season baseflow", "Peak flow", "Spring recession flow", "Dry-season baseflow")),
                                                         fill = subbasin)) +
  geom_tile() +
  ylab(label = "Flow Component") + xlab(label="Hydrograph Element") +
  scale_fill_gradient(name = "Number of\nAltered Subbasins",
                      low = "#fef0d9",
                      high = "#b30000") +
  ggtitle(label = "Altered Subbasins in Aliso, Oso, Smaller Coastal Tributaries, and San Juan") + theme_light() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) #+
  #geom_text()

mine.heatmap2

#ggsave(mine.heatmap2, file="C:/Users/KristineT/Documents/Git/SOC_FESS/heatmap_alteration.nofreqROC.allsubbasins.jpg", dpi=400, height=8, width=10)
ggsave(mine.heatmap2, file="C:/Users/KristineT.SCCWRP2K/Documents/Git/SOC_FESS/heatmap_alteration.nofreqROC.allsubbasins.jpg", dpi=400, height=8, width=10)



#simple study area plot highlighting focus for SAG presentation
study <- ggplot(basins) + 
  geom_sf(color = "grey", fill="white") +
  xlab("") + ylab("")

study <- ggplot(basins) + 
  geom_sf(color = "#969696", fill="#d9d9d9") +
  labs(x ="", y = "") + 
  annotation_scale() +
  annotation_north_arrow(pad_y = unit(0.9, "cm"),  height = unit(.8, "cm"),
                         width = unit(.8, "cm")) +
  theme(panel.background = element_rect(fill = "white"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_line(color = "white", size = 0.8))

  

domain <- study + geom_sf(data = basins3, color = "red", fill="white", size = 1.2) +
  labs(title="Study Domain for Flow Ecology Analysis",subtitle = "LSPC Model Domain", x ="", y = "") +
geom_sf(data = reaches, color = "#67a9cf", size = 0.5) 

#save domain map
#filename.domain <- paste0()
#ggsave(domain, file= "C:/Users/KristineT/Documents/Git/SOC_FESS/study_domain_allLSPC.jpg", dpi=400, height=6, width=8)
ggsave(domain, file= "C:/Users/KristineT.SCCWRP2K/Documents/Git/SOC_FESS/study_domain_allLSPC.jpg", dpi=400, height=6, width=8)

########################################################
# FFM METRIC alteration maps
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

# #for ref subbasins (upper trabuco (L02-040,  L02-041), replace as NA) 
# basins4$alteration.status.new[basins4$New_Name == "L02-040"] <- "NA"
# basins4$alteration.status.new[basins4$New_Name == "L02-041"] <- "NA"
# basins4$component_alteration[basins4$New_Name == "L02-040"] <- "NA"
# basins4$component_alteration[basins4$New_Name == "L02-041"] <- "NA"


#list of colors and alteration statuses, color current by alteration status
colors <- c("#cb181d", "#fdbe85", "#2171b5", "#f7f7f7", "#d9d9d9")
alteration.status.new <- c("Likely Altered High", "Likely Altered Low", "Likely Unaltered", "Indeterminate", "NA")
lookup <- data.frame(cbind(colors, alteration.status.new))

#output director for alteration maps FFMs
dir.alt <- paste0(alteration.dir, "AlterationMaps/")
dir.create(dir.alt)

#loop through each metric and plot
unique.ffm <- unique(basins4$ffm)

for(j in 1:length(unique.ffm)){
  #subset basins4 to ffm j
  basins4.sub <- basins4[basins4$ffm == unique.ffm[j],]
  
  #subset colors and status
  lookup.sub <- lookup[lookup$alteration.status.new %in% basins4.sub$alteration.status.new,]
  
  
  #find and replace names for timing low early, high late
  if(unique(basins4.sub$flow_characteristic) == "Timing (date)"){
    basins4.sub$alteration.status.new <- gsub("Likely Altered Low", "Likely Altered Early", basins4.sub$alteration.status.new)
    basins4.sub$alteration.status.new <- gsub("Likely Altered High", "Likely Altered Late", basins4.sub$alteration.status.new)
    lookup.sub$alteration.status.new <- gsub("Likely Altered Low", "Likely Altered Early", lookup.sub$alteration.status.new)
    lookup.sub$alteration.status.new <- gsub("Likely Altered High", "Likely Altered Late", lookup.sub$alteration.status.new)
  }
  unique(basins4.sub$alteration.status.new)
  
  #save as factor
  lookup.sub$alteration.status.new <- factor(lookup.sub$alteration.status.new, levels = lookup.sub$alteration.status.new)
  basins4.sub$alteration.status.new <- factor(basins4.sub$alteration.status.new, levels = lookup.sub$alteration.status.new)
  
  
  #base map 
  study2 <- ggplot(basins) + 
    geom_sf(color = "#969696", fill="#d9d9d9") +
    labs(title=unique(basins4.sub$title_name),x ="", y = "") + 
    annotation_scale() +
    annotation_north_arrow(pad_y = unit(0.9, "cm"),  height = unit(.8, "cm"),
                           width = unit(.8, "cm")) +
    theme(panel.background = element_rect(fill = "white"),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.grid = element_line(color = "white", size = 0.8))
  
  #filled alteration plots
  alt.plot <- study2 + geom_sf(data = basins4.sub, color= "#969696", aes(fill=alteration.status.new)) +
    scale_fill_manual(name = "Alteration Status", labels = lookup.sub$alteration.status.new, values=lookup.sub$colors) +
    geom_sf(data = reaches, color = "#67a9cf", size = 0.5) 
  
  # #add in model source
  # alt.plot <- alt.plot + geom_sf(data = basins4.sub, size = 1, fill = NA, aes(color=Source)) +
  #   scale_color_manual(name = "Model Source", labels = c("LSPC", "GSFLOW"), values=c("black", "hotpink")) +
  #   geom_sf(data = reaches, color = "#67a9cf", size = 0.5) 
  
  #print
 # print(alt.plot)
  
  #write plot
  #save as jpg
  plot.fname <- paste0(dir.alt,unique(basins4.sub$ffm), "_alteration.map.jpg")
  ggsave(alt.plot, file=plot.fname, dpi=300, height=8, width=12)
  
}


#############Loop through facet maps for components

#loop through each component and plot panel plots of the metrics
uniq.comp <- unique(basins4$flow_component)

#update the baseflow magnitude for title_ffm (Wet_BFL_Mag_10" "Wet_BFL_Mag_50" "DS_Mag_50" "DS_Mag_90")
unique(basins4$title_ffm2)
#Wet_BFL_Mag_10
basins4$title_ffm[basins4$ffm == "Wet_BFL_Mag_10"] <- " Magnitude 10th percentile (cfs)"
#Wet_BFL_Mag_50
basins4$title_ffm[basins4$ffm == "Wet_BFL_Mag_50"] <- " Magnitude 50th percentile (cfs)"
#Wet_BFL_Mag_10
basins4$title_ffm[basins4$ffm == "DS_Mag_50"] <- " Magnitude 50th percentile (cfs)"
#Wet_BFL_Mag_10
basins4$title_ffm[basins4$ffm == "DS_Mag_90"] <- " Magnitude 90th percentile (cfs)"

for(k in 1:length(uniq.comp)){
  #subset basins4 to ffm j
  basins4.sub <- basins4[basins4$flow_component == uniq.comp[k],]
  
  #subset colors and status
  lookup.sub <- lookup[lookup$alteration.status.new %in% basins4.sub$alteration.status.new,]
  #save as factor
  lookup.sub$alteration.status.new <- factor(lookup.sub$alteration.status.new, levels = unique(lookup.sub$alteration.status.new))
  basins4.sub$alteration.status.new <- factor(basins4.sub$alteration.status.new, levels = unique(lookup.sub$alteration.status.new))
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
    annotation_scale() +
    annotation_north_arrow(pad_y = unit(0.9, "cm"),  height = unit(.8, "cm"),
                           width = unit(.8, "cm")) +
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
  
  # #add in model source
  # alt.plot <- alt.plot + geom_sf(data = basins4.sub, size = 1, fill = NA, aes(color=Source)) +
  #   scale_color_manual(name = "Model Source", labels = c("LSPC", "GSFLOW"), values=c("black", "hotpink")) +
  #   geom_sf(data = reaches, color = "#67a9cf", size = 0.5) 
  
  #print
  #print(alt.plot)
  
  #write plot
  #save as jpg
  plot.fname <- paste0(dir.alt,unique(basins4.sub$flow_component), "_alteration.map.jpg")
  ggsave(alt.plot, file=plot.fname, dpi=300, height=8, width=10)
  
}


#####################################################
#create synthesis map for alteration vs wet, dry, peak

#subset component alteration data to wet, dry, peak
comp.synthesis <- c("Wet-season baseflow", "Peak flow", "Dry-season baseflow")
component.sub <- c[comp_alt$flow_component %in% comp.synthesis,] %>% 
  filter(component_alteration == "likely_altered") %>%
  group_by(New_Name) %>% 
  summarise(flow_component = toString(unique(flow_component))) %>% 
  ungroup() 
#check to see if subbasins with no alteration - some with all unaltered need to be added into component.sub as None
component.sub.unaltered <- comp_alt[comp_alt$flow_component %in% comp.synthesis,] %>% 
  filter(component_alteration == "likely_unaltered") %>%
  group_by(New_Name) %>% 
  summarise(flow_component = toString(unique(flow_component))) %>% 
  ungroup() 

#identify the subbasins with 3 unaltered component 
ind.unaltered <- grep("Wet-season baseflow, Peak flow, Dry-season baseflow", component.sub.unaltered$flow_component)
subbasins.unaltered <- component.sub.unaltered$New_Name[ind.unaltered]
setdiff(component.sub$New_Name, subbasins.unaltered)
#subset component.sub.unaltered to only the unaltered basins
component.sub.unaltered2 <- component.sub.unaltered[ind.unaltered,]
#change flow_component to None
component.sub.unaltered2$flow_component <- "None"
#combine none latered with altered dataset
component.sub <- rbind(component.sub, component.sub.unaltered2)


#save as data.frame
component.sub.df <- data.frame(component.sub)
#create new simplified categories
unique(component.sub.df$flow_component)

#get unaltered basin summary
component.sub.unaltered <- comp_alt[comp_alt$flow_component %in% comp.synthesis,] %>% 
  group_by(New_Name) %>% 
  summarise(component_alteration = toString(unique(component_alteration))) %>% 
  ungroup()
#turn to df
component.sub.unaltered.df <- data.frame(component.sub.unaltered)
#check to see if any NA (no alteration)
unique(component.sub.unaltered.df$component_alteration)

#combine with basins shapefile again
comp_alt_synth <- component.sub.df %>% 
  inner_join(basins, by = c('New_Name')) #%>% 
  #inner_join(source, by = c('New_Name'))
comp_alt_synth

#set new flow component alteration synthesis names
comp_alt_synth$flow_component <- gsub(" baseflow", "", comp_alt_synth$flow_component)
comp_alt_synth$flow_component <- gsub("flow", "Flow", comp_alt_synth$flow_component)
#find unique combos that need to be updated
unique(comp_alt_synth$flow_component)
comp_alt_synth$flow_component[comp_alt_synth$flow_component == "Wet-season, Peak Flow, Dry-season"] <- "All"
#comp_alt_synth$flow_component[comp_alt_synth$flow_component == "Dry-season, Wet-season, Peak Flow"] <- "All"
#comp_alt_synth$flow_component[comp_alt_synth$flow_component == "Dry-season, Wet-season"] <- "Wet-season, Dry-season"
comp_alt_synth$flow_component[comp_alt_synth$flow_component == "Peak Flow, Dry-season"] <- "Dry-season, Peak Flow"


unique(comp_alt_synth$flow_component)

# comp_alt_synth$altered_components <- factor(comp_alt_synth$flow_component, levels = c("All", "Wet-season, Dry-season", "Wet-season, Peak Flow", "Dry-season, Peak Flow", "Peak Flow"))
# colors <- c("#d7191c", "#fdae61", "#2c7bb6")
# levels <- c("All", "Dry-season, Peak Flow", "Peak Flow")

 comp_alt_synth$altered_components <- factor(comp_alt_synth$flow_component, levels = c("All", "Wet-season, Dry-season", "Wet-season, Peak Flow", "Dry-season", "Wet-season", "Peak Flow", "None"))
 colors <- c("#a50f15", "#d95f0e", "#fdae61", "#fff7bc", "#fee090", "pink", "#4575b4")
 levels <- c("All", "Wet-season, Dry-season", "Wet-season, Peak Flow", "Dry-season", "Wet-season", "Peak Flow", "None")

 #previous colors and categories
# comp_alt_synth$altered_components <- factor(comp_alt_synth$flow_component, levels = c("All",  "Wet-season, Peak Flow", "Dry-season, Peak Flow", "Peak Flow"))
# colors <- c("#d7191c", "#fdae61", "#fff7bc", "#2c7bb6")
# levels <- c("All", "Wet-season, Peak Flow", "Dry-season, Peak Flow", "Peak Flow")


#colors <- c("#ca0020", "#fdb863", "#5e3c99", "#a6dba0", "#b2abd2")
#levels <- c("All", "Wet-season, Dry-season", "Wet-season, Peak Flow", "Dry-season, Peak Flow", "Peak Flow")

#base map 
study2 <- ggplot(basins) + 
  geom_sf(color = "lightgrey", fill="white") +
  #geom_sf(color = "#969696", fill="white") +
  labs(title="Hydrologic Alteration Synthesis", subtitle = "Wet and Dry Season Baseflow, Peak Flow",x ="", y = "")  + 
  annotation_scale() +
  annotation_north_arrow(pad_y = unit(0.9, "cm"),  height = unit(.8, "cm"),
                         width = unit(.8, "cm")) +
  theme(panel.background = element_rect(fill = "white"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_line(color = "white", size = 0.8),
        plot.title = element_text(size=20),
        plot.subtitle = element_text(size=12),) 
study2

#make sure ref subbasins have NA
#for ref subbasins (upper trabuco (L02-040,  L02-041), replace as NA) 
#comp_alt_synth$altered_components[comp_alt_synth$New_Name == "L02-040"] <- NA
#comp_alt_synth$altered_components[comp_alt_synth$New_Name == "L02-041"] <- NA


#synthesis map
syn.plot <- study2 + geom_sf(data = comp_alt_synth, color= "gray89", aes(fill=altered_components, geometry = geometry)) +
  scale_fill_manual(name = "Alterated Components", labels = levels, values=colors) +
  geom_sf(data = reaches, color = "#67a9cf", size = 0.5) 

# #add in model source
# syn.plot2 <- syn.plot + geom_sf(data = comp_alt_synth, size = 1, fill = NA, aes(color=Source, geometry = geometry)) +
#   scale_color_manual(name = "Model Source", labels = c("LSPC", "GSFLOW"), values=c("black", "hotpink")) +
#   geom_sf(data = reaches, color = "#67a9cf", size = 0.5) 


#print
print(syn.plot)

#save image
plot.fname <- paste0(dir.alt, "Synthesis_Alteration_Map_wetdrypeak_allsubbasins.jpg")
ggsave(syn.plot, file=plot.fname, dpi=400, height=6, width=8)


