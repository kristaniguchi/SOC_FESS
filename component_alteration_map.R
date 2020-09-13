#Flow component alteration map

#install.packages("ggsn")
#install.packages("ggmap")
#install.packages("mapview")
#install.packages("geosphere")
#install.packages("rgeos")
  #to install spDataLarge
#devtools::install_github("robinlovelace/geocompr")
#library(spData)
#install.packages("spDataLarge")
#install.packages("spDataLarge", repos = "https://nowosad.github.io/drat/", type = "source")
#library(spDataLarge)

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


#load in data

#component alteration data
fname = "data/summary_component_alteration.csv" #update to directory
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
ggsave(sub1, filename="C:/Users/KristineT.SCCWRP2K/Documents/Git/SOC_FESS/subbasin1_compalteration.jpg", dpi=300, height=8, width=8)


#create separate barplot for just the first item

  
bar <-  ggplot(centroids[centroids$New_Name == unique.sites[1],]) +
        geom_bar(aes(factor(flow_component),n_ffm_altered,group=New_Name, fill = factor(flow_component)), 
        position='dodge',stat='identity', color = NA) +
        scale_fill_manual(name = "Flow Components", labels = c("Fall pulse flow", "Wet-season base flow", "Peak flow", "Spring recession flow", "Dry-season base flow"), values = c("#1b9e77","#d95f02","#7570b3","#e7298a","#66a61e")) +
        labs(x = "Flow Components", y = "Number of Altered Metrics") +
        theme(panel.background = element_rect(fill = "white"))
bar
ggsave(bar, filename="C:/Users/KristineT.SCCWRP2K/Documents/Git/SOC_FESS/zoom_barplot_compalteration.jpg", dpi=300, height=5, width=8.5)


#################################################
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


# bar.testplot_list <- 
#   lapply(1:length(unique.sites), function(i) { 
#     #subset to unique site i
#     sub.i <- centroids[centroids$New_Name == unique.sites[i],]
#     #get number of altered mag flow metrics in each component
#     mag.alt.n <- data.frame(aggregate(sub.i, by = sub.i[c('flow_component','alteration_status')], length))
#     mag.alt.n$count <- mag.alt.n$long
#     #subset to only likely altered
#     mag.alt.n <- mag.alt.n[mag.alt.n$alteration_status == "likely_altered",]
#     gt_plot <- ggplotGrob(
#       ggplot(mag.alt.n)+
#         geom_bar(aes(factor(flow_component),count, fill = factor(flow_component)), 
#                  position='dodge',stat='identity', color = NA) +
#         scale_fill_manual(name = "Altered Flow Components", labels = c("Fall pulse flow", "Wet-season base flow", "Peak flow", "Spring recession flow", "Dry-season base flow"), values = c("#1b9e77","#d95f02","#7570b3","#e7298a","#66a61e")) +
#         labs(x = NULL, y = NULL) + 
#         theme(legend.position = "none", rect = element_blank(),
#               line = element_blank(), text = element_blank()) 
#     )
#     panel_coords <- gt_plot$layout[gt_plot$layout$name == "panel",]
#     gt_plot[panel_coords$t:panel_coords$b, panel_coords$l:panel_coords$r]
#   })

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
data <- read.csv(file="L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_flowalteration_assessment/ffm_alteration.df.overall.join.csv")
#exclude the additive subbasins
data <- data[26:length(data$subbasin),]
#subset to altered only
altered <- data[data$alteration_status == "likely_altered",]
#remove NA
altered <- altered[-which(is.na(altered$subbasin)),]

#subset so if there is one altered characteristic per component (remove duplicates from one subbasin so we get number of subbasins with altered flow characteristics)
unique.altered.sites <- unique(altered$subbasin_model)
#create empty df that will be filled
altered.new <- altered[1,]
#fill with NA for first row to be removed later
altered.new[1,] <- NA
altered.new$comp.characteristic <- NA

for(i in 1:length(unique.altered.sites)){
  sub1 <- altered[altered$subbasin_model == unique.altered.sites[i],]
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
  ggtitle(label = "Altered Subbasins in Aliso and Oso") + theme_light() +
  theme(axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"))

mine.heatmap

ggsave(mine.heatmap, file="C:/Users/KristineT.SCCWRP2K/Documents/Git/SOC_FESS/heatmap_alteration.jpg", dpi=300, height=8, width=12)


#simple study area plot highlighting focus for SAG presentation
study <- ggplot(basins) + 
  geom_sf(color = "grey", fill="white") +
  xlab("") + ylab("")

study + geom_sf(data = basins3, color = "yellow", fill="white", size = 1.5) +
geom_sf(data = reaches, color = "#67a9cf", size = 0.5) 





















#########
#plot barplot

bar.testplot_list <- 
  lapply(1:length(basins2$New_Name), function(i) { 
    gt_plot <- ggplotGrob(
      ggplot(geo_data[geo_data$id == i,])+
        geom_bar(aes(factor(id),value,group=who), fill = rainbow(length(basins2$New_Name))[i],
                 position='dodge',stat='identity', color = "black") +
        labs(x = NULL, y = NULL) + 
        theme(legend.position = "none", rect = element_blank(),
              line = element_blank(), text = element_blank()) 
    )
    panel_coords <- gt_plot$layout[gt_plot$layout$name == "panel",]
    gt_plot[panel_coords$t:panel_coords$b, panel_coords$l:panel_coords$r]
  })

#add barplots to intial map with annotation_custom
bar_annotation_list <- lapply(1:length(basins2$New_Name), function(i) 
  annotation_custom(bar.testplot_list[[i]], 
                    xmin = map.test.centroids$x[map.test.centroids$KANTONSNUM == as.character(basins2$New_Name[i])] - 5e3,
                    xmax = map.test.centroids$x[map.test.centroids$KANTONSNUM == as.character(basins2$New_Name[i])] + 5e3,
                    ymin = map.test.centroids$y[map.test.centroids$KANTONSNUM == as.character(basins2$New_Name[i])] - 5e3,
                    ymax = map.test.centroids$y[map.test.centroids$KANTONSNUM == as.character(basins2$New_Name[i])] + 5e3) )

result_plot <- Reduce(`+`, bar_annotation_list, map.test)


















  geom_bar(data=centroids, aes(x = smc_lu, y = count, fill = factor(av.lat.rating)), stat = "identity", position = position_fill(reverse = TRUE), width = 0.7)
  


geom_rect(data = centroids,
            aes(xmin = long - barwidth,
                xmax = long + barwidth,
                ymin = lat,
                ymax = lat + n_ffm_altered*barheight)) + 
  geom_text(data = centroids %>% filter(!is.na(subbasin)),
            aes(x = long,
                y = lat + 0.5*n_ffm_altered*0.75,
                label = paste0(n_ffm_altered," %")),
            size = 2) + 
  ggsave(file = "test.pdf",
         width = 10,
         height = 10) 


  

ggplot()+ 
  geom_sf(data = worldMap, color = "black",fill = "lightgrey",
          colour = "white", size = 0.1)+
  coord_sf(xlim = c(-13, 35),  ylim = c(32, 71)) + 
  geom_rect(data = centroids,
            aes(xmin = long - barwidth,
                xmax = long + barwidth,
                ymin = lat,
                ymax = lat + Growth*barheight)) + 
  geom_text(data = centroids %>% filter(!is.na(Growth)),
            aes(x = long,
                y = lat + 0.5*Growth*0.75,
                label = paste0(Growth," %")),
            size = 2) + 
  ggsave(file = "test.pdf",
         width = 10,
         height = 10) 
  
  
  library(tidyverse)
#library(rworldmap)
library(sf)
# Data 
library(spData)      
library(spDataLarge)

# Get map data
worldMap <- map_data("world")

# Select only some countries and add values
europe <- 
  data.frame("country"=c("Austria",
                         "Belgium", 
                         "Germany",
                         "Spain", 
                         "Finland", 
                         "France", 
                         "Greece", 
                         "Ireland", 
                         "Italy", 
                         "Netherlands", 
                         "Portugal",
                         "Bulgaria","Croatia","Cyprus", "Czech Republic","Denmark","Estonia", "Hungary",
                         "Latvia", "Lithuania","Luxembourg","Malta", "Poland", "Romania","Slovakia",
                         "Slovenia","Sweden","UK", "Switzerland",
                         "Ukraine", "Turkey", "Macedonia", "Norway", "Slovakia", "Serbia", "Montenegro",
                         "Moldova", "Kosovo", "Georgia", "Bosnia and Herzegovina", "Belarus", 
                         "Armenia", "Albania", "Russia"),
             "Growth"=c(1.0, 0.5, 0.7, 5.2, 5.9, 2.1, 
                        1.4, 0.7, 5.9, 1.5, 2.2, rep(NA, 33)))

# Merge data and keep only Europe map data

data("world")

worldMap <- world

worldMap$value <- europe$Growth[match(worldMap$region,europe$country)]

centres <- 
  worldMap %>%
  filter()
st_centroid()

worldMap <- worldMap %>%
  filter(name_long %in% europe$country) 

# Plot it 

centroids <- 
  centres$geom %>% 
  purrr::map(.,.f = function(x){data.frame(long = x[1],lat = x[2])}) %>% 
  bind_rows %>% data.frame(name_long = centres$name_long) %>% 
  left_join(europe,by = c("name_long" = "country"))


barwidth = 1
barheight = 0.75

ggplot()+ 
  geom_sf(data = worldMap, color = "black",fill = "lightgrey",
          colour = "white", size = 0.1)+
  coord_sf(xlim = c(-13, 35),  ylim = c(32, 71)) + 
  geom_rect(data = centroids,
            aes(xmin = long - barwidth,
                xmax = long + barwidth,
                ymin = lat,
                ymax = lat + Growth*barheight)) + 
  geom_text(data = centroids %>% filter(!is.na(Growth)),
            aes(x = long,
                y = lat + 0.5*Growth*0.75,
                label = paste0(Growth," %")),
            size = 2) + 
  ggsave(file = "test.pdf",
         width = 10,
         height = 10)
