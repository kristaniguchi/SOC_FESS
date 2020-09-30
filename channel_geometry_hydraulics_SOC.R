#SOC FESS: Hydraulics Calculations - Simple

#### load packages ####
library("tidyverse")

#### load data ####
#XS geometry raw data for every XS in OC survey database
data1 <- read.csv("C:/Users/KristineT/Documents/Git/SOC_FESS/data/hydraulics/X_Sections_3D_Elevations.csv")
data2 <- read.csv("C:/Users/KristineT/Documents/Git/SOC_FESS/data/hydraulics/X_Sections_3D_Elevations2.csv")
data.all <- data.frame(rbind(data1, data2))

#model reach values (slope, manning's n)
reach.metrics <- read.csv("C:/Users/KristineT/Documents/Git/SOC_FESS/data/hydraulics/Full_Model_Reaches_av_geom_metrics.csv") %>% 
  select("Reach.ID", "LSPC.ID", "Slope", "Mannings.n", "Downstream.ID", "Downstream.LSPC.ID")
reach.metrics2 <- reach.metrics[2:length(reach.metrics$Reach.ID),]



#read in lookup table with subbasin and X_SECT_ID, filter, and merge with reach metrics by Reach.ID
lookup <- read.csv("C:/Users/KristineT/Documents/Git/SOC_FESS/data/hydraulics/nearest_XS_pourpoints_final.csv") %>% 
  rename(Reach.ID = Subbasin) %>% 
  select(X_SECT_ID, Reach.ID) %>% 
  merge(reach.metrics, by = "Reach.ID")

#determine which sites don't have reach parameters
no.param <- lookup[as.numeric(lookup$Slope) == 0,]


#unique ID
xs.id <- unique(lookup$X_SECT_ID) 
xs.id <- xs.id[-which(is.na(xs.id))] #omit NA values


#### Loop for XS plots and calcs ####
#loop through each XS, plot XSA to determine where to split channel
  #note: need to ID split channels and get multiple calculations for each 
  #later: add in max depth, velocity, stream power, shear stress

for(i in 1:length(xs.id)){
  
  #subset geom data for xs i, calc cumulative distance across (station_m)
  geom.sub <- data.all %>% 
    filter(X_SECT_ID == xs.id[i]) %>% #filter to xs i 
    mutate(station_m = cumsum(Distance_M) - 0.15) #calc distance across, first value should be 0, subtract 0.15 from all
  
  #subbasin name
  subbasin.name <- na.omit(lookup$Reach.ID[lookup$X_SECT_ID == xs.id[i]])
  
  #plot each XS  
  xs.prof <- ggplot(geom.sub, aes(x = station_m, y = ELEVATION_M)) +
    geom_line() + 
    labs(title = xs.id[i], subtitle = paste0("Subbasin: ", subbasin.name[1]), x = "Station (m)", y = "Elevation (m)") 
  
  print(xs.prof)
  #save plots
  file.name <- paste0("L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_hydraulics/XS_plots/", subbasin.name[1], "_",xs.id[i], "_XSplot.jpg")
  ggsave(xs.prof, filename=file.name, dpi=300, height=5, width=8)
  
  
  #### create rating curves for Q and depth and Q and velocity
  #find max_depth at capacity (based on lowest bank elev)
  bank_elev_min <- min(c(geom.sub$ELEVATION_M[1], geom.sub$ELEVATION_M[length(geom.sub$ELEVATION_M)]))
  max_depth <- bank_elev_min - min(geom.sub$ELEVATION_M) #min bank elev minus thalweg elevation
  
  #list of depths to create rating curve
  depth_m <- seq(0,max_depth, length.out = 100)
  
  #subset reach parameter data
  param.sub <- lookup %>% 
    filter(X_SECT_ID == xs.id[i]) 
  
  
  
  #if slope is 0, then put NA in 
  
  #estimate Q
  
  

}
