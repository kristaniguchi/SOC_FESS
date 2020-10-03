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


#read in list of subbasins that will be modeled (not all from above will get modeled)
modeled <- read.csv("C:/Users/KristineT/Documents/Git/SOC_FESS/data/hydraulics/Subbasins_subset_modeledonly_source.csv") %>% 
  rename(Reach.ID = New_Name)
  
#subset lookup to modeled only
lookup <- merge(lookup, modeled, by = "Reach.ID") %>% 
  rename(X_SECT_ID = X_SECT_ID.x)

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
  #subset reach parameter data (manning's n and slope)
  param.sub <- lookup %>% 
    filter(X_SECT_ID == xs.id[i]) 
  
  #if slope is not 0, then go on to create rating table
  if(param.sub$Slope > 0){
    ####determine total Q for given water surface elevation (wse)
    #find max_depth at capacity (based on lowest bank elev)
    bank_elev_min <- min(c(geom.sub$ELEVATION_M[1], geom.sub$ELEVATION_M[length(geom.sub$ELEVATION_M)]))
    thalweg_elev <-  min(geom.sub$ELEVATION_M) #min channel elev
    max_depth <- bank_elev_min - thalweg_elev #min bank elev minus thalweg elevation
    
    #list of water surface elevations to create rating curve
    wse_m <- seq(thalweg_elev,bank_elev_min, length.out = 100)
    
    #subset geometry data to L and R side of thalweg <- this will help with finding the L and R WSE stations
    thalweg.ind <- grep(thalweg_elev, geom.sub$ELEVATION_M)
    left.geom <- geom.sub[1:thalweg.ind[1],]
    right.geom <- geom.sub[(thalweg.ind[1]+1):length(geom.sub$ELEVATION_M),]
    
    #loop to calculate Q for every WSE
    #create empty output vectors, first WSE is at thalweg so all values will be zero
    q.cms <- 0
    top.width <- 0
    bottom.width <- 0
    
    for (j in 2:length(wse_m)) {
      #subset the values <= WSE j
      #############need to update this subset --> so it doesn't filter out top of islands --> need to make those NA values or something
      geom.sub2 <- geom.sub %>% 
        filter(ELEVATION_M <= wse_m[j])
      
      #interpolate the L and R water surface station that intersects with the banks
      approx.L <- approx(left.geom$ELEVATION_M, left.geom$station_m, wse_m[j], ties=min) #want to find distance on L for that wse
      approx.R <- approx(right.geom$ELEVATION_M, right.geom$station_m, wse_m[j], ties=max) #want to find distance on R for that wse
      wse.L.station <- approx.L$y
      wse.R.station <- approx.R$y
      #combine with stations and elevations
      stations <- c(wse.L.station, geom.sub2$station_m, wse.R.station)
      elevations <- c(wse_m[j], geom.sub2$ELEVATION_M, wse_m[j])
      
      #calc the flow area - split channel into trapezoids and triangles as the first and last portions
      heights <- stations[2:length(stations)] - stations[1:(length(stations)-1)] #height of the shapes
      L1 <- wse_m[j] - elevations
      L2 <- L1[2:length(L1)]
      #L1 should omit the last 0
      L1 <- L1[1:length(L1)-1]
      #calculate the area of the trapezoids and sum together to get total flow area
      flow.area <- (L1+L2)*.5 *heights
      total.flow.area <- sum(flow.area) #total flow area
      
      #calc top width:
      top.width[j] <- wse.R.station - wse.L.station
        
      #wetted perimeter - sum of distances between each pt using pythag. thrm
      a <- heights
      b <- abs(L1 - L2)
      c <- sqrt(a^2 + b^2)
      wetted.perim <- sum(c)
      
      #hydraulic radius
      hyd.radius <- total.flow.area/wetted.perim #R
      
      #slope and manning's n taken from lookup table
      s <-as.numeric(param.sub$Slope)
      mannings.n <- as.numeric(param.sub$Mannings.n)
      #Manning's equation to estimate overall Velocity
      v <- (hyd.radius^(2/3)* s^(1/2))/ mannings.n #manning's equation
      q.cms[j] <- v*total.flow.area
      
      #also want to get depths at specific points based on the WSE
      all.depths <- wse_m[j] - geom.sub$ELEVATION_M 
      #replace all neg values with zero
      all.depths[all.depths<0] <- 0
      
    }
    
    #plot Q WSE rating curve
    wse.rating <- data.frame(cbind(q.cms, wse_m))
    rating <- ggplot(wse.rating, aes(x=wse_m, y = q.cms)) +
      geom_line() +
      labs(title = xs.id[i], subtitle = paste0("Subbasin: ", subbasin.name[1]), x = "Water Surface Elevation (m)", y = "Discharge (cms)") 
    
    print(rating)
    
  }

}
