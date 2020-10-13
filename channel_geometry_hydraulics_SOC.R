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

#manning's n at the pourpoint reach [use this value rather than the reach average above]
mannings.n <- read.csv("L:/San Juan WQIP_KTQ/Data/SpatialData/Hydraulics/X_Sections_pourpts_modeled_condition2_mannings.csv") %>% 
  select(New_Name, FacilityTy, Material, Mannings_n) %>% 
  rename(Reach.ID = New_Name)


#read in lookup table with subbasin and X_SECT_ID, filter, and merge with reach metrics by Reach.ID and merge with manning's n at outlet
lookup <- read.csv("C:/Users/KristineT/Documents/Git/SOC_FESS/data/hydraulics/nearest_XS_pourpoints_final.csv") %>% 
  rename(Reach.ID = Subbasin) %>% 
  select(X_SECT_ID, Reach.ID) %>% 
  merge(reach.metrics, by = "Reach.ID") %>% 
  merge(mannings.n, by = "Reach.ID")


#read in list of subbasins that will be modeled (not all from above will get modeled)
modeled <- read.csv("C:/Users/KristineT/Documents/Git/SOC_FESS/data/hydraulics/Subbasins_subset_modeledonly_source.csv") %>% 
  rename(Reach.ID = New_Name)
  
#subset lookup to modeled only
lookup <- merge(lookup, modeled, by = "Reach.ID") %>% 
  rename(X_SECT_ID = X_SECT_ID.x)

#determine which sites don't have reach parameters, these need reach slopes calculated
no.param <- lookup[as.numeric(lookup$Slope) == 0,]

#unique ID
xs.id <- unique(lookup$X_SECT_ID) 
#xs.id <- xs.id[-which(is.na(xs.id))] #omit NA values


#### Loop for XS plots and calcs ####
#loop through each XS, plot XSA to determine where to split channel
  #note: need to ID split channels and get multiple calculations for each 
  #later: add in max depth, velocity, stream power, shear stress

#output the Max Q in rating curve
max.Q.rating <- rep(NA, length(xs.id))

for(i in 1:length(xs.id)){
  
  #subset geom data for xs i, calc cumulative distance across (station_m)
  geom.sub <- data.all %>% 
    filter(X_SECT_ID == xs.id[i]) %>% #filter to xs i 
    mutate(station_m = cumsum(Distance_M) - 0.15) #calc distance across, first value should be 0, subtract 0.15 from all
  
  #subbasin name
  subbasin.name <- na.omit(lookup$Reach.ID[lookup$X_SECT_ID == xs.id[i]])
  #channel type
  channel.type <- na.omit(lookup$FacilityTy[lookup$X_SECT_ID == xs.id[i]])
  
  #plot each XS  
  xs.prof <- ggplot(geom.sub, aes(x = station_m, y = ELEVATION_M)) +
    geom_line() + 
    labs(title = xs.id[i], subtitle = paste0("Subbasin: ", subbasin.name[1], "; ", channel.type), x = "Station (m)", y = "Elevation (m)") 
  
  print(xs.prof)
  #save plots
  file.name <- paste0("L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_hydraulics/XS_plots/", subbasin.name[1], "_",xs.id[i], "_XSplot.jpg")
  #ggsave(xs.prof, filename=file.name, dpi=300, height=5, width=8)
  
  
  #### create rating curves for Q and depth and Q and velocity ####
  #subset reach parameter data (manning's n and slope)
  param.sub <- lookup %>% 
    filter(X_SECT_ID == xs.id[i]) 
  
  #if slope is not 0 (NA), then go on to create rating table
  if(as.numeric(param.sub$Slope) > 0){
    ####determine total Q for given water surface elevation (wse)
    #find max_depth at capacity (based on lowest bank elev)
    bank_elev_min <- min(c(geom.sub$ELEVATION_M[1], geom.sub$ELEVATION_M[length(geom.sub$ELEVATION_M)]))
    thalweg_elev <-  min(geom.sub$ELEVATION_M) #min channel elev
    max_depth <- bank_elev_min - thalweg_elev #min bank elev minus thalweg elevation
    
    #list of water surface elevations to create rating curve
    wse_m <- seq(thalweg_elev,bank_elev_min, length.out = 100)
    
    #subset geometry data to L and R side of thalweg <- this will help with finding the L and R WSE stations
    #thalweg.ind <- grep(thalweg_elev, geom.sub$ELEVATION_M)
    #left.geom <- geom.sub[1:thalweg.ind[1],]
    #right.geom <- geom.sub[(thalweg.ind[1]+1):length(geom.sub$ELEVATION_M),]
    
    #loop to calculate Q for every WSE
    #create empty output vectors, first WSE is at thalweg so all values will be zero
    q.cms <- 0
    #top.width <- 0
    #bottom.width <- 0
    
    for (j in 2:length(wse_m)) {
      
      #### interpolate all of the WSE points that intersect with channel bed/banks at WSE
      X <- geom.sub$station_m
      Y1 <- rep(wse_m[j], length(X))
      Y2 <- geom.sub$ELEVATION_M

      # Find points where x1 is above x2.
      above <- Y1>Y2
      # Points always intersect when above=TRUE, then FALSE or above=FALSE, then TRUE
      intersect.points<-which(diff(above)!=0)
      # Find the slopes for each line segment.
      Y2.slopes <- (Y2[intersect.points+1]-Y2[intersect.points]) /
        (X[intersect.points+1]-X[intersect.points])
      Y1.slopes <- rep(0,length(Y2.slopes)) #horizontal line at WSE, slope = 0
      # Find the intersection for each segment
      X.points <- X[intersect.points] + ((Y2[intersect.points] - Y1[intersect.points]) / (Y1.slopes-Y2.slopes))
      Y.points <- Y1[intersect.points] + (Y1.slopes*(X.points-intersect.points))
      # Plot.
      #plot(Y1,type='l', ylim=c(3.75, 3.8), xlim=c(31, 34))
      #lines(X,Y2,type='l',col='red')
      #points(X.points,Y.points,col='blue')
      
      #add these interpolated points into geomsub, sort by station
      station.all <- c(X, X.points) 
      elev.all <- c(Y2, Y.points)
      #combine to new df and sort by station and subset to points within the first interp WSE to the last interp WSE
      geom.new <- data.frame(cbind(station.all, elev.all)) %>% 
        arrange(station.all) %>% 
        filter(station.all >= X.points[1] & station.all <= X.points[length(X.points)])
      #create own stations, elevations vectors to use in code below
      stations <- geom.new$station.all
      elevations <- geom.new$elev.all
      
      #### Flow calculuation for WSE j
      #calc the flow area - split channel into trapezoids and triangles as the first and last portions
      heights <- stations[2:length(stations)] - stations[1:(length(stations)-1)] #height of the shapes
      L1 <- wse_m[j] - elevations
        #L1 <- c(0,1,2,0,-1,0,3,2,1,0)
      #if negative L1 values, set to NA
      if(length(which(L1<0)) > 0){
        L1[L1<0] <- NA
      }
      L2 <- L1[2:length(L1)]
      #L1 should omit the last 0
      L1 <- L1[1:length(L1)-1]

      #calculate the area of the trapezoid slices and sum together to get total flow area
      flow.area <- (L1+L2)*.5 *heights
      #total.flow.area <- sum(flow.area, na.rm = TRUE) #total flow area
      
      ####Calculate other parameters for Manning's equation
      #wetted perimeter - sum of distances between each pt using pythag. thrm
      a <- heights
      b <- abs(L1 - L2)
      c <- sqrt(a^2 + b^2)
      wetted.perim <- c
      #wetted.perim <- sum(c, na.rm = TRUE)
      
      #hydraulic radius for each slice
      hyd.radius <- flow.area/wetted.perim #R
      #hyd.radius <- total.flow.area/wetted.perim #R
      
      #slope and manning's n taken from lookup table
      s <-as.numeric(param.sub$Slope)
      mannings.n <- as.numeric(param.sub$Mannings_n)
      #Manning's equation to estimate overall velocity
      v <- (hyd.radius^(2/3)* s^(1/2))/ mannings.n #manning's equation
      #flow in each slice
      q.cms.slice <- v*flow.area
      #total flow for that WSE
      q.cms[j] <- sum(q.cms.slice, na.rm = TRUE)
      #also want to get depths at specific points based on the WSE
      all.depths <- wse_m[j] - geom.new$elev.all 
      #replace all neg values with zero
      all.depths[all.depths<0] <- NA
      #add depths in each slice into geom_new df
      geom.new$depth_m <- all.depths
      #add velocity and Q for each slice into geom_new df, first velocity and Q is zero (at WSE)
      geom.new$velocity_ms <- c(0,  v)
      geom.new$q_cms <- c(0, q.cms.slice)
      
      #add in code to calc av depth, av. velocity, total. shear (hydradius [R] * slope * density water), total power (shear*av. vel) in LOB, MC, ROB
      #determine where channel is split, calc values for each subsection, summarize for each subsection
      #if station with split is not included in geom.new$station.all, then all values should be zero, else parse out geom.new and summarize
      
    }
    
    #plot Q WSE rating curve
    wse.rating <- data.frame(cbind(q.cms, wse_m))
    rating <- ggplot(wse.rating, aes(x=q.cms, y =wse_m)) +
      geom_line() +
      labs(title = xs.id[i], subtitle = paste0("Subbasin: ", subbasin.name[1]), y = "Water Surface Elevation (m)", x = "Discharge (cms)") 
    
    print(rating)
    
    #save max Q in rating table to compare with flow values from model
    max.Q.rating[i] <- max(q.cms, na.rm = TRUE)
    
  }

}

#omit first value of 
