#CEFF Application Manuscript - SOC FESS
#Hydraulics Calculations - rating curve development at Aliso @ STP, current morphology and restored
#Calc av depth, max depth, av. velocity, total. shear (hydradius [R] * slope * density water), total power (shear*av. vel) in LOB, MC, ROB
####updated the WSE max based on identified station of lowest bank, add in horizontal blue line for max WSE to plot
####also subset the geom to be summarized up to that lower bank station

#hydraulic variables
hyd.vars <- c("av.depth.m", "max.depth.m","av.vel.ms","total.shear.Pa", "total.power.watt.ms")


#### load packages ####
library("tidyverse")

#### load data ####
#read in channel split info for each XS - updated for restored aliso XS
split <- read.csv("C:/Users/KristineT.SCCWRP2K/SCCWRP/SOC WQIP - Flow Ecology Study - General/Manuscripts/CEFF_casestudy/data/lookup_reach_manningsn_channeltype_splits_092021_restoredxs.csv")
#split column indices
split.col.ind <- grep("split", names(split))

#XS geometry raw data for every XS in OC survey database
#laptop directory
#data1 <- read.csv("C:/Users/KristineT.SCCWRP2K/Documents/Git/SOC_FESS/data/hydraulics/X_Sections_3D_Elevations.csv")
#data2 <- read.csv("C:/Users/KristineT.SCCWRP2K/Documents/Git/SOC_FESS/data/hydraulics/X_Sections_3D_Elevations2.csv")
#data.all <- data.frame(rbind(data1, data2))
#make sure all distance between pts is 0.1524 (0.5 ft)
#read in xs geometry for new rating curve
data.all <- read.csv("C:/Users/KristineT.SCCWRP2K/SCCWRP/SOC WQIP - Flow Ecology Study - General/Manuscripts/CEFF_casestudy/data/aliso_geometry_revised.csv")
unique(data.all$Distance_M)

###model reach values (slope, manning's n)
reach.metrics <- read.csv("C:/Users/KristineT.SCCWRP2K/Documents/Git/SOC_FESS/data/hydraulics/Full_Model_Reaches_av_geom_metrics.csv") %>% 
  select("Reach.ID", "LSPC.ID", "Slope", "Mannings.n", "Downstream.ID", "Downstream.LSPC.ID")
reach.metrics2 <- reach.metrics[2:length(reach.metrics$Reach.ID),]


#manning's n at the pourpoint reach [use this value rather than the reach average above]
mannings.n <- read.csv("L:/San Juan WQIP_KTQ/Data/SpatialData/Hydraulics/X_Sections_pourpts_modeled_condition2_mannings.csv") %>% 
  select(New_Name, FacilityTy, Material, Mannings_n) %>% 
  rename(Reach.ID = New_Name)


#read in lookup table with subbasin and X_SECT_ID, filter, and merge with reach metrics by Reach.ID and merge with manning's n at outlet
lookup <- read.csv("C:/Users/KristineT.SCCWRP2K/Documents/Git/SOC_FESS/data/hydraulics/nearest_XS_pourpoints_final.csv") %>% 
  rename(Reach.ID = Subbasin) %>% 
  select(X_SECT_ID, Reach.ID) %>% 
  merge(reach.metrics, by = "Reach.ID") %>% 
  merge(mannings.n, by = "Reach.ID")

#write.csv(lookup, file="L:/San Juan WQIP_KTQ/Data/SpatialData/Hydraulics/lookup_reach_manningsn_channeltype.csv", row.names=FALSE)


#read in list of subbasins that will be modeled (not all from above will get modeled)
modeled <- read.csv("C:/Users/KristineT.SCCWRP2K/Documents/Git/SOC_FESS/data/hydraulics/Subbasins_subset_modeledonly_source.csv") %>% 
  rename(Reach.ID = New_Name)

#subset lookup to modeled only
lookup <- merge(lookup, modeled, by = "Reach.ID") %>% 
  rename(X_SECT_ID = X_SECT_ID.x)

#determine which sites don't have reach parameters, these need reach slopes calculated
no.param <- lookup[as.numeric(lookup$Slope) == 0,]
#write.csv(no.param, file="L:/San Juan WQIP_KTQ/Data/SpatialData/Hydraulics/missing.slope.XSECTID.csv")

#unique ID
xs.id <- unique(lookup$X_SECT_ID) 
#xs.id <- xs.id[-which(is.na(xs.id))] #omit NA values

#test Aliso_1_42 for split channel calcs
i <- grep("Aliso_1_42", xs.id)
#test Segunda_S_14_3
#i <- grep("Segunda_S_14_3", xs.id)


#### Loop for XS plots and calcs ####
#loop through each XS, plot XSA to determine where to split channel
#note: need to ID split channels and get multiple calculations for each 
#later: add in max depth, velocity, stream power, shear stress

#output the Max Q in rating curve for each xs
max.Q.rating <- rep(NA, length(xs.id))

for(i in i){
  
  #subset geom data for xs i, calc cumulative distance across (station_m)
  geom.sub <- data.all %>% 
    filter(X_SECT_ID == xs.id[i]) 
  
  #subbasin name
  subbasin.name <- na.omit(lookup$Reach.ID[lookup$X_SECT_ID == xs.id[i]])
  #channel type
  channel.type <- na.omit(lookup$FacilityTy[lookup$X_SECT_ID == xs.id[i]])
  
  #add in channel split info
  split.sub <- split[split$X_SECT_ID == xs.id[i],split.col.ind] %>% 
    pivot_longer(names(split[,split.col.ind])) %>% 
    as.matrix(nrow = length(split.col.ind), ncol=2) %>% 
    data.frame()
  #determine if splits or no splits (if first split value is NA then no)
  if(is.na(split.sub$value[1])){
    split.y.n <- "N"
    split.num <- 0
  }else{
    split.y.n <- "Y"
    
    #split stations listed into a vector
    split.stations.all <- as.numeric(na.omit(split.sub$value))
    split.num <- length(split.stations.all)
    #find actual station value closes to split.stations.all
    split.stations.actual <- NA
    
    for(l in 1:split.num){
      #find index of station closest to split.stations.all
      ind.station <- which(abs(geom.sub$station_m - split.stations.all[l])==min(abs(geom.sub$station_m - split.stations.all[l]), na.rm = TRUE))
      split.stations.actual[l] <- geom.sub$station_m[ind.station] 
    }
    #find all of the breaks to subsection each off of (add the first and last stations to list), will use to subset channel later on
    split.stations.to.subset <- c(0, split.stations.actual, geom.sub$station_m[length(geom.sub$station_m)])
  }
  
  
  ##### plot each XS ####
  
  #general plot
  #xs.prof <- ggplot(geom.sub, aes(x = station_m, y = ELEVATION_M)) +
  #geom_line() + 
  #labs(title = xs.id[i], subtitle = paste0("Subbasin: ", subbasin.name[1], "; ", channel.type), x = "Station (m)", y = "Elevation (m)") 
  #print(xs.prof)
  #save plots
  #file.name <- paste0("L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_hydraulics/XS_plots/", subbasin.name[1], "_",xs.id[i], "_XSplot.jpg")
  #ggsave(xs.prof, filename=file.name, dpi=300, height=5, width=8)
  
  
  #plot with added axes labels to determine channel positions and add vertical lines for channel splits
  spacing <- 1
  #if trabuco (name has L02), then spacing every 10 m 
  if(length(grep("L02",subbasin.name ))> 0){
    spacing <- 2
  }
  xs.prof2 <- ggplot(geom.sub, aes(x = station_m, y = ELEVATION_M)) +
    geom_line() + 
    #geom_point()+
    scale_x_continuous(breaks = round(seq(0, max(geom.sub$station_m), by = spacing),1), expand = c(0, 0)) +
    labs(title = xs.id[i], subtitle = paste0("Subbasin: ", subbasin.name[1], "; ", channel.type), x = "Station (m)", y = "Elevation (m)") 
  
  #if channel splits, add vertical lines for channel split
  if(split.y.n == "Y"){
    xs.prof2 <- xs.prof2 +
      geom_vline(xintercept = split.stations.actual, linetype="longdash")
  }
  
  #print plot
  print(xs.prof2)
  #save plots
  file.name2 <- paste0("C:/Users/KristineT.SCCWRP2K/SCCWRP/SOC WQIP - Flow Ecology Study - General/Manuscripts/CEFF_casestudy/data/", subbasin.name[1], "_",xs.id[i], "_XSrestoredplot.jpg")
  
  #write geometry data to csv
  #write.csv(geom.sub, file="C:/Users/KristineT.SCCWRP2K/SCCWRP/SOC WQIP - Flow Ecology Study - General/Manuscripts/CEFF_casestudy/data/aliso_geometry_original.csv")
  
  #if station > 40, width of jpg should be larger, if > 80 width should be larger
  if(max(geom.sub$station_m) > 40 & max(geom.sub$station_m) < 80 ){
    width.print <- 12
  }else{
    if(max(geom.sub$station_m) > 80){
      width.print <- 17
    }else{
      width.print <- 8
    }
  }
  #save plot
  ggsave(xs.prof2, filename=file.name2, dpi=300, height=5, width=width.print)
  
  #TEST check WSE on xsgeom
  #xs.prof2 + geom_hline(yintercept=8.985354)
  
  #### create rating curves for Q and WSE ####
  #subset reach parameter data (manning's n and slope)
  param.sub <- lookup %>% 
    filter(X_SECT_ID == xs.id[i]) 
  
  #if slope is not 0 (NA), then go on to create rating table
  if(as.numeric(param.sub$Slope) > 0){
    ####determine total Q for given water surface elevation (wse)
    
    #max WSE station: find index of closest station
    WSE.max.station.estimate <- split$station_WSE_Max[split$X_SECT_ID == xs.id[i]]    
    #if concrete channel without max WSE station designated, use lowest bank elev
    if(is.na(WSE.max.station.estimate)){
      #find max_depth at capacity (based on lowest bank elev)
      bank_elev_min <- min(c(geom.sub$ELEVATION_M[1], geom.sub$ELEVATION_M[length(geom.sub$ELEVATION_M)]))
      #find station of WSE.max.station (lowest bank)
      #if elev is left then WSE.max.station is first value, else it is the last
      diff.left <- geom.sub$ELEVATION_M[1] - bank_elev_min
      diff.right <- geom.sub$ELEVATION_M[length(geom.sub$ELEVATION_M)] - bank_elev_min
      if(diff.left == 0){
        WSE.max.station <- geom.sub$station_m[1]
        ind.wse.max.station <- 1
      }else{
        WSE.max.station <- geom.sub$station_m[length(geom.sub$ELEVATION_M)]
        ind.wse.max.station <- length(geom.sub$ELEVATION_M)
      }
      
    }else{
      #find index of wse.max.station
      ind.wse.max.station <- which(abs(geom.sub$station_m - WSE.max.station.estimate)==min(abs(geom.sub$station_m - WSE.max.station.estimate)))
      #closest station to the max WSE (lowest bank elev)
      WSE.max.station <- geom.sub$station_m[ind.wse.max.station] 
      #elevation to the lowest bank at WSE.max.station
      bank_elev_min <- geom.sub$ELEVATION_M[ind.wse.max.station]
      #need to update split.stations.to.subset depending on L or R replace with WSE.max.station
      #if WSE.max.station is on left then WSE.max.station is first value, else it is the last
      diff.left <- abs(WSE.max.station - geom.sub$station_m[1]) 
      diff.right <- abs(geom.sub$station_m[length(geom.sub$station_m)] - WSE.max.station)
      #if on left bank (diff.left < diff.right), change first split to WSE.max.station
      if(diff.left < diff.right){
        split.stations.to.subset[1] <-  WSE.max.station
      }else{
        #else it is on the right bank, change last split to WSE.max.station
        split.stations.to.subset[length(split.stations.to.subset)] <-  WSE.max.station
      }
    }
    
    #find thalweg elevation
    thalweg_elev <-  min(geom.sub$ELEVATION_M) #min channel elev
    
    #list of water surface elevations to create rating curve
    wse_length <- 200
    wse_m <- seq(thalweg_elev,bank_elev_min, length.out = wse_length)
    
    #filter channel geometry only to exclude any weird channel floodplain features outside of the max WSE on the lowest bank elevation
    #determine if the lowest bank is on the left or the right side of channel (will filter out fp values outside of this bank)
    #find difference between station and first value vs. difference between station and last value
    dist.to.first.pt <- abs(geom.sub$station_m[1] - WSE.max.station)
    dist.to.last.pt <- abs(geom.sub$station_m[length(geom.sub$ELEVATION_M)] - WSE.max.station)
    #if WSE max station is closer to the first pt, lower bank is on the left; else it is on the right
    #if lower bank is on the left (distance of lower bank station is closer to first pt)
    if(dist.to.first.pt < dist.to.last.pt){
      #subset geom.sub from ind.wse.max.station to last 
      geom.sub <- geom.sub[ind.wse.max.station:length(geom.sub[,1]),]
    }else{
      #else, lower bank is on right, subset from first point to the ind.wse.max.station
      geom.sub <- geom.sub[1:ind.wse.max.station,]
    }
    
    
    #loop to calculate Q for every WSE and summarize
    #create empty output vectors, first WSE is at thalweg elev. so all values will be zero
    q.cms <- 0
    
    #create output df for split channel rating curves
    #total sections or slices
    total.sections <- split.num + 1
    #column names will be discharge (total), each hydraulic variable name with _ section number (1:6)
    #create vector of column names for each slice/section
    hyd.var.col.names <- NA
    for(o in 1:total.sections){
      hyd.var.col.names <- c(hyd.var.col.names, paste0(hyd.vars, "_slice", o) )
    }
    hyd.var.col.names <- hyd.var.col.names[2:length(hyd.var.col.names)]
    numbercols.total <- length(hyd.var.col.names) + 1 #hyd variable columns plus Q
    
    #create output dataframe with each row for each WSE
    out.rating.all <- data.frame(matrix(nrow=wse_length, ncol=numbercols.total))
    #first column will be zeros for zero Q
    out.rating.all[1,] <- 0
    #set col names
    names(out.rating.all) <- c("q.cms", hyd.var.col.names)
    
    for (j in 2:length(wse_m)) {
      
      #Estimating Q from manning's for WSE j
      #### interpolate all of the WSE points that intersect with channel bed/banks at WSE
      X <- geom.sub$station_m
      Y1 <- rep(wse_m[j], length(X))
      Y2 <- geom.sub$ELEVATION_M
      
      # Find points where x1 is above x2.
      above <- Y1>Y2
      # Points always intersect when above=TRUE, then FALSE or above=FALSE, then TRUE
      intersect.points <- which(diff(above)!=0)
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
      
      
      #slope and manning's n taken from lookup table
      s <-as.numeric(param.sub$Slope)
      Mannings.n <- as.numeric(param.sub$Mannings_n)
      #Manning's equation to estimate overall velocity
      v <- (hyd.radius^(2/3)* s^(1/2))/ Mannings.n #manning's equation
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
      geom.new$flow.area <- c(0, flow.area)
      geom.new$wetted.perim <- c(0, wetted.perim)
      
      ##### Hydraulic Calculations
      #Calc av depth, max depth, av. velocity, total. shear (hydradius [R] * slope * density water), total power (shear*av. vel) in LOB, MC, ROB
      #determine where channel is split, calc values for each subsection, summarize for each subsection
      #if station with split is not included in geom.new$station.all, then all values should be zero, else parse out geom.new and summarize
      if(split.y.n == "n"){
        #if not split (concrete channel), calc metrics across the entire XS
        #for all of the variables, loop through the variables and find value for given flow
        
        #flow at wse j 
        out.rating.all$q.cms[j] <- q.cms[j] 
        
        #determine the hydraulic variables for each wse/Q j
        ##depth
        #remove zeros from depths
        depth.nozero <- geom.new$depth_m[geom.new$depth_m >0]
        #av depth
        out.rating.all$av.depth.m_slice1[j] <- mean(depth.nozero, na.rm= TRUE)
        #max depth
        out.rating.all$max.depth.m_slice1[j] <- max(depth.nozero, na.rm= TRUE)
        
        #av. velocity
        vel.nozero <- geom.new$velocity_ms[geom.new$velocity_ms >0]
        out.rating.all$av.vel.ms_slice1[j] <- mean(vel.nozero, na.rm= TRUE)
        
        #total. shear (hydradius [R] * slope * density water), density water 9806 N/m3
        #overall hydraulic radius (flow area/wetted perim)
        hyd.radius.total <- sum(flow.area, na.rm=TRUE)/sum(wetted.perim, na.rm=TRUE)
        out.rating.all$total.shear.Pa_slice1[j] <- hyd.radius.total * s * 9806
        
        #total power (shear*av. vel)
        out.rating.all$total.power.watt.ms_slice1[j] <- out.rating.all$total.shear.Pa_slice1[j] *  out.rating.all$av.vel.ms_slice1[j]
        
        
      }else{
        #else split == Y, loop through each split and calc hydraulics
        
        #save the one flow value that will link to all sections for each WSE
        #flow at wse j 
        out.rating.all$q.cms[j] <- q.cms[j] 
        
        for(z in 1:total.sections){
          #determine which slice z you are working on and which cols correspond to that slice
          ind.slice.all <- grep(z, names(out.rating.all))
          slice.col.names <- names(out.rating.all)[ind.slice.all]
          
          #subset geometry into slice z using split.stations.to.subset
          #find start and end stations for this section
          section.split.start <- split.stations.to.subset[z]
          section.split.end <- split.stations.to.subset[z+1]
          #if the section is not wetted, then all values should be zero
          #find if the section is outside of wetted geom.new, if outside then no overlap and all values 0
          if(section.split.start < geom.new$station.all[1] & section.split.end < geom.new$station.all[1]){
            overlap.y.n <- "no"
          }else{
            if(section.split.start > geom.new$station.all[length(geom.new$station.all)] & section.split.end > geom.new$station.all[length(geom.new$station.all)]){
              overlap.y.n <- "no"
            }else{
              overlap.y.n <- "yes"
            }
          }
          #if slice z is outside of wetted section, all values zero
          if(overlap.y.n == "no"){
            #all values saved as zero
            #column index start and end 
            out.rating.all[j,ind.slice.all] <- 0
          }else{
            #determine the hydraulic variables for each wse/Q j slice z
            
            #subset wetted area (geom.new) at the slice breaks
            new.slice.geom <- geom.new[geom.new$station.all >= section.split.start & geom.new$station.all <= section.split.end,]
            ##depth
            #remove zeros from depths
            depth.nozero <- new.slice.geom$depth_m[new.slice.geom$depth_m >0]
            #av depth
            out.rating.all[j, ind.slice.all[1]] <- mean(depth.nozero, na.rm= TRUE)
            #max depth
            out.rating.all[j, ind.slice.all[2]] <- max(depth.nozero, na.rm= TRUE)
            
            #av. velocity
            vel.nozero <- new.slice.geom$velocity_ms[new.slice.geom$velocity_ms >0]
            out.rating.all[j, ind.slice.all[3]] <- mean(vel.nozero, na.rm= TRUE)
            
            #total. shear (hydradius [R] * slope * density water), density water 9806 N/m3
            #overall hydraulic radius (flow area/wetted perim)
            #note: already removed any negative values for flow area and wetted perim calculation
            hyd.radius.total <- sum(new.slice.geom$flow.area, na.rm=TRUE)/sum(new.slice.geom$wetted.perim, na.rm=TRUE)
            out.rating.all[j, ind.slice.all[4]] <- hyd.radius.total * s * 9806
            
            #total power (shear*av. vel)
            out.rating.all[j, ind.slice.all[5]] <- out.rating.all[j, ind.slice.all[4]] *  out.rating.all[j, ind.slice.all[3]]
            
          }
          
        }
      }
    }
    
    #save max Q in rating table to compare with flow values from model
    max.Q.rating[i] <- max(q.cms, na.rm = TRUE)
    
    #write.csv out.rating.all
    file.name.outrating <- paste0("C:/Users/KristineT.SCCWRP2K/SCCWRP/SOC WQIP - Flow Ecology Study - General/Manuscripts/CEFF_casestudy/data/hydraulics/", subbasin.name[1], "_",xs.id[i], "_ratingcurve.data.restoredxs.csv")
    write.csv(out.rating.all, file = file.name.outrating, row.names = FALSE)
    
    #plot Q WSE rating curve
    wse.rating <- data.frame(cbind(q.cms, wse_m))
    rating <- ggplot(wse.rating, aes(x=q.cms, y =wse_m)) +
      geom_line() +
      labs(title = xs.id[i], subtitle = paste0("Subbasin: ", subbasin.name[1]), y = "Water Surface Elevation (m)", x = "Discharge (cms)") 
    #print(rating)
    
    #plot each rating curve and save
    col.indices.rating <- 2:length(names(out.rating.all))
    
    for(column in col.indices.rating){
      #subset to first column q.cms and cols
      rating.sub <- out.rating.all[,c(1, column)]
      #save orig column name
      rating.name <- names(rating.sub)[2]
      #rename col to generic name
      names(rating.sub)[2] <- "variable"
      
      #plot rating curve
      rating2 <- ggplot(rating.sub, aes(x=q.cms, y =variable)) +
        #geom_point() +
        geom_line() +
        labs(title = xs.id[i], subtitle = paste0("Subbasin: ", subbasin.name[1]), y = rating.name, x = "Discharge (cms)") 
      
      #Attemp smoothing using spline but still follows stepping features of curve, will use approxfun() to interpolate the values
      #loess() #another option but don't know equation off the bat for each relationship
      #test <- smooth.spline(rating.sub$q.cms, rating.sub$variable)
      #smooth the line using spline is another option but gives very similar to original values, doesn't do much
      #test<- spline(rating.sub$q.cms, rating.sub$variable)
      #add smoothed spline to plot
      #spline.data <- data.frame(cbind(test$x, test$y))
      #rating2 + geom_line(data = spline.data, aes(x=X1, y=X2), color="red")
      
      #print(rating2)
      
      #filename
      file.name.ratingplot <- paste0("C:/Users/KristineT.SCCWRP2K/SCCWRP/SOC WQIP - Flow Ecology Study - General/Manuscripts/CEFF_casestudy/data/hydraulics/rating_curve_plots/", subbasin.name[1], "_",xs.id[i], "_",rating.name,".jpg")
      
      #save plot
      ggsave(rating2, filename=file.name.ratingplot, dpi=300, height=4, width=8)
      
      
    }
    
  }
  
}

#omit first value of 
