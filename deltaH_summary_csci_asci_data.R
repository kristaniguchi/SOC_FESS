#synthesize delta H values (min, median, max, mean delta H for each metric)


#read in delta H data
deltaH <- read.csv("L:/San Juan WQIP_KTQ/Data/Working/Regional_Curves_CSCI_ASCI_Annie/Data/Flow/subset_woutLARsitematches/FFMS_deltaH/FFMS_deltaH_all.csv")
#round deltaH to 0.01
deltaH$deltaH <- round(deltaH$deltaH, digits = 2)
deltaH$water_year_type <- as.character(deltaH$water_year_type)
##remove all NA values for deltaH
#find index of NA values
ind.NA <- which(is.na(deltaH$deltaH))
#remove NA values
deltaH <- data.frame(deltaH[-ind.NA,])
#create site.year vector
deltaH$site.year <- paste0(deltaH$site, "_", as.character(deltaH$year))
#list of unique sites for this analysis
sites.deltaH <- unique(deltaH$site)

#subset to only the SD sites and review values (max for curr and ref) if values are off, may only want to keep the sites where we have the subset years identified
#list of sites from SD current group (not included in Ashmita's original dataset)
sd.sites <- read.csv("L:/San Juan WQIP_KTQ/Data/Working/Regional_Curves_CSCI_ASCI_Annie/Data/Flow/Site_latlong/AssessmentSite_Area_Tc_Ts_Imperv_SD_revised_04122019.csv")
unique.sd <- unique(sd.sites$BugID)
#check to see if SD sites are okay
#subset to only the sd sites --> may want to exclude these new SD sites because I don't know which years are good and not good
deltaH.sd <- subset(deltaH, site %in% unique.sd)
max(deltaH.sd$current_value[deltaH.sd$flow_metric == "SP_Dur"])
max(deltaH.sd$current_value[deltaH.sd$flow_metric == "DS_Dur_WS"]) #looks like there are trouble years still in this dataset


#subset deltaH values to only include years properly modeled by Ashmita
#read in years to subset for the 
subset.yrs <- read.csv("C:/Users/KristineT.SCCWRP2K/Documents/Git/SOC_FESS/yearstosubsetflow_current_ashmitamodels.csv")
#create site.year vector
subset.yrs$site.year <- paste0(subset.yrs$Site, "_", as.character(subset.yrs$WaterYear))
subset.yrs$site <- subset.yrs$Site
#subset to include only sites for this anlaysis
subset.yrs.2 <- subset(subset.yrs, site %in% sites.deltaH)
#find unique sites that have subset years and are included in this analysis
unique.sites.subset <- unique(subset.yrs.2$Site)
#find unique site.years to subset deltaH data upon
unique.site.years <- unique(subset.yrs.2$site.year)
#add in the site.NA (those are peak magnitude metrics calcuated on entire POR)
peak.mag.site.years <- paste(unique.sites.subset, "NA", sep="_")
unique.site.years <- c(unique.site.years, peak.mag.site.years)

#subset data to only the site.years for analysis
sub.deltaH <- subset(deltaH, site.year %in% unique.site.years)
#check to see if unreasonable values for duration and timing
max(sub.deltaH$current_value[sub.deltaH$flow_metric == "SP_Dur"])
max(sub.deltaH$current_value[sub.deltaH$flow_metric == "DS_Dur_WS"]) #looks like there are trouble years still in this dataset
#find sites where duration is super long for dry season
sub.deltaH$site.year[]

write.csv(sub.deltaH, file="C:/Users/KristineT.SCCWRP2K/Documents/Git/SOC_FESS/sub.deltaH.csv", row.names=FALSE)







#loop to summarize delta H for each site and FFM
#unique sites
unique.sites <- unique(deltaH$site)
length(unique.sites)

#create empty dataframe for the summarized deltaH
delta.h.df <- data.frame(matrix(nrow = 1, ncol = 8))
names(delta.h.df) <- c("site","summary.statistic","n_year","flow_metric","current_value","reference_value","deltaH","notes")


#loop through each unique site
for(i in 1:length(unique.sites)){
  #subset data for site i
  sub <- deltaH[deltaH$site == unique.sites[i],]
  #unique flow metrics to loop through
  unique.fm <- unique(sub$flow_metric)
  
  #loop through unique flow metrics
  for(j in 1:length(unique.fm)){
    #subset flow metric j rows
    fm.j <- sub[sub$flow_metric == unique.fm[j],]
    
    #if peak mag metrics, use values as is
    if(unique.fm[j] == "Peak_10" | unique.fm[j] == "Peak_5" | unique.fm[j] == "Peak_2"){
      new.row <- fm.j
      #names new.row same as names(delta.h.df)
      names(new.row) <- names(delta.h.df)
      #add in this row
      delta.h.df <- data.frame(rbind(delta.h.df, new.row))
      
    }else{ 
      #else, take the min, max, median, and mean delta H's and save as
      
      #sort the current values in ascending order by delta h and current value
      fm.j <- fm.j[order(fm.j$deltaH, fm.j$current_value),]
      #names new.row same as names(delta.h.df)
      names(fm.j) <- names(delta.h.df)
      
      ##summary statistics
      #min index
      min.ind <- grep(min(fm.j$deltaH), fm.j$deltaH)
      min <- fm.j[min.ind[1],]
      min$notes <- paste0(min$notes, ", ", min$summary.statistic)
      #max index, take the max value row but if there's multiple rows with that max, take the largest current
      max.ind <- grep(max(fm.j$deltaH), fm.j$deltaH)
      max <- fm.j[max.ind[length(max.ind)],]
      max$notes <- paste0(max$notes, ", ", max$summary.statistic)
      #median index, if there's more than 1 match for median, take the middle value (length/2 rounded to nearest whole number)
      median.ind <- grep(median(fm.j$deltaH), fm.j$deltaH)
      if(length(median.ind) > 1){
        median.ind <- round(length(median.ind)/2, 0)
        #set median index to 
        median <- fm.j[median.ind,]
        median$notes <- paste0(median$notes, ", ", median$summary.statistic)
        }else{ #else if not match for median, then just use min row and update the values
          median <- min
          #update values
          median[,5:6] <- NA
          median[,7] <- median(fm.j$deltaH)
          median[,8] <- NA
        }
      #mean, use min row but change values
      mean  <- min
      #update values
      mean[,5:6] <- NA
      mean[,7] <- mean(fm.j$deltaH)
      mean[,8] <- NA
      
      #n number of years with flow metric value, repeat value 4 times for 4 rows min max med mean
      n <- length(fm.j$site)
      n.years <- rep(n, 4)
      
      #new rows to be added with n years and summary statistics added to it
      new.rows.df <- data.frame(rbind(min, median, max, mean))
      new.rows.df$summary.statistic <- c("min", "median", "max", "mean")
      new.rows.df$n_year <- n.years
      #save row into df
      delta.h.df <- data.frame(rbind(delta.h.df, new.rows.df))
      
      
    #}else{ #if only NA value, then just take NA row for all
      #new.row <- fm.j
      #names new.row same as names(delta.h.df)
      #names(new.row) <- names(delta.h.df)
      #update year to NA, n_year to 0, and notes to NA
      #new.row[1,2] <- NA
      #new.row[1,3] <- 0
      #new.row[1,8] <- NA
      
      #save row into df
      #delta.h.df <- data.frame(rbind(delta.h.df, new.row))
      
    }
    
  }
  
}

write.csv(delta.h.df, row.names = FALSE, file="L:/San Juan WQIP_KTQ/Data/Working/Regional_Curves_CSCI_ASCI_Annie/Data/Flow/subset_woutLARsitematches/FFMS_deltaH/deltaH_summary_all_KTQ.csv")

