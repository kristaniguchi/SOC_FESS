#synthesize delta H values (min, median, max, mean delta H for each metric)


#read in delta H data
#original run
#OLD: deltaH <- read.csv("L:/San Juan WQIP_KTQ/Data/Working/Regional_Curves_CSCI_ASCI_Annie/Data/Flow/subset_woutLARsitematches/DeltaH/FFMS_deltaH_original/FFMS_deltaH_all_class.csv")
#changed just bad years (no wet season timing) to NA and ran ffc
#OLD: deltaH <- read.csv("L:/San Juan WQIP_KTQ/Data/Working/Regional_Curves_CSCI_ASCI_Annie/Data/Flow/subset_woutLARsitematches/DeltaH/FFMs_deltaH_badyrs/FFMS_deltaH_all_badyrs_class.csv")
#deltaH <- read.csv("L:/San Juan WQIP_KTQ/Data/Working/Regional_Curves_CSCI_ASCI_Annie/Data/Flow/subset_woutLARsitematches/DeltaH/FFMS_deltaH/deltaH_badyrs_updated.csv")  
#changed all zero flow days to very low value 0.00001, ID all bad years (no wet season timing), replaced with NA and ran ffc
#OLD: deltaH <- read.csv("L:/San Juan WQIP_KTQ/Data/Working/Regional_Curves_CSCI_ASCI_Annie/Data/Flow/subset_woutLARsitematches/DeltaH/FFMs_deltaH_badyrs_nonzero/FFMs_deltaH_all_badyrs_nonzero_class.csv")
deltaH <- read.csv("L:/San Juan WQIP_KTQ/Data/Working/Regional_Curves_CSCI_ASCI_Annie/Data/Flow/subset_woutLARsitematches/DeltaH/FFMS_deltaH_updated/deltaH_badyrs_nonzero_updated.csv")
#round deltaH to 0.01
deltaH$deltaH_updated <- round(deltaH$deltaH_updated, digits = 4)
deltaH$water_year_type <- as.character(deltaH$water_year_type)
##remove all NA values for deltaH
#find index of NA values
ind.NA <- which(is.na(deltaH$deltaH_updated))
#remove NA values
deltaH <- data.frame(deltaH[-ind.NA,])
#create site.year vector
deltaH$site.year <- paste0(deltaH$site, "_", as.character(deltaH$year))
#list of unique sites for this analysis
sites.deltaH <- unique(deltaH$site)

#UPDATE this depending on run
#output file name:
#orig
#output.filename <- "L:/San Juan WQIP_KTQ/Data/Working/Regional_Curves_CSCI_ASCI_Annie/Data/Flow/subset_woutLARsitematches/DeltaH/FFMS_deltaH_original/deltaH_summary_all_orig.csv"
#bad years replace with NA
#output.filename <- "L:/San Juan WQIP_KTQ/Data/Working/Regional_Curves_CSCI_ASCI_Annie/Data/Flow/subset_woutLARsitematches/DeltaH/FFMs_deltaH_badyrs/deltaH_summary_all_badyrs.csv"
#no zero flow days, changed to low value and bad years replace with NA
#OLD: output.filename <- "L:/San Juan WQIP_KTQ/Data/Working/Regional_Curves_CSCI_ASCI_Annie/Data/Flow/subset_woutLARsitematches/DeltaH/FFMs_deltaH_badyrs_nonzero/deltaH_summary_all_badyrs_nonzero.csv"
output.filename <- "L:/San Juan WQIP_KTQ/Data/Working/Regional_Curves_CSCI_ASCI_Annie/Data/Flow/subset_woutLARsitematches/DeltaH/FFMS_deltaH_updated/deltaH_summary_badyrs_nonzero_updated_mag_pct_chnge.csv"

#############################################################

#REQUIRED:
#loop to summarize delta H updated for each site and FFM: for mag metrics, keep deltaH as mag change (pct change creates very large values for small mag change if ref close to zero starting off)
#unique sites
unique.sites <- unique(deltaH$site)
length(unique.sites)

#add in notes and summary.statistic column to deltah df
deltaH$notes <- NA
deltaH$summary.statistic <- NA

#create empty dataframe for the summarized deltaH
delta.h.df <- data.frame(matrix(nrow = 1, ncol = length(deltaH)))
names(delta.h.df) <- c(names(deltaH))



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
      fm.j <- fm.j[order(fm.j$deltaH_updated, fm.j$current_value),]
      #names new.row same as names(delta.h.df)
      names(fm.j) <- names(delta.h.df)
      
      ##summary statistics
      #min index
      min.ind <- grep(min(fm.j$deltaH_updated, na.rm = TRUE), fm.j$deltaH_updated)
      min <- fm.j[min.ind[1],]
      min$notes <- paste0(min$notes, ", ", min$summary.statistic)
      #max index, take the max value row but if there's multiple rows with that max, take the largest current
      max.ind <- grep(max(fm.j$deltaH_updated, na.rm = TRUE), fm.j$deltaH_updated)
      max <- fm.j[max.ind[length(max.ind)],]
      max$notes <- paste0(max$notes, ", ", max$summary.statistic)
      #median index, if there's more than 1 match for median, take the middle value (length/2 rounded to nearest whole number)
      median.ind <- grep(median(fm.j$deltaH_updated), fm.j$deltaH_updated)
      if(length(median.ind) > 1){
        median.ind <- round(length(median.ind)/2, 0)
        #set median index to 
        median <- fm.j[median.ind,]
        median$notes <- paste0(median$notes, ", ", median$summary.statistic)
      }else{ #else if not match for median, then just use min row and update the values
        median <- min
        #update values
        median[,5:7] <- NA
        median[,11] <- median(fm.j$deltaH_updated, na.rm = TRUE)
        median[,8] <- NA
        median[,2] <- NA
      }
      #mean, use min row but change values
      mean  <- min
      #update values
      mean[,5:7] <- NA
      mean[,11] <- mean(fm.j$deltaH_updated, na.rm = TRUE)
      mean[,8] <- NA
      mean[,2] <- NA
      
      #n number of years with flow metric value, repeat value 4 times for 4 rows min max med mean
      n <- length(na.omit(fm.j$deltaH_updated))
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

write.csv(delta.h.df, row.names = FALSE, file=output.filename)

######################################################################















#######################
#OPTIONAL: 
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
#######################



######################################################
#Look at the final list of original sites used in CSCI curve development take out original sites that were screened out
orig.sites.used <- read.csv("L:/San Juan WQIP_KTQ/Data/Working/Regional_Curves_CSCI_ASCI_Annie/Data/Flow/Site_latlong/Site_Summary_final_list_used_orig_CSCI_curves.csv")
orig.unique.sites <- unique(orig.sites.used$StationCode)

#unique sites used with good rainfall data [includes all 800 sites, need to subset to orig.unique.sites]
#read in years to subset for the 
subset.yrs <- read.csv("C:/Users/KristineT.SCCWRP2K/Documents/Git/SOC_FESS/yearstosubsetflow_current_ashmitamodels.csv")
#create site.year vector
subset.yrs$site.year <- paste0(subset.yrs$Site, "_", as.character(subset.yrs$WaterYear))
subset.yrs$site <- subset.yrs$Site
#subset to include only sites for this anlaysis (orig.unique.sites)
subset.yrs.2 <- subset(subset.yrs, site %in% orig.unique.sites)
length(unique(subset.yrs.2$Site))

#find unique sites that have subset years and are included in this analysis
unique.sites.subset <- unique(subset.yrs.2$Site)
#find unique site.years to subset deltaH data upon
unique.site.years <- unique(subset.yrs.2$site.year)
#add in the site.NA (those are peak magnitude metrics calcuated on entire POR)
peak.mag.site.years <- paste(unique.sites.subset, "NA", sep="_")
unique.site.years <- c(unique.site.years, peak.mag.site.years)

#subset original deltaH data to only the site.years for analysis
sub.deltaH <- subset(deltaH, site.year %in% unique.site.years)
#check to see if unreasonable values for duration and timing
max(sub.deltaH$current_value[sub.deltaH$flow_metric == "SP_Dur"])
max(sub.deltaH$current_value[sub.deltaH$flow_metric == "DS_Dur_WS"]) #looks like there are trouble years still in this dataset

#subset summary deltaH data to only good sites used in analysis for CSCI curve development
#summary <- read.csv(output.filename) #if not running whole script in sequence
summary <- delta.h.df

#subset to only sites that should be used in model
sub.deltaH.summary <- subset(summary, site %in% orig.unique.sites)
length(unique(sub.deltaH.summary$site))
#check out median ffm values
median <- sub.deltaH.summary[sub.deltaH.summary$summary.statistic =="median",]
#check to see if unreasonable values for duration and timing
max(median$deltaH_updated[median$flow_metric == "SP_Dur"], na.rm = TRUE)
max(median$deltaH_updated[median$flow_metric == "DS_Dur_WS"], na.rm = TRUE) #looks like there are trouble years still in this dataset
max(median$deltaH_updated[median$flow_metric == "SP_Dur"], na.rm = TRUE)
max(median$deltaH_updated[median$flow_metric == "DS_Dur_WS"], na.rm = TRUE) #looks like there are trouble years still in this dataset


#check to see if unreasonable values for duration and timing from orig summary
max(delta.h.df$deltaH_updated[delta.h.df$flow_metric == "SP_Dur"], na.rm = TRUE)
max(delta.h.df$deltaH_updated[delta.h.df$flow_metric == "DS_Dur_WS"], na.rm = TRUE) #looks like there are trouble years still in this dataset


#read csv for subset data to sites with only good years
sub.output.filename <- gsub(".csv", "_subset.csv", output.filename)
write.csv(sub.deltaH.summary, file = sub.output.filename, row.names = FALSE, )
