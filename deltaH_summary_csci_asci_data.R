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
    if(unique.fm[j] == "Peak_10" | unique.fm[j] == "Peak_5" | unique.fm[j] == "Peak_12"){
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

