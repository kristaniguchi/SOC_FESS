


#read in the subsetted deltaH values for the sites that Ashmita modeled that have the years for analysis indicated
sub.deltaH <- read.csv(file="C:/Users/KristineT.SCCWRP2K/Documents/Git/SOC_FESS/sub.deltaH.csv")

#look at the distribution of spring duration and dry season timing/duration

#spring timing
sptm <- sub.deltaH[sub.deltaH$flow_metric == "SP_Tim",]

#jitter plot of distribution current and ref: thresholds: >75, < 250 (mid-december to early june) 
sptm.plot.cur <- ggplot(data = sptm, aes(x=flow_metric, y=current_value)) +
  geom_violin() #+ geom_jitter()
sptm.plot.cur

sptm.plot.ref <- ggplot(data = sptm, aes(x=flow_metric, y=reference_value)) +
  geom_violin() #+ geom_jitter()
sptm.plot.ref

#spring duration
#jitter plot of distribution current and ref: thresholds:  < 175 
spdur.plot.cur <- ggplot(data = sub.deltaH[sub.deltaH$flow_metric == "SP_Dur",], aes(x=flow_metric, y=current_value)) +
  geom_violin() #+ geom_jitter()
spdur.plot.cur

spdur.plot.ref <- ggplot(data = sub.deltaH[sub.deltaH$flow_metric == "SP_Dur",], aes(x=flow_metric, y=reference_value)) +
  geom_violin() #+ geom_jitter()
spdur.plot.ref


#dry season duration
#jitter plot of distribution current and ref: thresholds: <275
dsdur.plot.cur <- ggplot(data = sub.deltaH[sub.deltaH$flow_metric == "DS_Dur_WS",], aes(x=flow_metric, y=current_value)) +
  geom_violin() #+ geom_jitter()
dsdur.plot.cur

dsdur.plot.ref <- ggplot(data = sub.deltaH[sub.deltaH$flow_metric == "DS_Dur_WS",], aes(x=flow_metric, y=reference_value)) +
  geom_violin() #+ geom_jitter()
dsdur.plot.ref

#dry season timing --> (> 150, <350 [end feb to mid-sept])
#jitter plot of distribution current and ref: thresholds:(> 150, <350 [end feb to mid-sept])
dstim.plot.cur <- ggplot(data = sub.deltaH[sub.deltaH$flow_metric == "DS_Tim",], aes(x=flow_metric, y=current_value)) +
  geom_violin() #+ geom_jitter()
dstim.plot.cur

dstim.plot.ref <- ggplot(data = sub.deltaH[sub.deltaH$flow_metric == "DS_Tim",], aes(x=flow_metric, y=reference_value)) +
  geom_violin() #+ geom_jitter()
dstim.plot.ref


##############################################
#for starters, filter data based on just the spring metrics and see what the summer distribution looks like
unique.sites <- unique(sub.deltaH$site)
#create empty df for the filtered data
new.sub <- sub.deltaH[1,]
new.sub[1,] <- NA
spring.excluded.years <- NA 


for(k in 1:length(unique.sites)){
  #subset site k
  subk <- sub.deltaH[sub.deltaH$site == unique.sites[k],]
  
  #determine if spring timing and duration metrics pass filter
  sptim.sub <- subk[subk$flow_metric == "SP_Tim",]
  #spring timing filter (>75, < 250 [mid-december to early june])
  #identify which year to exclude based on this filter for current and ref
  year.exclude.cur <- sptim.sub$year[sptim.sub$current_value < 75 | sptim.sub$current_value > 250]
  year.exclude.ref <- sptim.sub$year[sptim.sub$reference_value < 75 | sptim.sub$reference_value > 250]
  
  #identify which years excluded based on spring duration filter < 175 
  spdur.sub <- subk[subk$flow_metric == "SP_Dur",]
  #spring timing filter 
  #identify which year to exclude based on this filter for current and ref
  year.exclude.cur2 <- spdur.sub$year[spdur.sub$current_value > 175]
  year.exclude.ref2 <- spdur.sub$year[spdur.sub$reference_value > 175]
  
  #comine all years that will be excluded based on these spring values
  years.exclude.spring.all <- unique(c(year.exclude.cur, year.exclude.ref, year.exclude.cur2, year.exclude.ref2))
  
  #dry season filters
  #determine if dry season timing and duration metrics pass filter
  dstim.sub <- subk[subk$flow_metric == "DS_Tim",]
  #dry season timing filter (> 150, <350 [end feb to mid-sept])
  #identify which year to exclude based on this filter for current and ref
  year.exclude.cur3 <- dstim.sub$year[dstim.sub$current_value < 150 | dstim.sub$current_value > 350]
  year.exclude.ref3 <- dstim.sub$year[dstim.sub$reference_value < 150 | dstim.sub$reference_value > 350]
  
  #determine if dry season duration metrics pass filter
  dsdur.sub <- subk[subk$flow_metric == "DS_Dur_WS",]
  #dry season duration filter (< 275)
  #identify which year to exclude based on this filter for current and ref
  year.exclude.cur4 <- dsdur.sub$year[dsdur.sub$current_value > 275]
  year.exclude.ref4 <- dsdur.sub$year[dsdur.sub$reference_value > 275]
  
  #dry years that need to be excluded
  years.exclude.dry.all <- unique(c(year.exclude.cur3, year.exclude.ref3, year.exclude.cur4, year.exclude.ref4))
  
  #subset data based on excluded
  all.yrs.excluded <- unique(c(years.exclude.spring.all, years.exclude.dry.all))
  #if no years excluded then keep subset as is, else only use the years that should be included and the peak magnitude metrics
  if(length(all.yrs.excluded) < 1){
    test <- subk
  }else{
    #exclude years
    '%ni%' <- Negate('%in%')
    ind <- which(subk$year %ni% all.yrs.excluded)
    test <- subk[ind,]
  }
  
  #add data into new sub
  new.sub <- rbind(new.sub, test)
}

newsub.sites <- unique(new.sub$site)
length(newsub.sites)

#######check to see new distributions for new subset


#jitter plot of distribution current and ref: thresholds: >75, < 250 (mid-december to early june) 
sptm.plot.cur <- ggplot(data = new.sub[new.sub$flow_metric == "SP_Tim",], aes(x=flow_metric, y=current_value)) +
  geom_violin() #+ geom_jitter()
sptm.plot.cur

sptm.plot.ref <- ggplot(data = new.sub[new.sub$flow_metric == "SP_Dur",], aes(x=flow_metric, y=reference_value)) +
  geom_violin() #+ geom_jitter()
sptm.plot.ref

#spring duration
#jitter plot of distribution current and ref: thresholds:  < 175 
spdur.plot.cur <- ggplot(data = new.sub[new.sub$flow_metric == "SP_Dur",], aes(x=flow_metric, y=current_value)) +
  geom_violin() #+ geom_jitter()
spdur.plot.cur

spdur.plot.ref <- ggplot(data = new.sub[new.sub$flow_metric == "SP_Dur",], aes(x=flow_metric, y=reference_value)) +
  geom_violin() #+ geom_jitter()
spdur.plot.ref


#dry season duration
#jitter plot of distribution current and ref: thresholds: <275
dsdur.plot.cur <- ggplot(data = new.sub[new.sub$flow_metric == "DS_Dur_WS",], aes(x=flow_metric, y=current_value)) +
  geom_violin() #+ geom_jitter()
dsdur.plot.cur

dsdur.plot.ref <- ggplot(data = new.sub[new.sub$flow_metric == "DS_Dur_WS",], aes(x=flow_metric, y=reference_value)) +
  geom_violin() #+ geom_jitter()
dsdur.plot.ref

#jitter plots of deltaH before and after subset

#dry season timing --> (> 150, <350 [end feb to mid-sept])
#jitter plot of distribution current and ref: thresholds:(> 150, <350 [end feb to mid-sept])
dstim.plot.cur <- ggplot(data = new.sub[new.sub$flow_metric == "DS_Tim",], aes(x=flow_metric, y=current_value)) +
  geom_violin() #+ geom_jitter()
dstim.plot.cur

dstim.plot.ref <- ggplot(data = new.sub[new.sub$flow_metric == "DS_Tim",], aes(x=flow_metric, y=reference_value)) +
  geom_violin() #+ geom_jitter()
dstim.plot.ref

#jitter plot of distribution current and ref: thresholds:(> 150, <350 [end feb to mid-sept])
dstim.plot.cur <- ggplot(data = sub.deltaH[sub.deltaH$flow_metric == "DS_Tim",], aes(x=flow_metric, y=deltaH)) +
  geom_violin() #+ geom_jitter()
dstim.plot.cur

dstim.plot.ref <- ggplot(data = new.sub[new.sub$flow_metric == "DS_Tim",], aes(x=flow_metric, y=deltaH)) +
  geom_violin() #+ geom_jitter()
dstim.plot.ref

dsdur.plot.cur <- ggplot(data = new.sub[new.sub$flow_metric == "DS_Dur_WS",], aes(x=flow_metric, y=deltaH)) +
  geom_violin() #+ geom_jitter()
dsdur.plot.cur

dsdur.plot.cur <- ggplot(data = sub.deltaH[sub.deltaH$flow_metric == "DS_Dur_WS",], aes(x=flow_metric, y=deltaH)) +
  geom_violin() #+ geom_jitter()
dsdur.plot.cur

#summarize how years per site and per each metric
new.sub <- new.sub[2:length(new.sub$site),]
write.csv(new.sub, file="C:/Users/KristineT.SCCWRP2K/Documents/Git/SOC_FESS/sub.deltaH.spds.outliers.exclude.csv", row.names=FALSE)


#loop to summarize delta H for each site and FFM
#unique sites
newsub.sites <- unique(new.sub$site)

#create empty dataframe for the summarized deltaH
delta.h.df <- data.frame(matrix(nrow = 1, ncol = 9))
names(delta.h.df) <- c("site","summary.statistic","n_year","flow_metric","current_value","reference_value","deltaH","notes", "site.year")


#loop through each unique site
for(i in 1:length(newsub.sites)){
  #subset data for site i
  sub <- new.sub[new.sub$site == newsub.sites[i],]
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
        median.ind2 <- round(length(median.ind)/2, 0)
        #set median index to 
        median <- fm.j[median.ind[median.ind2],]
        median$notes <- paste0(median$notes, ", ", median$summary.statistic)
      }else{ #else if not match for median, then just use min row and update the values
        if(length(median.ind) == 1){
          median <- fm.j[median.ind,]
          median$notes <- paste0(median$notes, ", ", median$summary.statistic)
          
        }else{
          median <- min
          #update values
          median[,5:6] <- NA
          median[,7] <- median(fm.j$deltaH)
          median[,8] <- NA
          median[,9] <- NA
        }
        
      }
      
      #mean, use min row but change values
      mean  <- min
      #update values
      mean[,5:6] <- NA
      mean[,7] <- mean(fm.j$deltaH)
      mean[,8] <- NA
      mean[,9] <- NA
      
      #n number of years with flow metric value, repeat value 4 times for 4 rows min max med mean
      n <- length(fm.j$site)
      n.years <- rep(n, 4)
      
      #new rows to be added with n years and summary statistics added to it
      new.rows.df <- data.frame(rbind(min, median, max, mean))
      new.rows.df$summary.statistic <- c("min", "median", "max", "mean")
      new.rows.df$n_year <- n.years
      #save row into df
      delta.h.df <- data.frame(rbind(delta.h.df, new.rows.df))
      
    }
    
  }
  
}

write.csv(delta.h.df, row.names = FALSE, file="L:/San Juan WQIP_KTQ/Data/Working/Regional_Curves_CSCI_ASCI_Annie/Data/Flow/subset_woutLARsitematches/FFMS_deltaH/deltaH_summary_all_KTQ_excludeoutlier_dsspring.csv")
write.csv(delta.h.df, file="C:/Users/KristineT.SCCWRP2K/Documents/Git/SOC_FESS/deltaH_summary_all_KTQ_excludeoutlier_dsspring.csv", row.names=FALSE)


#look at median values only
med.sub <- delta.h.df[delta.h.df$summary.statistic == "median",]

max()
