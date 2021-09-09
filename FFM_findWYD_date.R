#find water year date from water year day

library("EflowStats")


date <- seq(from=as.Date("2010-01-01"),to=as.Date("2012-01-01"),by="1 days")
WYD <- get_waterYearDay(date)

lookup <- data.frame(cbind(as.character(date), WYD))

lookup$V1[lookup$WYD == 64]

