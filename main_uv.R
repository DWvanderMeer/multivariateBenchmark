#################################################################################
# This code is written by Dennis van der Meer
# Uppsala University, Department of Civil and Industrial Engineering
# Division of Civil Engineering and Built Environment
# email: dennis.vandermeer@angstrom.uu.se
#################################################################################

library(dplyr)

#################################################################################
# Inputs
#################################################################################
dir <- "~/Desktop/Drive/research/mvBenchmark/data" # Working directory path
# dir <- "C:/Users/denva787/Documents/dennis/mvBenchmark/data" # Working directory path (server)
station <- c("bon", "dra", "fpk", "gwn", "psu", "sxf", "tbl")#
tz <- c(-5, -7, -6, -5, -4, -5, -6)
#source("~/Desktop/Drive/research/mvBenchmark/functions.R")
K <- 0 # forecast horizon (CH-PeEn is independent of horizon)
zen_angle <- 85 # maximum zenith angle
# M <- 20 # number of ensemble members
#################################################################################

years <- seq(2015, 2005, by = -1)
# years <- seq(2012, 2010, by = -1) # For experiments
yrs = list()
for(i in 1:(length(years)-1)){ yrs[[i]] <- years[(i-1) + 1:2] }

z <- 1
pb <- txtProgressBar(min = 0, max = length(station)*length(yrs), style = 3)
pit_hist <- list() # To store the rank histograms
num_score_1 = num_score_2 <- list() # To store the numerical scores ES and VS
for(stn in 1:length(station)){ # Loop over the stations
  
  setwd(file.path(dir, station[stn]))# Set directory
  # SURFRAD
  surfrad <- read.table(file = "surfrad15_2004-2017.txt", header = TRUE, sep = "\t", colClasses = c("character", rep("numeric",4))) #read data
  surfrad$Time <-  as.POSIXct(surfrad$Time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  surfrad <- surfrad %>% mutate(Time = Time+tz[stn]*3600) # Adjust time zone to local tz
  surfrad$tod <- strftime(surfrad$Time, format = "%H:%M", tz = "UTC") # Time of day (UTC because tz is set manually)
  
  # Split train (complete history databases), test (2-year period for forecast evaluation) years
  surfrad_all <- surfrad %>% filter(lubridate::year(Time) < 2016)
  surfrad <- surfrad %>% filter(lubridate::year(Time) %in% c(2016,2017))
  
  # Observations of the test years (moved outside the "yrs" loop because test years are constant)
  OBS1 <- data.table::shift(surfrad$dw_solar,n=K,type="lead") # Create vector observations
  ICS1 <- data.table::shift(surfrad$Ineichen,n=K,type="lead") # Create vector clear-sky irradaince
  ICS2 <- data.table::shift(surfrad$McClear,n=K,type="lead") # Create vector clear-sky irradaince
  zen <- data.table::shift(surfrad$zen,n=K,type="lead") # Create zenith mask
  idx_zen <- zen <= zen_angle # Create zenith mask
  # OBS1[idx_zen == FALSE] <- NA
  # ICS1[idx_zen == FALSE] <- NA; ICS2[idx_zen == FALSE] <- NA
  OBS1 <- data.frame(surfrad,OBS1)
  OBS1 <- OBS1[OBS1$zen < zen_angle,]
  ICS1 <- data.frame(surfrad,ICS1); ICS2 <- data.frame(surfrad,ICS2) # Create df (could be skipped)
  ICS1 <- ICS1[ICS1$zen < zen_angle,]; ICS2 <- ICS2[ICS2$zen < zen_angle,] # To calculate GHI

  #################################################################################
  # distributions
  #################################################################################
  for(j in 1:length(yrs)){
    cat(sprintf("<Running SCRIPT for station = %s, using data from years %s -- %s>\n", station[stn], yrs[[j]][2], yrs[[j]][1]))
    
    select <- which(lubridate::year(surfrad_all$Time) %in% yrs[[j]])
    # Historical forecasts (observations of the train years):
    surfrad_fit <- surfrad_all[select,]
    CSI1 <- surfrad_fit$dw_solar/surfrad_fit$Ineichen # CSI using Ineichen-Perez
    CSI1 <- data.table::shift(CSI1,n=K,type="lead") # Create historical vector forecasts (lead doesn't do anything here)
    CSI2 <- surfrad_fit$dw_solar/surfrad_fit$McClear # CSI using McClear
    CSI2 <- data.table::shift(CSI2,n=K,type="lead") # Create historical vector forecasts (lead doesn't do anything here)
    zen <- data.table::shift(surfrad_fit$zen,n=K,type="lead")#; zen <- do.call(cbind,zen) # Create zenith mask
    # idx_zen <- zen <= zen_angle # Create zenith mask
    # CSI1[idx_zen == FALSE] <- NA; CSI2[idx_zen == FALSE] <- NA
    CSI1 <- ifelse(CSI1 > 1.5, 1.5, CSI1); CSI2 <- ifelse(CSI2 > 1.5, 1.5, CSI2) # Limit the CSI values to max = 1.5
    FCS1 <- data.frame(surfrad_fit,CSI1); FCS2 <- data.frame(surfrad_fit,CSI2)
    FCS1 <- FCS1[FCS1$zen < zen_angle,]; FCS2 <- FCS2[FCS2$zen < zen_angle,] # Remove rows based on zenith angle

    # Train times
    time_fit <- strftime(FCS1$Time, format = "%H:%M", tz = "UTC") # UTC because tz is set manually
    
    crps1 = crps2 = pit1 = pit2 <- NULL # Numerical scores and PIT
    for(t in 1:nrow(OBS1)){ # Loop over the test set
      ob <- OBS1[t,] # Get the t-th observations
      ci1 <- ICS1[ICS1$Time == ob$Time,]; ci2 <- ICS2[ICS2$Time == ob$Time,] # Get the clear-sky irradiance for the coming observations
      fc1 <- FCS1[which(time_fit==ob$tod),]; fc2 <- FCS2[which(time_fit==ob$tod),] # Get the historical 1..K observations.
      
      ob <- ob$OBS1   # Take only the observation
      ci1 <- ci1$ICS1 # Take only the clear-sky irradiance 
      ci2 <- ci2$ICS2 # Take only the clear-sky irradiance 
      fc1 <- fc1$CSI1 # Take only the forecast vector
      fc2 <- fc2$CSI2 # Take only the forecast vector
      
      fc1 <- fc1*ci1 # Vector by scalar multiplication for GHI
      fc2 <- fc2*ci2 # Vector by scalar multiplication for GHI
      
      # For numerical scores (before choosing M members for improved accuracy):
      crps1[[t]] <- scoringRules::crps_sample(y=ob,dat=fc1)
      crps2[[t]] <- scoringRules::crps_sample(y=ob,dat=fc2)
      
      # Compute PIT:
      fn1 <- ecdf(fc1); fn2 <- ecdf(fc2)
      pit1[[t]] <- fn1(ob); pit2[[t]] <- fn2(ob)
    }
    # PIT histograms:
    pit_hist1 <- do.call(c,pit1)
    pit_hist1 <- hist(pit_hist1,plot=FALSE)
    pit_hist1 <- data.frame(mids=pit_hist1$mids,counts=pit_hist1$counts,
                            years=paste(yrs[[j]][2],"-",yrs[[j]][1],sep=""),
                            station=toupper(station[stn]),model="Ineichen")
    
    pit_hist2 <- do.call(c,pit2)
    pit_hist2 <- hist(pit_hist2,plot=FALSE)
    pit_hist2 <- data.frame(mids=pit_hist2$mids,counts=pit_hist2$counts,
                            years=paste(yrs[[j]][2],"-",yrs[[j]][1],sep=""),
                            station=toupper(station[stn]),model="McClear")
    pit_hist[[z]] <- rbind(pit_hist1,pit_hist2)
    # CRPS:
    num_score_1[[z]] <- data.frame(crps=mean(do.call(c,crps1)),
                                   years=paste(yrs[[j]][2],"-",yrs[[j]][1],sep=""),
                                   station=toupper(station[stn]),model="Ineichen")
    num_score_2[[z]] <- data.frame(crps=mean(do.call(c,crps2)),
                                   years=paste(yrs[[j]][2],"-",yrs[[j]][1],sep=""),
                                   station=toupper(station[stn]),model="McClear")
    
    setTxtProgressBar(pb, z) # Update the progress bar
    z <- z + 1 # Iterations over stations and years combined
  }
}

pit_hist <- do.call(rbind,pit_hist)
# write.table(pit_hist, file = "~/Desktop/Drive/research/mvBenchmark/results/pit_histograms.txt",
#             sep = "\t", row.names = FALSE, col.names = TRUE)
write.table(pit_hist, file = "C:/Users/denva787/Documents/dennis/mvBenchmark/results/pit_histograms.txt",
            sep = "\t", row.names = FALSE, col.names = TRUE)

num_score_1 <- do.call(rbind,num_score_1)
# write.table(num_score_1, file = "~/Desktop/Drive/research/mvBenchmark/results/crps_Ineichen.txt",
#             sep = "\t", row.names = FALSE, col.names = TRUE)
write.table(num_score_1, file = "C:/Users/denva787/Documents/dennis/mvBenchmark/results/crps_Ineichen.txt",
            sep = "\t", row.names = FALSE, col.names = TRUE)

num_score_2 <- do.call(rbind,num_score_2)
# write.table(num_score_2, file = "~/Desktop/Drive/research/mvBenchmark/results/crps_McClear.txt",
#             sep = "\t", row.names = FALSE, col.names = TRUE)
write.table(num_score_2, file = "C:/Users/denva787/Documents/dennis/mvBenchmark/results/crps_McClear.txt",
            sep = "\t", row.names = FALSE, col.names = TRUE)
