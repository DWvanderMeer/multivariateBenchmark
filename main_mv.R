library(dplyr)
library(lubridate)
library(ggplot2)
#
#################################################################################
# Inputs
#################################################################################
dir <- "~/Desktop/Drive/research/mvBenchmark/data" # Working directory path
station <- c("bon", "dra", "fpk", "gwn", "psu", "sxf", "tbl")
tz <- c(-5, -7, -6, -5, -4, -5, -6)
source("~/Desktop/Drive/research/mvBenchmark/functions.R")
K <- seq(1,24,1) # forecast horizon
zen_angle <- 85 # maximum zenith angle
M <- 20 # number of ensemble members
#################################################################################

years <- seq(2015, 2005, by = -1)
yrs = list()
for(i in 1:(length(years)-1)){ yrs[[i]] <- years[(i-1) + 1:2] }


z <- 1
pb <- txtProgressBar(min = 0, max = length(station)*length(yrs), style = 3)
rank_histograms1 = rank_histograms2 <- list() # To store the rank histograms
num_score_1 = num_score_2 <- list() # To store the numerical scores ES and VS
for(stn in 1:length(station)){ # Loop over the stations
  
  setwd(file.path(dir, station[stn]))# Set directory
  # SURFRAD
  surfrad <- read.table(file = "surfrad15_2004-2017.txt", header = TRUE, sep = "\t", colClasses = c("character", rep("numeric",4))) #read data
  surfrad$Time <-  as.POSIXct(surfrad$Time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  surfrad <- surfrad %>% mutate(Time = Time+tz[stn]*3600) # Adjust time zone to local tz
  surfrad$tod <- strftime(surfrad$Time, format = "%H:%M", tz = "UTC") # Time of day (UTC because tz is set manually)
  
  #split train (complete history databases), test (2-year period for forecast evaluation) years
  surfrad_all <- surfrad %>% filter(lubridate::year(Time) < 2016)
  surfrad <- surfrad %>% filter(lubridate::year(Time) %in% c(2016,2017))
  
  # Observations of the test years (moved outside the "yrs" loop because test years are constant)
  OBS1 <- data.table::shift(surfrad$dw_solar,n=K,type="lead"); OBS1 <- do.call(cbind,OBS1) # Create vector observations
  # OBS2 <- data.table::shift(surfrad$dw_solar,n=K,type="lead"); OBS2 <- do.call(cbind,OBS2) # Create vector observations
  ICS1 <- data.table::shift(surfrad$Ineichen,n=K,type="lead"); ICS1 <- do.call(cbind,ICS1) # Create vector clear-sky irradaince
  ICS2 <- data.table::shift(surfrad$McClear,n=K,type="lead"); ICS2 <- do.call(cbind,ICS2) # Create vector clear-sky irradaince
  zen <- data.table::shift(surfrad$zen,n=K,type="lead"); zen <- do.call(cbind,zen) # Create zenith mask
  idx_zen <- zen <= zen_angle # Create zenith mask
  OBS1[idx_zen == FALSE] <- NA#; OBS2[idx_zen == FALSE] <- NA
  ICS1[idx_zen == FALSE] <- NA; ICS2[idx_zen == FALSE] <- NA
  OBS1 <- data.frame(surfrad,OBS1)#; OBS2 <- data.frame(surfrad,OBS2) # Create df (could be skipped)
  OBS1 <- OBS1[OBS1$zen < zen_angle,]#; OBS2 <- OBS2[OBS2$zen < zen_angle,] # Remove rows based on zenith
  OBS1 <- OBS1[complete.cases(OBS1),]#; OBS2 <- OBS2[complete.cases(OBS2),] # Remove rows based on zenith
  ICS1 <- data.frame(surfrad,ICS1); ICS2 <- data.frame(surfrad,ICS2) # Create df (could be skipped)
  ICS1 <- ICS1[ICS1$zen < zen_angle,]; ICS2 <- ICS2[ICS2$zen < zen_angle,] # To calculate GHI
  ICS1 <- ICS1[complete.cases(ICS1),]; ICS2 <- ICS2[complete.cases(ICS2),] # To calculate GHI
  
  #################################################################################
  # distributions
  #################################################################################
  # crps_bin1 = crps_bin2 <- array(NA, length(yrs))
  for(j in 1:length(yrs)){
    cat(sprintf("<Running SCRIPT for station = %s, using data from years %s -- %s>\n", station[stn], yrs[[j]][2], yrs[[j]][1]))
    
    select <- which(year(surfrad_all$Time) %in% yrs[[j]])
    # Historical forecasts (observations of the train years:
    surfrad_fit <- surfrad_all[select,]
    CSI1 <- surfrad_fit$dw_solar/surfrad_fit$Ineichen # CSI using Ineichen-Perez
    CSI1 <- data.table::shift(CSI1,n=K,type="lead"); CSI1 <- do.call(cbind,CSI1) # Create historical vector forecasts
    CSI2 <- surfrad_fit$dw_solar/surfrad_fit$McClear # CSI using McClear
    CSI2 <- data.table::shift(CSI2,n=K,type="lead"); CSI2 <- do.call(cbind,CSI2) # Create historical vector forecasts
    zen <- data.table::shift(surfrad_fit$zen,n=K,type="lead"); zen <- do.call(cbind,zen) # Create zenith mask
    idx_zen <- zen <= zen_angle # Create zenith mask
    CSI1[idx_zen == FALSE] <- NA; CSI2[idx_zen == FALSE] <- NA
    FCS1 <- data.frame(surfrad_fit,CSI1); FCS2 <- data.frame(surfrad_fit,CSI2)
    FCS1 <- FCS1[FCS1$zen < zen_angle,]; FCS2 <- FCS2[FCS2$zen < zen_angle,] # Remove rows based on zenith angle
    FCS1 <- FCS1[complete.cases(FCS1),]; FCS2 <- FCS2[complete.cases(FCS2),] # Remove rows based on zenith angle
    
    # Train times CH-PeEn
    time_fit <- strftime(FCS1$Time, format = "%H:%M", tz = "UTC") # UTC because tz is set manually
    # month_fit <- strftime(FCS1$Time, format = "%m", tz = "UTC")
    # # Test CH-PeEn # I don't use these)
    # timestamps <- unique(strftime(OBS1$Time, format = "%H:%M", tz = "UTC"))
    # time.group <- match(strftime(OBS1$Time, format = "%H:%M", tz = "UTC"), timestamps)
    
    lst1 = lst2 <- list() # Store the results for each clear-sky model separately
    es1 = es2 = vs1 = vs2 <- NULL # Numerical scores
    set.seed(123)
    i <- 1 # Counter in case a t iteration is skipped 
    for(t in 1:nrow(OBS1)){ # Loop over the test set
      ob <- OBS1[t,] # Get the t-th observations
      ci1 <- ICS1[ICS1$Time == ob$Time,]; ci2 <- ICS2[ICS2$Time == ob$Time,] # Get the clear-sky irradiance for the coming observations
      fc1 <- FCS1[which(time_fit==ob$tod),]; fc2 <- FCS2[which(time_fit==ob$tod),] # Get the historical 1..K observations.

      # If the lengths of the ensembles are too short, the observation rank is per definition 
      # low, resulting in left skewed rank histograms. That's why we skip here. Note that this
      # limit varies per location, depending on seasonal variation. How about normalizing the ranks?
      if(nrow(fc1) < M) next 
      
      ob <- as.matrix(ob[,((ncol(ob)-max(K)+1):ncol(ob))]) # Take only the observation vector
      ci1 <- as.matrix(ci1[,((ncol(ci1)-max(K)+1):ncol(ci1))]) # Take only the clear-sky irradiance vector
      ci2 <- as.matrix(ci2[,((ncol(ci2)-max(K)+1):ncol(ci2))]) # Take only the clear-sky irradiance vector
      fc1 <- as.matrix(fc1[,((ncol(fc1)-max(K)+1):ncol(fc1))]) # Take only teh forecast matrix
      fc2 <- as.matrix(fc2[,((ncol(fc2)-max(K)+1):ncol(fc2))]) # Take only teh forecast matrix
      # Randomly select M ensemble forecasts
      rows <- sample(nrow(fc1))[1:M]
      fc1 <- fc1[rows,]; fc2 <- fc2[rows,]
  
      fc1 <- sweep(fc1, MARGIN=2, ci1, `*`) # Matrix by vector multiplication for GHI
      fc2 <- sweep(fc2, MARGIN=2, ci2, `*`) # Matrix by vector multiplication for GHI
      
      # For rank histograms:
      B1 <- rbind(ob,fc1); B2 <- rbind(ob,fc2) 
      lst1[[i]] <- B1; lst2[[i]] <- B2
      # For numerical scores:
      es1[[i]] <- scoringRules::es_sample(y=ob[1,],dat=t(fc1))
      es2[[i]] <- scoringRules::es_sample(y=ob[1,],dat=t(fc2))
      vs1[[i]] <- scoringRules::vs_sample(y=ob[1,],dat=t(fc1),p=0.5) # p recommended by SCHEUERER AND HAMILL
      vs2[[i]] <- scoringRules::vs_sample(y=ob[1,],dat=t(fc2),p=0.5) # p recommended by SCHEUERER AND HAMILL
      i <- i + 1
    }
    # Rank histograms:
    avghist1 <- avg.rhist(lst1,M) # Average rank histogram based on Ineichen clear-sky
    avghist1 <- data.frame(mids=avghist1$mids,counts=avghist1$counts,
                           years=paste(yrs[[j]][2],"-",yrs[[j]][1],sep=""),
                           station=toupper(station[stn]),prerank="AVG")
    avghist2 <- avg.rhist(lst2,M) # Average rank histogram based on McClear clear-sky
    avghist2 <- data.frame(mids=avghist2$mids,counts=avghist2$counts,
                           years=paste(yrs[[j]][2],"-",yrs[[j]][1],sep=""),
                           station=toupper(station[stn]),prerank="AVG")
    bdhhist1 <- bd.rhist(lst1,M) # Band depth rank histogram based on Ineichen clear-sky
    bdhhist1 <- data.frame(mids=bdhhist1$mids,counts=bdhhist1$counts,
                           years=paste(yrs[[j]][2],"-",yrs[[j]][1],sep=""),
                           station=toupper(station[stn]),prerank="BDH")
    bdhhist2 <- bd.rhist(lst2,M) # Band depth rank histogram based on McClear clear-sky
    bdhhist2 <- data.frame(mids=bdhhist2$mids,counts=bdhhist2$counts,
                           years=paste(yrs[[j]][2],"-",yrs[[j]][1],sep=""),
                           station=toupper(station[stn]),prerank="BDH")
    msthist1 <- mst.rhist(lst1,M) # Minimum spinning tree rank histogram based on Ineichen clear-sky
    msthist1 <- data.frame(mids=msthist1$mids,counts=msthist1$counts,
                           years=paste(yrs[[j]][2],"-",yrs[[j]][1],sep=""),
                           station=toupper(station[stn]),prerank="MST")
    msthist2 <- mst.rhist(lst2,M) # Minimum spinning tree rank histogram based on McClear clear-sky
    msthist2 <- data.frame(mids=msthist2$mids,counts=msthist2$counts,
                           years=paste(yrs[[j]][2],"-",yrs[[j]][1],sep=""),
                           station=toupper(station[stn]),prerank="MST")
    rank_histograms1[[z]] <- rbind(avghist1,bdhhist1,msthist1)
    rank_histograms2[[z]] <- rbind(avghist2,bdhhist2,msthist2)
    # Energy score and variogram score:
    num_score_1[[z]] <- data.frame(es=mean(es1),vs=mean(vs1),
                                   years=paste(yrs[[j]][2],"-",yrs[[j]][1],sep=""),
                                   station=toupper(station[stn]))
    num_score_2[[z]] <- data.frame(es=mean(es2),vs=mean(vs2),
                                   years=paste(yrs[[j]][2],"-",yrs[[j]][1],sep=""),
                                   station=toupper(station[stn]))
    
    setTxtProgressBar(pb, z) # Update the progress bar
    z <- z + 1 # Iterations over stations and years combined
  }
}

res_1 <- do.call(rbind,rank_histograms1)
write.table(res_1, file = "~/Desktop/Drive/research/mvBenchmark/results/rank_histograms_Ineichen.txt",
            sep = "\t", row.names = FALSE, col.names = TRUE)

res_2 <- do.call(rbind,rank_histograms2)
write.table(res_2, file = "~/Desktop/Drive/research/mvBenchmark/results/rank_histograms_McClear.txt",
            sep = "\t", row.names = FALSE, col.names = TRUE)

res_3 <- do.call(rbind,num_score_1)
write.table(res_3, file = "~/Desktop/Drive/research/mvBenchmark/results/numerical_scores_Ineichen.txt",
            sep = "\t", row.names = FALSE, col.names = TRUE)

res_4 <- do.call(rbind,num_score_2)
write.table(res_4, file = "~/Desktop/Drive/research/mvBenchmark/results/numerical_scores_McClear.txt",
            sep = "\t", row.names = FALSE, col.names = TRUE)
