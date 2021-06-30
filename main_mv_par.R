#################################################################################
# This code is written by Dennis van der Meer
# Uppsala University, Department of Civil and Industrial Engineering
# Division of Civil Engineering and Built Environment
# email: dennis.vandermeer@angstrom.uu.se
#################################################################################

library(dplyr)
library(lubridate)
library(ggplot2)
library(foreach)
library(doParallel)

#################################################################################
# Inputs
#################################################################################
# dir <- "~/Desktop/Drive/research/mvBenchmark/data" # Working directory path
# dir <- "C:/Users/denva787/Documents/dennis/mvBenchmark/data" # Working directory path (server)
# dir <- "~/Google Drive/My Drive/research/multivariateBenchmark/data/" # Directory path to SURFRAD data
dir <- "C:/Users/dennis.van_der_meer/Documents/multivariateBenchmark/data/"
dir_save <- "C:/Users/dennis.van_der_meer/Documents/multivariateBenchmark/results/"
station <- c("bon", "dra", "fpk", "gwn", "psu", "sxf", "tbl")
tz <- c(-5, -7, -6, -5, -4, -5, -6)
# source("~/Desktop/Drive/research/mvBenchmark/scripts/functions.R")
# source("C:/Users/denva787/Documents/dennis/mvBenchmark/scripts/functions.R")
# source("~/Google Drive/My Drive/research/multivariateBenchmark/scripts/functions.R")
source("C:/Users/dennis.van_der_meer/Documents/multivariateBenchmark/scripts/functions.R")
K <- seq(1,24,1) # Forecast horizon
zen_angle <- 85 # Maximum zenith angle
M <- seq(20,100,20) # Number of ensemble members
# M <- 40 # Number of ensemble members
#################################################################################

ptm <- proc.time()
cl <- makeCluster(length(station))
registerDoParallel(cl)

years <- seq(2015, 2005, by = -1)
# years <- seq(2012, 2011, by = -1) # For experiments
yrs = list()
for(i in 1:(length(years)-1)){ yrs[[i]] <- years[(i-1) + 1:2] }

z <- 1
rank_histograms1 = rank_histograms2 = rank_histograms3 <- list() # To store the rank histograms
num_score_1 = num_score_2 = num_score_3 <- list() # To store the numerical scores ES and VS

script <- foreach(stn = 1:length(station), .combine='comb', .multicombine=TRUE,
                  .init=list(list(),list(),list(),list(),list(),list()), .packages = 'dplyr') %dopar% { # Loop over the stations
                    # Main script
                    setwd(file.path(dir, station[stn]))# Set directory
                    # SURFRAD
                    surfrad <- read.table(file = "surfrad15_2004-2017.txt", header = TRUE, sep = "\t", colClasses = c("character", rep("numeric",4))) #read data
                    surfrad$Time <-  as.POSIXct(surfrad$Time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
                    surfrad <- surfrad %>% mutate(Time = Time+tz[stn]*3600) # Adjust time zone to local tz
                    surfrad$tod <- strftime(surfrad$Time, format = "%H:%M", tz = "UTC") # Time of day (UTC because tz is set manually)
                    # REST2 
                    rest2 <- read.table(file = "rest2_clearsky.txt", header = TRUE, sep = "\t", colClasses = c("character", rep("numeric",2)))
                    rest2$Time <-  as.POSIXct(rest2$Time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
                    rest2 <- rest2 %>% mutate(Time = Time+tz[stn]*3600) # Adjust time zone to local tz
                    # Add rest2 to the surfrad data
                    surfrad <- left_join(surfrad, select(rest2, c(Time,REST2)), by="Time") %>%
                      mutate(REST2 = coalesce(REST2,McClear)) # In case REST2 has NAs, replace with McClear
                    
                    #split train (complete history databases), test (2-year period for forecast evaluation) years
                    surfrad_all <- surfrad %>% filter(lubridate::year(Time) < 2016)
                    surfrad <- surfrad %>% filter(lubridate::year(Time) %in% c(2016,2017))
                    
                    # Observations of the test years (moved outside the "yrs" loop because test years are constant)
                    OBS1 <- data.table::shift(surfrad$dw_solar,n=K,type="lead"); OBS1 <- do.call(cbind,OBS1) # Create vector observations
                    # OBS2 <- data.table::shift(surfrad$dw_solar,n=K,type="lead"); OBS2 <- do.call(cbind,OBS2) # Create vector observations
                    ICS1 <- data.table::shift(surfrad$Ineichen,n=K,type="lead"); ICS1 <- do.call(cbind,ICS1) # Create vector clear-sky irradaince
                    ICS2 <- data.table::shift(surfrad$McClear,n=K,type="lead"); ICS2 <- do.call(cbind,ICS2) # Create vector clear-sky irradaince
                    ICS3 <- data.table::shift(surfrad$REST2,n=K,type="lead"); ICS3 <- do.call(cbind,ICS3) # Create vector clear-sky irradiance
                    zen <- data.table::shift(surfrad$zen,n=K,type="lead"); zen <- do.call(cbind,zen) # Create zenith mask
                    idx_zen <- zen <= zen_angle # Create zenith mask
                    OBS1[idx_zen == FALSE] <- NA#; OBS2[idx_zen == FALSE] <- NA
                    ICS1[idx_zen == FALSE] <- NA; ICS2[idx_zen == FALSE] <- NA; ICS3[idx_zen == FALSE] <- NA
                    OBS1 <- data.frame(surfrad,OBS1)#; OBS2 <- data.frame(surfrad,OBS2) # Create df (could be skipped)
                    OBS1 <- OBS1[OBS1$zen < zen_angle,]#; OBS2 <- OBS2[OBS2$zen < zen_angle,] # Remove rows based on zenith
                    OBS1 <- OBS1[complete.cases(OBS1),]#; OBS2 <- OBS2[complete.cases(OBS2),] # Remove rows based on zenith
                    ICS1 <- data.frame(surfrad,ICS1); ICS2 <- data.frame(surfrad,ICS2); ICS3 <- data.frame(surfrad,ICS3) # Create df (could be skipped)
                    ICS1 <- ICS1[ICS1$zen < zen_angle,]; ICS2 <- ICS2[ICS2$zen < zen_angle,]; ICS3 <- ICS3[ICS3$zen < zen_angle,] # To calculate GHI
                    ICS1 <- ICS1[complete.cases(ICS1),]; ICS2 <- ICS2[complete.cases(ICS2),]; ICS3 <- ICS3[complete.cases(ICS3),] # To calculate GHI
                    
                    #################################################################################
                    # distributions
                    #################################################################################
                    # crps_bin1 = crps_bin2 <- array(NA, length(yrs))
                    for(j in 1:length(yrs)){
                      # cat(sprintf("<Running SCRIPT for station = %s, using data from years %s -- %s>\n", station[stn], yrs[[j]][2], yrs[[j]][1]))
                      
                      select <- which(lubridate::year(surfrad_all$Time) %in% yrs[[j]])
                      # Historical forecasts (observations of the train years:
                      surfrad_fit <- surfrad_all[select,]
                      CSI1 <- surfrad_fit$dw_solar/surfrad_fit$Ineichen # CSI using Ineichen-Perez
                      CSI1 <- ifelse(CSI1 > 1.5, 1.5, CSI1)
                      CSI1 <- data.table::shift(CSI1,n=K,type="lead"); CSI1 <- do.call(cbind,CSI1) # Create historical vector forecasts
                      CSI2 <- surfrad_fit$dw_solar/surfrad_fit$McClear # CSI using McClear
                      CSI2 <- ifelse(CSI2 > 1.5, 1.5, CSI2)
                      CSI2 <- data.table::shift(CSI2,n=K,type="lead"); CSI2 <- do.call(cbind,CSI2) # Create historical vector forecasts
                      CSI3 <- surfrad_fit$dw_solar/surfrad_fit$REST2 # CSI using REST2
                      CSI3 <- ifelse(CSI3 > 1.5, 1.5, CSI3)
                      CSI3 <- data.table::shift(CSI3,n=K,type="lead"); CSI3 <- do.call(cbind,CSI3) # Create historical vector forecasts
                      zen <- data.table::shift(surfrad_fit$zen,n=K,type="lead"); zen <- do.call(cbind,zen) # Create zenith mask
                      idx_zen <- zen <= zen_angle # Create zenith mask
                      CSI1[idx_zen == FALSE] <- NA; CSI2[idx_zen == FALSE] <- NA
                      FCS1 <- data.frame(surfrad_fit,CSI1); FCS2 <- data.frame(surfrad_fit,CSI2); FCS3 <- data.frame(surfrad_fit,CSI3)
                      FCS1 <- FCS1[FCS1$zen < zen_angle,]; FCS2 <- FCS2[FCS2$zen < zen_angle,]; FCS3 <- FCS3[FCS3$zen < zen_angle,] # Remove rows based on zenith angle
                      FCS1 <- FCS1[complete.cases(FCS1),]; FCS2 <- FCS2[complete.cases(FCS2),]; FCS3 <- FCS3[complete.cases(FCS3),] # Remove rows based on zenith angle
                      
                      # Train times
                      time_fit <- strftime(FCS1$Time, format = "%H:%M", tz = "UTC") # UTC because tz is set manually
                      
                      lst1 = lst2 = lst3 <- list() # Store the results for each clear-sky model separately
                      es1 = es2 = es3 = vs1 = vs2 = vs3 <- NULL # Numerical scores
                      
                      for(m in M){ # Loop over the number of ensemble members
                        set.seed(123) # Set seed for pseudo random ensemble members
                        i <- 1 # Counter in case a t iteration is skipped 
                        for(t in 1:nrow(OBS1)){ # Loop over the test set
                          ob <- OBS1[t,] # Get the t-th observations
                          ci1 <- ICS1[ICS1$Time == ob$Time,]; ci2 <- ICS2[ICS2$Time == ob$Time,]; ci3 <- ICS3[ICS3$Time == ob$Time,] # Get the clear-sky irradiance for the coming observations
                          fc1 <- FCS1[which(time_fit==ob$tod),]; fc2 <- FCS2[which(time_fit==ob$tod),]; fc3 <- FCS3[which(time_fit==ob$tod),] # Get the historical 1..K observations.
                          
                          # If the lengths of the ensembles are too short, the observation rank is per definition 
                          # low, resulting in left skewed rank histograms. That's why we skip here. Note that this
                          # limit varies per location, depending on seasonal variation. How about normalizing the ranks?
                          if(nrow(fc1) < m) next 
                          
                          ob <- as.matrix(ob[,((ncol(ob)-max(K)+1):ncol(ob))]) # Take only the observation vector
                          ci1 <- as.matrix(ci1[,((ncol(ci1)-max(K)+1):ncol(ci1))]) # Take only the clear-sky irradiance vector
                          ci2 <- as.matrix(ci2[,((ncol(ci2)-max(K)+1):ncol(ci2))]) # Take only the clear-sky irradiance vector
                          ci3 <- as.matrix(ci3[,((ncol(ci3)-max(K)+1):ncol(ci3))]) # Take only the clear-sky irradiance vector
                          fc1 <- as.matrix(fc1[,((ncol(fc1)-max(K)+1):ncol(fc1))]) # Take only teh forecast matrix
                          fc2 <- as.matrix(fc2[,((ncol(fc2)-max(K)+1):ncol(fc2))]) # Take only teh forecast matrix
                          fc3 <- as.matrix(fc3[,((ncol(fc3)-max(K)+1):ncol(fc3))]) # Take only teh forecast matrix
                          # Randomly select M ensemble forecasts
                          rows <- sample(nrow(fc1))[1:m]
                          fc1 <- fc1[rows,]; fc2 <- fc2[rows,]; fc3 <- fc3[rows,]
                          
                          fc1 <- sweep(fc1, MARGIN=2, ci1, `*`) # Matrix by vector multiplication for GHI
                          fc2 <- sweep(fc2, MARGIN=2, ci2, `*`) # Matrix by vector multiplication for GHI
                          fc3 <- sweep(fc3, MARGIN=2, ci3, `*`) # Matrix by vector multiplication for GHI
                          
                          # For rank histograms:
                          B1 <- rbind(ob,fc1); B2 <- rbind(ob,fc2); B3 <- rbind(ob,fc3) 
                          lst1[[i]] <- B1; lst2[[i]] <- B2; lst3[[i]] <- B3
                          # For numerical scores:
                          es1[[i]] <- scoringRules::es_sample(y=ob[1,],dat=t(fc1))
                          es2[[i]] <- scoringRules::es_sample(y=ob[1,],dat=t(fc2))
                          es3[[i]] <- scoringRules::es_sample(y=ob[1,],dat=t(fc3))
                          vs1[[i]] <- scoringRules::vs_sample(y=ob[1,],dat=t(fc1),p=0.5) # p recommended by SCHEUERER AND HAMILL
                          vs2[[i]] <- scoringRules::vs_sample(y=ob[1,],dat=t(fc2),p=0.5) # p recommended by SCHEUERER AND HAMILL
                          vs3[[i]] <- scoringRules::vs_sample(y=ob[1,],dat=t(fc3),p=0.5) # p recommended by SCHEUERER AND HAMILL
                          i <- i + 1
                        }
                        # Rank histograms:
                        avghist1 <- avg.rhist(lst1,m) # Average rank histogram based on Ineichen clear-sky
                        avghist1 <- data.frame(mids=avghist1$mids,counts=avghist1$counts,
                                               members = m,
                                               years=paste(yrs[[j]][2],"-",yrs[[j]][1],sep=""),
                                               station=toupper(station[stn]),prerank="AVG")
                        avghist2 <- avg.rhist(lst2,m) # Average rank histogram based on McClear clear-sky
                        avghist2 <- data.frame(mids=avghist2$mids,counts=avghist2$counts,
                                               members = m,
                                               years=paste(yrs[[j]][2],"-",yrs[[j]][1],sep=""),
                                               station=toupper(station[stn]),prerank="AVG")
                        avghist3 <- avg.rhist(lst3,m) # Average rank histogram based on REST2 clear-sky
                        avghist3 <- data.frame(mids=avghist3$mids,counts=avghist3$counts,
                                               members = m,
                                               years=paste(yrs[[j]][2],"-",yrs[[j]][1],sep=""),
                                               station=toupper(station[stn]),prerank="AVG")
                        bdhhist1 <- bd.rhist(lst1,m) # Band depth rank histogram based on Ineichen clear-sky
                        bdhhist1 <- data.frame(mids=bdhhist1$mids,counts=bdhhist1$counts,
                                               members = m,
                                               years=paste(yrs[[j]][2],"-",yrs[[j]][1],sep=""),
                                               station=toupper(station[stn]),prerank="BDH")
                        bdhhist2 <- bd.rhist(lst2,m) # Band depth rank histogram based on McClear clear-sky
                        bdhhist2 <- data.frame(mids=bdhhist2$mids,counts=bdhhist2$counts,
                                               members = m,
                                               years=paste(yrs[[j]][2],"-",yrs[[j]][1],sep=""),
                                               station=toupper(station[stn]),prerank="BDH")
                        bdhhist3 <- bd.rhist(lst3,m) # Band depth rank histogram based on REST2 clear-sky
                        bdhhist3 <- data.frame(mids=bdhhist3$mids,counts=bdhhist3$counts,
                                               members = m,
                                               years=paste(yrs[[j]][2],"-",yrs[[j]][1],sep=""),
                                               station=toupper(station[stn]),prerank="BDH")
                        msthist1 <- mst.rhist(lst1,m) # Minimum spinning tree rank histogram based on Ineichen clear-sky
                        msthist1 <- data.frame(mids=msthist1$mids,counts=msthist1$counts,
                                               members = m,
                                               years=paste(yrs[[j]][2],"-",yrs[[j]][1],sep=""),
                                               station=toupper(station[stn]),prerank="MST")
                        msthist2 <- mst.rhist(lst2,m) # Minimum spinning tree rank histogram based on McClear clear-sky
                        msthist2 <- data.frame(mids=msthist2$mids,counts=msthist2$counts,
                                               members = m,
                                               years=paste(yrs[[j]][2],"-",yrs[[j]][1],sep=""),
                                               station=toupper(station[stn]),prerank="MST")
                        msthist3 <- mst.rhist(lst3,m) # Minimum spinning tree rank histogram based on REST2 clear-sky
                        msthist3 <- data.frame(mids=msthist3$mids,counts=msthist3$counts,
                                               members = m,
                                               years=paste(yrs[[j]][2],"-",yrs[[j]][1],sep=""),
                                               station=toupper(station[stn]),prerank="MST")
                        rank_histograms1[[z]] <- rbind(avghist1,bdhhist1,msthist1)
                        rank_histograms2[[z]] <- rbind(avghist2,bdhhist2,msthist2)
                        rank_histograms3[[z]] <- rbind(avghist3,bdhhist3,msthist3)
                        # Energy score and variogram score:
                        num_score_1[[z]] <- data.frame(es=mean(do.call(c,es1)),vs=mean(do.call(c,vs1)),
                                                       members = m,
                                                       years=paste(yrs[[j]][2],"-",yrs[[j]][1],sep=""),
                                                       station=toupper(station[stn]))
                        num_score_2[[z]] <- data.frame(es=mean(do.call(c,es2)),vs=mean(do.call(c,vs2)),
                                                       members = m,
                                                       years=paste(yrs[[j]][2],"-",yrs[[j]][1],sep=""),
                                                       station=toupper(station[stn]))
                        num_score_3[[z]] <- data.frame(es=mean(do.call(c,es3)),vs=mean(do.call(c,vs3)),
                                                       members = m,
                                                       years=paste(yrs[[j]][2],"-",yrs[[j]][1],sep=""),
                                                       station=toupper(station[stn]))
                        
                        z <- z + 1 # Iterations over stations, years and ensemble members combined
                      }
                      
                    }
                    # Output results:
                    list(rank_histograms1,rank_histograms2,rank_histograms3,
                         num_score_1,num_score_2,num_score_3)
                  }

stopCluster(cl)
proc.time() - ptm

# Extract results from "script" and save
# res_1 = rank histograms Ineichen, res_2 = rank histograms McClear, res_3 = rank histograms REST2,
# res_4 = numerical scores Ineichen, res_5 = numerical scores McClear, res_6 = numerical scores REST2.

res_1 <- do.call(rbind,do.call(rbind,script[[1]]))
# write.table(res_1, file = "~/Desktop/Drive/research/mvBenchmark/results/rank_histograms_Ineichen.txt",
#             sep = "\t", row.names = FALSE, col.names = TRUE)
# write.table(res_1, file = "C:/Users/denva787/Documents/dennis/mvBenchmark/results/rank_histograms_Ineichen.txt",
#             sep = "\t", row.names = FALSE, col.names = TRUE)
write.table(res_1, file = paste(dir_save,"rank_histograms_Ineichen.txt",sep = ""),
            sep = "\t", row.names = FALSE, col.names = TRUE)

res_2 <- do.call(rbind,do.call(rbind,script[[2]]))
# write.table(res_2, file = "~/Desktop/Drive/research/mvBenchmark/results/rank_histograms_McClear.txt",
#             sep = "\t", row.names = FALSE, col.names = TRUE)
# write.table(res_2, file = "C:/Users/denva787/Documents/dennis/mvBenchmark/results/rank_histograms_McClear.txt",
#             sep = "\t", row.names = FALSE, col.names = TRUE)
write.table(res_2, file = paste(dir_save,"rank_histograms_McClear.txt",sep = ""),
            sep = "\t", row.names = FALSE, col.names = TRUE)

res_3 <- do.call(rbind,do.call(rbind,script[[3]]))
# write.table(res_3, file = "~/Desktop/Drive/research/mvBenchmark/results/rank_histograms_REST2.txt",
#             sep = "\t", row.names = FALSE, col.names = TRUE)
# write.table(res_3, file = "C:/Users/denva787/Documents/dennis/mvBenchmark/results/rank_histograms_REST2.txt",
#             sep = "\t", row.names = FALSE, col.names = TRUE)
write.table(res_3, file = paste(dir_save,"rank_histograms_REST2.txt",sep = ""),
            sep = "\t", row.names = FALSE, col.names = TRUE)

res_4 <- do.call(rbind,do.call(rbind,script[[4]]))
# write.table(res_4, file = "~/Desktop/Drive/research/mvBenchmark/results/numerical_scores_Ineichen.txt",
#             sep = "\t", row.names = FALSE, col.names = TRUE)
# write.table(res_4, file = "C:/Users/denva787/Documents/dennis/mvBenchmark/results/numerical_scores_Ineichen.txt",
#             sep = "\t", row.names = FALSE, col.names = TRUE)
write.table(res_4, file = paste(dir_save,"numerical_scores_Ineichen.txt",sep = ""),
            sep = "\t", row.names = FALSE, col.names = TRUE)

res_5 <- do.call(rbind,do.call(rbind,script[[5]]))
# write.table(res_5, file = "~/Desktop/Drive/research/mvBenchmark/results/numerical_scores_McClear.txt",
#             sep = "\t", row.names = FALSE, col.names = TRUE)
# write.table(res_5, file = "C:/Users/denva787/Documents/dennis/mvBenchmark/results/numerical_scores_McClear.txt",
#             sep = "\t", row.names = FALSE, col.names = TRUE)
write.table(res_5, file = paste(dir_save,"numerical_scores_McClear.txt",sep = ""),
            sep = "\t", row.names = FALSE, col.names = TRUE)

res_6 <- do.call(rbind,do.call(rbind,script[[6]]))
# write.table(res_6, file = "~/Desktop/Drive/research/mvBenchmark/results/numerical_scores_REST2.txt",
#             sep = "\t", row.names = FALSE, col.names = TRUE)
# write.table(res_6, file = "C:/Users/denva787/Documents/dennis/mvBenchmark/results/numerical_scores_REST2.txt",
#             sep = "\t", row.names = FALSE, col.names = TRUE)
write.table(res_6, file = paste(dir_save,"numerical_scores_REST2.txt",sep = ""),
            sep = "\t", row.names = FALSE, col.names = TRUE)


