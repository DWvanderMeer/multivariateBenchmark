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
source("~/Desktop/Drive/research/mvBenchmark/scripts/functions.R")
source("C:/Users/denva787/Documents/dennis/mvBenchmark/scripts/functions.R")
K <- seq(1,24,1) # forecast horizon
zen_angle <- 85 # maximum zenith angle
M <- seq(20,100,20) # number of ensemble members
#################################################################################

#################################################################################
# Load data
#################################################################################
# Load McClear data, can be downloaded here: http://www.soda-pro.com/fr/web-services/radiation/cams-mcclear
mcclear <- read.csv(file = "~/Desktop/Drive/research/mvBenchmark/data/mcclear_hawaii.csv", skip = 37, sep = ";")
mcclear <- read.csv(file = "C:/Users/denva787/Documents/dennis/mvBenchmark/data/mcclear_hawaii.csv", skip = 37, sep = ";")
mcclear <- subset(mcclear, select = c(X..Observation.period,Clear.sky.GHI))
colnames(mcclear) <- c("Time","McClear")
mcclear$McClear <- 4*mcclear$McClear # Data is Wh/m2 and 15 min resolution
datetime <- strsplit(mcclear$Time,"/") # Split the observation period character string on /
tmp <- NULL # Convert the end of the period to posixct format
for(i in 1:length(datetime)){tmp[[i]] <- as.POSIXct(datetime[[i]][2], format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")}
mcclear$Time <- do.call(c, tmp)
attributes(mcclear$Time)$tzone <- "HST" # Hawaii standard time
# Load Hawaii data:
dat <- read.table(file = "~/Desktop/Drive/research/mvBenchmark/data/hawaii.txt",
                  header = T, sep = "\t")
dat <- read.table(file = "C:/Users/denva787/Documents/dennis/mvBenchmark/data/hawaii.txt",
                  header = T, sep = "\t")
dat$Time <- as.POSIXct(dat$Time, format = "%Y-%m-%d %H:%M:%S", tz = "HST") # Not actually UTC but for simplicity
mcclear <- mcclear[mcclear$Time %in% dat$Time,] # Subset mcclear so that it covers the same period.
dat <- subset(dat,select = -AP3) # AP3 contains many 0 values (for some reason)
dat <- tibble::add_column(dat, McClear = mcclear$McClear, .after = 3)
dat <- tibble::add_column(dat, tod = strftime(dat$Time, format = "%H:%M", tz = "HST"), .after = 5)
ns <- ncol(dat)-6 # Number of stations (16) is ncol minus time/zen/Ics/McClear/Ioh/tod columns
#################################################################################

# Define training data periods (length increases with 3 months on every iteration)
train_months_seq <- seq(3,15,3) # Intervals of 3 months, starting in March (month 3)
train_months <- lubridate::month(seq(as.Date("2010-03-01"), as.Date("2011-05-01"), by = "month"))
train_years <- lubridate::year(seq(as.Date("2010-03-01"), as.Date("2011-05-01"), by = "month"))
mnths <- years <- list()
j <- 1
for(i in train_months_seq){
  mnths[[j]] <- train_months[1:i] # Training data increases in length
  years[[j]] <- train_years[1:i]
  j <- j+1
}

# Define testing data period (5 months that are fixed)
te_idx_m <- lubridate::month(seq(as.Date("2011-06-01"), as.Date("2011-10-01"), by = "month")) # 5 months
te_idx_y <- lubridate::year(seq(as.Date("2011-06-01"), as.Date("2011-10-01"), by = "month"))
idx_te <- (lubridate::year(dat$Time) %in% te_idx_y & 
             lubridate::month(dat$Time) %in% te_idx_m) # The test set is fixed
te_dat <- dat[idx_te,] # Testing data set
# Prepare testing data and clear-sky expectation
OBS1 <- list()
for(k in K){ # Loop over the forecast horizons to shift the entire matrix of observations
  X <- data.table::shift(x = te_dat[,(ncol(te_dat)-ns+1):ncol(te_dat)], n = k, fill = NA, type = "lead") # Start at 6th column for first station    
  X <- do.call(what = cbind, X)
  OBS1[[k]] <- X
}
OBS1 <- do.call(cbind,OBS1)
ICS1 <- data.table::shift(te_dat$Ics,n=K,type="lead"); ICS1 <- do.call(cbind,ICS1) # Create matrix clear-sky irradaince
ICS2 <- data.table::shift(te_dat$McClear,n=K,type="lead"); ICS2 <- do.call(cbind,ICS2) # Create matrix clear-sky irradaince
# Assume clear-sky irradiance is identical at each station and repeat it 1..K times for each station:
ICS1 <- ICS1[,rep(K, each = ns)]
ICS2 <- ICS2[,rep(K, each = ns)]
zen <- data.table::shift(te_dat$zen,n=K,type="lead"); zen <- do.call(cbind,zen) # Create zenith mask
zen <- zen[,rep(K, each = ns)] # 16 stations excluding AP3
idx_zen <- zen <= zen_angle # Create zenith mask
OBS1[idx_zen == FALSE] <- NA
ICS1[idx_zen == FALSE] <- NA
ICS2[idx_zen == FALSE] <- NA
# Combine with time, zenith angle and clear-sky irradiance:
OBS1 <- data.frame(subset(te_dat,select = c("Time","zen","Ics","tod")),OBS1)
OBS1 <- OBS1[OBS1$zen < zen_angle,] # Remove rows based on zenith
OBS1 <- OBS1[complete.cases(OBS1),] # Remove rows based on zenith
OBS1[OBS1==0] <- NA # Check if there are rows with zero values remaining --> these are missing values.
idx_zero <- complete.cases(OBS1) # Index these rows so we can also subset ICS1 later on.
# Remove certain rows:
OBS1 <- OBS1[idx_zero,]
ICS1 <- data.frame(subset(te_dat,select = c("Time","zen","Ics","tod")),ICS1)
ICS1 <- ICS1[ICS1$zen < zen_angle,] # Remove rows with zenith angle lower than limit
ICS1 <- ICS1[complete.cases(ICS1),] # Remove rows with NAs
ICS1 <- ICS1[idx_zero,] # Remove rows that are zero in the observations OBS1.
ICS2 <- data.frame(subset(te_dat,select = c("Time","zen","McClear","tod")),ICS2)
ICS2 <- ICS2[ICS2$zen < zen_angle,] # Remove rows with zenith angle lower than limit
ICS2 <- ICS2[complete.cases(ICS2),] # Remove rows with NAs
ICS2 <- ICS2[idx_zero,] # Remove rows that are zero in the observations OBS1.

# Prepare multiprocessing
cl <- makeCluster(length(mnths))
registerDoParallel(cl)

rank_histograms1 = rank_histograms2 <- list() # To store the rank histograms
num_score_1 = num_score_2 <- list() # To store the numerical scores ES and VS
z <- 1
set.seed(123) # Set seed for repeatability
script <- foreach(j = 1:length(mnths), .combine='comb', .multicombine=TRUE, 
                  .init=list(list(),list(),list(),list()), .packages = 'dplyr') %dopar% { # Loop over the months
                    
                    idx_tr <- (lubridate::year(dat$Time) %in% years[[j]] & 
                                 lubridate::month(dat$Time) %in% mnths[[j]]) # This should be the iterable
                    
                    tr_dat <- dat[idx_tr,]
                    
                    # Prepare training data:
                    CSI1 <- tr_dat %>% select(tail(names(.), ns)) / tr_dat$Ics      # Select only GHI data from stations
                    CSI2 <- tr_dat %>% select(tail(names(.), ns)) / tr_dat$McClear  # and convert to clear-sky index
                    for(co in 1:ncol(CSI1)){
                      CSI1[,co] <- ifelse(CSI1[,co] > 1.5, 1.5, CSI1[,co]) # Limit clear-sky index.
                      CSI2[,co] <- ifelse(CSI2[,co] > 1.5, 1.5, CSI2[,co])
                    }
                    lst1 = lst2 <- list()
                    for(k in K){
                      X1 <- data.table::shift(x = CSI1, n = k, fill = NA, type = "lead")     
                      X2 <- data.table::shift(x = CSI2, n = k, fill = NA, type = "lead")     
                      X1 <- do.call(what = cbind, X1); X2 <- do.call(what = cbind, X2)
                      lst1[[k]] <- X1; lst2[[k]] <- X2
                    }
                    CSI1 <- do.call(cbind,lst1)
                    CSI2 <- do.call(cbind,lst2)
                    
                    zen <- data.table::shift(tr_dat$zen,n=K,type="lead"); zen <- do.call(cbind,zen) # Create zenith mask
                    # Assume clear-sky irradiance is identical at each station and repeat it 1..K times for each station.
                    zen <- zen[,rep(K, each = ns)]
                    idx_zen <- zen <= zen_angle # Create zenith mask
                    CSI1[idx_zen == FALSE] <- NA; CSI2[idx_zen == FALSE] <- NA
                    # Forecasts based on Ineichen-Perez:
                    FCS1 <- data.frame(subset(tr_dat,select = c("Time","zen","Ics","tod")),CSI1)
                    FCS1 <- FCS1[FCS1$zen < zen_angle,] # Remove rows based on zenith angle
                    FCS1[FCS1==0] <- NA
                    FCS1 <- FCS1[complete.cases(FCS1),] # Remove rows based on zenith angle and potential zero values
                    # Forecasts based on McClear
                    FCS2 <- data.frame(subset(tr_dat,select = c("Time","zen","McClear","tod")),CSI2)
                    FCS2 <- FCS2[FCS2$zen < zen_angle,] # Remove rows based on zenith angle
                    FCS2[FCS2==0] <- NA
                    FCS2 <- FCS2[complete.cases(FCS2),] # Remove rows based on zenith angle and potential zero values
                    
                    time_fit <- strftime(FCS1$Time, format = "%H:%M", tz = "HST") # Hawwaii standard time
                    
                    lst1 = lst2 <- list() # Store the results for each clear-sky model separately
                    es1 = es2 = vs1 = vs2 <- NULL # Numerical scores
                    
                    for(m in M){
                      i <- 1 # Counter in case a t iteration is skipped 
                      for(t in 1:nrow(OBS1)){ # Loop over the test set
                        ob <- OBS1[t,] # Get the t-th observations
                        ci1 <- ICS1[ICS1$Time == ob$Time,] # Get the clear-sky irradiance for the coming observations
                        ci2 <- ICS2[ICS2$Time == ob$Time,] # Get the clear-sky irradiance for the coming observations
                        fc1 <- FCS1[which(time_fit==ob$tod),] # Get the historical 1..K observations.
                        fc2 <- FCS2[which(time_fit==ob$tod),] # Get the historical 1..K observations.
                        
                        # If the lengths of the ensembles are too short, the observation rank is per definition 
                        # low, resulting in left skewed rank histograms. That's why we skip here. Note that this
                        # limit varies per location, depending on seasonal variation. How about normalizing the ranks?
                        if(nrow(fc1) < m) next 
                        
                        ob <- as.matrix(ob[,((ncol(ob)-max(K)*ns+1):ncol(ob))]) # Take only the observation vector
                        ci1 <- as.matrix(ci1[,((ncol(ci1)-max(K)*ns+1):ncol(ci1))]) # Take only the clear-sky irradiance vector
                        ci2 <- as.matrix(ci2[,((ncol(ci2)-max(K)*ns+1):ncol(ci2))]) # Take only the clear-sky irradiance vector
                        fc1 <- as.matrix(fc1[,((ncol(fc1)-max(K)*ns+1):ncol(fc1))]) # Take only the forecast matrix
                        fc2 <- as.matrix(fc2[,((ncol(fc2)-max(K)*ns+1):ncol(fc2))]) # Take only the forecast matrix
                        # Randomly select M ensemble forecasts
                        rows <- sample(nrow(fc1))[1:m]
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
                      
                      # Training period string (month year - month year):
                      train_period_string <-  paste(month.abb[mnths[[j]][1]]," ",
                                                    years[[j]][1],"-",
                                                    month.abb[mnths[[j]][length(mnths[[j]])]]," ",
                                                    years[[j]][length(years[[j]])],sep="")
                      # Rank histograms:
                      avghist1 <- avg.rhist(lst1,m) # Average rank histogram based on Ineichen clear-sky
                      avghist1 <- data.frame(mids=avghist1$mids,counts=avghist1$counts,
                                             members = m,
                                             months=train_period_string,
                                             prerank="AVG")
                      avghist2 <- avg.rhist(lst2,m) # Average rank histogram based on McClear clear-sky
                      avghist2 <- data.frame(mids=avghist2$mids,counts=avghist2$counts,
                                             members = m,
                                             months=train_period_string,
                                             prerank="AVG")
                      bdhhist1 <- bd.rhist(lst1,m) # Band depth rank histogram based on Ineichen clear-sky
                      bdhhist1 <- data.frame(mids=bdhhist1$mids,counts=bdhhist1$counts,
                                             members = m,
                                             months=train_period_string,
                                             prerank="BDH")
                      bdhhist2 <- bd.rhist(lst2,m) # Band depth rank histogram based on McClear clear-sky
                      bdhhist2 <- data.frame(mids=bdhhist2$mids,counts=bdhhist2$counts,
                                             members = m,
                                             months=train_period_string,
                                             prerank="BDH")
                      msthist1 <- mst.rhist(lst1,m) # Minimum spinning tree rank histogram based on Ineichen clear-sky
                      msthist1 <- data.frame(mids=msthist1$mids,counts=msthist1$counts,
                                             members = m,
                                             months=train_period_string,
                                             prerank="MST")
                      msthist2 <- mst.rhist(lst2,m) # Minimum spinning tree rank histogram based on McClear clear-sky
                      msthist2 <- data.frame(mids=msthist2$mids,counts=msthist2$counts,
                                             members = m,
                                             months=train_period_string,
                                             prerank="MST")
                      # Store rank histograms:
                      rank_histograms1[[z]] <- rbind(avghist1,bdhhist1,msthist1)
                      rank_histograms2[[z]] <- rbind(avghist2,bdhhist2,msthist2)
                      # Energy score and variogram score:
                      num_score_1[[z]] <- data.frame(es=mean(do.call(c,es1)),vs=mean(do.call(c,vs1)),
                                                     members = m,
                                                     months=train_period_string)
                      num_score_2[[z]] <- data.frame(es=mean(do.call(c,es2)),vs=mean(do.call(c,vs2)),
                                                     members = m,
                                                     months=train_period_string)
                      z <- z + 1 # Iterations over ensemble member combinations
                    }
                    
                    # Output results:
                    list(rank_histograms1,rank_histograms2,
                         num_score_1,num_score_2)
                  }

stopCluster(cl)

# Combine and store results:
res_1 <- do.call(rbind,do.call(rbind,script[[1]]))
# write.table(res_1, file = "~/Desktop/Drive/research/mvBenchmark/results/rank_histograms_Ineichen_Hawaii.txt",
#             sep = "\t", row.names = FALSE, col.names = TRUE)
write.table(res_1, file = "C:/Users/denva787/Documents/dennis/mvBenchmark/results/rank_histograms_Ineichen_Hawaii.txt",
            sep = "\t", row.names = FALSE, col.names = TRUE)

res_2 <- do.call(rbind,do.call(rbind,script[[2]]))
# write.table(res_2, file = "~/Desktop/Drive/research/mvBenchmark/results/rank_histograms_McClear_Hawaii.txt",
#             sep = "\t", row.names = FALSE, col.names = TRUE)
write.table(res_2, file = "C:/Users/denva787/Documents/dennis/mvBenchmark/results/rank_histograms_McClear_Hawaii.txt",
            sep = "\t", row.names = FALSE, col.names = TRUE)

res_3 <- do.call(rbind,do.call(rbind,script[[3]]))
# write.table(res_3, file = "~/Desktop/Drive/research/mvBenchmark/results/numerical_scores_Ineichen_Hawaii.txt",
#             sep = "\t", row.names = FALSE, col.names = TRUE)
write.table(res_3, file = "C:/Users/denva787/Documents/dennis/mvBenchmark/results/numerical_scores_Ineichen_Hawaii.txt",
            sep = "\t", row.names = FALSE, col.names = TRUE)

res_4 <- do.call(rbind,do.call(rbind,script[[4]]))
# write.table(res_4, file = "~/Desktop/Drive/research/mvBenchmark/results/numerical_scores_McClear_Hawaii.txt",
#             sep = "\t", row.names = FALSE, col.names = TRUE)
write.table(res_4, file = "C:/Users/denva787/Documents/dennis/mvBenchmark/results/numerical_scores_McClear_Hawaii.txt",
            sep = "\t", row.names = FALSE, col.names = TRUE)

