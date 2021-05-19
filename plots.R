#################################################################################
# This code is written by Dennis van der Meer
# Uppsala University, Department of Civil and Industrial Engineering
# Division of Civil Engineering and Built Environment
# email: dennis.vandermeer@angstrom.uu.se
#################################################################################

library(ggplot2)
library(dplyr)

#################################################################################
# Inputs
#################################################################################
dir <- "~/Desktop/Drive/research/mvBenchmark/results" # Working directory path
setwd(dir)
plot.size = 8; line.size = 0.1; point.size = 0.6
#################################################################################

#################################################################################
# Data analysis
#################################################################################

#################################################################################
# SURFRAD
#################################################################################
# dir <- "~/Desktop/Drive/research/mvBenchmark/data" # Working directory path
# station <- c("bon", "dra", "fpk", "gwn", "psu", "sxf", "tbl")
# tz <- c(-5, -7, -6, -5, -4, -5, -6)
# # zen_angle <- 85 # maximum zenith angle
# 
# stn = 7
# # Main script
# setwd(file.path(dir, station[stn]))# Set directory
# # SURFRAD
# surfrad <- read.table(file = "surfrad15_2004-2017.txt", header = TRUE, sep = "\t", colClasses = c("character", rep("numeric",4))) #read data
# surfrad$Time <-  as.POSIXct(surfrad$Time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# surfrad <- surfrad %>% mutate(Time = Time+tz[stn]*3600) # Adjust time zone to local tz
# surfrad$tod <- strftime(surfrad$Time, format = "%H:%M", tz = "UTC") # Time of day (UTC because tz is set manually)
# 
# surfrad_1011 <- surfrad %>% filter(lubridate::year(Time) %in% c(2010,2011))
# surfrad_1011$year <- "'10-'11"
# 
# surfrad_1617 <- surfrad %>% filter(lubridate::year(Time) %in% c(2016,2017))
# surfrad_1617$year <- "'16-'17"
# 
# mydf <- rbind(surfrad_1011,surfrad_1617)
# mydf$csi <- mydf$dw_solar/mydf$McClear
# 
# mydf_ghi <- subset(mydf, select = c("dw_solar","tod","year"))
# mydf_csi <- subset(mydf, select = c("csi","tod","year"))
# 
# mydf_csi$csi <- ifelse(mydf_csi$csi > 1.5, 1.5, mydf_csi$csi)
# mydf_csi$csi <- ifelse(is.na(mydf_csi$csi), 0, mydf_csi$csi)
# 
# plot.size = 8; line.size = 0.1; point.size = 0.6
# 
# p_ghi <- ggplot(data = mydf_ghi, aes(x=tod,y=dw_solar,group=tod))+
#   facet_grid(~year, scales = "free_y") +
#   geom_boxplot(outlier.size = 0.5, outlier.alpha = 0.4) +
#   scale_x_discrete(breaks = unique(mydf_ghi$tod)[seq(1,96,8)]) +
#   theme_bw() +
#   theme(plot.margin = unit(c(0.2,0.075,0,0.05), "lines"),
#         text = element_text(family = "Times"),
#         axis.text=element_text(family = "Times", size=plot.size),
#         axis.title=element_text(family = "Times", size=plot.size),
#         axis.title.x = element_blank(),
#         axis.text.x = element_blank(),
#         axis.ticks.x = element_blank(),
#         plot.title=element_text(family = "Times", size=plot.size),
#         legend.title = element_text(family = "Times", size=plot.size),
#         legend.text = element_text(family = "Times", size=plot.size),
#         legend.box.margin = ggplot2::margin(-5,-4,0,-10),
#         legend.margin = ggplot2::margin(0,0,0,0),
#         strip.text = element_text(family = "Times", size=plot.size),
#         strip.text.x = element_text(margin = margin(0.05,0,0.05,0, "lines")),
#         strip.text.y = element_text(margin = margin(0,0.05,0,0.05, "lines")),
#         panel.spacing = unit(0.0, "lines"),
#         panel.grid = element_blank()) +
#   xlab("Time (HH:MM)") +
#   ylab(bquote('Global horizontal irradiance (W/m'^2*')'))
# # p_ghi
# 
# p_csi <- ggplot(data = mydf_csi, aes(x=tod,y=csi,group=tod))+
#   facet_grid(~year, scales = "free_y") +
#   geom_boxplot(outlier.size = 0.5, outlier.alpha = 0.4) +
#   scale_x_discrete(breaks = unique(mydf_csi$tod)[seq(1,96,8)]) +
#   theme_bw() +
#   theme(plot.margin = unit(c(-0.2,0.075,0,0.05), "lines"),
#         text = element_text(family = "Times"),
#         axis.text=element_text(family = "Times", size=plot.size),
#         axis.text.x=element_text(family = "Times", size=plot.size, angle = 90, vjust = 0.5),
#         axis.title=element_text(family = "Times", size=plot.size),
#         plot.title=element_text(family = "Times", size=plot.size),
#         legend.title = element_text(family = "Times", size=plot.size),
#         legend.text = element_text(family = "Times", size=plot.size),
#         legend.box.margin = ggplot2::margin(-5,-4,0,-10),
#         legend.margin = ggplot2::margin(0,0,0,0),
#         strip.text = element_text(family = "Times", size=plot.size),
#         strip.text.x = element_text(margin = margin(0.05,0,0.05,0, "lines")),
#         strip.text.y = element_text(margin = margin(0,0.05,0,0.05, "lines")),
#         panel.spacing = unit(0.0, "lines"),
#         panel.grid = element_blank()) +
#   xlab("Time (HH:MM)") +
#   ylab("Clear-sky index (-)")
# # p_csi
# 
# p <- cowplot::plot_grid(p_ghi, p_csi, align = "v", nrow = 2, rel_heights = c(1,1.2)) # 9.5,4,4,4
# # p
# ggsave(filename = "~/Desktop/Drive/research/mvBenchmark/paper/images/data_analysis_tbl.pdf",
#        plot = p, device = "pdf", units = "cm", height = 10, width = 17) 

dir <- "~/Desktop/Drive/research/mvBenchmark/data" # Working directory path
station <- c("bon", "dra", "fpk", "gwn", "psu", "sxf", "tbl")
tz <- c(-5, -7, -6, -5, -4, -5, -6)

stns <- c(2,3)
lst <- list()
i <- 1
for(stn in stns){
  setwd(file.path(dir, station[stn]))# Set directory
  # SURFRAD
  surfrad <- read.table(file = "surfrad15_2004-2017.txt", header = TRUE, sep = "\t", colClasses = c("character", rep("numeric",4))) #read data
  surfrad$Time <-  as.POSIXct(surfrad$Time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  surfrad <- surfrad %>% mutate(Time = Time+tz[stn]*3600) # Adjust time zone to local tz
  surfrad$tod <- strftime(surfrad$Time, format = "%H:%M", tz = "UTC") # Time of day (UTC because tz is set manually)
  
  surfrad_1112 <- surfrad %>% filter(lubridate::year(Time) %in% c(2011,2012))
  surfrad_1112$year <- "2011-2012"
  
  surfrad_1617 <- surfrad %>% filter(lubridate::year(Time) %in% c(2016,2017))
  surfrad_1617$year <- "2016-2017"
  
  mydf <- rbind(surfrad_1112,surfrad_1617)
  mydf$csi <- mydf$dw_solar/mydf$McClear
  mydf$station <- toupper(station[stn])
  lst[[i]] <- mydf
  i <- i+1
}
mydf <- do.call(rbind,lst)
mydf_csi <- subset(mydf, select = c("csi","tod","year","station"))

mydf_csi$csi <- ifelse(mydf_csi$csi > 1.5, 1.5, mydf_csi$csi)
mydf_csi$csi <- ifelse(is.na(mydf_csi$csi), 0, mydf_csi$csi)

plot.size = 8; line.size = 0.1; point.size = 0.6

p <- ggplot(data = mydf_csi, aes(x=tod,y=csi,group=tod))+
  facet_grid(station~year, scales = "free_y") +
  geom_boxplot(outlier.size = 0.5, outlier.alpha = 0.4) +
  scale_x_discrete(breaks = unique(mydf_csi$tod)[seq(1,96,8)]) +
  theme_bw() +
  theme(plot.margin = unit(c(0.2,0.075,0,0.05), "lines"),
        text = element_text(family = "Times"),
        axis.text=element_text(family = "Times", size=plot.size),
        axis.text.x=element_text(family = "Times", size=plot.size, angle = 90, vjust = 0.5),
        axis.title=element_text(family = "Times", size=plot.size),
        plot.title=element_text(family = "Times", size=plot.size),
        legend.title = element_text(family = "Times", size=plot.size),
        legend.text = element_text(family = "Times", size=plot.size),
        legend.box.margin = ggplot2::margin(-5,-4,0,-10),
        legend.margin = ggplot2::margin(0,0,0,0),
        strip.text = element_text(family = "Times", size=plot.size),
        strip.text.x = element_text(margin = margin(0.05,0,0.05,0, "lines")),
        strip.text.y = element_text(margin = margin(0,0.05,0,0.05, "lines")),
        panel.spacing = unit(0.0, "lines"),
        panel.grid = element_blank()) +
  xlab("Time (HH:MM)") +
  ylab("Clear-sky index (-)")
p
ggsave(filename = "~/Desktop/Drive/research/mvBenchmark/paper/images/data_analysis.pdf",
       plot = p, device = "pdf", units = "cm", height = 10, width = 17) 

# Compute Kolmogorov-Smirnov statistic
t_vec <- format( seq.POSIXt(as.POSIXct("2021-03-11 14:15:00"), as.POSIXct("2021-03-11 18:00:00"), 
                            by = "15 min"), "%H:%M")
D_dra = D_fpk = p_dra = p_fpk <- NULL
for(t in t_vec){
  tmp1 <- mydf_csi[mydf_csi$tod == t & mydf_csi$year == "2011-2012" & mydf_csi$station == "DRA",]
  tmp2 <- mydf_csi[mydf_csi$tod == t & mydf_csi$year == "2016-2017" & mydf_csi$station == "DRA",]
  D_dra <- c(D_dra,ks.test(tmp1$csi,tmp2$csi,alternative = "two.sided")[1][[1]])
  p_dra <- c(p_dra,ks.test(tmp1$csi,tmp2$csi,alternative = "two.sided")[2][[1]])
  
  tmp3 <- mydf_csi[mydf_csi$tod == t & mydf_csi$year == "2011-2012" & mydf_csi$station == "FPK",]
  tmp4 <- mydf_csi[mydf_csi$tod == t & mydf_csi$year == "2016-2017" & mydf_csi$station == "FPK",]
  D_fpk <- c(D_fpk,ks.test(tmp3$csi,tmp4$csi,alternative = "two.sided")[1][[1]])
  p_fpk <- c(p_fpk,ks.test(tmp3$csi,tmp4$csi,alternative = "two.sided")[2][[1]])
}

res_dra <- data.frame(D = D_dra, p = p_dra, station = "DRA", time = t_vec)
res_fpk <- data.frame(D = D_fpk, p = p_fpk, station = "FPK", time = t_vec)
res <- rbind(res_dra,res_fpk)

results <- res %>% group_by(station) %>%
  mutate(value = paste(sanitize.numbers(format(D, scientific = TRUE, digits = 2),
                                        type = "latex", math.style.exponents = TRUE), 
                       " (", 
                       sanitize.numbers(format(p, scientific = TRUE, digits = 2),
                                        type = "latex", math.style.exponents = TRUE),
                       ")", sep = "")) %>%
  dplyr::select(-one_of(c("D", "p")))

tmp <- tidyr::pivot_wider(data = results, names_from = "station", values_from = "value", id_cols = "time")
print(xtable(tmp), include.rownames=FALSE, sanitize.text.function = function(x){x})

#################################################################################
# Hawaii
#################################################################################
dat <- read.table(file = "~/Desktop/Drive/research/mvBenchmark/data/hawaii.txt",
                  header = T, sep = "\t")
dat$Time <- as.POSIXct(dat$Time, format = "%Y-%m-%d %H:%M:%S", tz = "HST") # Not actually UTC but for simplicity
dat <- subset(dat,select = -AP3) # AP3 contains many 0 values (for some reason)
dat <- tibble::add_column(dat, tod = strftime(dat$Time, format = "%H:%M", tz = "HST"), .after = 4)
ns <- ncol(dat)-6 # Number of stations (16) is ncol minus time/zen/Ics/McClear/Ioh/tod columns

# Define training and testing data
month <- seq(10, 4, by = -1)
mnths = list()
for(i in 1:(length(month)-2)){ mnths[[i]] <- month[(i-1) + 1:3] } # Test months
year <- 2011 # Test year

ghi_lst_tr = ghi_lst_te <- list()
csi_lst_tr = csi_lst_te <- list()
for(i in 1:length(mnths)){
  idx <- (lubridate::year(dat$Time) %in% year & 
            lubridate::month(dat$Time) %in% mnths[[i]]) # This should be the iterable
  te_dat <- dat[idx,]
  tr_dat <- dat[-idx,]
  
  mydf_ghi_tr <- data.frame(ghi=rowMeans(tr_dat[,6:21]),
                            tod = tr_dat$tod,
                            # months = paste(mnths[[i]][3],"-",mnths[[i]][1],sep=""),
                            data = "Training")
  mydf_ghi_te <- data.frame(ghi=rowMeans(te_dat[,6:21]),
                            tod = te_dat$tod,
                            months = paste(month.abb[mnths[[i]][3]],"-",month.abb[mnths[[i]][1]],sep=""),
                            month1 = mnths[[i]][3],
                            month2 = mnths[[i]][1])
  mydf_csi_tr <- data.frame(csi=rowMeans(tr_dat[,6:21]) / tr_dat$Ics,
                            tod = tr_dat$tod,
                            # months = paste(mnths[[i]][3],"-",mnths[[i]][1],sep=""),
                            data = "Training")
  mydf_csi_te <- data.frame(csi=rowMeans(te_dat[,6:21]) / te_dat$Ics,
                            tod = te_dat$tod,
                            months = paste(month.abb[mnths[[i]][3]],"-",month.abb[mnths[[i]][1]],sep=""))
  mydf_csi_tr$csi <- ifelse(mydf_csi_tr$csi > 1.5, 1.5, mydf_csi_tr$csi)
  mydf_csi_tr$csi <- ifelse(is.na(mydf_csi_tr$csi), 0, mydf_csi_tr$csi)
  mydf_csi_te$csi <- ifelse(mydf_csi_te$csi > 1.5, 1.5, mydf_csi_te$csi)
  mydf_csi_te$csi <- ifelse(is.na(mydf_csi_te$csi), 0, mydf_csi_te$csi)
  # mydf_csi_te$data <- "Testing"
  
  ghi_lst_tr[[i]] <- mydf_ghi_tr
  ghi_lst_te[[i]] <- mydf_ghi_te
  csi_lst_tr[[i]] <- mydf_csi_tr
  csi_lst_te[[i]] <- mydf_csi_te
}
mydf_ghi_te <- do.call(rbind,ghi_lst_te)
# Trying to make the months appear in chronological order after using their abbreviations
# instead of the month number
mydf_ghi_te$month1 <- factor(mydf_ghi_te$month1)
mydf_ghi_te$month = factor(mydf_ghi_te$months, levels = levels(mydf_ghi_te$month1))

mydf_csi_te <- do.call(rbind,csi_lst_te)

plot.size = 8; line.size = 0.1; point.size = 0.6

p_ghi <- ggplot(data = mydf_ghi_te, aes(x=tod,y=ghi,group=tod))+
  facet_grid(~months, scales = "free_y") +
  geom_boxplot(outlier.size = 0.5, outlier.alpha = 0.4) +
  scale_x_discrete(breaks = unique(mydf_ghi_te$tod)[seq(2,61,8)]) +
  theme_bw() +
  theme(plot.margin = unit(c(0.2,0.075,0,0.05), "lines"),
        text = element_text(family = "Times"),
        axis.text=element_text(family = "Times", size=plot.size),
        axis.title=element_text(family = "Times", size=plot.size),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title=element_text(family = "Times", size=plot.size),
        legend.title = element_text(family = "Times", size=plot.size),
        legend.text = element_text(family = "Times", size=plot.size),
        legend.box.margin = ggplot2::margin(-5,-4,0,-10),
        legend.margin = ggplot2::margin(0,0,0,0),
        strip.text = element_text(family = "Times", size=plot.size),
        strip.text.x = element_text(margin = margin(0.05,0,0.05,0, "lines")),
        strip.text.y = element_text(margin = margin(0,0.05,0,0.05, "lines")),
        panel.spacing = unit(0.0, "lines"),
        panel.grid = element_blank()) +
  xlab("Time (HH:MM)") +
  ylab(bquote('Global horizontal irradiance (W/m'^2*')'))
# p_ghi

p_csi <- ggplot(data = mydf_csi_te, aes(x=tod,y=csi,group=tod))+
  facet_grid(~months, scales = "free_y") +
  geom_boxplot(outlier.size = 0.5, outlier.alpha = 0.4) +
  scale_x_discrete(breaks = unique(mydf_csi_te$tod)[seq(2,61,8)]) +
  theme_bw() +
  theme(plot.margin = unit(c(-0.2,0.075,0,0.05), "lines"),
        text = element_text(family = "Times"),
        axis.text=element_text(family = "Times", size=plot.size),
        axis.text.x=element_text(family = "Times", size=plot.size, angle = 90, vjust = 0.5),
        axis.title=element_text(family = "Times", size=plot.size),
        plot.title=element_text(family = "Times", size=plot.size),
        legend.title = element_text(family = "Times", size=plot.size),
        legend.text = element_text(family = "Times", size=plot.size),
        legend.box.margin = ggplot2::margin(-5,-4,0,-10),
        legend.margin = ggplot2::margin(0,0,0,0),
        strip.text = element_text(family = "Times", size=plot.size),
        strip.text.x = element_text(margin = margin(0.05,0,0.05,0, "lines")),
        strip.text.y = element_text(margin = margin(0,0.05,0,0.05, "lines")),
        panel.spacing = unit(0.0, "lines"),
        panel.grid = element_blank()) +
  xlab("Time (HH:MM)") +
  ylab("Clear-sky index (-)")
# p_csi
# p <- cowplot::plot_grid(p_ghi, p_csi, align = "v", nrow = 2, rel_heights = c(1,1.2)) # 9.5,4,4,4
# p

p_ghi_tr <- ggplot(data = mydf_ghi_tr, aes(x=tod,y=ghi,group=tod))+
  facet_grid(~data, scales = "free_y") +
  geom_boxplot(outlier.size = 0.5, outlier.alpha = 0.4) +
  scale_x_discrete(breaks = unique(mydf_ghi_tr$tod)[seq(2,61,8)]) +
  theme_bw() +
  theme(plot.margin = unit(c(0.2,0.075,0,0.05), "lines"),
        text = element_text(family = "Times"),
        axis.text=element_text(family = "Times", size=plot.size),
        axis.title=element_text(family = "Times", size=plot.size),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title=element_text(family = "Times", size=plot.size),
        legend.title = element_text(family = "Times", size=plot.size),
        legend.text = element_text(family = "Times", size=plot.size),
        legend.box.margin = ggplot2::margin(-5,-4,0,-10),
        legend.margin = ggplot2::margin(0,0,0,0),
        strip.text = element_text(family = "Times", size=plot.size),
        strip.text.x = element_text(margin = margin(0.05,0,0.05,0, "lines")),
        strip.text.y = element_text(margin = margin(0,0.05,0,0.05, "lines")),
        panel.spacing = unit(0.0, "lines"),
        panel.grid = element_blank()) +
  xlab("Time (HH:MM)") +
  ylab(bquote('Global horizontal irradiance (W/m'^2*')'))
# p_ghi_tr

p_csi_tr <- ggplot(data = mydf_csi_tr, aes(x=tod,y=csi,group=tod))+
  facet_grid(~data, scales = "free_y") +
  geom_boxplot(outlier.size = 0.5, outlier.alpha = 0.4) +
  scale_x_discrete(breaks = unique(mydf_csi_tr$tod)[seq(2,61,8)]) +
  theme_bw() +
  theme(plot.margin = unit(c(-0.2,0.075,0,0.05), "lines"),
        text = element_text(family = "Times"),
        axis.text=element_text(family = "Times", size=plot.size),
        axis.text.x=element_text(family = "Times", size=plot.size, angle = 90, vjust = 0.5),
        axis.title=element_text(family = "Times", size=plot.size),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title=element_text(family = "Times", size=plot.size),
        legend.title = element_text(family = "Times", size=plot.size),
        legend.text = element_text(family = "Times", size=plot.size),
        legend.box.margin = ggplot2::margin(-5,-4,0,-10),
        legend.margin = ggplot2::margin(0,0,0,0),
        strip.text = element_text(family = "Times", size=plot.size),
        strip.text.x = element_text(margin = margin(0.05,0,0.05,0, "lines")),
        strip.text.y = element_text(margin = margin(0,0.05,0,0.05, "lines")),
        panel.spacing = unit(0.0, "lines"),
        panel.grid = element_blank()) +
  xlab("Time (HH:MM)") +
  ylab("Clear-sky index (-)")
# p_csi_tr
p1 <- cowplot::plot_grid(p_ghi, p_csi, align = "v", nrow = 2, rel_heights = c(1,1.2)) # 9.5,4,4,4
p2 <- cowplot::plot_grid(p_ghi_tr, p_csi_tr, align = "v", nrow = 2, rel_heights = c(1,1.2)) # 9.5,4,4,4
p <- cowplot::plot_grid(p1,p2,nrow = 1, align = "v", rel_widths = c(1,0.2))
p
ggsave(filename = "~/Desktop/Drive/research/mvBenchmark/paper/images/data_analysis_hawaii.pdf",
       plot = p, device = "pdf", units = "cm", height = 10, width = 17) 

#################################################################################
# Results
#################################################################################

#################################################################################
# Univariate
#################################################################################

# Table with time CRPS scores organized by year and station.
csi_models <- c("Ineichen","McClear")

for(csi_model in csi_models){
  tmp_long <- read.table(file = paste("crps_",csi_model,".txt",sep = ""),
                         header = TRUE, sep = "\t")
  tmp_wide <- reshape(tmp_long,idvar = "station",timevar = "years",direction = "wide", drop = "model")
  colnames(tmp_wide) <- stringr::str_replace_all(colnames(tmp_wide),"crps.","")
  print(xtable::xtable(tmp_wide, digits = 2, caption = paste("crps of",csi_model)), 
        include.rownames=FALSE, caption.placement = "top")
}

# Plot PIT histograms
pit_histograms <- read.table(file = "pit_histograms.txt", header = TRUE, sep = "\t")
tmp_long <- pit_histograms[pit_histograms$years %in% c("2010-2011","2011-2012"),]
years <- stringr::str_replace_all(as.character(tmp_long$years),"20","'") # Abbreviate the years
tmp_long$years <- years
p <- ggplot(data = tmp_long, aes(x=mids,y=counts)) +
  geom_bar(stat = "identity") +
  facet_grid(station+years~model,scales = "free_y") +
  theme_bw() +
  theme(plot.margin = unit(c(0.2,0.2,0,0), "lines"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(family = "Times"),
        strip.text.x = element_text(margin = margin(0.05,0,0.05,0, "lines")),
        strip.text.y = element_text(margin = margin(0,0.05,0,0.05, "lines")),
        panel.spacing = unit(0, "mm"),
        panel.grid = element_blank()) 
p
ggsave(filename = "~/Desktop/Drive/research/mvBenchmark/paper/images/pit_histograms.pdf",
       plot = p, device = "pdf", units = "cm", height = 17, width = 8.5) 

#################################################################################
# Multivariate
#################################################################################

csi_models <- c("Ineichen","McClear")
scores <- c("es","vs")

# Table with time energy and variogram scores organized by year and station.
for(csi_model in csi_models){
  for(score in scores){
    tmp_long <- read.table(file = paste("numerical_scores_",csi_model,".txt",sep = ""),
                           header = TRUE, sep = "\t")
    tmp_long <- tmp_long[tmp_long$members == 40,] # Select case where m=40
    if(score=="es"){
      tmp_wide <- reshape(tmp_long,idvar = "station",timevar = "years",direction = "wide", drop = c("members","vs"))
      colnames(tmp_wide) <- stringr::str_replace_all(colnames(tmp_wide),"es.","")
      print(xtable::xtable(tmp_wide, digits = 2, caption = paste("energy score of ",csi_model)), 
            include.rownames=FALSE, caption.placement = "top")
    } else {
      tmp_wide <- reshape(tmp_long,idvar = "station",timevar = "years",direction = "wide", drop = c("members","es"))
      colnames(tmp_wide) <- stringr::str_replace_all(colnames(tmp_wide),"vs.","")
      print(xtable::xtable(tmp_wide, digits = 2, caption = paste("variogram score of ",csi_model)), 
            include.rownames=FALSE, caption.placement = "top")
    }
  }
}

# Plot multivariate rank histograms
csi_model <- "McClear" # "McClear" or "Ineichen"

tmp_long <- read.table(file = paste("rank_histograms_",csi_model,".txt",sep = ""),
                       header = TRUE, sep = "\t")
tmp_long <- tmp_long[tmp_long$members == 40,] # Select case where m=40
tmp_long <- tmp_long[tmp_long$years %in% c("2011-2012"),] # "2010-2011", "2011-2012"
# years <- stringr::str_replace_all(as.character(tmp_long$years),"20","'") # Abbreviate the years
# tmp_long$years <- years
p <- ggplot(data = tmp_long, aes(x=mids,y=counts)) +
  geom_bar(stat = "identity") +
  facet_grid(prerank~station+years,scales = "free_y") +
  theme_bw() +
  theme(plot.margin = unit(c(0.2,0.2,0,0), "lines"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(family = "Times"),
        strip.text.x = element_text(margin = margin(0.05,0,0.05,0, "lines")),
        strip.text.y = element_text(margin = margin(0,0.05,0,0.05, "lines")),
        panel.spacing = unit(0, "mm"),
        panel.grid = element_blank()) 
p
ggsave(filename = paste("~/Desktop/Drive/research/mvBenchmark/paper/images/rankhistogram_",csi_model,".pdf",sep = ""),
       # plot = p, device = "pdf", units = "cm", height = 17, width = 8.5) 
       plot = p, device = "pdf", units = "cm", height = 6, width = 17) 

# Plot multivariate rank histograms for Hawaii
csi_model <- "McClear" # "McClear" or "Ineichen"
tmp_long <- read.table(file = paste("rank_histograms_",csi_model,"_Hawaii_new.txt",sep = ""),
                       header = TRUE, sep = "\t")
tmp_long <- tmp_long[tmp_long$members == 40,]
tmp_long$months <- factor(tmp_long$months, levels = unique(tmp_long$months)) # Keep original order
p <- ggplot(data = tmp_long, aes(x=mids,y=counts)) +
  geom_bar(stat = "identity") +
  facet_grid(prerank~months,scales = "free_y") +
  theme_bw() +
  theme(plot.margin = unit(c(0.2,0.2,0,0), "lines"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(family = "Times"),
        strip.text.x = element_text(margin = margin(0.05,0,0.05,0, "lines")),
        strip.text.y = element_text(margin = margin(0,0.05,0,0.05, "lines")),
        panel.spacing = unit(0, "mm"),
        panel.grid = element_blank()) 
p
ggsave(filename = paste("~/Desktop/Drive/research/mvBenchmark/paper/images/rankhistogram_",csi_model,"_Hawaii.pdf",sep = ""),
       plot = p, device = "pdf", units = "cm", height = 5, width = 17) 

# Tabulated results:
csi_models <- c("Ineichen","McClear")
scores <- c("es","vs")

# Table with time energy and variogram scores organized by training data.
for(csi_model in csi_models){
  for(score in scores){
    tmp_long <- read.table(file = paste("numerical_scores_",csi_model,"_Hawaii_new.txt",sep = ""),
                           header = TRUE, sep = "\t")
    tmp_long$es <- tmp_long$es #/ 1000
    tmp_long$vs <- tmp_long$vs #/ 1000000
    if(score=="es"){
      tmp_wide <- reshape(tmp_long,idvar = "members",timevar = "months",direction = "wide", drop = "vs")
      colnames(tmp_wide) <- stringr::str_replace_all(colnames(tmp_wide),"es.","")
      print(xtable::xtable(tmp_wide, digits = 2, caption = paste("energy score of ",csi_model)), 
            include.rownames=FALSE, caption.placement = "top")
    } else {
      tmp_wide <- reshape(tmp_long,idvar = "members",timevar = "months",direction = "wide", drop = "es")
      colnames(tmp_wide) <- stringr::str_replace_all(colnames(tmp_wide),"vs.","")
      # print(xtable::xtable(tmp_wide, digits = 2, caption = paste("variogram score of ",csi_model)),
      #       include.rownames=FALSE, caption.placement = "top")
      res <- sanitize.numbers(apply(tmp_wide, 2, format, scientific = TRUE, digits = 4),
                              type = "latex", math.style.exponents = TRUE)
      print(xtable(res, caption = paste("variogram score of ",csi_model), caption.placement = "top"),
            include.rownames=FALSE, sanitize.text.function = function(x){x})
    }
  }
}

