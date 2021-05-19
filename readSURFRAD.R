library(SolarData)

# main_dir <- "~/Desktop/SURFRAD"
# main_dir <- "/Volumes/G-DRIVE_USB-C/Data/SURFRAD/2016/bon"
# setwd(main_dir)
stations <- c("bon","dra","fpk","gwn","psu","sxf","tbl")
years <- c("2016","2017","2018")

for(station in stations){
  lst <- list()
  y <- 1
  for(year in years){
    main_dir <- paste("/Volumes/G-DRIVE_USB-C/Data/SURFRAD/",year,"/",station,sep = "")
    setwd(main_dir)
    files <- dir()
    skip_to_next <- FALSE # To ensure that the script doesn't crash when a file is unavailable
    tryCatch(dat <- SURFRAD.read(files,
                                  use.original.qc = FALSE,
                                  use.qc = TRUE,
                                  test = c("ext","dr","clim"),
                                  directory = main_dir,
                                  agg = 15), 
             error = function(e) { skip_to_next <<- TRUE})
    if(skip_to_next) { next }
    if(skip_to_next==FALSE){
      lst[[y]] <- dat
    }
    y <- y + 1
  }
  dat <- do.call(rbind,lst)
  filename <- paste("/Volumes/G-DRIVE_USB-C/Data/SURFRAD/aggregated/",
                    station,"_",years[1],"-",years[length(years)],".txt",
                    sep = "")
  write.table(dat, file = filename,
              row.names = FALSE, sep = "\t")
}
