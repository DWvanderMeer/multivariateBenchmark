library(SolarData)

main_dir <- "~/Desktop/SURFRAD"
main_dir <- "/Volumes/G-DRIVE_USB-C/Data/SURFRAD"
setwd(main_dir)
stations <- c("bon","dra","fpk","gwn","psu","sxf","tbl")
years <- "2018"#c("2016","2017") # 2016 is leap year

for(year in years){
  newdir <- paste(main_dir,"/",year,sep = "")
  dir.create(newdir,showWarnings = FALSE)
  for(station in stations){
    newdir <- paste(main_dir,"/",year,"/",station,sep = "")
    dir.create(newdir,showWarnings = FALSE) # should test for error
    for(day in 1:365){
      skip_to_next <- FALSE # To ensure that the script doesn't crash when a file is unavailable
      tryCatch(SURFRAD.get(station = station,
                           year = year,
                           day.of.year = day,
                           directory = newdir), 
               error = function(e) { skip_to_next <<- TRUE})
      if(skip_to_next) { next }
    }
  }
}