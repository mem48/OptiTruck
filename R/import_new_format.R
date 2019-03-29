file = "D:/Users/earmmor/OneDrive - University of Leeds/OptiTruck/Reb_20180918_Exp15_Run01_3177_enivornment_sensors_relative.txt"
foo2 = readLines(file)
foo3 = strsplit(foo2,",")
foo3[[1]]  = NULL
foo4 <- data.frame(matrix(unlist(foo3), nrow=length(foo3), byrow=T),stringsAsFactors=FALSE)
names(foo4) <- c("log_timestamp","log_applicationid","log_stationid","x","y")
foo4$x <- as.numeric(gsub("[^0-9.]","",foo4$x))
foo4$y <- as.numeric(gsub("[^0-9.]","",foo4$y))
library(mapview)
library(sf)

pnts <- sf::st_as_sf(foo4, coords = c("x","y"), crs = 4326)
pnts$log_timestamp <- floor(as.numeric(pnts$log_timestamp) / 1000)
pnts$log_timestamp <- as.POSIXct(pnts$log_timestamp, origin="1970-01-01")
pnts$log_applicationid <- as.numeric(pnts$log_applicationid)
pnts$log_stationid <- as.numeric(pnts$log_stationid)
summary(pnts)
#plot(pnts)
mapview(pnts, zcol = "log_timestamp")
