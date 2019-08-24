#Load the three packages below 
library(sp)
library(raster)
library(rgdal)

rasterOptions(maxmemory = 1e+11)
rasterOptions(tmpdir = "F:/RasterTmp/")

##Load Raster

r <- raster("D:/otp_optitruck/graphs/current/Europe_DEM_WGS84.tif")
fun <- function(x) {
  ifelse(x == 32767, 0, x)
}
rc <- calc(r, fun) #Performs a raster calculation 

##WriteRaster to a .tif

writeRaster(rc, "D:/otp_optitruck/graphs/current/Europe_DEM_WGS84_2.tif", format="GTiff")
