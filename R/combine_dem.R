# Merge TIF to make Single DEM
library(raster)
# Settings


dir = "D:/Users/earmmor/OneDrive - University of Leeds/OptiTruck/European DEM/DEM"
rasterOptions(tmpdir = "F:/RasterTmp")
rasterOptions(maxmemory = 6e+11) # about 120 GB of RAM


# list files
files = list.files(dir, recursive = T, full.names = T)

#get file type
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

files = files[substrRight(files, 3) == "TIF"]

# read in the raster files
raster.list = list()
#for(i in 1:length(files)){
for(i in 1:2){
  message(paste0("Doing ",i))
  tmp = raster(files[i])
  raster.list[[i]] = tmp
  rm(tmp)
}

raster.list$filename <- 'merged_raster.tif'
raster.list$overwrite <- TRUE
raster.merge <- do.call(raster::merge, raster.list)
