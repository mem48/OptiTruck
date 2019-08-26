# merge profiles

folder = "C:/Users/Malcolm/OneDrive - University of Leeds/OptiTruck/Data/Results"
files = list.files(folder, pattern = "bch", full.names = TRUE)

res <- list()
for(i in 1:length(files)){
  tmp <- readRDS(files[i])
  res[[i]] <- tmp
}
res <- dplyr::bind_rows(res)
