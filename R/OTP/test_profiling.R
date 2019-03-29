# test profiles
library(sf)
library(tmap)
tmap_mode("view")

routes <- readRDS("data/tentec-routes.Rds")
source("R/OTP/line_curvature.R")

# profvis::profvis(res_curve <- radius_curvature(routes[1,]))
# profvis::profvis(res_curve <- radius_curvature2(routes[1,]))
# profvis::profvis(res_incline <- road_incline(routes[1,]))
# profvis::profvis(res_incline <- road_incline2(routes[1,]))

t1 <- Sys.time()
res_curve <- radius_curvature(routes[1:10,])
t2 <- Sys.time()
res_incline <- road_incline(routes[1:10,])
t3 <- Sys.time()
difftime(t2,t1)
difftime(t3,t2)
qtm(st_zm(routes[1:10,]))

foo = profile_road(routes[1:10,])
l1 = st_length(routes[1:10,])
l2 = rowSums(foo)
l1/ l2
