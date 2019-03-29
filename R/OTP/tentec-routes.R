# Get tetec routes
library(sf)
library(tmap)
tmap_mode("view")

tentec_ports <- st_read("data/TENtec_Mapping_11.03.2019/TENtec_Ports_Mk2/TENtecPorts_Mk2.shp")
tentec_airports <- st_read("data/TENtec_Mapping_11.03.2019/TENtec_Airports/TEntec_Airports.shp")
tentec_rail <- st_read("data/TENtec_Mapping_11.03.2019/TEN_tec_Rail/TEN_tec_Rail.shp")

#tentec <- st_read("data/TENtec_Ports_Mk2/TENtecPorts_Mk2.shp")
tentec <- rbind(tentec_ports, tentec_airports)
tentec <- rbind(tentec, tentec_rail)
rm(tentec_ports, tentec_airports, tentec_rail)
#tentec <- tentec[16:49,]
#tentec <- tentec[tentec$id  != 15,]
#tentec2 <- tentec[10:15,]

lines <- stplanr::points2flow(tentec)
lines <- lines[lines$O != lines$D,]
lines$value <- 0
lines <- stplanr::onewayid(lines, "value")
lines <- lines[,c("O","D","geometry")]

lines.coods <- st_coordinates(lines)
from <- lines.coods[seq(1, nrow(lines.coods), 2),c(2,1)]
to <- lines.coods[seq(2, nrow(lines.coods), 2),c(2,1)]

#route = otp_plan(otpcon = otpcon, fromPlace = from[1,] , toPlace = to[1,], mode = "CAR")
# tentec_key = tentec[tentec$Descript %in% c("Halkali","Vlore","Brindisi", "Orbassano (TO)"),]
# lines_key <- stplanr::points2flow(tentec_key)
# lines_key <- lines_key[lines_key$O != lines_key$D,]
# lines_key$value <- 0
# lines_key <- stplanr::onewayid(lines_key, "value")
# lines_key <- lines_key[,c("O","D","geometry")]
# 
# lines.coods_key <- st_coordinates(lines_key)
# from_key <- lines.coods_key[seq(1, nrow(lines.coods_key), 2),c(2,1)]
# to_key <- lines.coods_key[seq(2, nrow(lines.coods_key), 2),c(2,1)]
# 
# routes_key = otp_plan_batch(otpcon = otpcon, 
#                         fromPlace = from_key, 
#                         toPlace = to_key, mode = "CAR", ncores = 4)
# saveRDS(routes_key,"data/tentec-internationa-route.Rds")

routes = otp_plan_batch(otpcon = otpcon, 
                      fromPlace = from, 
                      toPlace = to, mode = "CAR", ncores = 2)
saveRDS(routes,"data/tentec-routes-190311.Rds")

source("R/OTP/line_curvature.R")

route_profiles <- profile_road(routes)
saveRDS(route_profiles,"data/tentec-routes-profiles-190311.Rds")

res_incline <- road_incline(routes[1,])
res_incline <- res_incline[[1]]
res_incline$cumlength <- cumsum(res_incline$length)
plot(res_incline$cumlength / 1000 , res_incline$incline * 100, type = "l", xlab = "Route Length (km)", ylab = "Incline %")
