# Get tetec routes
library(sf)
library(tmap)
tmap_mode("view")

tentec_ports    <- st_read("data/TENtec Points/Ports_SHP/TENtec_Ports_Final.shp")
tentec_airports <- st_read("data/TENtec Points/Airports_SHP/TENtec_Airports.shp")
tentec_rail     <- st_read("data/TENtec Points/Rail_SHP/TEN_tec_Rail.shp")
tentec_rail$Notes <- NA
tentec_rail$Type <- "Rail"
tentec_rail <- tentec_rail[,names(tentec_ports)]

tentec <- rbind(tentec_ports, tentec_airports)
tentec <- rbind(tentec, tentec_rail)
rm(tentec_ports, tentec_airports, tentec_rail)

# Clean
lapply(as.data.frame(tentec)[,2:6], unique)
tentec[,2:6] <- lapply(as.data.frame(tentec)[,2:6], as.character)
tentec$Type[tentec$Type == "Inalnd Waterways"] <- "Inland Waterways"
tentec$Network[tentec$Network == "Comprehensive Network"] <- "Comprehensive"
tentec$Corridors <- as.character(tentec$Corridors)
tentec$Corridors[tentec$Corridors == "Rhine - Alpine, Atlantic, North Sea - Mediterranean, Rhrine - Danube"] <- "Rhine - Alpine, Atlantic, North Sea - Mediterranean, Rhine - Danube" 
tentec$Corridors[tentec$Corridors == "North Sea -Baltic"] <- "North Sea - Baltic"
tentec$Corridors[tentec$Corridors == "North Sea - Baltic, Rhine - Alpine,  North Sea - Mediterranean"] <- "North Sea - Baltic, Rhine - Alpine, North Sea - Mediterranean" 

write_sf(tentec,"data/TENtec Points/TENtec_points_clean.gpkg")
qtm(tentec, dots.col = "Type")

# Make into OD Dataset
tentec <- tentec[,c("Descript")]
toPlace   = tentec[rep(1:nrow(tentec), times = nrow(tentec)),]
fromPlace = tentec[rep(1:nrow(tentec), each  = nrow(tentec)),]

routes = otp_plan_batch(otpcon  = otpcon, 
                      fromPlace = fromPlace, 
                      toPlace   = toPlace, mode = "CAR", ncores = 1)
saveRDS(routes,"data/tentec-routes-290311.Rds")

source("R/OTP/line_curvature.R")

route_profiles <- profile_road(routes)
saveRDS(route_profiles,"data/tentec-routes-profiles-190311.Rds")

res_incline <- road_incline(routes[1,])
res_incline <- res_incline[[1]]
res_incline$cumlength <- cumsum(res_incline$length)
plot(res_incline$cumlength / 1000 , res_incline$incline * 100, type = "l", xlab = "Route Length (km)", ylab = "Incline %")
