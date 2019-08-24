# for slope

points <- readxl::read_xlsx("D:/Users/earmmor/OneDrive - University of Leeds/OptiTruck/For_Slope.xlsx")
points <- sf::st_as_sf(points, coords = c("Longitude","Latitude"), crs = 4326)

fromPlace <- points[1,]
toPlace <- points[nrow(points),]
route <- otp_plan(otpcon, fromPlace, toPlace)
qtm(sf::st_zm(route)) +
  qtm(points)

coords <- st_coordinates(route)
write.csv(coords,"D:/Users/earmmor/OneDrive - University of Leeds/OptiTruck/For_Slope_result.csv")

