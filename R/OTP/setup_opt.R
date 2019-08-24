# Get Elevations across Europe
library(tmap)
tmap_mode("view")

# Get scripts
devtools::install_github("ITSleeds/opentripplanner")
library(opentripplanner)

# Set Up the OTP
path_otp = "D:/otp_optitruck/otp.jar"
dir = "F:/otp-optitruck"
memory = 100

log1 = otp_build_graph(otp = path_otp, dir = dir,memory = memory, router = "current") # only build once
# load opt, this takes a very long time ~ 1 hour
log2 = otp_setup(otp = path_otp,
                 dir = dir,
                 memory = memory,
                 router = "current",
                 port = 8801,
                 securePort = 8802,
                 analyst = FALSE,
                 wait = TRUE)

otpcon <- otp_connect(hostname =  "localhost", router = "current", port = 8801)
# route = otp_plan(otpcon = otpcon, fromPlace = c(41.02809,28.92460) , toPlace = c(40.48776,19.47696), mode = "CAR")
# qtm(sf::st_zm(route))
# plot(route$geometry)
# 
# foo = as.data.frame(st_coordinates(r1))
# 
# routes = otp_plan_batch(otpcon = otpcon, 
#                         fromPlace = matrix(c(50.64590, -1.17502,50.72266, -1.15339), ncol = 2, byrow = T ), 
#                         toPlace = matrix(c(50.72266, -1.15339,50.64590, -1.17502), ncol = 2, byrow = T ), mode = "CAR")
