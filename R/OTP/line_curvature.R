# Function that calualtes the radius of curvature of a line

radius_curvature  <- function(x, measure = "cheap"){
  # Helper Funcion
  
  three_points <- function(i,j,k, measure){
    
    #docircle <- TRUE
    # Vertical Lines cause problems
    if(any(duplicated(c(i[1],j[1],k[1])))){
      if(i[1] == j[1] & i[1] == k[1]){
        # Straight Vertical Line
        return(Inf)
      }else if(j[1] == i[1] | j[1] == k[1]){
        # Gradient method does not work so make tiny ajustment to non vertical
        j[1] <- j[1] + 0.0000001
      }else{
        i[1] <- i[1] + 0.0000001
      }
    }
    
    # horisonal Lines cause problems
    if(any(duplicated(c(i[2],j[2],k[2])))){
      if(i[2] == j[2] & i[2] == k[2]){
        # Straight Vertical Line
        return(Inf)
      }else if(j[2] == i[2] | j[2] == k[2]){
        # Gradient method does not work so make tiny ajustment to non vertical
        j[2] <- j[2] + 0.0000001
      }else{
        i[2] <- i[2] + 0.0000001
      }
    }
    
    # straight (non vertical) line check
    ms <- (i[2] - k[2])/(i[1]-k[1])
    cs <- i[2] - ms * i[1]
    if(j[2] == ms * j[1] + cs){
      return(Inf)
    }
    
    
    # http://paulbourke.net/geometry/circlesphere/
    ma <- (j[2] - i[2])/(j[1] - i[1])
    mb <- (k[2] - j[2])/(k[1] - j[1])
    cx <- (ma * mb * (i[2] - k[2]) + mb * (i[1] + j[1]) - ma * (j[1] + k[1]))/( 2 * (mb - ma))
    cy <- (-1 / ma) * (cx - 0.5 * (i[1] + j[1])) + 0.5 * (i[2] + j[2])
    
    centre <- c(cx, cy)
    if(cx > 180 | cx < -180 | cy > 90 | cy < -90){
      # check for off the world answers, not this doe not work close to the poles or latitute = 180
      return(Inf)
    }else{
      #radius <- geosphere::distHaversine(p1 = j, p2 = centre)
      j <- matrix(j, ncol = 2)
      centre <- matrix(centre, ncol = 2)
      colnames(j) <- c("X","Y")
      colnames(centre) <- c("X","Y")
      radius <- geodist::geodist(j, centre, paired = T, measure = measure)
      return(radius)
    }
    
    
        
    
  }
  
  rad_curve <- function(y, measure){
    y <- matrix(y, ncol = 4, byrow = F)
    curve <- sapply(seq(2,nrow(y)-1),
                    function(z){
                      three_points(i = y[z-1,1:2], j = y[z,1:2], k = y[z+1,1:2], measure)
                    })
    return(curve)
  }

  coords <- st_coordinates(x)
  coords_list <- split(coords, f = coords[,4])
  
  radius <- pbapply::pblapply(coords_list, rad_curve, measure = measure)


  return(radius)
}



road_incline <- function(x, measure = "cheap"){
  coords <- st_coordinates(x)
  coords_list <- split(coords, f = coords[,4])
  
  int_func <- function(z, measure){
    z <- matrix(z, ncol = 4, byrow = F)
    rows <- nrow(z)
    from <- z[seq(1,rows-1), 1:2]
    colnames(from) <- c("x","y")
    to   <- z[seq(2,rows),   1:2]
    colnames(to) <- c("x","y")
    dists <- geodist::geodist(from, to, paired = T, measure = measure)
    deltaz <- sapply(seq(2,rows),function(y){z[y,3] - z[y-1,3]})
    incline <- deltaz/dists
    incline[dists == 0] <- 0
    res <- data.frame(length = dists, incline = incline, line = z[seq(2,rows), 4])
    return(res)
  }

  result <- pbapply::pblapply(coords_list, int_func, measure = measure)
  result <- dplyr::bind_rows(result)
  return(result)
}



profile_road <- function(x, elevation_breaks = c(-0.03, -0.01, 0.01, 0.03) , curve_breaks = c(100, 500, 1000, 3000)){
  message("Profile Curvature")
  res_curve <- radius_curvature(x)
  message("Profile Incline")
  res_incline <- road_incline(x)
  message("Summarise Results")
  res_final <- list()
  for(i in seq(1,length(res_curve))){
    res_all <- res_incline[res_incline$line == i,]
    res_all$curve = c(res_curve[[i]],NA)
    r_curve <- list()
    l_cb <- length(curve_breaks)
    for(j in seq(1, l_cb + 1)){
      if(j == 1){
        r_curve[[j]] <- sum(res_all$length[res_all$curve < curve_breaks[1]], na.rm = TRUE)
      }else if(j == (l_cb + 1)){
        r_curve[[j]] <- sum(res_all$length[res_all$curve > curve_breaks[l_cb]], na.rm = TRUE)
      }else{
        r_curve[[j]] <- sum(res_all$length[res_all$curve < curve_breaks[j] & res_all$curve >= curve_breaks[j-1]], na.rm = TRUE)
      }
    }
    
    r_inc <- list()
    l_eb <- length(elevation_breaks)
    for(j in seq(1, l_eb + 1)){
      if(j == 1){
        r_inc[[j]] <- sum(res_all$length[res_all$incline < elevation_breaks[1]], na.rm = TRUE)
      }else if(j == (l_eb + 1)){
        r_inc[[j]] <- sum(res_all$length[res_all$incline > elevation_breaks[l_eb]], na.rm = TRUE)
      }else{
        r_inc[[j]] <- sum(res_all$length[res_all$incline < elevation_breaks[j] & res_all$incline >= elevation_breaks[j-1]], na.rm = TRUE)
      }
    }
    
    
    
    Length <- c(unlist(r_inc), unlist(r_curve))
    
    #res_all$cum_length = cumsum(res_all$length)
    #plot(res_all$cum_length, res_all$incline, type = "l")
    #plot(res_all$cum_length, res_all$curve, type = "l", ylim = c(0,800))
    
    # summary_profile <- data.frame(Variable = c("Flat: -1% to +1%",
    #                                             "Uphill 1: +1% to +3%",
    #                                             "Uphill 2: > +3%",
    #                                             "Downhill 1: -1% to -3%",
    #                                             "Downhill 2: < -3%",
    #                                             "Straight: > 3000 m",
    #                                             "Gentle Curve: > 1000 m",
    #                                             "Moderate Curve: > 500 m",
    #                                             "Tight Curve: > 100 m",
    #                                             "Extreme Curve <= 100 m"
    #                                             
    # ),
    # Length = c(sum(res_all$length[res_all$incline >= -0.01 & res_all$incline <= 0.01]),
    #            sum(res_all$length[res_all$incline > 0.01 & res_all$incline <= 0.03]),
    #            sum(res_all$length[res_all$incline > 0.03]),
    #            sum(res_all$length[res_all$incline < -0.01 & res_all$incline >= -0.03]),
    #            sum(res_all$length[res_all$incline < -0.03]),
    #            
    #            sum(res_all$length[res_all$curve > 3000], na.rm = T),
    #            sum(res_all$length[res_all$curve <= 3000 & res_all$curve > 1000], na.rm = T),
    #            sum(res_all$length[res_all$curve <= 1000 & res_all$curve > 500], na.rm = T),
    #            sum(res_all$length[res_all$curve <= 500 & res_all$curve > 100], na.rm = T),
    #            sum(res_all$length[res_all$curve <= 100], na.rm = T)
    # )
    #)
    #summary_profile$Percent = summary_profile$Length / sum(res_all$length) * 100
    #foo <- t(summary_profile)
    
    res_final[[i]] <- Length
  }
  
  
  res_final <- data.frame(matrix(unlist(res_final), nrow=length(res_final), byrow=T))
  #names(res_final) <- c()
  nms_cur <- list()
  for(j in seq(1, l_cb + 1)){
    if(j == 1){
      nms_cur[[j]] <- paste0("Curve < ",curve_breaks[j])
    }else if(j == (l_cb + 1)){
      nms_cur[[j]] <- paste0("Curve > ",curve_breaks[l_cb])
    }else{
      nms_cur[[j]] <- paste0("Curve ",curve_breaks[j-1], " - ",curve_breaks[j])
    }
  }
  
  nms_inc <- list()
  for(j in seq(1, l_eb + 1)){
    if(j == 1){
      nms_inc[[j]] <- paste0("Slope < ",elevation_breaks[j])
    }else if(j == (l_eb + 1)){
      nms_inc[[j]] <- paste0("Slope > ",elevation_breaks[l_eb])
    }else{
      nms_inc[[j]] <- paste0("Slope ",elevation_breaks[j-1], " - ",elevation_breaks[j])
    }
  }
  
  names(res_final) <- c(unlist(nms_inc),unlist(nms_cur))
  # names(res_final) <- c("Flat: -1% to +1%","Uphill 1: +1% to +3%","Uphill 2: > +3%",
  #                    "Downhill 1: -1% to -3%","Downhill 2: < -3%","Straight: > 3000 m","Gentle Curve: > 1000 m",
  #                    "Moderate Curve: > 500 m","Tight Curve: > 100 m","Extreme Curve <= 100 m")
  return(res_final)
}

# res_curve <- radius_curvature(route)
# res_incline <- road_incline(route)
# foo = profile_road(routes[1:3,])
# qtm(st_zm(routes))
# bar = foo[[1]]
# # 
# # sum(summary_profile$Percent)
# # 
# points = st_cast(route,"POINT")
# points = points[,1]
# #points = points[2:(nrow(points)-1),]
# points$curve = c(NA,res_curve[[1]],NA)
# points$incline = c(res_incline[[1]]$incline,NA)
# # 
# tm_shape(points) +
# tm_dots(col = "curve", breaks = c(0,100,500, 1000, 3000, 99999999), style = "fixed")
# 
# tm_shape(points) +
#   tm_dots(col = "incline", breaks = c(-1,-0.03,-0.01,0.01,0.03,1), style = "fixed")
# # 
# # y = 401
# # i = coords[y-1,]
# # j = coords[y,]
# # k = coords[y+1,]
# # 
# # 
# # 
# # mat2 = matrix(c(i,j,k), ncol = 2, byrow = T)
# # mat <- matrix(c(i,j,k,centre), ncol = 2, byrow = T)
# # mat
# # plot(mat, col = c("red","orange","pink","blue"))
