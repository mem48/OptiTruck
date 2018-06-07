# FInd coordinates

known.base = segments.sf
unknown.base = incidents.active.sf[!duplicated(incidents.active.sf$geometry),]

known.base = st_transform(known.base, 27700)
known.base = st_buffer(known.base, 1000)
known.base = st_union(known.base)
known.base = st_simplify(known.base, 10)

foo = find_crs(known.base = known.base, unknown.base = unknown.base)


find_crs = function(known.base, unknown.base, silent = TRUE){
  codes = rgdal::make_EPSG()
  codes = codes$code
  codes = as.integer(codes)
  codes = codes[order(codes)]
  codes = codes[!is.na(codes)]
  
  # Internal Function
  find_crs2 = function(crs, known.base, unknown.base, silent = TRUE){
    if(!silent){
      message(paste0(Sys.time()," Trying CRS = ",crs))
    }
    
    known = st_transform(known.base, crs)
    unknown = unknown.base
    unknown = suppressWarnings(st_set_crs(unknown, crs))
    inster = as.numeric(suppressWarnings(st_intersects(known, unknown)))
    if(!any(is.na(inster))){
      message(paste0("Try ",crs))
      return(crs)
    }else{
      return(NA)
    }
    
  }
  
  # loop over
  res = lapply(codes, find_crs2, known.base = known.base, unknown.base = unknown.base, silent = silent)
  res = res[!is.na(res)]
  return(res)
}


3819