# Must be in order
cluster_times = function(x, tolerance = 10, units = "mins"){
  lng = length(x)
  org = x
  #nxt = x[c(seq(2,lng),lng)]
  pre = x[c(1,seq(1,lng-1))]
  dif = difftime(org, pre, units =  units)
  gap = dif > tolerance
  grp = list()
  grp[[1]] = 1
  
  for(i in seq(2,lng) ){
    if(gap[i]){
      grp[[i]] = grp[[i-1]] + 1
    }else{
      grp[[i]] = grp[[i-1]]
    }
  }
  grp = unlist(grp)
  return(grp)
}