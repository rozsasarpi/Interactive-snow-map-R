plot_point_hist = function(coord, D){
  
  idx = coord$idx

  # select the corresponding dataset and plot it
  dataset = D$value[idx,]
  graphics::hist(dataset)
  
}

