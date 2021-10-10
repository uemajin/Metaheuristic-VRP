rm(list =ls())
dir <- "C:/Users/j.uema/Documents/Metaheuristicas/Datasets/Christofides 1969/CSV"

list.files(dir)

#lapply(list.files(dir), function(x) {
  
  x <- "E-n013-k04.csv"
  dir <- "C:/Users/j.uema/Documents/Metaheuristicas/Datasets/Christofides 1969/CSV"
  file <- paste(dir,x, sep='/')
  print(file)
  data<-read.csv2(file, sep=',')
  
  vehicle_capacity <- data$fleet__vehicle_profile__capacity[1]
  
  data[1] <- NULL 
  data$info__name <- NULL
  data$network__euclidean <- NULL
  data$network__decimals <- NULL
  data$fleet__vehicle_profile__type <- NULL
  data$fleet__vehicle_profile__departure_node <- NULL
  data$fleet__vehicle_profile__arrival_node <- NULL
  data$fleet__vehicle_profile__capacity <- NULL
  data$requests__request__id <- NULL
  data$requests__request__node <- NULL
  data$network__nodes__node__type <- NULL
  data$network__links__symmetric <- NULL
  
  data <- head(data, -1)
  
  colnames(data) <- c('P','X','Y','W')
  
  # Create Vehicles
  
  vec_start <- max(data$P) + 1
  vec_end <- max(data$P) + 10
  vec_start_x <- data$X[1]
  vec_start_y <- data$Y[1]
  
  data_append = data.frame(P = seq(vec_start, vec_end, 1),
                           X = rep(vec_start_x, vec_end+1 - vec_start),
                           Y = rep(vec_start_y, vec_end+1 - vec_start),
                           W = rep(vehicle_capacity, vec_end+1 - vec_start)
  )
  
  seq(vec_start, vec_end, 1)
  
  data_f = rbind(data,data_append)
  
  write.table(data_f, file, sep=';')
  
#}
#)


