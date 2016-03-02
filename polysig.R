# polysig: pollutant signature polyogons
# e.g. lapply(1:2, FUN = function(i) polysig(caravan[i, ]))
polysig <- function(x, pm10 = "pm10", pm2.5 = "pm2.5", so2 = "so2", no2="no2"){
  # check that x is a data frame
  path = rbind(c(orig, orig), 
               c(x["pm10"], orig), 
               c(x["pm10"],x["so2"]), 
               c(x["pm2.5"],x["no2"]), 
               c(orig, orig))
  p = data.frame(x=unlist(path[,1]), y = unlist(path[,2]))
  #ggplot(p, aes(x=x, y=y)) + geom_polygon() + coord_fixed(ratio = 4/1)
  plot(p)
  polygon(p)
}

polysig.summary <- function(x, pm10 = "pm10", pm2.5 = "pm2.5", so2 = "so2", no2="no2", group = "cluster"){
  for (i in 1:nrow(x)){
    
  }
}