library(maps)
library(maptools)
gpclibPermit()
setwd('/Users/Runze/Documents/Github/twitter_sentiment')

states = map('state', fill=TRUE, col='transparent', plot = F)
ids = sapply(strsplit(states$names, ':'), function(x) x[1])
states_sp = map2SpatialPolygons(states, IDs = ids,
                                proj4string = CRS('+proj=longlat +datum=WGS84'))

save(states_sp, file = 'data/states_sp.RData')
