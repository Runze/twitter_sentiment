library(streamR)
library(sp)
library(maps)
library(mapproj)
library(ggplot2)
library(grid)
library(ROAuth)
library(dplyr)

load(file = 'data/my_oauth.RData')
load(file = 'data/pos_words.RData')
load(file = 'data/neg_words.RData')
load(file = 'data/states_sp.RData')
load(file = 'data/tz.RData')
states_map = map_data('state')
names(states_map)[5] = 'state'

#function to get states from longitude and latitude
get_states = function(lon, lat) {
  coords = data.frame(cbind(lon, lat))
  points_sp = SpatialPoints(coords)
  proj4string(points_sp) = proj4string(states_sp)
  i = over(points_sp, states_sp)
  names = sapply(states_sp@polygons, function(x) x@ID)
  return (names[i])
}

#function to calculate sentiment score
get_sentiment = function(txt) {
  words = strsplit(txt, ' +')
  words = unlist(words)
  pos_matches = match(words, pos)
  neg_matches = match(words, neg)
  
  #count all the mapped positive and negative words and subtract the latter from the former
  score = sum(!is.na(pos_matches)) - sum(!is.na(neg_matches))
  return(score)
}

#function to get live tweets
get_tweets = function() {
  #gather all geo-coded u.s. tweets for 5 seconds
  tweets = filterStream(file.name = '', language = 'en', locations=c(-124, 23, -67, 50), timeout = 5, oauth = my_oauth)
  
  #check if at least 30 tweets were received (sometimes a lower number errors out)
  if (length(tweets) >= 30) {
    tweets = parseTweets(tweets)
    tweets = subset(tweets, country == 'United States' & !is.na(place_lon) & !is.na(place_lat))
    
    #get states
    #first try place_lon and place_lat
    s1 = get_states(tweets$place_lon, tweets$place_lat)
    
    #then try lon and lat
    #replace na lon and lat to 0 so that the program won't break
    #0 (lon, lat) pair will return na in the end
    lon = ifelse(is.na(tweets$lon), 0, tweets$lon)
    lat = ifelse(is.na(tweets$lat), 0, tweets$lat)
    s2 = get_states(lon, lat)
    
    #combine
    tweets$state = ifelse(is.na(s1), s2, s1)
    tweets = subset(tweets, !is.na(state))
    
    #clean tweets
    text = gsub('[^[:graph:]]', ' ', as.character(tweets$text))
    text = gsub('^ +', '', text)
    text = gsub(' +$', '', text)
    text = gsub(' +', ' ', text)
    tweets$text = text
    
    #estimate sentiment of the tweets
    text = gsub('[^[:alpha:]]', ' ', text)
    text = tolower(text)
    
    tweets$sentiment = sapply(text, get_sentiment)
    
    #clean up
    #clean time stamps
    tweets$created_at = as.character(tweets$created_at)
    date1 = substr(tweets$created_at, 5, 10)
    date2 = substr(tweets$created_at, nchar(tweets$created_at)-3, nchar(tweets$created_at))
    time = substr(tweets$created_at, 12, 19)
    tweets$ts = paste(date1, date2, time)
    
    #create time stamps showing time ranges
    tweets$ts_r = paste(min(time, na.rm = T), max(time, na.rm = T), sep = '-')
    tweets = subset(tweets, select = c(text, state, sentiment, ts, ts_r))
    
    #merge with time zone data
    tweets = left_join(tweets, tz, by = 'state')
    
    return(tweets) 
  }
  else {
    return(data.frame(text = character(), state = character(), sentiment = numeric(), ts = character(), ts_r = character()))
  }
}

#function to aggregate sentiment scores at the state level
agg_sentiment = function(x) {
  #taking into account the magnitude of the positivity and negativity
  pos_sum = sum(x[which(x > 0)])
  abs_sum = sum(abs(x))
  return(pos_sum / abs_sum)
}

#function to plot the sentiment on a heat map
plot_map = function(dat, title) {
  #calculate % of positive sentiment per state
  sentiment_prop = dat %>%
    group_by(state) %>%
    summarise(sentiment_prc = agg_sentiment(sentiment))
  
  #merge with map data
  sent_map = left_join(states_map, sentiment_prop, by = 'state')
  sent_map = arrange(sent_map, order)

  #replace na with .5 (i.e., neutral states)
  sent_map$sentiment_prc[is.na(sent_map$sentiment_prc)] = .5
  
  #plot sentiment on map
  map_plot = 
    ggplot(sent_map, aes(x = long, y = lat, group = group, fill = sentiment_prc)) +
    geom_polygon() + coord_map('polyconic') +
    scale_fill_gradient2(low = '#9ecae1', mid = '#4292c6', high = '#084594', midpoint = .5, 
                         breaks = seq(0, 1, .25), 
                         labels = c('Negative', '', 'Neutral', '', 'Positive')) +
    labs(fill = '') + 
    ggtitle(title) +
    theme(legend.position = 'bottom', plot.margin = unit(c(0, 0, 0, 0), 'mm'),
          panel.grid = element_blank(), panel.border = element_blank(),
          axis.title = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(),
          panel.background = element_blank(), plot.background = element_blank(),
          legend.background = element_blank())
  
  return(map_plot)
}

#function to create simple trend plot
plot_trend_simple = function(dat_all, title) {
  #create sequence number to indicate time
  dat_all$seq = match(dat_all$ts_r, sort(unique(dat_all$ts_r)))
  
  #calculate % of positive sentiment per time zone and time
  sentiment_prop = dat_all %>%
    group_by(tz, seq) %>%
    summarise(sentiment_prc = agg_sentiment(sentiment))
  
  trend_plot = ggplot(sentiment_prop, aes(x = seq, y = sentiment_prc, col = tz)) + 
    geom_point() + geom_line() +
    guides(col = guide_legend(title = NULL)) +
    scale_y_continuous(breaks = seq(0, 1, .25),
                       labels = c('Negative', '', 'Neutral', '', 'Positive')) +
    theme(legend.position = 'bottom', axis.text.x = element_blank(),
          panel.grid = element_blank(), panel.border = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_blank(), plot.background = element_blank(),
          legend.background = element_blank()) +
    xlab('Time Interval') + ylab('Sentiment') +
    ggtitle(title)
  
  return(trend_plot)
}

#function to create loess-smoothed trend plot
plot_trend_loess = function(dat_all, title) {
  #create sequence number to indicate time
  dat_all$seq = match(dat_all$ts_r, sort(unique(dat_all$ts_r)))
  dat_all = arrange(dat_all, seq)
  
  #calculate % of positive sentiment per time zone and time
  sentiment_prop = dat_all %>%
    group_by(tz, seq) %>%
    summarise(sentiment_prc = agg_sentiment(sentiment))
  
  #plot sentiment trend
  trend_plot = ggplot(sentiment_prop, aes(x = seq, y = sentiment_prc, col = tz)) + 
    geom_point() + stat_smooth(method = 'loess', se = F) +
    guides(col = guide_legend(title = NULL)) +
    scale_y_continuous(breaks = seq(0, 1, .25),
                       labels = c('Negative', '', 'Neutral', '', 'Positive')) +
    theme(legend.position = 'bottom', axis.text.x = element_blank(),
          panel.grid = element_blank(), panel.border = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_blank(), plot.background = element_blank(),
          legend.background = element_blank()) +
    xlab('Time Interval') + ylab('Sentiment') +
    ggtitle(title)
  
  return(trend_plot)
}