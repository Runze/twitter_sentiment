library(shiny)
library(shinyIncubator)
library(streamR)
library(sp)
library(maps)
library(ggplot2)
library(ROAuth)
library(plyr)

load(file = 'data/my_oauth.RData')
load(file = 'data/pos_words.RData')
load(file = 'data/neg_words.RData')
load(file = 'data/states_sp.RData')
load(file = 'data/tz.RData')

#create name for temp file created on the fly
dat_all_name = paste0('data/dat_all_', format(Sys.time(), '%Y-%m-%d_%H-%M-%S'), '.RData')

#function to get live tweets
get_tweets = function() {
  #gather all geo-coded u.s. tweets for 5 seconds
  tweets = filterStream(file.name = '', language = 'en', locations=c(-124, 23, -67, 50), timeout = 5, oauth = my_oauth)
  
  #check if at least 30 tweets were received (sometimes a lower number errors out)
  if (length(tweets) >= 30) {
    tweets = parseTweets(tweets)
    tweets = subset(tweets, country == 'United States' & !is.na(place_lon) & !is.na(place_lat))
    
    #function to get states from longitude and latitude
    get_states = function(lon, lat) {
      coords = data.frame(cbind(lon, lat))
      points_sp = SpatialPoints(coords)
      proj4string(points_sp) = proj4string(states_sp)
      i = over(points_sp, states_sp)
      names = sapply(states_sp@polygons, function(x) x@ID)
      return (names[i])
    }
    
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
    tweets = merge(tweets, tz, by = 'state', all.x = T)
    
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

#get a list of unique states to be used for charting below
states_map = map_data('state')
unique_states = data.frame(state = unique(states_map$region))

#function to plot the sentiment on a heat map
plot_map = function(dat, title) {
  #calculate % of positive sentiment per state
  sentiment_prop = aggregate(sentiment ~ state, dat, agg_sentiment)
  sentiment_prop = merge(sentiment_prop, unique_states, by = 'state', all = T)
  
  #replace na with .5 (i.e., neutral states)
  sentiment_prop$sentiment[is.na(sentiment_prop$sentiment)] = .5
  
  #plot sentiment on map
  map_plot = ggplot(sentiment_prop, aes(map_id = state)) +
    geom_map(aes(fill = sentiment), map = states_map) +
    expand_limits(x = states_map$long, y = states_map$lat) +
    scale_fill_gradient2(low = '#9ecae1', mid = '#4292c6', high = '#084594', midpoint = .5, 
                         breaks = seq(0, 1, .25), 
                         labels = c('Most\nNegative', 'Negative', 'Neutral', 'Positive', 'Most\nPositive')) +
    labs(fill = 'Sentiment\n') + 
    xlab('Longitude') + ylab('Latitude') +
    ggtitle(title)
  
  return(map_plot)
}

#function to create simple trend plot
plot_trend_simple = function(dat_all, title) {
  #create sequence number to indicate time
  dat_all$seq = match(dat_all$ts_r, sort(unique(dat_all$ts_r)))
  
  #calculate % of positive sentiment per time zone and time
  sentiment_prop = aggregate(sentiment ~ tz + seq, dat_all, agg_sentiment)
  
  trend_plot = ggplot(sentiment_prop, aes(x = seq, y = sentiment, col = tz)) + 
    geom_point() + geom_line() +
    guides(col = guide_legend(title = NULL)) +
    scale_y_continuous(breaks = seq(0, 1, .25),
                       labels = c('Most\nNegative', 'Negative', 'Neutral', 'Positive', 'Most\nPositive')) +
    theme(legend.position = 'bottom', axis.text.x = element_blank()) +
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
  sentiment_prop = aggregate(sentiment ~ tz + seq, dat_all, agg_sentiment)
   
  #plot sentiment trend
  trend_plot = ggplot(sentiment_prop, aes(x = seq, y = sentiment, col = tz)) + 
    geom_point() + stat_smooth(method = 'loess', se = F) +
    guides(col = guide_legend(title = NULL)) +
    scale_y_continuous(breaks = seq(0, 1, .25),
                      labels = c('Most\nNegative', 'Negative', 'Neutral', 'Positive', 'Most\nPositive')) +
    theme(legend.position = 'bottom', axis.text.x = element_blank()) +
    xlab('Time Interval') + ylab('Sentiment') +
    ggtitle(title)
  
  return(trend_plot)
}

######SERVER FUNCTION######

shinyServer(function(input, output, session) {
  #set reactive timer that retrieves new tweets every 6 second
  autoInvalidate = reactiveTimer(6000, session)
  
  #reactive function used to update the data every time it is auto-invalidated
  get_input = reactive({
    autoInvalidate()
    #show progress bar
    withProgress(session, {
      setProgress(message = 'Collecting new tweets...')
      get_tweets()
    })
  })
  
  #observe function used to wait until data are downloaded to process them
  observe({
    dat = get_input()
    
    #check to see the temp file for this session has already been created
    if (file.exists(file = dat_all_name)) {
      load(file = dat_all_name)
      #if so, append after the existing file
      dat_all = rbind(dat_all, dat)
    }
    else {
      #if not (i.e., first time), create file
      dat_all = dat
    }
    save(dat_all, file = dat_all_name)
    
    #summary statistics
    #for the tweets collected over the last 5 seconds
    obs = sum(!is.na(dat$text))
    if (obs > 0) {
      ts_min = min(dat$ts, na.rm = T)
      ts_max = max(dat$ts, na.rm = T)
      
      #get sample positive and negative tweets (just for fun)
      pos_sample = ifelse(max(dat$sentiment, na.rm = T) > 0,
                          sample(dat$text[which(dat$sentiment > 0)], 1),
                          '')
      neg_sample = ifelse(min(dat$sentiment, na.rm = T) < 0,
                          sample(dat$text[which(dat$sentiment < 0)], 1),
                          '')
    }
    
    #for all the tweets collected so far
    obs_all = sum(!is.na(dat_all$text))
    if (obs_all > 0) {
      ts_min_all = min(dat_all$ts, na.rm = T)
      ts_max_all = max(dat_all$ts, na.rm = T)
      intervals = length(unique(dat_all$ts_r))
    }
    
    #output heat map for the tweets collected over the last 5 seconds
    output$header_map1 = renderUI({
      if (obs > 0) {
        h = paste('Based on the', obs, 'tweets collected from', ts_min, 'to', ts_max, 'GMT')  
      }
      else {
        h = 'No geo-coded tweets from the U.S. were collected in the last 5 seconds.'
      }
      HTML('<h6>', h, '<h6>')
    })
    
    output$plot_map1 = renderPlot({
      if (obs > 0) {
        print(plot_map(dat, 'Sentiment in the last 5 seconds'))
      }
    })
    
    #output heat map for all the tweets collected so far
    output$header_map2 = renderUI({
      if (obs_all > 0) {
        h = paste('Based on the', obs_all, 'tweets collected from', ts_min_all, 'to', ts_max_all, 'GMT') 
      }
      else {
        h = 'No geo-coded tweets from the U.S. have been collected yet.'
      }
      HTML('<h6>', h, '<h6>')
    })
    
    output$plot_map2 = renderPlot({
      if (obs_all > 0) {
        print(plot_map(dat_all, 'Cumulative sentiment'))
      }
    })
    
    #output trend plot
    output$plot_trend = renderPlot({
      if (obs_all > 0 && intervals > 8) {
        #only use loess-smoothing when more than 8 data points are collected (determined through trial and error)
        print(plot_trend_loess(dat_all, 'Sentiment trend by time zone over 5-second intervals'))
      }
      else if (obs_all > 0) {
        print(plot_trend_simple(dat_all, 'Sentiment trend by time zone over 5-second intervals'))
      }
    })
    
    #display sample tweets for fun
    output$sample_tweets = renderUI({
      if (obs > 0) {
        h_pos = ifelse(pos_sample != '',
                       paste('Sample positive tweet:', pos_sample),
                       'No positive tweet was collected in the last 5 seconds :-(')
        h_neg = ifelse(neg_sample != '',
                       paste('Sample negative tweet:', neg_sample),
                       'No negative tweet was collected in the last 5 seconds :-)')
        HTML('<h5>', h_pos, '<br>', h_neg, '<h5>')
      }
      else {
        h = 'No geo-coded tweets from the U.S. were collected in the last 5 seconds.'
        HTML('<h5>', h, '<h5>')
      }
    })
  })
})