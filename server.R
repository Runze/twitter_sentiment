library(shiny)
library(shinyIncubator)
source('_helper_functions.R')

#create name for temp file created on the fly
dat_all_name = paste0('data/dat_all_', format(Sys.time(), '%Y-%m-%d_%H-%M-%S'), '.RData')

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
    
    #check to see if the temp file for this session has already been created
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
    }, bg = 'transparent', width = 600, height = 450)
    
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
    }, bg = 'transparent', width = 600, height = 450)
    
    #output trend plot
    output$plot_trend = renderPlot({
      if (obs_all > 0 && intervals > 8) {
        #only use loess-smoothing when more than 8 data points are collected (determined through trial and error)
        print(plot_trend_loess(dat_all, 'Sentiment trend by time zone over 5-second intervals'))
      }
      else if (obs_all > 0) {
        print(plot_trend_simple(dat_all, 'Sentiment trend by time zone over 5-second intervals'))
      }
    }, bg = 'transparent')
    
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