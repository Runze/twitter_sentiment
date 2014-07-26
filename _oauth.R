library(ROAuth)
setwd('/Users/Runze/Documents/Github/twitter_sentiment')

requestURL = 'https://api.twitter.com/oauth/request_token'
accessURL = 'https://api.twitter.com/oauth/access_token'
authURL = 'https://api.twitter.com/oauth/authorize'
consumerKey = '<your key>'
consumerSecret = '<your secret>'
my_oauth = OAuthFactory$new(consumerKey = consumerKey, consumerSecret = consumerSecret, 
                             requestURL = requestURL, accessURL = accessURL, authURL = authURL)
my_oauth$handshake(cainfo = system.file('CurlSSL', 'cacert.pem', package = 'RCurl'))
save(my_oauth, file = 'data/my_oauth.RData')
