Live national sentiment analysis using tweets
=================

The whole project was documented in my blog post [here](http://www.runzemc.com/2014/07/state-sentiment-analysis-using-twitter-live-stream-and-r.html). The finished app can be accessed [here](https://runzemc.shinyapps.io/sentiment/).

Besides the `server.R` and `ui.R` used for the shiny app, code to perform authentication with twitter and parse the state shape file are also provided.

The data folder includes states and timezone data and a list of positive and negative words. Note my authentication file is not included. You can generate your own using the code provided in `_oauth.R`.

Hope you'll like it :-)
