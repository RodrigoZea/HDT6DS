# Instalaci?n de paquetes necesarios para procesar datos:
# load twitter library - the rtweet library is recommended now over twitteR
library(rtweet)
# plotting and pipes - tidyverse!
library(ggplot2)
library(dplyr)
# text mining library
library(tidytext)
library("rjson")

app_details <- fromJSON(file = "api_key.json")
appname <- "hdt6-SC"

# create token named "twitter_token"
twitter_token <- create_token(
  app = appname,
  consumer_key = app_details$consumer_key,
  consumer_secret = app_details$consumer_secret,
  access_token = app_details$access_token,
  access_secret = app_details$access_token_secret)

## search for 500 tweets using the #rstats hashtag
traficogt <- search_tweets(q = "@amilcarmontejo AND #traficogt",
                               n = 1000,
                               include_rts = FALSE)

# view the first 3 rows of the dataframe
head(traficogt$text, n = 100)
