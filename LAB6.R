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
print(app_details)

appname <- "hdt6-SC"

# create token named "twitter_token"
twitter_token <- create_token(
  app = appname,
  consumer_key = app_details$consumer_key,
  consumer_secret = app_details$consumer_secret,
  access_token = app_details$access_token,
  access_secret = app_details$access_token_secret)