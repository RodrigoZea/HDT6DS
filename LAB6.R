# Instalaci?n de paquetes necesarios para procesar datos:
# load twitter library - the rtweet library is recommended now over twitteR

# Instalaci?n de paquetes necesarios para procesar datos:
#install.packages("tm")  
#install.packages("SnowballC") 
#install.packages("wordcloud") 
#install.packages("RColorBrewer") 
#install.packages("syuzhet") 
#install.packages("ggplot2") 
#install.packages("readr")
unwanted_array = list( 'S'='S', 's'='s', 'Z'='Z', 'z'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E', 
                       'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U', 
                       'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c', 
                       'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o', 
                       'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y') 

# Importaci?n de paquetes
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("syuzhet")
library("ggplot2")
library("readr")
library(rtweet)
library(ggplot2)
library(dplyr)
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

tmls <- get_timelines(c("amilcarmontejo"),n=1000)
tmls$text <- gsub("\n\n", " ", tmls$text)
tmls$text <- gsub("\n", " ", tmls$text)

## search for 500 tweets using the #rstats hashtag
traficogt <- search_tweets(q = "#traficogt",
                           n = 1000,
                           include_rts = FALSE)

# view the first 3 rows of the dataframe

traficogt$text <- gsub("\n\n", " ", traficogt$text)
traficogt$text <- gsub("\n", " ", traficogt$text)

#traficogt$text <- iconv(traficogt$text, to='ASCII//TRANSLIT') 
for(i in seq_along(unwanted_array)) 
  traficogt$text <- gsub(names(unwanted_array)[i],unwanted_array[i],traficogt$text) 
  tmls$text <- gsub(names(unwanted_array)[i],unwanted_array[i],tmls$text) 

head(traficogt$text, n = 5)

lapply(traficogt$text, write, "./tweets/test.txt", append=TRUE)
lapply(tmls$text, write, "./tweets/test.txt", append=TRUE)


get_corpus <- function(dir) {
  return (VCorpus(DirSource(dir, encoding = "UTF-8"), readerControl = list(language = "en")))
}

tweets <- get_corpus("./tweets")

#porciento <- 0.05
#set.seed(50)
#blogs[[1]]$content <- sample(blogs[[1]]$content, length(blogs[[1]]$content)*porciento)

# Funcion para cambiar a un espacio
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))

# Eliminar URLs
# Extraido de: https://stackoverflow.com/questions/41109773/gsub-function-in-tm-package-to-remove-urls-does-not-remove-the-entire-string
#removeURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T))
# Extraido de: https://stackoverflow.com/questions/30994194/quotes-and-hyphens-not-removed-by-tm-package-functions-while-cleaning-corpus
#removeSpecialChars <- content_transformer(function(x) gsub(""."","",x))
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
removeURL2 <- function(x) gsub("www[[:alnum:]]*", "", x)
removeURL3 <- function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T)
removeEmojis <- function(x) gsub("[^\x01-\x7F]", "", x)
### myCorpus <- tm_map(myCorpus, removeURL, lazy=TRUE) 




data_cleaning <- function(corpus) {
  #   - Normalizar Texto -> minuscula/mayuscula
  corpus <- tm_map(corpus, content_transformer(tolower))
  
  #corpus <- tm_map(corpus, content_transformer(removeURL))
  corpus <- tm_map(corpus, content_transformer(removeURL3))
  #corpus <- tm_map(corpus, content_transformer(removeEmojis))
  #   - Remover signos de puntuacion
  corpus <- tm_map(corpus, removePunctuation)
  
  #   - Remover numeros si no aportan nada.
  corpus <- tm_map(corpus, removeNumbers)
  
  #   - Remover articulos, preposiciones y conjunciones
  corpus <- tm_map(corpus, removeWords, stopwords("spanish"))
  
  
  
  #   - Remover  caracteres especiales
  #corpus <- tm_map(corpus, toSpace, "/")
  #corpus <- tm_map(corpus, toSpace, "@")
  #corpus <- tm_map(corpus, toSpace, "\\|")
  #corpus <- tm_map(corpus, toSpace, "#")
  #corpus <- tm_map(corpus, toSpace, "£")
  
  #   - Remover emoticones 
  #   Non Ascii
  #gsub("[^\x01-\x7F]", "", corpus)
  #
  #gsub(""."","",corpus)
  #   - Remover espacios extra
  corpus <- tm_map(corpus, stripWhitespace)
  
  
}

tweets <- data_cleaning(tweets)

head(tweets[[1]]$content, n=100)

#tmls <- get_timelines(c("amilcarmontejo"), n = 3200)






term_doc_matrix <- function(corpus){
  # Build a term-document matrix
  tdm <- TermDocumentMatrix(corpus)
  dtm_m <- as.matrix(tdm)
  #Sort by descearing value of frequency
  dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
  dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)
  # Display the top 5 most frequent words
  head(dtm_d, 5)
  
  # palabras mas frecuents
  barplot(dtm_d[1:5,]$freq, las = 2, names.arg = dtm_d[1:5,]$word,
          col ="lightgreen", main ="Top 5 most frequent words",
          ylab = "Word frequencies")
  # nube de palabras
  wordcloud(words = dtm_d$word, freq = dtm_d$freq, min.freq = 5,
            max.words=100, random.order=FALSE, rot.per=0.40, 
            colors=brewer.pal(8, "Dark2"))
}

term_doc_matrix(tweets)

writeCorpus(tweets, path="./CleanData")



