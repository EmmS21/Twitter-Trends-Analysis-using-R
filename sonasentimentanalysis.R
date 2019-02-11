install.packages("wordcloud") 
install.packages("RColorBrewer")
devtools::install_github("lchiffon/wordcloud2")
library("wordcloud")
library("RColorBrewer")
library(twitteR)
library(rtweet)
library(tm) 
create_token(
  consumer_key = "***",
  consumer_secret = "***",
  access_token = "***",
  access_secret = "***")
#factoring in Twitter API rate limit
tweets <- search_tweets('#SONA2019',n=50000, type='mixed', retryonratelimit = TRUE)
tweets_df <- data.frame(tweets)
a <-data.frame(table(unlist(tweets_df$hashtags)))
a<- a[which(a$Freq > 4),] #remove hashtags that appear less than 2 times
#remove obvious hashtags 
removal_list <- c('SONA2019','SONA19','Sona2019','sona2019','SONA','sona19','8May2019','08MAY2019','SOna2019','SoNA2019','Kenya','Uganda','SözleşmeliMEMURSENsiz','NBATradeDeadline','EyyamınKralıTurkHakemleri','NZvIND','CemKaraca','ItaliaFrancia','8febbraio','RafaleDeal','BuenViernes','MogadishuToday','BarışMehdiyleGelecek','MTAOLY','giatinparea','ATRAEK','JkwMentokDi5Persen','Fiverr','Ethiopia','StopkillingJkuatstudents','ガリベンガーV')
hashtag = a[which(!(a$Var1 %in% removal_list)),]
#creating wordcloud
set.seed(1234)
wordcloud(words = hashtag$Var1, freq = hashtag$Freq,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
#sentiment analysis
the_tweets <- tweets_df$text
#cleanup
tweets_corp <- Corpus(VectorSource(the_tweets))
twitterHandleRemover <- function(x) gsub("@\\S+","", x)
hashtagRemover <- function(x) gsub("#\\S+","", x)
emojiRemover <- function(x) gsub("[^\x01-\x74F]","",x)
toSpace = content_transformer(function(x,pattern)gsub(pattern,"",x))
cleaner <- function(tweets_corp){
  tweets_corp <- tm_map(tweets_corp, toSpace," ?(f|ht)tp(s?)://(.*)[.][a-z]+")
  tweets_corp <- tm_map(tweets_corp, content_transformer(twitterHandleRemover))
  tweets_corp <- tm_map(tweets_corp, content_transformer(hashtagRemover))
  tweets_corp <- tm_map(tweets_corp, removePunctuation)
  tweets_corp <- tm_map(tweets_corp, emojiRemover)
  tweets_corp <- tm_map(tweets_corp, stemDocument)
  tweets_corp <- tm_map(tweets_corp, content_transformer(tolower))
  return(tweets_corp)
}
tweets_corp <- cleaner(tweets_corp)
new_tweetsdf <- data.frame(text = sapply(tweets_corp, as.character), stringsAsFactors = FALSE)
#new_tweetsdf is a list column
new_tweetsdf <- unlist(new_tweetsdf)
#some rows are now empty and have to remove these to clean up
pattern = "^[[:space:]]*$"
new_df <- new_tweetsdf[grep(pattern, new_tweetsdf, invert = TRUE)]
#sentiment analysis
install.packages("devtools")
devtools::install_github("exploratory-io/exploratory_func")
install_github('trinker/sentimentr')
library(exploratory)
library(dplyr)
library(devtools)
library(sentimentr)
#sentimentr package
sentiments_df <- sentiment_attributes(new_df)
new_2 <- get_sentences(new_df)
tweet_sentiment<-sentiment_by(new_2, averaging.function = average_weighted_mixed_sentiment)
#visualization of sentiments
library(plotly)
# Make the graph
sentiment_graph = plot_ly(x=tweet_sentiment$word_count,y=tweet_sentiment$ave_sentiment,mode="markers",colors =c("red","yellow"),size=abs(tweet_sentiment$ave_sentiment)/3 , color=ifelse(tweet_sentiment$ave_sentiment>0,"Positive","Negative") ) %>% 
#Change hover mode in the layout argument 
layout( hovermode="closest",title="Sentiment analysis by Tweet",xaxis= list(title = "Number of words per Tweet",size=18),yaxis = list(title = "Sentiments by Tweet",size=18))
# show the graph
sentiment_graph

