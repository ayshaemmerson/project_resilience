# Project Resilience Script; By Aysha Emmerson.
# Gov 1005 - "Data"; Fall 2019.

####################################                 
# LOAD PACKAGES 
####################################

# For tweet download.

library(rtweet)
library(twitteR)

# General transformations and visualizations.

library(fs)
library(tidyverse)
library(ggthemes)
library(tidytext)
library(ggpubr)
library(stringr)
library(textdata)

# For word cloud.

library(wordcloud)
library(wordcloud2)
library(SnowballC)

# For sentiment analysis.

library(tm)
library(reshape2)
library(RColorBrewer)
library(lubridate)

####################################                 
#### AUTHORIZE - TWITTER API #####
####################################

# Following code chunk is used to download data from directly twitter, after having filled out
# an API request online at https://developer.twitter.com/. 

# Access keys from developer app for Twitter's API. I created my own Twitter account to become a
# developer by following instructions outlined by the webite below.
# https://medium.com/@GalarnykMichael/accessing-data-from-twitter-api-using-r-part1-b387a1c7d3e.

consumer_key <- "ACcvV5MUjFPcAEU1hSSYschCg"
consumer_secret <- "EKmI9BhhxFOwXH4pZzq8u0g7SAqN519fJMu2fH5iGQMhBFk6UF"
access_token <- "1190736536196636672-Yj4VeAHZ491GBcMmAtkyAmqnj2ouhp"
access_secret <- "aBOpSp46kVBuE9FKzE9dwBhDhaVn7mV9GuAmIEnqGTvB8"

# R twitter authorization.

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

####################################                 
#### ORGANIZE FOLDERS ####
####################################

# Need to create directories in order to save the downloaded/transformed data, so it can be imported
# into the shiny app. Decided to sort the data into directories ~roughly according to the tabs
# it appears in, in the shiny app.

# The stop words data is necessary later for sentiment analysis.

data("stop_words")

# Directory for raw, downloaded data. This data was not uploaded to Github as the file size was too large.

dir.create("raw_data")

# Directory for all cleaned data used in various visualizations and the Explore Page.

dir.create("clean_data")

# Directory for all cleaned data / visualizations used in General Summary section of app.

dir.create("general_summary")

# Directory for all cleaned data / visualizations used in Sentiment Analysis and Word Cloud sections of app.

dir.create("word_analysis")

####################################                 
#### LOAD DATA & INITIAL CLEAN ####
####################################

#### TWEETS MENTIONING RESILIENCE ####

# The following is for the initial download and analysis of my twitter data for tweets mentioning resilience.
# These tweets were posted between 2019-11-06 and 2019-11-15. The goal of this set of data is to explore how 
# resilience is used and what feeling/concepts it is associated with, in our general/popular culture. In what
# context does the word resilience appear? How often? 

# Used search_tweets to search for 100,000 tweets containing the word resilience (search_tweets only shows tweets within a 
# 6-9 day span). Intentionally included retweets, as they also seem indicative of what ideas of resilience resonate most with the public.
# Set retryonratelimit to TRUE to automate process of conducting big searches (>18000).

word_resilience <- search_tweets(
  "resilience OR Resilience", n = 100000, exclude_hashtags = TRUE, retryonratelimit = TRUE)

# Stored raw tweet data in raw_data folder.

write_rds(word_resilience, "raw_data/word_resilience.rds")

# Preliminary cleaning of word_resilience tweets.
# Kept the most important values of the dataset, 
# excluding retweets and with only including tweets in english.
# Chose to exclude retweets so that there were no repeated tweets.

wr_cleaned <- word_resilience %>% 
  filter(lang == "en") %>%
  filter(is_retweet == FALSE) %>%
  select(screen_name, created_at, text, retweet_count, favorite_count) 

# Stored clean data in clean_data folder. All data stored using write_rds 
# function will be imported into the app using read_rds.
# This data will be used in multiple places in app, including Word Analysis.

write_rds(wr_cleaned, "clean_data/wr_cleaned.rds")

# Same cleaning procedure except only for the 50 most popular tweets, 
# as measured by the number of times the tweet was favorited. As before, I  
# selected for values of interest; specifically, screen_name, 
# time created, number of retweets, and number of favorites.

top50wr <- word_resilience %>% 
  filter(lang == "en") %>%
  filter(is_retweet == FALSE) %>%
  group_by(screen_name, favorite_count) %>% 
  arrange(desc(favorite_count)) %>%
  head(50) %>%
  select(screen_name, created_at, text, retweet_count, favorite_count) 

# Stored cleaned data in clean_data folder.
# This data will be used for Explore page.

write_rds(top50wr, "clean_data/top50wr.rds")

#### TWEETS USING #RESILIENCE ####

# Searched for up to 100,000 tweets containing #resilience or #Resilience. 
# Only have access to tweets from the past 6-9 days, so did not actually download 100,000 tweets.
# as there were not that many posted. This data collected shows tweets from 2019-11-06 to 2019-11-15. 
# Intentionally kept out retweets for this download, as only wanted to see original tweets.
# Set retryonratelimit to TRUE to automate process of conducting big searches (>18000).
# While this dataframe appears here in my script and is used later on in my code, it does not actually appear in 
# my raw_data folder as it was accidently deleted from my computer and redownloading the exact same set of tweets  
# is no longer possible with search_tweets.

hashtag_resilience <- search_tweets(
  "#resilience", n = 100000, include_rts = FALSE, retryonratelimit = TRUE)

# Stored raw data in raw_data folder.
# This data is not used anywhere directly in the app.

write_rds(hashtag_resilience, "raw_data/hashtag_resilience.rds")

# Cleaned the data as done above for word_resilience.
# Set retweet as FALSE again for good measure.
# To use for wordcloud in Word Analysis page.

hr_cleaned <- hashtag_resilience %>% 
  filter(lang == "en") %>%
  filter(is_retweet == FALSE) %>%
  select(screen_name, created_at, text, retweet_count, favorite_count) 

# Stored cleaned data in clean_data folder.

write_rds(hr_cleaned, "clean_data/hr_cleaned.rds")

# Same cleaning procedure as above except only for the 50 most popular tweets, 
# as measured by the number of times the tweet was favorited. Select for
# values of interest; specifically, screen_name, time created, number of retweets, and number of favorites.

top50hr <- hashtag_resilience %>% 
  filter(lang == "en") %>%
  filter(is_retweet == FALSE) %>%
  group_by(screen_name, favorite_count) %>% 
  arrange(desc(favorite_count)) %>%
  head(50) %>%
  select(screen_name, created_at, text, retweet_count, favorite_count) 

# Stored cleaned data in clean_data folder.
# To be used for Explore page.

write_rds(top50hr, "clean_data/top50hr.rds")

#### TWITTER USERS ####

# The following Data is for Users with the word "Resilience" in their title.
# After initial download of all tweets from user's timelines between 2019-11-06 and 2019-11-15,
# must format into dataframe. 3200 is the maximum number of tweets that UserTimeline can download.
# Chose to exclude retweets, so that all tweets obtained were posted directly by the user.

gri <- userTimeline("Resilience_NU", n = 3200, includeRts=F)
df.gri <- twListToDF(gri)

rfnd <- userTimeline("resiliencefnd", n = 3200, includeRts=F)
df.rfnd <- twListToDF(rfnd)

undp <- userTimeline("UNDP_GCRED", n = 3200, includeRts=F)
df.undp <- twListToDF(undp)

rshft <- userTimeline("resilienceshift", n = 3200, includeRts=F)
df.rshft <- twListToDF(rshft)

rcode <- userTimeline("resiliencecode", n = 3200, includeRts=F)
df.rcode <- twListToDF(rcode) 

amr <- userTimeline("AmResilience", n = 3200, includeRts=F)
df.amr <- twListToDF(amr) 

# Combined all of these users into a single data frame and sorted for variables of interest.
# Set retweet as false again, even though it wasn't really necessary as I already excluded them above. 

user_tweets <- bind_rows(
  df.gri %>% filter(isRetweet==F) %>%
    select(text, screenName, created, retweetCount, favoriteCount),
  
  df.rfnd %>% filter(isRetweet==F) %>%
    select(text, screenName, created, retweetCount, favoriteCount),
  
  df.undp %>% filter(isRetweet==F) %>%
    select(text, screenName, created, retweetCount, favoriteCount),
  
  df.rshft %>% filter(isRetweet==F) %>%
    select(text, screenName, created, retweetCount, favoriteCount),  
  
  df.rcode %>% filter(isRetweet==F) %>%
    select(text, screenName, created, retweetCount, favoriteCount),
  
  df.amr %>% filter(isRetweet==F) %>%
    select(text, screenName, created, retweetCount, favoriteCount))

# Store cleaned data in clean_data folder.
# Will be used for Explore page.

user_tweets <- write_rds(user_tweets, "clean_data/user_tweets.rds")

####################################                 
##### WORD ANALYSIS ####
####################################

#### WORD CLOUDS ####

# Create wordcloud for tweets mentioning resilience. Word clouds are an effective way to visualize data as they
# present information about the frequency of words in a clear, visually appealing format. The size of the word
# in the cloud, depends on how often it is employed. I will create a wordcloud for the cleaned data for
# hashtag_resilience, showing the most common word used in these tweets.

# First, the data needs to be cleaned and text extracted as a vector.
# Cleaning involves removing common stop words in order to produce
# meaningful results. Removed the word "amp" and other gibberish words
# that kept reappearing as well as all urls. Also removed the word resilience because it was too 
# large (as it was mentioned so frequently in the tweets) that you could not see other words.

# The following code requires the following packages:
# library(wordcloud)
# library(SnowballC)
# library(tm)
# Corpus is way of storing a collection of documents in format that is readable by R software.
# The “tm” package operates on this corpus format.

text <- hr_cleaned$text
docs <- Corpus(VectorSource(text))

# Following code finds key words and removes banal ones. Text is cleaned by converting all text 
# to lowercase and removing punctuation, links, and unnecessary symbols or whitespace. "Stop words" are 
# common, filler words that do not provide us with any useful information.

docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace) 
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, c("amp", "resilience", "'s", "'re", "smedian"))
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
docs <- tm_map(docs, content_transformer(removeURL))

# Create a "document-term" matrix using the tm package.
# This creates a dataframe containing each word in the first 
# column and its frequency in the second.

dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words) 

# Stored data in word_analysis folder.
# To use for making the word cloud.

write_rds(df, "word_analysis/df.rds")

# Using the wordcloud package, generated the word cloud.
# Played around with different ways to visualize and decided upon 
# the one that is tested out below. Will construct identical cloud in shiny app.

wr_word_cloud <- wordcloud2(data=df, size=5, color='random-dark')

# Alternatively, I could have used the following code to create the plot; I
# just found the previous one more visually appealing:
# wordcloud(words = df$word, freq = df$freq, min.freq = 1,
# max.words=200, random.order=FALSE, rot.per=0.35,            
# colors=brewer.pal(8, "Dark2"))

####################################                 
#### GENERAL SUMMARY ####
####################################

#### FREQUENCY PLOTS ####

# Created new dataframe with fewer variables to construct frequency plot of
# the times resilience was mentioned between 2019-11-06 and 2019-11-15. The same frequency plot 
# could also be constructed directly using the data from word_resilience; however, 
# wr_tweets was too large to push to Git Hub but this transformed dataframe is not.

tweets_bydate <- word_resilience %>%
  group_by(created_at) %>%
  summarise(ntweet = n()) 

# Tested to see what plot would look like here.
# Will reconstruct in shiny app.

ts_plot(tweets_bydate)

# Stored cleaned data in clean_data folder.
# Will be used for General Summary page.

write_rds(tweets_bydate, "general_summary/tweets_bydate.rds")

#### GENERAL STATS ON USERS ####

# Interested in creating a basic summary table of twitter metrics, including length of tweet,
# number of tweets, and average number of favorites and retweets for twitter users.
# Used the mutate function to create these new variables.

summary_table <- user_tweets %>% 
  group_by(screenName) %>% 
  select(screenName, text, favoriteCount, retweetCount) %>% 
  mutate(tweet_count = n()) %>% 
  mutate(tweet_length = str_length(text)) %>%
  mutate(mean_tweet_length = round(mean(tweet_length)), digits = 0) %>% 
  mutate(fav_average = round(mean(favoriteCount)), digits = 0) %>% 
  mutate(rt_average = round(mean(retweetCount)), digits = 0) 

# With the table created, needed to clean it a bit further, selecting for user's handle in
# addition to the new variables created above. 

clean_summary_table <- summary_table %>% 
  select(screenName, tweet_count, mean_tweet_length, fav_average, rt_average) %>% 
  distinct()

# Stored cleaned data in general_summary folder.

write_rds(clean_summary_table, "general_summary/clean_summary_table")

####################################                 
##### SENTIMENT ANALYSIS ####
####################################

# The purpose of this section is to perform a series of sentiment analysis that provides general
# information about the tone used in tweets containing the word "resilience" 
# in them. There are three main lexicons: bing, nrc, and AFINN. I chose to use bing and nrc as I felt
# AFINN was slightly redundant after using the other two.

# The sentiment analysis will be on the tweets with the designated key word (resilience) in their text. 
# After locating these keywords and assigning them a relevant keyword value, the text 
# can then be broken up to determine the relevant sentiments for all words in the text. 
# After, we can group by each keyword value tally and total of different sentiments. 
# The resulting sentiments are the sentiments for the word resilience, given the other words surrounding it.

# First, create object for Keyword Tweets -- resilience.

kw_resilience <- c("resilience")

# Bing analysis. The bing output will provide information about whether these tweets express, in general, positive or negative 
# sentiments by giving us a binary value based on the aggregate rating of each word in the text.

wr_sentiment_bing <- wr_cleaned %>%
  filter(str_detect(text, paste(kw_resilience, collapse="|"))) %>% 
  mutate(keyword = str_match(text, paste(kw_resilience, collapse="|"))) %>% 
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing")) %>%
  group_by(keyword, sentiment) %>%
  tally()

# Stored bing sentiment analysis data in sentiment_analysis folder.

write_rds(wr_sentiment_bing, "word_analysis/wr_sentiment_bing.rds")

# Nrc analysis for tweets in word_resilience.
# Instead of just providing binary values, this nrc analyis assigns a specific tweet in addition to a binary 
# of positive/negative. These emotions include anger, anticipation, disgust, fear, joy, sadness, surprise, and trust.

wr_sentiment_nrc <- wr_cleaned %>%
  filter(str_detect(text, paste(kw_resilience, collapse="|"))) %>% 
  mutate(keyword = str_match(text, paste(kw_resilience, collapse="|"))) %>% 
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("nrc")) %>%
  group_by(keyword, sentiment) %>%
  tally() %>%
  arrange(desc(n)) 

# Stored sentiment analysis data in sentiment_analysis folder.

write_rds(wr_sentiment_nrc, "word_analysis/wr_sentiment_nrc.rds")

####################################                 
#### EXPLORE TWEETS ####
####################################

# Code used to create table in server for top 50 tweets mentioning resilience.
# Tested it out here and then constructed within the app itself.

datatable(top_50 %>% filter(str_detect(text, input$keyword1)) ,
          class = 'display',
          rownames = FALSE,
          selection = 'single',
          colnames = c('Tweet Text', 'Date', 'Time', 'Retweets', 'Favorites'),
          options = list(dom = 'tip')) %>% formatStyle(2, cursor = 'pointer')

# Code used to create table in surver for top 50 tweets with #resilience.
# Tested it out here and then constructed within the app itself.

datatable(top50hr %>% filter(str_detect(text, input$keyword2)) ,
          class = 'display',
          rownames = FALSE,
          selection = 'single',
          colnames = c('Tweet Text', 'Date', 'Time', 'Retweets', 'Favorites'),
          options = list(dom = 'tip')
) %>% formatStyle(2, cursor = 'pointer')

# Code used to create table in surver for users.
# Tested it out here and then constructed within the app itself.

datatable(user_tweets %>% filter(str_detect(text, input$keyword3)) ,
          class = 'display',
          rownames = FALSE,
          selection = 'single',
          colnames = c('Tweet Text', 'Date', 'Time', 'Retweets', 'Favorites'),
          options = list(dom = 'tip')
) %>% formatStyle(2, cursor = 'pointer')

########################################                
#### ADDITIONAL NOTES ####
########################################

# I played around with bootstrapping a bing sentiment analysis of different tweets, 
# and comparing the proportions of positive to negative tweets, for my six different users. 
# I also tried doing a linear regression to use the sentiment of a tweet to predict how many 
# times it is favorited. The results have not been particularly interesting and I’m not sure how 
# statically valid they are given the small number of tweets I am working with… So I decided not to 
# include them in the final version of my project. 

# I originally placed the "sentiment analysis" tab after the "word cloud" tab; however, found that the published  
# version had trouble loading with this ordering, as the word cloud requires a significant amount of time to 
# process, so I switched them.

# In the end, I did not use as much of the user's data as I originally had planned; however, in the future,
# I would like to create individual word cloud frequencies for each of these users, to see how they compare thematically.

# I plan to use all of this code in the future to create similar apps, with tweets mentionning resilience
# from different periods of time. For example, it would be interesting to replicate this code after 
# a hurricane, mass shooting, or other tragedy to see how tweets mentionning resilience change or increase.

# That's it! Thanks for reading.

