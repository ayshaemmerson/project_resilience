# Project Resilience; By Aysha Emmerson.
# Gov 1005 - "Data"; Fall 2019.


####################################                 
# LOAD PACKAGES 
####################################


library(rtweet)
library(twitteR)
library(fs)
library(tidyverse)
library(ggthemes)
library(tidytext)
library(ggpubr)
library(stringr)
library(textdata)
library(wordcloud)
library(wordcloud2)
library(SnowballC)
library(tm)
library(reshape2)
library(gt)
library(RColorBrewer)
library(lubridate)


####################################                 
#### AUTHORIZE - TWITTER API #####
####################################


# Following code chunk is used to download data from directly twitter, after having filled out
# an API request online at https://developer.twitter.com/. 

# Access keys from developer app for Twitter's API. I created my ownaccount to become a
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


# The stop words data is necessary later for sentiment analysis.

data("stop_words")

# Directory for raw, downloaded data.

dir.create("raw_data")

# Directory for all cleaned data used in visualizations and Explore Page.

dir.create("clean_data")

# Directory for all cleaned data / visualizations used in General Summary  section of app.

dir.create("general_summary")

# Directory for all cleaned data / visualizations used in Word Analysis section of app.

dir.create("word_analysis")


####################################                 
#### LOAD DATA & INITIAL CLEAN ####
####################################


#### TWEETS MENTIONING RESILIENCE ####

# The following is for the initial download and analysis of my twitter data for tweets mentioning resilience.
# Mentions of resilience for week of Nov 6 - Nov 15. Goal of this set of data is to explore how 
# resilience is used and what feeling/concepts it is associated with, in our general/popular culture.

# Search for 18,000 tweets containing the word resilience. Search_tweets only shows tweets within a 
# 6-9 day span. This data will show tweets from 2019-11-06 to 2019-11-15. Intentionally included
# retweets, as they also seem indicative of what ideas of resilience resonate most with the public.

word_resilience <- search_tweets(
  "resilience OR Resilience", n = 100000, exclude_hashtags = TRUE, retryonratelimit = TRUE)

# Store raw data in raw_data folder.

write_rds(word_resilience, "raw_data/word_resilience.rds")

# Kept the most important values of the entire word_resilience dataset, 
# excluding retweets and with only including tweets in english.

wr_cleaned <- word_resilience %>% 
  filter(lang == "en") %>%
  filter(is_retweet == FALSE) %>%
  select(screen_name, created_at, text, retweet_count, favorite_count) 

# Store clean data in clean_data folder. All data stored using write_rds 
# function will be imported into the app using read_rds.
# Will be used in multiple places in app... including Word Analysis.

write_rds(wr_cleaned, "clean_data/wr_cleaned.rds")

# Same cleaning procedure except only for the 50 most popular tweets, 
# as measured by the number of times the tweet was favorited. Selected 
# for values of interest; specifically, screen_name, 
# time created, number of retweets, and number of favorites.

top50wr <- word_resilience %>% 
  filter(lang == "en") %>%
  filter(is_retweet == FALSE) %>%
  group_by(screen_name, favorite_count) %>% 
  arrange(desc(favorite_count)) %>%
  head(50) %>%
  select(screen_name, created_at, text, retweet_count, favorite_count) 

# Store cleaned data in clean_data folder.
# Will be used for Explore page.

write_rds(top50wr, "clean_data/top50wr.rds")

#### TWEETS USING #RESILIENCE ####

# Search for up to 100,000 tweets containing #resilience or #Resilience. 
# Only have access to tweets from the past 6-9 days, so may not actually download 100,000 tweets.
# if there were not that many. This data collected will show tweets from 2019-11-06 to 2019-11-15. 
# Intentionally kept out retweets, as they seem indicative of what uses of resilience resonate with people most.

hashtag_resilience <- search_tweets(
  "#resilience", n = 100000, include_rts = FALSE, retryonratelimit = TRUE)

# Store raw data in raw_data folder.
# This data is not used anywhere directly in the app.

write_rds(hashtag_resilience, "raw_data/hashtag_resilience.rds")

# Clean the data... as done above.
# To use for wordcloud in Word Analysis page.

hr_cleaned <- hashtag_resilience %>% 
  filter(lang == "en") %>%
  filter(is_retweet == FALSE) %>%
  select(screen_name, created_at, text, retweet_count, favorite_count) 

# Store clean data in clean_data folder.

write_rds(hr_cleaned, "clean_data/hr_cleaned.rds")

# Same cleaning procedure except only for the 50 most popular tweets, 
# as measured by the number of times the tweet was favorited. Select for
# values of interest; specifically, screen_name, 
# time created, number of retweets, and number of favorites.

top50hr <- hashtag_resilience %>% 
  filter(lang == "en") %>%
  filter(is_retweet == FALSE) %>%
  group_by(screen_name, favorite_count) %>% 
  arrange(desc(favorite_count)) %>%
  head(50) %>%
  select(screen_name, created_at, text, retweet_count, favorite_count) 

# Store cleaned data in clean_data folder.
# Will be used for Explore page.

write_rds(top50hr, "clean_data/top50hr.rds")

#### TWITTER USERS ####

# The following Data is for Users with the word "Resilience" in their title.
# After initial download, must format into dataframe.

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
# Set retweet as false to only see tweets produced by these users.

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
#### GENERAL SUMMARY ####
####################################


#### FREQUENCY PLOTS ####

# Created new dataframe with day/month variables to construct frequency plot of
# the times resilience was mentioned from Nov 6 to Nov 11. The same frequency plot 
# could also be constructed directly using the data from word_resilience; however, 
# wr_tweets was too large to push to Git Hub but this transformed dataframe is not.

tweets_bydate <- word_resilience %>%
  group_by(created_at) %>%
  summarise(ntweet = n()) 

# Tested to see what plot would look like here.

ts_plot(tweets_bydate)

# Store cleaned data in clean_data folder.
# Will be used for General Summary page.

write_rds(tweets_bydate, "clean_data/tweets_bydate.rds")

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

# With the table created, need to clean it a bit further, selecting for user's handle in
# addition to the new variables created above. 

clean_summary_table <- summary_table %>% 
  select(screenName, tweet_count, mean_tweet_length, fav_average, rt_average) %>% 
  distinct()

# With the table clean, export it as an rds for later use within the app.

write_rds(clean_summary_table, "general_summary/clean_summary_table")


####################################                 
##### WORD ANALYSIS ####
####################################


#### WORD CLOUDS ####

# Create wordcloud for tweets mentioning resilience. Word clouds are an effective way to visualize data as they
# present information about the frequency of words in a clear, visually appealing format. The size of the word
# in the cloud, depends on how often it is employed. I will create a wordcloud for the cleaned data for
# hashtag_resilience.

# Word cloud for hr_cleaned.

# First, the data needs to be cleaned and text extracted as a vector.
# Cleaning involves removing common stop words in order to produce
# meaningful results. Also removed the word "amp" and other gibberish words
# thst kept reappearing as well as all urls. Removed the word resilience because it was too 
# large that you could not see other words.

text <- hr_cleaned$text
docs <- Corpus(VectorSource(text))

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
# column and uts frequency in the second.

dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words) 

write_rds(df, "word_analysis/df.rds")

# Using the wordcloud package, generate the word cloud.
# Play around with different ways to visualize... 
# Test out plotted wordcloud here, before constructing in shiny app.

wr_word_cloud <- wordcloud2(data=df, size=5, color='random-dark')

# Alternatively, I could have used the following code to create the plot; I
# just found the previous one more visually appealing:
# wordcloud(words = df$word, freq = df$freq, min.freq = 1,
          # max.words=200, random.order=FALSE, rot.per=0.35,            
          # colors=brewer.pal(8, "Dark2"))

#### SENTIMENT ANALYSIS ####

# The purpose of this section is to perform a series of sentiment analysis that provide general
# information about the tone used in tweets containing the hashtag "resilience" or word "resilience" 
# in them. There are three main lexicons: bing, nrc, and AFINN.

# The first sentiment analysis will be done for tweets that include #resilience. The output will
# provide information about whether these tweets express, in general, positive or negative sentiments
# by giving us a binary value based on the aggregate rating of each word in the text.

hr_bing_tweets <- hr_cleaned %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% 
  inner_join(get_sentiments("bing")) %>%
  group_by(screen_name, sentiment) %>%
  tally() 

# Store sentiment analysis data in sentiment_analysis folder.

write_rds(hr_bing_tweets, "word_analysis/hr_bing_tweets.rds")

# The second sentiment analysis will be done for tweets that include #resilience. Instead of just
# providing binary values though, this analyis will assign a specific tweet in addition to a binary 
# of positive/negative. These emotions include anger, anticipation, disgust, fear, joy, sadness, 
# surprise, and trust.

hr_nrc_tweets <- hr_cleaned %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% 
  inner_join(get_sentiments("nrc")) %>%
  group_by(screen_name, sentiment) %>%
  tally() 

# Store sentiment analysis data in sentiment_analysis folder.

write_rds(hr_nrc_tweets, "word_analysis/hr_nrc_tweets.rds")

# The third sentiment analysis will be on the tweets with the designated key word (resilience) in the text. 
# After locating these keywords and assigning them a relevant keyword value, the text 
# can then be broken up to determine the relevant sentiments for all words in the text. 
# After, we can group by each keyword value tally and total of different sentiments. 

# First, create object for Keyword Tweets -- Resilience or resilience.

kw_resilience <- c("resilience")

# Bing analysis.

wr_sentiment_bing <- wr_cleaned %>%
  filter(str_detect(text, paste(kw_resilience, collapse="|"))) %>% 
  mutate(keyword = str_match(text, paste(kw_resilience, collapse="|"))) %>% 
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing")) %>%
  group_by(keyword, sentiment) %>%
  tally()

# Store sentiment analysis data in sentiment_analysis folder.

write_rds(wr_sentiment_bing, "word_analysis/wr_sentiment_bing.rds")

# Nrc analysis for tweets in word_resilience.

wr_sentiment_nrc <- wr_cleaned %>%
  filter(str_detect(text, paste(kw_resilience, collapse="|"))) %>% 
  mutate(keyword = str_match(text, paste(kw_resilience, collapse="|"))) %>% 
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("nrc")) %>%
  group_by(keyword, sentiment) %>%
  tally() %>%
  arrange(desc(n)) 

# Store sentiment analysis data in sentiment_analysis folder.

write_rds(wr_sentiment_nrc, "word_analysis/wr_sentiment_nrc.rds")


####################################                 
#### EXPLORE TWEETS ####
####################################

# Code used to create table in surver for top 50 tweets mentioning resilience.
# Tested it out here and then constructed within the app itself.

datatable(top_50 %>% filter(str_detect(text, input$keyword)) ,
          class = 'display',
          rownames = FALSE,
          selection = 'single',
          colnames = c('Tweet Text', 'Date', 'Time', 'Retweets', 'Favorites'),
          options = list(dom = 'tip')
) %>% formatStyle(2, cursor = 'pointer')

# Code used to create table in surver for top 50 tweets with #resilience.

datatable(top50hr %>% filter(str_detect(text, input$keyword)) ,
          class = 'display',
          rownames = FALSE,
          selection = 'single',
          colnames = c('Tweet Text', 'Date', 'Time', 'Retweets', 'Favorites'),
          options = list(dom = 'tip')
) %>% formatStyle(2, cursor = 'pointer')

# Code used to create table in surver for users.

datatable(user_tweets %>% filter(str_detect(text, input$keyword)) ,
          class = 'display',
          rownames = FALSE,
          selection = 'single',
          colnames = c('Tweet Text', 'Date', 'Time', 'Retweets', 'Favorites'),
          options = list(dom = 'tip')
) %>% formatStyle(2, cursor = 'pointer')




