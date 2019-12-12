# Project Resilience App; By Aysha Emmerson.
# Gov 1005 - "Data"; Fall 2019.

# For shiny app.

library(shiny)
library(rsconnect)

# Must load libraries again, depsite being loaded in the script.

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
library(twitteR)
library(ggplot2)
library(lubridate)
library(ggthemes)
library(shinythemes)
library(tidytext)
library(plotly)
library(devtools)
library(DT)
library(lubridate)

# The stop words data is necessary later for sentiment analysis.

data("stop_words")

# Allow for more data to be used in shiny.
(shiny.maxRequestSize = 30*1024^2) 


####################################                 
#### LOAD DATA ####
####################################

# Data used for "General Summary" page.

bydate <- read_rds("general_summary/tweets_bydate.rds")
summary_table <- read_rds("general_summary/clean_summary_table") %>% 
    rename("Handle" = "screenName", 
           "# of Tweets in 2019" = "tweet_count", 
           "Average Tweet Length in Characters" = "mean_tweet_length", 
           "Average Favorites Per Tweet" = "fav_average", 
           "Average Retweets Per Tweet" = "rt_average") 

# Data used for "Word Cloud" page.

cloud <- read_rds("word_analysis/df.rds")

# Data used for "Sentiment Analysis" page.

wr_bing <- read_rds("word_analysis/wr_sentiment_bing.rds")
wr_nrc <- read_rds("word_analysis/wr_sentiment_nrc.rds")

# Data used for "Explore" page.

usr_tweets <- read_rds("clean_data/user_tweets.rds")
top_50wr <- read_rds("clean_data/top50wr.rds")
top_50hr <- read_rds("clean_data/top50hr.rds")

####################################                 
#### USER INTERFACE ####
####################################

# Define UI for the application, including a navigation bar at the top.
# Set theme of the app to simplex as I liked its minimalist, aesthetic layout.

ui <- navbarPage("Project Resilience", theme = shinytheme("simplex"),
                
                 
####################################
#### WORD ANALYSIS ####
####################################
                 
tabPanel("Word Cloud",
         
         fluidPage(
             
             titlePanel("Words Associated with Resilience"),
             
             p(paste("Resilience, defined by researchers at Columbia’s Department of Human Development,
                     'refers to a dynamic process encompassing positive adaptation within the context of 
                     significant adversity.'")),
                     
             p(paste("The word cloud below is derived from tweets that use #resilience. By showcasing which words
                      appear the most often in tweets using #resilience, the plot provides a broader picture of the kinds of themes/categories 
                      that people associate with resilience. What themes do you observe? Are they what you would expect?")),
                              
             br(),
                              
             wordcloud2Output('wordcloud2'),
                              
             br()
                              
             )),
                 
                                 
####################################
#### GENERAL SUMMARY ####
####################################

# Page is a general summary of interesting findings from two of the sets of data.
# Did not show the same information for each dataset to keep my outputs diverse!
                 
tabPanel("General Summary",
         
         fluidPage(
             
             titlePanel("Summary Statistics"),
             
             br(),
                                  
             # The main panel will feature text and three of the visualizations for this tab.
                                  
             mainPanel(
                                  
             "The graphic below shows the number of tweets mentioning resilience over a 9 day period. 
             Why might the graph spike on November 11th?",
                                      
             br(),
                                      
             # This function pulls the created visualization from the server below, and outputs
             # it in the app.
                                      
             h3("Frequency of 'resilience' Tweets"),
                                      
             plotOutput("tweet_freq1"),
                                      
             br(),
             br(),
                                     
             p(paste("The following compares various summary statistics for the different users,
             providing insight into the more general patterns of their twitter usage and
             the kinds of reactions their tweets receieve.")),
                                  
             br(),
                                      
             h3("Summary of Users' Tweets"),
                                
             br(),
                                      
             # The DT package is helps to make interactive tables.
                                      
             DTOutput("summary_table"),
                                      
             br(),
             br()
                                
             ))),
                 

####################################
#### SENTIMENT ANALYSIS ####
####################################

tabPanel("Sentiment Analysis",
         
         fluidPage(
             
             titlePanel("Sentiment Analysis"),
             
             br(),
             
             p(paste("Using a method called Sentiment Analysis, we can discern what sentiments are 
                              commonly associated with resilience. This is done by checking each tweet
                              for specific 'baskets of words,' which are known as lexicons. I chose to use the 'nrc' and
                              'bing' lexicons.")),
                        
             br(),
                              
             p(paste("The nrc lexicon provides a list of (english) words and their associations with eight basic emotions and 
             two sentiments: anger, fear, anticipation, trust, surprise, sadness, joy, as well as negative and positive. The analysis
             assigns every word in a specific tweet to one of these words, and, based off of these results, it assigns a word to the tweet 
             as a whole. The histogram below compares the proportions of tweets for each feeling/sentiment. What do you think these results
             indicate about the content of tweets that mention resilience?")),
                              
             br(),
                              
             plotlyOutput("nrc"),
                              
             br(),
                              
             p(paste("The bing lexicon is  more straightforward than the nrc lexicon. It provides a list of (english) words and their associations  
             with only two sentiments: negative or positive. Thus, the analysis assigns every word in a specific tweet to one of the two, and, 
             based off of these results, it assigns a positive or negative sentiment to the tweet as a whole. The histogram below compares the proportions 
             of tweets that are positive versus negative. To what extent do these results align with the previous graph? What can we conclude about 
             the way in which the word 'resilience' is used and understood?")),
                               
             br(),
                              
             plotlyOutput("bing")
                              
             )),
                 
####################################                 
#### EXPLORE TWEETS ####
####################################

tabPanel("Explore Tweets",
         
         fluidPage(
             
             titlePanel("Top 50 Tweets Mentioning Resilience"),
             
             br(),
             
             p(paste("Explore the the 50 most popular tweets mentioning resilience, as measured by the number of times the tweet 
             was favorited.")),
             
             # Word resilience tweets.
             
             h3("Tweet Search"),
             
             # DTable Keyword Input - input$keyword1
             
             textInput("keyword1", "Please enter a keyword to search, e.g. strength", "strength"),
             
             DTOutput("word_table1")
             
             )),

####################################                 
#### EXPLORE HASHTAG ####
####################################

tabPanel("Explore Hashtags",
         
         fluidPage(
             
             titlePanel("Explore Top 50 #resilience Tweets"),
             
             # Hashtag resilience tweets.
             
             br(),
             
             p(paste("Explore the the 50 most popular tweets using #resilience, as measured by the number of times the tweet 
             was favorited.")),
             
             h3("Tweet Search"),
             
             # DTable Keyword Input - input$keyword2
             
             textInput("keyword2", "Please enter a keyword to search, e.g. support", "support"),
             
             DTOutput("word_table2"),
             
             br()
             
             )),
                            
####################################                 
#### EXPLORE USERS ####
####################################

tabPanel("Explore Users",
         
         fluidPage(
             
             titlePanel("Explore 'Resilience' Users' Tweets"),
             
             br(),
             
             p(paste("Explore the tweets of the six users. How do they differ in their content?")),
             
             # User tweets.
             
             h3("Tweet Search"),
             
             # DTable Keyword Input - input$keyword3
             
             textInput("keyword3", "Please enter a keyword to search, e.g. climate", "climate"),
             
             DTOutput("word_table3"),
             
             br()
             
             )),

####################################                 
#### ABOUT PAGE ####
####################################

# Page serves to give an overview of the "why", "what", and "how" of the project. 

tabPanel("About",
         
         fluidPage(
             
             # Panel title, repeated many times throughout the app.
             
             titlePanel("Investigating Resilience Using Twitter"),
             
             # The br() function adds white space to the app, repeated many times.
             
             br(),
             
             # The p(paste("text")) function is how I insert text into the app, repeated many times.
             
             p(paste("Who is most capable of overcoming personal challenges? How do divided countries reconcile themselves, 
             moving forward as one nation? How do people across time and space make sense of pain and tragedy, in order 
             to move forward? To what extent is resilience a narrative we choose, versus an intrinsic or learned quality?")),
             
             br(),
             
             p(paste("The word “resilience” is a buzz-word in many popular and academic spheres. It is a concept that can be both 
             aspirational and feared, offering a lens for understanding the interlocking of past and present, for addressing crisis, 
             and for living a meaningful life. It also offers a framework for understanding why certain patterns across history tend 
             to re-emerge.")),
             
             br(),
             
             p(paste("But it is not just resilient people, systems, and ideas that have endured across history and the globe: 
             it is the idea of resilience itself.")),
             
             br(),
             
             p(paste("This project aims to investigate how the concept of resilience enters into public discourse—specifically Twitter.
             Twitter acts as a barometer for popular sentiments and understandings, reflecting and forecasting the changing attitudes 
             and opinions of different groups. By downloading tweets that were posted between November 6th and November 15th 2019, using 
             the Twitter API, this project is an attempt to conduct preliminary research on the concept of resilience, and the different 
             ways in which it is applied. It downloads three kinds of tweets: 1) tweets that mention the word “resilience”; 2) tweets that 
             use #resilience; 3) tweets from six users (all non-profit organizations) with a twitter name that includes the word resilience.")),
             
             br(),
             
             p(paste("These six users were selected for their diversity, each aspiring to engender resilience in a different context (social, environmental,
             economic, etc). The Global Resilience Institute (Resilience_NU) at Northeastern, is a univeristy-wide, interdisciplinary 
             effort that collaborates with different experts devise solutions to resilience challenges. The Childhood Resilience Foundation (resiliencefnd) is 
             aims to protect youth and empowering the underserved. The UNDP's Resilience Center (UNDP_GCRED) works in fragile ecosystems to promote inclusive development with
             a focus on environmental resilience. The Resilience Shift (resilienceshift) is dedicated to creating more resilient infastructure. Resilience Code
             tries to make more resilient bodies. The American Resilience Project's (AmResilience) goal is to foster American resilience using the power of stories.
             By comparing the tweets made by these different users, we can compare different visions of resilience more directly, while highlighting how
             this concept transcends across various disciplines.")),
             
             br(),
             
             p(paste("Using this data, essentially, the project asks and starts to try and answer, what does the general public mean when the use the term 
             “resilience”? What social valences/affect does this word possess? What uses of the word resilience, receive the most appraisal? In the following six tabs, 
             you will have the opportunity to explore what I believe to be the most interesting and relevant analyses of this data. Enjoy!")),
             
             br(),
             
             p(paste("Created by Aysha Emmerson, November 2019, Gov 1005 'Data', Harvard College. GitHub repo: https://github.com/ayshaemmerson/project_resilience")),
             
             br()
         )
),

####################
#### FOOTNOTES ####
#####################
                 
tabPanel("Footnotes",
         
         fluidPage(
             
             # Acknowledgements page at the end.
             # Does not require connection to the server.
             
             titlePanel("Acknowledgments"),
             
             br(),
             
             p(paste("I'd like to acknowledge William Smiles, Hemanth Bharatha Chakravarthy, and Tanner Gildea, for
             their formidable final projects for Gov 1005. Thank you for writing such clear comments and code,
             which I used to help guide my own project.")),
             
             br(),
             
             p(paste("The article mentionned on the 'Word Analysis' page can be cited as follows:
             Luthar, S S et al. “The construct of resilience: a critical evaluation and guidelines for future work.” 
             Child development vol. 71,3 (2000): 543-62. doi:10.1111/1467-8624.00164.")),
             
             br(),
             
             p(paste("Check out the link https://towardsdatascience.com/create-a-word-cloud-with-r-bde3e7422e8a,
             which in exceedingly helpful when learning how to create word clouds. Also, check out 
             https://medium.com/@GalarnykMichael/accessing-data-from-twitter-api-using-r-part1-b387a1c7d3e
             for coherent instructions on how to access data from twitter by making an API request.")),
     
             br(),
                              
             p(paste("I would like to thank Preceptor, the Gov 1005 TFs and CAs, and my fellow classmates, for all 
             of their support in learning R, throughout the semester. In particular, thank you to Claire Fridkin, whose
             Study Halls were so great that I would structure my entire weekend around them.")),
                              
             br(),
                              
             p(paste("Finally, I would like to thank my brother Jasper, blockmates Kendra and Luke, as well as all of my other friends 
             for being there during all of the ups and downs of what has been, without competition, the most time
             consuming, tedious, intensive class that I have ever taken - but also one of the most rewarding.")),
                              
             br()
                              
             )))


###################################
#### SERVER ####
###################################

# Define server logic required to create all the visualizations above. This is what's 
# "under the hood" of the app itself.

server <- function(input, output) {

####################################
#### WORD ANALYSIS ####
####################################
    
# Output for word cloud.
    
    output$wordcloud2 <- renderWordcloud2({
        wordcloud2(data=cloud, size=0.9, color='random-dark')
    })
    
####################################
#### GENERAL SUMMARY ####
####################################
    
    # Tweet frequency histogram.
    
    output$tweet_freq1 <- renderPlot({

        ts_plot(bydate) 
        
    })
    
    # Summary table.
    
    output$summary_table <- renderDT(
        
        summary_table,
        
        # These are additional attributes particular to the DT package.
        
        class = 'display', 
        rownames = FALSE,
        options = list(dom = 't')
        
    )
    
####################################
#### SENTIMENT ANALYSIS ####
####################################
    
    # NRC output for "word resilience."
    
    output$nrc <- renderPlotly({
        
        ggplot(wr_nrc, aes(x = sentiment, y = n, fill = sentiment)) +
            geom_bar(position = "dodge", stat = "identity") +
            labs(title = "Emotions Conveyed by Tweets",
                 subtitle = "Per NRC",
                 x = "Sentiment",
                 y = "Count",
                 fill = "Sentiment") +
            theme(axis.title.x=element_blank(),
                  axis.text.x = element_text(angle = 60, hjust = 1))
    })
    
    # Bing output for "word resilience."
    
    output$bing <- renderPlotly({
        
        ggplot(wr_bing, aes(x = sentiment, y = n, fill = sentiment)) +
            geom_bar(position = "dodge", stat = "identity") +
            labs(title = "Number of Positive and Negative Tweets",
                 subtitle = "Per BING",
                 x = "Sentiment",
                 y = "Count",
                 fill = "Sentiment") +
            theme(axis.title.x=element_blank(),
                  axis.text.x = element_text(angle = 60, hjust = 1))
    })
    
####################################                 
#### EXPLORE TWEETS ####
####################################
    
    # DTable of Tweets. Filters for "text" containing the input$keyword. 
    # Selection = 'single' enables only one row (and date cell) to be selected at a time.
    # Format style is used to change the cursor to a more click-friendly pointer over the date column.
    # list(dom = "tip") removes the defualt search bar that comes with DTables.
    
    # Explore top 50 "word resilience" tweets.

    output$word_table1 <- renderDT({

        datatable(top_50wr %>% filter(str_detect(text, input$keyword1)),
                  class = 'display',
                  rownames = FALSE,
                  selection = 'single',
                  colnames = c('Tweet Text', 'User', 'Date/Time', 'Retweets', 'Favorites'),
                  options = list(dom = 'tip')
        ) %>% formatStyle(2, cursor = 'pointer')
    })
    
    
####################################                 
#### EXPLORE HASHTAGS ####
####################################

    # Explore top 50 "#resilience" tweets.

    output$word_table2 <- renderDT({

        datatable(top_50hr %>% filter(str_detect(text, input$keyword2)),
                  class = 'display',
                  rownames = FALSE,
                  selection = 'single',
                  colnames = c('User', 'Date/Time', 'Tweet text', 'Retweets', 'Favorites'),
                  options = list(dom = 'tip')
        ) %>% formatStyle(2, cursor = 'pointer')
    })

####################################                 
#### EXPLORE USERS ####
####################################    
    
    # Explore all "resilience users" tweets.
    
    output$word_table3 <- renderDT({
        
        datatable(usr_tweets %>% filter(str_detect(text, input$keyword3)),
                  class = 'display',
                  rownames = FALSE,
                  selection = 'single',
                  colnames = c('Tweet text', 'User', 'Date/Time', 'Retweets', 'Favorites'),
                  options = list(dom = 'tip')
        ) %>% formatStyle(2, cursor = 'pointer')
    })
    
}


# Run the application 
shinyApp(ui = ui, server = server)