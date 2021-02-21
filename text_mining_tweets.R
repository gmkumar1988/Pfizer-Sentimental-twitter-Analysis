# Install
install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes
install.packages("syuzhet") # for sentiment analysis
install.packages("ggplot2") # for plotting graphs
install.packages("tidytext") # for text mining
install.packages("dplyr")
install.packages("magrittr")
install.packages("textclean")
install.packages("qdapRegex")
install.packages("stringr")

# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("syuzhet")
library("ggplot2")
library(lubridate)
library(dplyr)
library(tidytext)
library(readr)
library(magrittr)
library(textclean)
library(qdapRegex)
library(stringr)


options(warn = -1)

raw_tweet_vaccine = read.csv("D:\\Datasetssss\\pfizer_tweets\\vaccination_tweets.csv", 
                         header = TRUE, stringsAsFactors = FALSE)


####na.strings = c("#","@","&"))
                         

View(raw_tweet_vaccine)

head(raw_tweet_vaccine)
str(raw_tweet_vaccine)
summary(raw_tweet_vaccine)

head(raw_tweet_vaccine)


install.packages("textclean")
library(textclean)
install.packages("qdapRegex")
library(qdapRegex)


raw_tweet_vaccine_text = rm_url(raw_tweet_vaccine$text)
raw_tweet_vaccine_user_location = raw_tweet_vaccine$user_location
raw_tweet_vaccine_hashtags = raw_tweet_vaccine$hashtags
raw_tweet_retweet_check = raw_tweet_vaccine$is_retweet
raw_tweet_user_verification = raw_tweet_vaccine$user_verified
raw_tweet_user_favorite = raw_tweet_vaccine$favorites
raw_tweet_user_retweet = raw_tweet_vaccine$retweets
###raw_tweet_vaccine$date = as.numeric(month(raw_tweet_vaccine$date))
###raw_tweet_user_date_month = raw_tweet_vaccine$date

raw_tweet_vaccine_date_month = format(as.Date(raw_tweet_vaccine$date), "%Y-%m")


complete.cases(raw_tweet_vaccine_user_location)
tweet_vaccine_user_location = raw_tweet_vaccine_user_location

###This package is used to replace NA values 
###install.packages("naniar")
###library(naniar)

###Replacing Empty value in the string to NA :
raw_tweet_vaccine_user_location[raw_tweet_vaccine_user_location == ''] = NA
tweet_vaccine_date = raw_tweet_vaccine_date_month

####Alternate 
raw_tweet_vaccine_user_location[raw_tweet_vaccine_user_location == 'دب�S, ا�"ا�.ارات ا�"عرب�Sة ا�"�.تحدة
'] = NA
tweet_vaccine_user_location = raw_tweet_vaccine_user_location


### This view is to calculate the tweet metrics 
tweet_metrics = data.frame(tweet_vaccine_user_location,
                           tweet_vaccine_user_verification_true,
                           raw_tweet_vaccine_date_month,
                           raw_tweet_user_favorite,
                           raw_tweet_user_retweet)

tweet_vaccine_user_location = na.omit(tweet_vaccine_user_location)

View(tweet_vaccine_user_location)

###Grouping the data by location:
tweet_vaccine_user_location = tweet_metrics %>% group_by(raw_tweet_vaccine_user_location)



###Grouping the True data based on userverification:
tweet_vaccine_user_verification_true = raw_tweet_user_verification [raw_tweet_vaccine$user_verified == "True"]


tweet_vaccine_user_verification_true = raw_tweet_vaccine$user_verified

tweet_vaccine_hashtags = raw_tweet_vaccine_hashtags 

View(tweet_vaccine_user_verification_true)

str(tweet_vaccine_user_location)
head(tweet_vaccine_user_location)

tweet_vaccine_user_location [tweet_vaccine_user_location  == ""] = NA
tweet_vaccine_hashtags [tweet_vaccine_hashtags  == ""] = NA


###Removal Special Characters in single column in the dataset:
tweet_vaccine_hashtags = gsub("[[:punct:]]", "",tweet_vaccine_hashtags)
tweet_vaccine_user_location = gsub("[[:punct:]]","",tweet_vaccine_user_location)

###to be checked:
sentimental_analysis = data.frame(tweet_vaccine_date,tweet_vaccine_user_location,tweet_vaccine_hashtags,tweet_vaccine_user_verification_true)


###Identify Most Frequent Words in the single column using R:
tweet_vaccine_hashtags[tweet_vaccine_hashtags != "NA"]
tweet_vaccine_user_location[tweet_vaccine_user_location != "NA"]

tweet_vaccine_hashtags[tweet_vaccine_hashtags != "na"]
tweet_vaccine_user_location[tweet_vaccine_user_location != "na"]

tweet_vaccine_hashtags_collapse = paste(tweet_vaccine_hashtags, collapse = " ")
tweet_vaccine_user_location_collapse = paste(tweet_vaccine_user_location, collapse = " ")


tweet_vaccine_table_setup = data_frame(Text = tweet_vaccine_hashtags_collapse)
tweet_vaccine_user_loc_setup = data_frame(Text = tweet_vaccine_user_location_collapse)

text_tweet_words = tweet_vaccine_table_setup %>% unnest_tokens(output = word, input = Text)
text_tweet_location = tweet_vaccine_user_loc_setup %>% unnest_tokens(output = word, input = Text)

data("stop_words")

text_tweet_words = text_tweet_words %>% anti_join(stop_words)

text_tweet_location = text_tweet_location %>% anti_join(stop_words)

text_tweet_words = text_tweet_words %>% count(word, sort = TRUE)

text_tweet_location = text_tweet_location %>% count(word, sort = TRUE)

text_tweet_words

text_tweet_location

### this is used to remove the "na" values in the dataset:
text_tweet_location = text_tweet_location[-1,] 

View(text_tweet_location)

str(text_tweet_words)


text_tweet_words = text_tweet_words %>%
  top_n(21)
colourCount = length(unique(text_tweet_words$word))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))



text_tweet_words %>% mutate(word = reorder(text_tweet_words$word,text_tweet_words$n)) %>% ggplot(aes(x = text_tweet_words$word, y = text_tweet_words$n)) + labs(subtitle = "Example - Pfizer Vaccine Sentiment ", x = "Tweet Words Used by the People", y = "Number of tweets about this vaccine
") + 
  geom_col(fill = getPalette(colourCount)) +
  coord_flip()



text_tweet_location = text_tweet_location %>% filter(tweet_vaccine_user_location != "earth") %>%
  top_n(21)
colourCount = length(unique(text_tweet_location$word))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

text_tweet_location %>% mutate(word = reorder(text_tweet_location$word,text_tweet_location$n)) %>% top_n(500) %>% ggplot(aes(x = text_tweet_location$word, y = text_tweet_location$n)) + labs(subtitle = "Pfizer Vaccine Sentimental Analysis By Location Sample Tweets", x = "Tweet by the People by location", y = "Number of tweets about this vaccine
") + geom_point() + geom_text(aes(label=text_tweet_location$n)) +
  geom_col(fill = getPalette(colourCount)) +
  coord_flip()




set.seed(12345)


##wordcloud graph packages:
install.packages(c("tm", "SnowballC", "wordcloud", "RColorBrewer", "RCurl", "XML"))

install.packages("XML")
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(RCurl)
library(XML)

text_tweed_wordcloud = wordcloud(words = text_tweet_words$word,freq = text_tweet_words$n,min.freq = 1,max.words = 20000, random.order = FALSE,rot.per = 0.35,
          colors = brewer.pal(14,"Dark2"), font = 32, title = "Tweet Text Mining Analysis ")


View(sentimental_analysis)
str(sentimental_analysis)
head(sentimental_analysis)
View(sentimental_analysis)

graph = sentimental_analysis %>% filter(tweet_vaccine_user_location != "NA" & tweet_vaccine_hashtags != "NA" ) %>% group_by(tweet_vaccine_user_location, tweet_vaccine_date)

location = sentimental_analysis %>% filter(tweet_vaccine_user_location != "NA" & tweet_vaccine_hashtags != "NA" )

head(location)

nrow(graph)

View(graph)



###This exercise is for Twitter Metrics:
tweet_metrics_main = data.frame(raw_tweet_vaccine_date_month,raw_tweet_user_favorite, raw_tweet_user_retweet)

View(tweet_metrics_main)

temp = tweet_metrics_main %>% group_by(raw_tweet_vaccine_date_month)
temp = temp %>% tally()


ggplot(temp, aes(raw_tweet_vaccine_date_month,n)) + geom_line() + geom_point()+geom_line()
  theme(legend.position="bottom",plot.title=element_text(hjust=0.5))+
  labs(x="Date Of Tweet",y="Number of Tweets verified users",title="Twitter Users Taken Pfizer Vaccination Drug for Covid")+
  scale_y_continuous(limits=c(0,200),breaks=seq(0,200,25))

###%>% select(raw_tweet_user_favorite,raw_tweet_user_retweet)

        





head(raw_tweet_vaccine_text)

clean_tweet_vaccine_text = paste(unlist(str_extract_all(raw_tweet_vaccine_text,
                                                        '[0-9a-zA-Z ]+')),collapse = '')

###This package is used to identify the most frequent words in the text:
library(tm)
raw_tweet_vaccine_text = tolower(raw_tweet_vaccine_text)
raw_tweet_vaccine_text = removePunctuation(clean_tweet_vaccine_text)
raw_tweet_vaccine_text = removeNumbers(clean_tweet_vaccine_text)
myCorpus = Corpus(VectorSource(clean_tweet_vaccine_text))
tweet_freq_words <- TermDocumentMatrix(myCorpus)
frequency_tweets = findFreqTerms(tweet_freq_words)

head(raw_tweet_vaccine_text)
summary(raw_tweet_vaccine_text)

head(raw_tweet_vaccine_user_location)


tweet_vaccine = data.frame(raw_tweet_vaccine_user_location,
                           raw_tweet_vaccine_text)



head(tweet_vaccine, n=10)

str(tweet_vaccine)

View(tweet_vaccine)



par(mfrow = c(1,2))








############################################Alternate View - Not to touch now 
head(raw_tweet_vaccine_text)

tweet_words = raw_tweet_vaccine_text



raw_tweet_vaccine_hashtags = paste(unlist(str_extract_all(raw_tweet_vaccine_hashtags, '[0-9a-zA-Z ]+')),
                                   collapse = '')
raw_tweet_vaccine_user_location = paste(unlist(str_extract_all(raw_tweet_vaccine_user_location, '[0-9a-zA-Z ]+')),
                                        collapse = '')

raw_tweet_vaccine_text = paste(unlist(str_extract_all(raw_tweet_vaccine_text,' [0-9a-zA-Z ]+')), 
                               collapse = '')

raw_tweet_vaccine_date = raw_tweet_vaccine$date


tweet_vaccine = data.frame(raw_tweet_vaccine_date,
                           raw_tweet_vaccine_user_location,
                           raw_tweet_vaccine_text,
                           raw_tweet_vaccine_hashtags)

View(tweet_vaccine)




###Example for Removing URL from the text or column file:

###install.packages("textclean")
###library(textclean)
###install.packages("qdapRegex")
###library(qdapRegex)

###Example for removing special characters in string : 
###install.packages("stringr")


###m = "ll#$@$!@hjgds fsd f#!#!@"
###paste(unlist(str_extract_all(m, '[0-9a-zA-Z ]+')),collapse = '')

###x <- "a1~!@#$%^&*(){}_+:\"<>?,./;'[]-=" 
####str_replace_all(x, "[^[:alnum:]]", " ")


###x <- " I like www.talkstats.com and http://stackoverflow.com"
###rm_url(x)

###Distribution of Tweets 
tweet_vaccine_month = format(as.Date(tweet_vaccine$date), "%m")
tweet_vaccine_year = format(as.Date(tweet_vaccine$date), "%Y")


tweet_vaccine$text = as.character(tweet_vaccine$text)

tweet_vaccine_text = tweet_vaccine$text

tweet_vaccine_textdoc = Corpus(VectorSource(tweet_vaccine_text))

#Replacing "/", "@" and "|" with space

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))

tweet_vaccine_textdoc <- tm_map(tweet_vaccine_textdoc, toSpace, "/")

tweet_vaccine_textdoc <- tm_map(tweet_vaccine_textdoc, toSpace, "T")
tweet_vaccine_textdoc <- tm_map(tweet_vaccine_textdoc, toSpace, "@")
tweet_vaccine_textdoc <- tm_map(tweet_vaccine_textdoc, toSpace, "|")

# Convert the text to lower case
tweet_vaccine_textdoc <- tm_map(tweet_vaccine_textdoc, content_transformer(tolower))
# Remove numbers
tweet_vaccine_textdoc <- tm_map(tweet_vaccine_textdoc, removeNumbers)
# Remove english common stopwords
tweet_vaccine_textdoc <- tm_map(tweet_vaccine_textdoc, removeWords, stopwords("english"))
# Remove your own stop word
# specify your custom stopwords as a character vector
tweet_vaccine_textdoc <- tm_map(tweet_vaccine_textdoc, removeWords, c("s", "company", "team")) 
# Remove punctuations
tweet_vaccine_textdoc <- tm_map(tweet_vaccine_textdoc, removePunctuation)
# Eliminate extra white spaces
tweet_vaccine_textdoc <- tm_map(tweet_vaccine_textdoc, stripWhitespace)
# Text stemming - which reduces words to their root form
tweet_vaccine_textdoc <- tm_map(tweet_vaccine_textdoc, stemDocument)


# Build a term-document matrix
TextDoc_dtm <- TermDocumentMatrix(tweet_vaccine_textdoc)
dtm_m <- as.matrix(TextDoc_dtm)
# Sort by descearing value of frequency
dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)
# Display the top 5 most frequent words
head(dtm_d, 5)













####tidy_tweet = tweet_vaccine %>% unnest_tokens(word,text)

summary(tweet_vaccine$user_created)

summary(tweet_vaccine$user_location)

class(tweet_vaccine$user_created)

tweet_vaccine_positive = tweet_vaccine %>%
  count(word, sort = TRUE)








