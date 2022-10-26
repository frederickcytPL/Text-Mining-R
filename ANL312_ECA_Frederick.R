#For ANL312 Text Mining & Applied Project Formulation.
#By Frederick Chng Yu Tao, BSc Business Analytics
#All packages belongs solely to their respective owners, usage is only for academia purposes
#This project has no commercial intent.



install.packages("twitteR")
install.packages("tidytext")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("wordcloud")
install.packages("topicmodels")
install.packages("tm")
install.packages("textdata")



#Creating the setup needed to link to Twitter Developer API.

library("twitteR")

consumerKey = 'XXXXXXXXXX'
consumerSecret ='XXXXXXXXXX'
accessToken = 'XXXXXXXXXX'
accessTokenSecret = 'XXXXXXXXXX'
setup_twitter_oauth(consumerKey,consumerSecret,accessToken,accessTokenSecret)


#Where n = number of tweets to retrieve and lang = language.
tweetsDataFrame = searchTwitter("inflation", n=6000, lang="en")
newDataFrame <- do.call("rbind", lapply(tweetsDataFrame, as.data.frame))


#Create CSV from Data Frame
write.csv(newDataFrame, file = '/Users/fcyt/Documents/ANL312/ECA/twitterData.csv')


library(tm)
library(topicmodels)
library(tidyverse)


#Conversion to data frame using the twListtoDF function
tweets_DF <- twListToDF(tweetsDataFrame)
#extract the data frame save it locally
saveRDS(tweets_DF, file="inflation_tweets.rds")
#newTweets_DF <- readRDS("inflation_tweets.rds")

tweets = readRDS('inflation_tweets.rds')
writeLines(as.character(tweets[[1]]))


#tweets = readRDS('inflation_tweets.rds')

#tweets = gsub("http.+ |http.+$", " ", tweets)  # Remove html links

#Data Cleansing

tweets <- iconv(tweets, to = "ASCII", sub = " ")  
tweets <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets)  # Remove the "RT" (retweet) and usernames 
tweets = gsub("http[[:alnum:]]*", "", tweets)
tweets = gsub("[[:punct:]]", " ", tweets)  # Remove punctuation
tweets = gsub("[ |\t]{2,}", " ", tweets)  # Remove tabs
tweets = gsub("^ ", "", tweets)  # Leading blanks
tweets = gsub(" $", "", tweets)  # Lagging blanks
tweets = gsub(" +", " ", tweets) # General spaces 
tweets = tolower(tweets)
tweets = unique(tweets)

#Periodic Checking
writeLines(as.character(tweets[[1]]))



#Creation of Corpus

install.packages("corpus")

library("corpus")

corpus <- Corpus(VectorSource(tweets))

corpus <- tm_map(corpus, removeWords, stopwords("english"))  
corpus <- tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, removeWords, c("amp","apple","buttigieg","download", "iphone", "android", "twitter", 'nofollow', 'com', 'href', 'true', 'false','mobile','web','app','rel'))

writeLines(as.character(tweets[[1]]))
print(corpus)


#Creation of Word Cloud

library(wordcloud)
set.seed(5000)
colour_palette  = brewer.pal(3, 'Dark2')
wordcloud(corpus, min.freq = 200, scale = c(4, 1) , random.order = TRUE, col = colour_palette)





#Creation of document term matrix

dtmatrix = DocumentTermMatrix(corpus)

doc.length = apply(dtmatrix, 1, sum)
dtmatrix = dtmatrix[doc.length > 0,]
dtmatrix

inspect(dtmatrix[1:2,10:15])

library(dplyr)
freq = colSums(as.matrix(dtmatrix))
length(freq)

ord = order(freq, decreasing = TRUE)
freq[head(ord, n = 20)]

#Visualising word frequency

plot = data.frame(words = names(freq), count = freq)
library(ggplot2)
plot = subset(plot, plot$count > 150) #creating a subset of words having more than 100 frequency
str(plot)
ggplot(data = plot, aes(words, count)) + geom_bar(stat = 'identity') + ggtitle('Words used more than 150 times')+coord_flip()


#LDA Topic Modeling

library(topicmodels)
#LDA model with 5 topics selected
lda_5 = LDA(dtmatrix, k = 5, method = 'Gibbs', 
            control = list(nstart = 5, seed = list(1505,99,36,56,88), best = TRUE, 
                           thin = 500, burnin = 4000, iter = 2000))

top10terms_5 = as.matrix(terms(lda_5,10))
top10terms_5





library(tidytext)

library(dplyr)

library(textdata)

library(ggplot2)


#Tokenizing character vector file 'tweets'.
token = data.frame(text=tweets, stringsAsFactors = FALSE) %>% unnest_tokens(word, text)

#Matching sentiment words from the 'loughran' sentiment lexicon
senti = inner_join(token, get_sentiments("loughran")) %>% count(sentiment)
senti$percent = (senti$n/sum(senti$n))*100

#Plotting the sentiment summary 
ggplot(senti, aes(sentiment, percent)) +   
  geom_bar(aes(fill = sentiment), position = 'dodge', stat = 'identity')+ 
  ggtitle("Inflation Sentiment analysis based on lexicon: 'Loughran'")+
  coord_flip() +
  theme(legend.position = 'none', plot.title = element_text(size=14, face = 'bold'),
        axis.text=element_text(size=16),
        axis.title=element_text(size=14,face="bold"))

