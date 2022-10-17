# setwd("/Applications/WSMA")
# getwd()

#Clear the Environment
rm(list=ls(all=T))

##################################
#### Code to download tweets  ####
#################################
install.packages("rtweet")
## load rtweet
library(rtweet)

## store api keys (these are fake example values; replace with your own keys)
api_key <- "oomm4xzwpI7aglXUDPawyzc4C"
api_secret_key <- "fNFmq50qLvOrz31eO8i0hD5OiG28U0AoZgI3CL9s7OzLdzeCX2"

## authenticate via web browser
token <- create_token(
  app = "WSMA-SentimentAnalysis",
  consumer_key = api_key,
  consumer_secret = api_secret_key)
## view token (you should see the correct app name)
token
get_token()
## search for 3000 tweets using the rstats hashtag
VodafoneIdea <- search_tweets("#VodafoneIdea", n = 3000, include_rts = FALSE,token = NULL,retryonratelimit = TRUE, lang = "en")
## Write csv file
library(data.table)
fwrite(VodafoneIdea, file ="VodafoneIdea.csv")

#NOTE : https://developer.twitter.com/en/docs/tweets/search/overview
#Since the twitter api gives recent tweets of just 7 days, we could hardly get maximum 450 tweets for 2 weeks for our topic vodafoneidea.
#Because of which we have used below python code to get more than 1000 tweets.

######Python code
# pip install twitterscraper
# from twitterscraper import query_tweets
# import datetime as dt
# import pandas as pd
# begin_date = dt.date(2019,10,1)
# end_date = dt.date(2020,3,17)
# limit = 2000
# lang = 'english'
# tweets = query_tweets('#vodafoneidea', begindate = begin_date, enddate = end_date, limit = limit, lang = lang)
# df = pd.DataFrame(t.__dict__ for t in tweets)
# df.head()
# df.to_csv('VodafoneIdea11.csv', index=False)

#####Read Twitter Data
tweets.df <- read.csv("VodafoneIdea11.csv",stringsAsFactors = FALSE)
tweets.df$timestamp <- as.Date(tweets.df$timestamp, format= "%Y-%m-%d")
View(tweets.df)

dim(tweets.df)
str(tweets.df)
install.packages("rJava")
#Load the required R libraries
# install.packages("SnowballC")
# install.packages("tm")
# install.packages("wordcloud")
# install.packages("syuzhet")
install.packages("NCmisc")

install.packages("qdap")
library(SnowballC)
library(NLP)
library(tm)
library(ggplot2)
library(RColorBrewer)
library(wordcloud)
library(topicmodels)
library(data.table)
library(stringi)
library(syuzhet)
library(dplyr)
library(plyr)
library(grid)
library(gridExtra)
library(NCmisc)

Sys.setenv(JAVA_HOME='C:/Program Files/Java/jdk1.8.0_241')
library(rJava)
library(xlsxjars)
library(xlsx)
library(RCurl)
library(stringdist)
library(venneuler)
library(XML)
library(qdapTools)
library(qdap)
library(RSentiment)
library(DT)
#*******************************************************************************************
# Process/clean the Twitter data
#*******************************************************************************************

# Create document corpus with tweet text
myCorpus<- Corpus(VectorSource(tweets.df$text)) 
writeLines(strwrap(myCorpus[[242]]$content,60))

# convert to Lowercase
myCorpus <- tm_map(myCorpus, content_transformer(stri_trans_tolower))
writeLines(strwrap(myCorpus[[242]]$content,60))

# Remove the links (URLs)
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)  
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
writeLines(strwrap(myCorpus[[242]]$content,60))

#Replace comma with space
removeComma <- function(x) gsub(",", " ", x)   
myCorpus <- tm_map(myCorpus, content_transformer(removeComma))
writeLines(strwrap(myCorpus[[1328]]$content,60))

# Remove Single letter words
removeSingle <- function(x) gsub(" . ", " ", x)   
myCorpus <- tm_map(myCorpus, content_transformer(removeSingle))
writeLines(strwrap(myCorpus[[242]]$content,60))

# Remove anything except the english language and space
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)   
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))
writeLines(strwrap(myCorpus[[242]]$content,60))

#Replace words
replace1 <- function(x) gsub("calls", "call", x)
myCorpus <- tm_map(myCorpus, content_transformer(replace1))

replace2 <- function(x) gsub("days", "day", x)
myCorpus <- tm_map(myCorpus, content_transformer(replace2))

replace3 <- function(x) gsub("issues", "issue", x)
myCorpus <- tm_map(myCorpus, content_transformer(replace3))

replace4 <- function(x) gsub("ghatiya", "worst", x)
myCorpus <- tm_map(myCorpus, content_transformer(replace4))

replace5 <- function(x) gsub("gatiya", "worst", x)
myCorpus <- tm_map(myCorpus, content_transformer(replace5))

replace6 <- function(x) gsub("bakwaas", "worst", x)
myCorpus <- tm_map(myCorpus, content_transformer(replace6))

replace7 <- function(x) gsub("fucbullshit", "bullshit", x)
myCorpus <- tm_map(myCorpus, content_transformer(replace7))

replace8 <- function(x) gsub("pictwitter[A-z]*", " ", x)
myCorpus <- tm_map(myCorpus, content_transformer(replace8))

replace9 <- function(x) gsub("mails", "mail", x)
myCorpus <- tm_map(myCorpus, content_transformer(replace9))

replace10 <- function(x) gsub("service provider", "provider", x)
myCorpus <- tm_map(myCorpus, content_transformer(replace10))

replace11 <- function(x) gsub("backs", "back", x)
myCorpus <- tm_map(myCorpus, content_transformer(replace11))

replace12 <- function(x) gsub("complaints", "complaint", x)
myCorpus <- tm_map(myCorpus, content_transformer(replace12))

replace13 <- function(x) gsub("drops", "drop", x)
myCorpus <- tm_map(myCorpus, content_transformer(replace13))

replace14 <- function(x) gsub("plans", "plan", x)
myCorpus <- tm_map(myCorpus, content_transformer(replace14))

replace15 <- function(x) gsub("serviceprovider", "provider", x)
myCorpus <- tm_map(myCorpus, content_transformer(replace15))

replace16 <- function(x) gsub("vodafoneboykot", "vodafoneboycott", x)
myCorpus <- tm_map(myCorpus, content_transformer(replace16))

replace17 <- function(x) gsub("buri", "bad", x)
myCorpus <- tm_map(myCorpus, content_transformer(replace17))
writeLines(strwrap(myCorpus[[1]]$content,60))

# Remove Extra Whitespaces
myCorpus<- tm_map(myCorpus, stripWhitespace) 
writeLines(strwrap(myCorpus[[242]]$content,60))

# keep a copy of "myCorpus" for stem completion later
myCorpusCopy<- myCorpus

#Removing stopwords
myCorpus = tm_map(myCorpus, removeWords,  c("can",stopwords("english")))

#####Remove Stopwords  
myStopWords<- c((stopwords('english')),c("rs", "dot", "g", "cr", "hai","network","vodafoneidea","per","got","see","dear","viadear","trai",
                                         "sugu","even","services","bhartiairtel","india","agr","itni","will","vodafone","telecom","yet",
                                         "now","vodafonein","idea","airtel","jio","reliancejio","government", "ur","bsnl", "airtelindia",
                                         "ideacelluar","please","crore","dues","debt","stocks","vodafonegroup","unable","via",
                                         "vodafonebusiness","vodaideanews","govt","one","just","go","hi","want","also","get","dont",
                                         "th","week","bharti","kumarmangalambirla","take","still","bill","like","cant","us","get",
                                         "didnt","indian","dotindian","give","birla","without","getting","birla","able","since",
                                         "yesbank","going","se","back","given","says","know","may","times","dotindia","pune","never",
                                         "rsprasad","giving","im","guys","res","another","supremecourt","well","today","case","working",
                                         "said","ka","customer","sc","ka","de","kr","correct","pali","indiatv","hd","zeenews","vodafoneuk",
                                         "narendramodi","nifty","pictwittercombnigklqmkp","vodafonepictwittercomyozkkmvqe","abpmajha",
                                         "allot","indiatv","hd","prefer","sudhagad","ping","connection","number","stock","fund",
                                         "adityabirlagroup","taking","photos","court","number","customers","nifty","ideas","last","reliance",
                                         "rehta","krne","esi","halat","krrhe","hum","hota","rahe","kaha","exe","iuc","ho","sir","money",
                                         "kolkata","bana","badwe","laate","yakeen","ok","abt","till","ncr","anywherewe","dene","naranpura",
                                         "years","inspite","vodafoneindia","ring","jiocare","dep","upon","gg","nov","day","ki","ideacares","care"))

myCorpus<- tm_map(myCorpus,removeWords , myStopWords) 
writeLines(strwrap(myCorpus[[242]]$content,60))
myCorpus
#####Creating a term document matrix
tdm<- TermDocumentMatrix(myCorpus, control= list(wordLengths= c(1, Inf)))
tdm

dev.off()
plot.new()

#############################
######## Wordcloud   ########
#############################

#Overall wordcloud
word.freq <-sort(rowSums(as.matrix(tdm)), decreasing= F)
pal<- brewer.pal(8, "Dark2")
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 2, random.order = F, colors = pal, max.words = 150,fixed.asp = TRUE)

#Positive and negaive wordclouds
dtm1 = DocumentTermMatrix(VCorpus(VectorSource(myCorpus$content)))
freq_up <- colSums(as.matrix(dtm1))
sentiments_up = calculate_sentiment(names(freq_up))
sentiments_up = cbind(sentiments_up, as.data.frame(freq_up))
sent_pos_up = sentiments_up[sentiments_up$sentiment == 'Positive',]
sent_neg_up = sentiments_up[sentiments_up$sentiment == 'Negative',]


DT::datatable(sent_pos_up)
DT::datatable(sent_neg_up)
#layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
#par(mar=rep(0, 4))
plot.new()
set.seed(1)
wordcloud(sent_pos_up$text,sent_pos_up$freq,min.freq=10,colors=brewer.pal(6,"Dark2"),fixed.asp = TRUE)

plot.new()
set.seed(5000)
wordcloud(sent_neg_up$text,sent_neg_up$freq,min.freq=10,colors=brewer.pal(6,"Dark2"), fixed.asp = TRUE)
?wordcloud


#####Find the terms used most frequently
(freq.terms <- findFreqTerms(tdm, lowfreq = 25))
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq > 25)
#View(term.freq)
df <- data.frame(term = names(term.freq), freq= term.freq)
df

#####Frequency analysis
(freq.terms <- findFreqTerms(tdm, lowfreq = 10))
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq > 10)
df1 <- data.frame(term = names(term.freq), freq= term.freq)

(freq.terms <- findFreqTerms(tdm, lowfreq = 55))
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq > 55)
df2 <- data.frame(term = names(term.freq), freq= term.freq)

(freq.terms <- findFreqTerms(tdm, lowfreq = 85))
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq > 85)
df3 <- data.frame(term = names(term.freq), freq= term.freq)


#####plotting the graph of frequent terms
p1=ggplot(df1, aes(reorder(term, freq),freq)) + theme_bw() + geom_bar(stat = "identity")  + coord_flip() +labs(list(title="@10", x="Terms", y="Term Counts")) + theme(axis.text.y = element_text(size=7))


p2=ggplot(df, aes(reorder(term, freq),freq)) + theme_bw() + geom_bar(stat = "identity")  + coord_flip() +labs(list(title="@25", x="Terms", y="Term Counts"))+
  theme(axis.text.y = element_text(size=7))


p3=ggplot(df2, aes(reorder(term, freq),freq)) + theme_bw() + geom_bar(stat = "identity")  + coord_flip() +labs(list(title="@55", x="Terms", y="Term Counts"))

p4=ggplot(df3, aes(reorder(term, freq),freq)) + theme_bw() + geom_bar(stat = "identity")  + coord_flip() +labs(list(title="@85", x="Terms", y="Term Counts")) 

#####plotting the graph of frequent terms
grid.arrange(p1,p2,ncol=2)
grid.arrange(p3,p4,ncol=2)

#####calculate the frequency of words and sort it by frequency and setting up the Wordcloud

##### Find association with a specific keyword in the tweets - vodafonedown,internet
list1<- findAssocs(tdm, "vodafonedown", 0.2)
corrdf1 <- t(data.frame(t(sapply(list1,c))))
corrdf1
barplot(t(as.matrix(corrdf1)), beside=TRUE,xlab = "Words",ylab = "Corr",col = "blue",main = "Vodafonedown",border = "black")

# Identify and plot word correlations. For example - year

toi <- "vodafonedown" # term of interest
corlimit <- 0.2 #  lower correlation bound limit.
cor_rlt <- data.frame(corr = findAssocs(tdm, toi, corlimit)[[1]],
                      terms = names(findAssocs(tdm, toi, corlimit)[[1]]))
cor_rlt$terms <- factor(cor_rlt$terms ,levels = cor_rlt$terms)
ggplot(cor_rlt, aes( y = terms  ) ) +
  geom_point(aes(x = corr), data = cor_rlt) +
  xlab(paste0("Correlation with the term ", "\"", toi, "\""))

#Find Association2
list2<- findAssocs(tdm, "internet", 0.2)
corrdf2 <- t(data.frame(t(sapply(list2,c))))
corrdf2
barplot(t(as.matrix(corrdf2)), beside=TRUE,xlab = "Words",ylab = "Corr",col = "blue",main = "internet",border = "black")

# Identify and plot word correlations. For example - year

toi <- "internet" # term of interest
corlimit <- 0.2 #  lower correlation bound limit.
cor_rlt <- data.frame(corr = findAssocs(tdm, toi, corlimit)[[1]],
                      terms = names(findAssocs(tdm, toi, corlimit)[[1]]))
cor_rlt$terms <- factor(cor_rlt$terms ,levels = cor_rlt$terms)
ggplot(cor_rlt, aes( y = terms  ) ) +
  geom_point(aes(x = corr), data = cor_rlt) +
  xlab(paste0("Correlation with the term ", "\"", toi, "\""))

#Find Association3
list3<- findAssocs(tdm, "service", 0.2)
corrdf3 <- t(data.frame(t(sapply(list3,c))))
corrdf3
barplot(t(as.matrix(corrdf3)), beside=TRUE,xlab = "Words",ylab = "Corr",col = "blue",main = "service",border = "black")

# Identify and plot word correlations. For example - year

toi <- "service" # term of interest
corlimit <- 0.2 #  lower correlation bound limit.
cor_rlt <- data.frame(corr = findAssocs(tdm, toi, corlimit)[[1]],
                      terms = names(findAssocs(tdm, toi, corlimit)[[1]]))
cor_rlt$terms <- factor(cor_rlt$terms ,levels = cor_rlt$terms)
ggplot(cor_rlt, aes( y = terms  ) ) +
  geom_point(aes(x = corr), data = cor_rlt) +
  xlab(paste0("Correlation with the term ", "\"", toi, "\""))


#Find Association4
list4<- findAssocs(tdm, "call", 0.2)
corrdf4 <- t(data.frame(t(sapply(list4,c))))
corrdf4
barplot(t(as.matrix(corrdf4)), beside=TRUE,xlab = "Words",ylab = "Corr",col = "blue",main = "call",border = "black")

# Identify and plot word correlations. For example - year

toi <- "call" # term of interest
corlimit <- 0.2 #  lower correlation bound limit.
cor_rlt <- data.frame(corr = findAssocs(tdm, toi, corlimit)[[1]],
                      terms = names(findAssocs(tdm, toi, corlimit)[[1]]))
cor_rlt$terms <- factor(cor_rlt$terms ,levels = cor_rlt$terms)
ggplot(cor_rlt, aes( y = terms  ) ) +
  geom_point(aes(x = corr), data = cor_rlt) +
  xlab(paste0("Correlation with the term ", "\"", toi, "\""))


#Find Association5
list5<- findAssocs(tdm, "issue", 0.2)
corrdf5 <- t(data.frame(t(sapply(list5,c))))
corrdf5
barplot(t(as.matrix(corrdf5)), beside=TRUE,xlab = "Words",ylab = "Corr",col = "blue",main = "issue",border = "black")

# Identify and plot word correlations. For example - year

toi <- "issue" # term of interest
corlimit <- 0.2 #  lower correlation bound limit.
cor_rlt <- data.frame(corr = findAssocs(tdm, toi, corlimit)[[1]],
                      terms = names(findAssocs(tdm, toi, corlimit)[[1]]))
cor_rlt$terms <- factor(cor_rlt$terms ,levels = cor_rlt$terms)
ggplot(cor_rlt, aes( y = terms  ) ) +
  geom_point(aes(x = corr), data = cor_rlt) +
  xlab(paste0("Correlation with the term ", "\"", toi, "\""))

##### Topic Modelling to identify latent/hidden topics using LDA technique
dtm <- as.DocumentTermMatrix(tdm)
rowTotals <- apply(dtm , 1, sum)

NullDocs <- dtm[rowTotals==0, ]
dtm   <- dtm[rowTotals> 0, ]

if (length(NullDocs$dimnames$Docs) > 0) {
  tweets.df <- tweets.df[-as.numeric(NullDocs$dimnames$Docs),]
}

lda <- LDA(dtm, k = 5) # find 5 topic
term <- terms(lda, 7) # first 7 terms of every topic
(term <- apply(term, MARGIN = 2, paste, collapse = ", "))
term
topics<- topics(lda)
topics<- data.frame(date=(tweets.df$timestamp), topic = topics)
qplot (date, ..count.., data=topics, geom ="density", fill= term[topic], position="stack")


#*******************************************************************************************
# Sentiment Analysis
#*******************************************************************************************

#####Sentiment Analysis: understanding emotional valence in tweets using syuzhet

mysentiment<-get_nrc_sentiment((tweets.df$text))
#mysentiment
# Get the sentiment score for each emotion
mysentiment.positive =sum(mysentiment$positive)
mysentiment.anger =sum(mysentiment$anger)
mysentiment.anticipation =sum(mysentiment$anticipation)
mysentiment.disgust =sum(mysentiment$disgust)
mysentiment.fear =sum(mysentiment$fear)
mysentiment.joy =sum(mysentiment$joy)
mysentiment.sadness =sum(mysentiment$sadness)
mysentiment.surprise =sum(mysentiment$surprise)
mysentiment.trust =sum(mysentiment$trust)
mysentiment.negative =sum(mysentiment$negative)

# Create the bar chart
yAxis <- c(mysentiment.positive,
           + mysentiment.anger,
           + mysentiment.anticipation,
           + mysentiment.disgust,
           + mysentiment.fear,
           + mysentiment.joy,
           + mysentiment.sadness,
           + mysentiment.surprise,
           + mysentiment.trust,
           + mysentiment.negative)


xAxis <- c("Positive","Anger","Anticipation","Disgust","Fear","Joy","Sadness","Surprise","Trust","Negative")
colors <- c("green","red","blue","orange","yellow","brown","black","pink","grey","purple")
yRange <- range(0,yAxis) + 100
barplot(yAxis, names.arg = xAxis, 
        xlab = "Emotional valence", ylab = "Score", main = "Twitter sentiment for VodafineIdea", sub = "service", col = colors, border = "black", ylim = yRange, xpd = F, axisnames = T, cex.axis = 0.8, cex.sub = 0.8, col.sub = "blue")
colSums(mysentiment)



#https://www.analyticsvidhya.com/blog/2017/03/measuring-audience-sentiments-about-movies-using-twitter-and-text-analytics/


