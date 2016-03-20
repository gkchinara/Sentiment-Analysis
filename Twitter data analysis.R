#Shiraj twitter text clustering Classs6
install.packages("tm")#text-mining
library(tm)
install.packages("twitteR")
library(twitteR)
reqURL <- 
accessURL <- 
authURL <- 
consumerKey <- 
consumerSecret <- 
require(twitteR)

install.packages("base64enc")
library(base64enc)
setup_twitter_oauth()
rdntweets=userTimeline("rdatamining",n=100)
rdntweets[1:5]
fdatweets=userTimeline("FDArecalls",n=500)
n=length(fdatweets)
class(fdatweets)
df=twListToDF(fdatweets)
class(df)
dim(df)
names(df)
######################################################
#Text mining(Cleaning the text corpus)
######################################################
myCorpus=Corpus(VectorSource(df$text))
inspect(myCorpus[1:5])
myCorpus[[1]]$content
?tm_map
myCorpus=tm_map(myCorpus, tolower)
myCorpus[[2]]
myCorpus=tm_map(myCorpus, removePunctuation)
myCorpus=tm_map(myCorpus, removeNumbers)
stopwords("english")
mystopwords=c(stopwords("english"),"modi","obama","US")
myCorpus=tm_map(myCorpus,removeWords,mystopwords)
myCorpus[[3]]

removeurl=function(x){
  gsub("http[[:alnum:]]*","",x)
}
myCorpus=tm_map(myCorpus,removeurl)
?DocumentTermMatrix

class(myCorpus)
myCorpus=tm_map(myCorpus,PlainTextDocument)
myCorpus=tm_map(myCorpus, content_transformer(tolower))
dtm=DocumentTermMatrix(myCorpus)
dim(dtm)
inspect(dtm[11:15,1:15])
findFreqTerms(dtm,lowfreq = 18)


dtm_tfxidf=weightTfIdf(dtm)
inspect(dtm_tfxidf[1:496,1:3])
dim(dtm_tfxidf)
maxK=15
wss=kmeans.wss(dtm_tfxidf,maxK)
plot(wss,type = 'b')
######################################################
tweets <- searchTwitter( '#rstats', n=50) 
names(tweets)
df <- twListToDF(tweets)
df
names(df)
#####################################################################################################
#text clustering data set Assignement
setwd("C:/Users/GUPTESH/Documents/DataScience/Text Clustering Dataset")
textc=read.csv("Tweets.csv")
textc
class(textc)
names(textc)
head(textc)

myCorpus=Corpus(VectorSource(textc$content))#Create a vector sourse and put it as a corpus
inspect(myCorpus[1:5])#display detailed info on a corpus/tdm
class(myCorpus)#display class of corpus
myCorpus[[1]]$content#display content of 1st element
?tm_map#to apply transformation function
myCorpus=tm_map(myCorpus, tolower)#Make alll lower case
myCorpus[[2]]
myCorpus=tm_map(myCorpus, removePunctuation)
myCorpus=tm_map(myCorpus, removeNumbers)
stopwords("english")#generallly used words in english like "the" "and"
mystopwords=c(stopwords("english"),"and","the","in")#add extra stopwords
myCorpus=tm_map(myCorpus,removeWords,mystopwords)#remove stop words
myCorpus[[2]]
#function to remove http links
removeurl=function(x){
  gsub("http[[:alnum:]]*","",x)
}
myCorpus=tm_map(myCorpus,removeurl)
?DocumentTermMatrix#constructs a term-doc matrix

class(myCorpus)
myCorpus=tm_map(myCorpus,PlainTextDocument)#Create plain text doc

myCorpus=tm_map(myCorpus, content_transformer(tolower))#transform to lowr
dtm=DocumentTermMatrix(myCorpus)
dim(dtm)
inspect(dtm[11:15,1:15])
inspect(dtm[1:5,1:10])
findFreqTerms(dtm,lowfreq = 25)
#findAssocs(dtm,'budweiser')

dtm_tfxidf=weightTfIdf(dtm)
dim(dtm_tfxidf)
dtm_tfxidf[1:5,103:106]
#inspect(dtm_tfxidf[11:10,1:15])
m<- as.matrix(dtm_tfxidf)
#m
rownames(m)<- 1:nrow(m)
norm_eucl <- function(m) m/apply(m, MARGIN=1, FUN=function(x) sum(x^2)^.5)  
m_norm <- norm_eucl(m)  
cl <- kmeans(m_norm, 10)
cl$cluster
cl$size



comments_out<-cbind(as.character(myCorpus$content),cl$cluster) 
write.csv(comments_out,"Output_clusters.csv") 
#m_norm=scale(m)
#head(m_norm)
#dim(m_norm)
#km=kmeans(m_norm,2,iter.max = 100,nstart = 10)
#m_norm

maxK=15
wss=kmeans.wss(dtm_tfxidf,maxK)
plot(wss,type = 'b')
kmeans.wss.k(dtm_tfxidf,10)
km=kmeans(dtm_tfxidf,10,iter.max = 100,nstart = 10)
km
str(km)
km$cluster
myCorpus[[km$cluster[[2]]==1]]$content
################################################################################
library(wordcloud)
 m <- as.matrix(dtm)
 # calculate the frequency of words
 v <- sort(rowSums(m), decreasing=TRUE)
 myNames <- names(v)
 k <- which(names(v)=="miners")
 myNames[k] <- "mining"
 d <- data.frame(word=myNames, freq=v)
 wordcloud(d$word, d$freq, min.freq=3)
 m
 