setwd("C:/Users/GUPTESH/Documents/DataScience/NFL - Dataset")
nfl_dt <- read.csv("NFL_SocialMedia_sample_data1.csv")#Fetching required data to R
head(nfl_dt)
str(nfl_dt)
class(nfl_dt)
dim(nfl_dt)
names(nfl_dt)
library(tm)
###############################################
nfl_Corpus=Corpus(VectorSource(nfl_dt$content))#Creating text corpus
inspect(nfl_Corpus[1:5])
nfl_Corpus[[1]]$content#display 1st row
?tm_map
################################################################
#Cleaning data
################################################################
stopwords("english")
mystopwords=c(stopwords("english"))
nfl_Corpus_tr=tm_map(nfl_Corpus,removeWords,mystopwords)
nfl_Corpus_tr=tm_map(nfl_Corpus_tr, tolower)
nfl_Corpus_tr[[1]]
nfl_Corpus_tr=tm_map(nfl_Corpus_tr, removePunctuation)
nfl_Corpus_trv=tm_map(nfl_Corpus_tr, removeNumbers)

removeurl=function(x){
  gsub("http[[:alnum:]]*","",x)
}

nfl_Corpus_tr=tm_map(nfl_Corpus_tr,removeurl)
nfl_Corpus_tr=tm_map(nfl_Corpus_tr,stripWhitespace)


nfl_Corpus_tr[[1]]
inspect(nfl_Corpus_tr[1:5])
###############################################################################
#transformed Corpus
###############################################################################

class(nfl_Corpus_tr)
nfl_Corpus_tr=tm_map(nfl_Corpus_tr,PlainTextDocument)
nfl_Corpus_tr=tm_map(nfl_Corpus_tr, content_transformer(tolower))
#######################################################################
#Creation of document term matrix
#######################################################################
dtm=DocumentTermMatrix(nfl_Corpus_tr)
dim(dtm)
str(dtm)
row.names(dtm)<-1:2000

#names(dim)
inspect(dtm[1:10,1001:1005])
findFreqTerms(dtm,lowfreq = 100)

?weightTfIdf
####tfidf matrix creation
dtm_tfxidf=weightTfIdf(dtm)
inspect(dtm_tfxidf[1:10,1001:1010])
dim(dtm_tfxidf)
m<- as.matrix(dtm_tfxidf)
#vector normalization
norm_eucl <- function(m) m/apply(m, MARGIN=1, FUN=function(x) sum(x^2)^.5)
#FUN(5)
m_norm <- norm_eucl(m)
#function that compute value of distortion
kmeans.wss.k=function(D,k){
  km=kmeans(D,centers = k,nstart = 5)
  return(km$tot.withinss)
}

# Function to compute the list of distortion, for a given D and maxK
kmeans.wss=function(D,maxK){
  wss=(nrow(D)-1)*sum(apply(D,2,var))
  wss[2:maxK]=sapply(2:maxK,kmeans.wss.k,D=D,simplify = T)
  return(wss)
}
maxK=15
wss=kmeans.wss(dtm_tfxidf,maxK)
plot(wss,type = 'b')#Elbow plot
km=kmeans(m_norm,10,iter.max = 100,nstart = 10)
table(km$cluster)
km$cluster

############################################################
x<- data.frame(nfl_dt$content,km$cluster)

write.csv(x, file = "C:/Users/GUPTESH/Documents/DataScience/NFL - Dataset/Cluster_Out.csv", row.names=TRUE)

data1 <- read.csv("C:/Users/GUPTESH/Documents/DataScience/NFL - Dataset/Cluster_Out.csv")
head(x)
reduced_dtm <- removeSparseTerms(dtm, sparse= 0.9999)
newdtm <- as.matrix(reduced_dtm)
head(newdtm)


################################################
#Getting top 5 words from the list
################################################
for (N in 1:length(km$withinss)) {
  a <- sort(colSums(newdtm[km$cluster == N, ]),
            decreasing = TRUE)
  df <- data.frame(names(a), a)
  colnames(df) <- c("word","count")
  
  if (N == 1){
    x <- data.frame(N, length(which(data1$Cluster == N )), df$word[1:5], 
                    df$count[1:5], as.numeric(rownames(x))[1:5])
    colnames(x) = c("Loggroup", "Logcount", "Top Words", "Word Count", "Counter")
  } else {
    y <- data.frame(N, length(which(data1$Cluster == N )), df$word[1:5],
                    df$count[1:5], as.numeric(rownames(x))[1:5]) 
    colnames(y) = c("Loggroup", "Logcount", "Top Words", "Word Count", "Counter")
    x <- rbind(x, y)
  }
}

write.csv(x, file = "C:/Users/GUPTESH/Documents/DataScience/NFL - Dataset/Topwords.csv")

#Wordcloud
library(wordcloud)
for(n in 1:10){
  wordcloud(nfl_Corpus_tr[km$cluster==n])
}
wordcloud(nfl_Corpus_tr[km$cluster==1])
wordcloud(nfl_Corpus_tr[km$cluster==2])
wordcloud(nfl_Corpus_tr[km$cluster==3])
wordcloud(nfl_Corpus_tr[km$cluster==4])
wordcloud(nfl_Corpus_tr[km$cluster==5])
wordcloud(nfl_Corpus_tr[km$cluster==6])
wordcloud(nfl_Corpus_tr[km$cluster==7])
wordcloud(nfl_Corpus_tr[km$cluster==8])
wordcloud(nfl_Corpus_tr[km$cluster==9])
wordcloud(nfl_Corpus_tr[km$cluster==10])

##test
b <- head(sort(colSums(newdtm[km$cluster == 1, ]),
     decreasing = TRUE))
df <- data.frame(names(b),b)
df
cm<- data.frame(c(1,2,3,4,1,1,3),c(10,10,10,10,10,10,10))
colnames(cm)<- c("tt","ff")
cm
colSums(cm[cm$tt==1,])
