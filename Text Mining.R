library(NLP)   
library(tm)   
library(SnowballC)   
library(ggplot2)   
library(RColorBrewer)
library(wordcloud)  
library(cluster) 
library(fpc) 

cname <- file.path("~", "Desktop", "texts")   
cname 

docs <- Corpus(DirSource(cname))   
summary(docs) 

#Inspection
inspect(docs[1])

docs <- tm_map(docs, removePunctuation)   
# inspect(docs[3]) # Check to see if it worked. 

for(j in seq(docs))   
{   
  docs[[j]] <- gsub("/", " ", docs[[j]])   
  docs[[j]] <- gsub("@", " ", docs[[j]])   
  docs[[j]] <- gsub("\\|", " ", docs[[j]])   
}   
# inspect(docs[1]) # You can check a document (in this case the first) to see if it worked. 


docs <- tm_map(docs, removeNumbers)   
# inspect(docs[3]) # Check to see if it worked. 

docs <- tm_map(docs, tolower)   
# inspect(docs[3]) # Check to see if it worked. 


# For a list of the stopwords, see:   
# length(stopwords("english"))   
# stopwords("english")   
docs <- tm_map(docs, removeWords, stopwords("english"))   
# inspect(docs[3]) # Check to see if it worked. 


docs <- tm_map(docs, removeWords, c("department", "email"))   
# Just replace "department" and "email" with words that you would like to remove. 

#Combining words that should stay together
for (j in seq(docs))
{
  docs[[j]] <- gsub("qualitative research", "QDA", docs[[j]])
  docs[[j]] <- gsub("qualitative studies", "QDA", docs[[j]])
  docs[[j]] <- gsub("qualitative analysis", "QDA", docs[[j]])
  docs[[j]] <- gsub("research methods", "research_methods", docs[[j]])
}


#Removing common word endings (e.g., “ing”, “es”, “s”)
docs <- tm_map(docs, stemDocument)   
# inspect(docs[3]) # Check to see if it worked. 


docs <- tm_map(docs, stripWhitespace)   
# inspect(docs[3]) # Check to see if it worked. 

#Finish
docs <- tm_map(docs, PlainTextDocument) 


#To proceed, create a document term matrix.
dtm <- DocumentTermMatrix(docs)   
dtm   


tdm <- TermDocumentMatrix(docs)   
tdm 


#Explore your data, Organize terms by their frequency:
freq <- colSums(as.matrix(dtm))   
length(freq) 

ord <- order(freq) 


#Report to excel
m <- as.matrix(dtm)   
dim(m)   
write.csv(m, file="dtm.csv")  


#  Start by removing sparse terms:   
dtms <- removeSparseTerms(dtm, 0.1) # This makes a matrix that is 10% empty space, maximum.   
inspect(dtms)

#Word Frequency, There are a lot of terms, so for now, just check out some of the most and least frequently occurring words.
freq[head(ord)] 
freq[tail(ord)]  

#Check out the frequency of frequencies.
head(table(freq), 20) 
tail(table(freq), 20)  


freq <- colSums(as.matrix(dtms))   
freq 

freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)   
head(freq, 14)  

#An alternate view of term frequency:
findFreqTerms(dtm, lowfreq=50)   # Change "50" to whatever is most appropriate for your text data

#Yet another way to do this:
wf <- data.frame(word=names(freq), freq=freq)   
head(wf) 


#Plot Word Frequencies, Plot words that appear at least 50 times.
p <- ggplot(subset(wf, freq>400), aes(word, freq))    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p  


#Term Correlations, If words always appear together, then correlation=1.0.
findAssocs(dtm, c("good" , "bad"), corlimit=0.98) # specifying a correlation limit of 0.98 

findAssocs(dtms, "great", corlimit=0.90) # specifying a correlation limit of 0.95   


#########################   Ploting   #########################
#Word Clouds!, Plot words that occur at least 25 times.
set.seed(142)   
wordcloud(names(freq), freq, min.freq=100) 


#Plot the 100 most frequently used words.
set.seed(142)   
wordcloud(names(freq), freq, max.words=50) 


#Add some color and plot words occurring at least 200 times.
set.seed(142)   
wordcloud(names(freq), freq, min.freq=200, scale=c(5, .1), colors=brewer.pal(6, "Dark2"))


#Plot the 100 most frequently occurring words.
set.seed(142)   
dark2 <- brewer.pal(6, "Dark2")   
wordcloud(names(freq), freq, max.words=100, rot.per=0.2, colors=dark2)   


#Clustering by Term Similarity, First remove a lot of the uninteresting or infrequent words. If you have not done so already, you can remove these with the following code.
dtmss <- removeSparseTerms(dtm, 0.15) # This makes a matrix that is only 15% empty space, maximum.   
inspect(dtmss) 

#Plot
d <- dist(t(dtmss), method="euclidian")   
fit <- hclust(d=d, method="ward")   
fit  
plot(fit, hang=-1)  


#Helping to Read a Dendrogram
plot.new()
plot(fit, hang=-1)
groups <- cutree(fit, k=5)   # "k=" defines the number of clusters you are using   
rect.hclust(fit, k=5, border="red") # draw dendogram with red borders around the 5 clusters


#K-Means Clustering
d <- dist(t(dtmss), method="euclidian")   
kfit <- kmeans(d, 2)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)


