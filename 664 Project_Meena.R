#milestone1
library(jsonlite)
mydata<- fromJSON("http://ist.gmu.edu/~hpurohit/courses/ait690-proj-data-spring17.json")
write.csv(mydata, "mydata.csv")


#milestone2
library(stringr)
first_split <- str_split_fixed(mydata$DATETIME, " ", 2)
mydf3 <- cbind(mydata, first_split)
mydf3$DATE<-mydf3$`1`
mydf3$TIME<-mydf3$`2`
mydata1<-mydf3[,-6]
mydata2<-mydata1[,-6]
Mydata<-mydata2[,-5]
Mydata$MESSAGE= gsub("@\\w+", "", Mydata$MESSAGE)
data1<-as.data.frame(str_replace_all(Mydata$MESSAGE, "[^[:alnum:]]", " "))
data2 <- as.data.frame(gsub('[[:digit:]]+', '', data1$`str_replace_all(Mydata$MESSAGE, "[^[:alnum:]]", " ")`))
colnames(data2) <- "Message"
MyData<- cbind(Mydata,data2)
MyData$MESSAGE <-NULL
MyData$DOCUMENT_ID<-NULL
library(NLP)
library(stopwords)
stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
MyData$Message = stringr::str_replace_all(MyData$Message, stopwords_regex, '')
MyData$Message<- gsub(" *\\b[[:alpha:]]{1,2}\\b *", " ", MyData$Message)
MyData$LATITUDE <- as.numeric(as.character(MyData$LATITUDE ))
MyData$LONGITUDE <- as.numeric(as.character(MyData$LONGITUDE ))
row=nrow(MyData)
count<-0
actual_sum<-0
for(i in 1:row)
{
  if( MyData$LATITUDE[i]!= 10000)
  { count<-count+1
  actual_sum<-MyData$LATITUDE[i]+actual_sum
  }
}
countlubridate
actual_sum
actual_avg<- (actual_sum/count)
actual_avg

for(i in 1:row)
{
  if( MyData$LATITUDE[i]==10000)
  { MyData$LATITUDE[i]<-36.11976}
}

count2<-0
actual_sum2<-0
for(i in 1:row)
{
  if( MyData$LONGITUDE[i]!= 10000)
  { count2<-count2+1
  actual_sum2<-MyData$LONGITUDE[i]+actual_sum2
  }
}
count2
actual_sum2
actual_avg2<- (actual_sum2/count2)
actual_avg2

for(i in 1:row)
{
  if( MyData$LONGITUDE[i]==10000)
  { MyData$LONGITUDE[i]<- -74.75161}
}

MyData$Message<-tolower(MyData[,5])
#MyData<- cbind(MyData,Message1)
#MyData$Message<-NULL
MyData$TIME<- NULL
View(MyData)

write.csv(MyData, "final6.csv") 
library(dplyr) 
library(lubridate)
library(foreign)     
byd = read.csv('final6.csv')
byd %>% glimpse()
byd = byd %>% mutate(tradeDate = as.Date(tradeDate))
write.arff(byd, file='finaldata6.arff')



#cluster1
cluster<-read.csv('ClusterData.csv')
library("NLP")
library("stopwords")
library("tm")
library("wordcloud")
library("RColorBrewer")
document <- Corpus(VectorSource(cluster$Cluster1))
data<- TermDocumentMatrix(document)
matrix <- as.matrix(data)
sort <- sort(rowSums(matrix),decreasing=TRUE)
dataframe <- data.frame(word = names(sort),freq=sort)
head(dataframe, 10)

#cluster2
document <- Corpus(VectorSource(cluster$Cluster2))
data<- TermDocumentMatrix(document)
matrix <- as.matrix(data)
sort <- sort(rowSums(matrix),decreasing=TRUE)
dataframe <- data.frame(word = names(sort),freq=sort)
head(dataframe, 10)

#cluster3
document <- Corpus(VectorSource(cluster$Cluster3))
data<- TermDocumentMatrix(document)
matrix <- as.matrix(data)
sort <- sort(rowSums(matrix),decreasing=TRUE)
dataframe <- data.frame(word = names(sort),freq=sort)
head(dataframe, 10)
