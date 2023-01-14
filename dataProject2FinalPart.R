

#medium post: https://medium.com/@ilakhani/what-makes-a-ted-talk-a-ted-talk-2520e018138e

library(ggplot2)
library(tidyr)
library(lubridate)
library(ggplot2)
library(wordcloud) 
library(topicmodels)
library(dplyr)
library(tidyverse)
library(rvest) 
library(tm) 

categoriesTED <- read_csv(file.choose())

corp.orignal <- VCorpus(VectorSource(categoriesTED$Transcript))
corp.clean = tm_map(corp.orignal, removePunctuation)  
corp.clean = tm_map(corp.clean, removeNumbers)  
corp.clean = tm_map(corp.clean, content_transformer(tolower) ,lazy=TRUE)  
corp.clean = tm_map(corp.clean, content_transformer(removeWords), c("TIL") ,lazy=TRUE) 
corp.clean = tm_map(corp.clean, content_transformer(removeWords), stopwords("en") ,lazy=TRUE) 
corp.clean = tm_map(corp.clean, stripWhitespace)
corp.final = tm_map(corp.clean, content_transformer(removeWords), 
                    c("the", "said", "will", "can", "also", "and", "like", "but", 
                      "almost", "going", "just", "dont", "this", "now", "one", 
                      "thats", "they", "people", "youre", "its", "get", "stories","beetles",
                      "hes", "actually", "things", "theres", "were", "say", "weve","she",
                      "come"), lazy=TRUE)

dtm <- DocumentTermMatrix(corp.final) 
dtm <- removeSparseTerms(dtm, 0.995)
m <- as.matrix(dtm) 
train = m[sample(nrow(m), 20),]
ldaModel = LDA(x = train, control=list(seed = 8), k = 7)
dic <- terms(ldaModel, 10)
print(dic)
categoriesTED
topics = c("Dazzle with wonder", "The big idea", "The personal", "The tech demo", 
           "The tech demo", "The issue ", 
           "The big idea")
print(topics)

library(stringr)

tedData <- read_csv(file.choose())
transcripts <- read_csv(file.choose())

tedFinal <- merge(tedData, transcripts, by.x = 'talk_name', by.y = 'title')
head(tedFinal$talk_name)
corp.news <- VCorpus(VectorSource(tedFinal$transcript))
corp.news = tm_map(corp.news, removePunctuation)  
corp.news = tm_map(corp.news, removeNumbers)  
corp.news = tm_map(corp.news, content_transformer(tolower) ,lazy=TRUE)  
corp.news = tm_map(corp.news, content_transformer(removeWords), c("TIL") ,lazy=TRUE) 
corp.news = tm_map(corp.news, content_transformer(removeWords), stopwords("en") ,lazy=TRUE) 
corp.news = tm_map(corp.news, stripWhitespace)
corp.news = tm_map(corp.news, content_transformer(removeWords), 
                   c("the", "said", "will", "can", "also"), lazy=TRUE)

#taken from hw2 file
new_dtm = DocumentTermMatrix(corp.news, control=list(dictionary = dic))
new_dtm = new_dtm[rowSums(as.matrix(new_dtm))]
topic_probabilities = posterior(ldaModel, new_dtm)$topics
head(topic_probabilities)
#returns topic with the highest probability 
topic = function(topic_probabilities) {
  return(topics[which.max(topic_probabilities)])
}

#returns topic with the highest probability 
nrow(topic_probabilities)
classify = data.frame(Category = apply(topic_probabilities,1, topic))
classify
final <- merge(classify, tedFinal)
final$Category

finalSum <- aggregate(final$views, list(final$Category), FUN=mean) 
head(finalSum)

ggplot(data, aes(x=type, y=V2)) + geom_bar(stat="identity", aes(fill=type), 
                                           width = 0.9, position = position_dodge(width = 0.5))+ 
  labs(x="Type", y="Avg. Views") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
   labs(subtitle="Avg. Views per Category of TED talk'",title= "Themes Matter?") 


