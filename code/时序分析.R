#分词
library(xlsx)
install.packages("wordcloud2")
library(jiebaR)
library(wordcloud2)
install.packages("rJava")
library(rJava)
install.packages("wordcloud")
library(wordcloud)

news<-read.csv('C:/Users/SST/Desktop/疫情 比赛/丁香园时间序列/DXYNews.csv',encoding="UTF-8")
news<-news[,-10]
news<-news[,-8]
head(news)
news2<-read.xlsx('C:/Users/SST/Desktop/DXYNews.xlsx',2,encoding="UTF-8")
#DXYNews的第二个sheet是纯日期和title的表
#分词
library(jiebaR)
wk = worker()
write.csv(segment(as.character(news[,4]),wk), file="C:/Users/SST/Desktop/DXYNewstitle分词.csv")


#========================================================================
#1-29
news.1.29<-read.xlsx('C:/Users/SST/Desktop/1.29.xlsx',1,encoding="UTF-8")
write.csv(segment(as.character(news.1.29[,2]),wk), file="C:/Users/SST/Desktop/1.29分词.csv")
freq1.29<-freq(segment(as.character(news.1.29[,2]),wk))
wordcloud1.29<-wordcloud2(freq1.29)
#
news.1.29R<-read.xlsx('C:/Users/SST/Desktop/1.29%.xlsx',1,encoding="UTF-8")
freq1.29R<-freq(as.character(news.1.29R[,2]))
wordcloud1.29R<-wordcloud2(freq1.29R,color = "random-light", backgroundColor = "grey")
wordcloud1.29R


#3-23
news.3.23<-read.xlsx('C:/Users/SST/Desktop/3.23.xlsx',1,encoding="UTF-8")
write.csv(segment(as.character(news.3.23[,2]),wk), file="C:/Users/SST/Desktop/3.23分词.csv")
freq3.23<-freq(segment(as.character(news.1.29[,2]),wk))
wordcloud3.23<-wordcloud2(freq3.23)
wordcloud3.23
#
news.3.23R<-read.xlsx('C:/Users/SST/Desktop/3.23%.xlsx',1,encoding="UTF-8")
freq3.23R<-freq(as.character(news.3.23R[,2]))
wordcloud3.23R<-wordcloud2(freq3.23R,size=0.6,color = "random-light", backgroundColor = "grey")
wordcloud3.23R
#4-6
news.4.6<-read.xlsx('C:/Users/SST/Desktop/4.6.xlsx',1,encoding="UTF-8")
write.csv(segment(as.character(news.4.6[,2]),wk), file="C:/Users/SST/Desktop/4.6分词.csv")
freq4.6<-freq(segment(as.character(news.4.6[,2]),wk))
wordcloud4.6<-wordcloud2(freq4.6)
wordcloud4.6
#
news.4.6R<-read.xlsx('C:/Users/SST/Desktop/4.6%.xlsx',1,encoding="UTF-8")
freq4.6R<-freq(as.character(news.4.6R[,2]))
wordcloud4.6R<-wordcloud2(freq4.6R,size=0.6,color = "random-light", backgroundColor = "grey")
wordcloud4.6R