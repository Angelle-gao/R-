setwd("E:/homework/news")
setwd("E:/")
library(dplyr)
library(ggplot2)
##词云
library(wordcloud)
mycolors <- brewer.pal(8,"Dark2")
#设置字体
View(myfile)
freq<-table(myfile.words)
freq<-data.frame(freq)
freq<-arrange(freq,desc(freq$Freq))
View(freq)
write.csv(freq,"获得帮助词频.csv")
freq<-read.csv("E:/homework/news/获得帮助词频.csv")
windowsFonts(myFont=windowsFont("华文中宋"))
wordcloud(freq[1:100,]$myfile,freq[1:100,]$Freq,random.order=FALSE,random.color=FALSE,colors=mycolors,family="muFont")

##词典型情感分析
data<-read.csv("E:/homework/news/中文对话非对话部分.csv")
View(data)
illness <- data %>%
  group_by(病情摘要及初步印象) %>%
  summarise(数量=n())
illness1 <- data %>%
  group_by(疾病) %>%
  summarise(疾病病例数量=n())
View(illness1)
write.csv(illness1,"6 疾病.csv")
View(doctor)
#先做医生对话的情感分析
doctor<-data$doctor
patient<-data$patient
pos <- read.table("E:/homework/news/positive.txt")
weight <- rep(1, length(pos[,1]))
pos <- cbind(pos, weight)
neg <- read.table("E:/homework/news/negative.txt")
weight <- rep(-1, length(neg[,1]))
neg <- cbind(neg, weight)
posneg <- rbind(pos,neg)  #正负词典合并
names(posneg) <- c("word", "weight")
posneg <- posneg[!duplicated(posneg$term), ]#`duplicated`函数的作用和`unique`函数比较相似，它返回重复项的位置编号
dict <- posneg[, "word"]
pos<-data.frame(pos)
posneg<-data.frame(posneg)
##分词和数据清洗
library(Rwordseg)
insertWords(dict)
sentence <- as.vector(doctor)
#载入停用词表
data_stw=read.table(file=file.choose(),colClasses="character")
stopwords_CN=c(NULL)
for(i in 1:dim(data_stw)[1]){
  stopwords_CN=c(stopwords_CN,data_stw[i,1])
}
for(j in 1:length(stopwords_CN)){
  myfile.words <- subset(sentence,sentence!=stopwords_CN[j])
}
#分词
myfile.words <- unlist(lapply(X = myfile.words,FUN = segmentCN))
#清理不必要的字符
myfile.words <- gsub(pattern="[[:digit:]]*","",myfile.words)
myfile.words <- gsub("\n","",myfile.words)
myfile.words <- gsub("　","",myfile.words)
#筛选词长度大于1的词语
myfile.words <- subset(myfile.words, nchar(as.character(myfile.words))>1)
myfile.words
#筛选词长度小于2的词语
myfile.words <- myfile.words[!nchar(myfile.words) < 2]
myfile<-as.data.frame(myfile.words)
write.table(myfile.words,file="myfile-patient.txt",quote=F,row.names = F)
library(plyr)
library(dplyr)
names(myfile) <- c("words")
testterm <- join(myfile, posneg)
View(pos)
View(testterm)
View(myfile)
testterm <- testterm[!is.na(testterm$weight), ]
dictresult <- aggregate(weight ~ V1, data = testterm, sum)
dictlabel <- rep(-1, length(dictresult[, 1]))
dictlabel[dictresult$weight > 0] <- 1          #很有技巧地把情感词语正负赋值到情感得分表中
dictresult <- as.data.frame(cbind(dictresult, dictlabel), stringsAsFactors = F)
View(dictresult)
write.csv(dictresult,file="情感分析建模-病人.csv",quote=F,row.names = F)
set.seed(20200213)
ids <- myfile$id
ids.train <- sample(ids,length(ids) * 0.8)
ids.test <- setdiff(ids,ids.train)
library(text2vec)
it_train <- itoken(myfile$word,ids = myfile$id,
                   progressbar = F)



#文本聚类
#创建文档矩阵

text_corpus <- Corpus(x = VectorSource(myfile.words))
text_corpus
#设置矩阵条件
control=list(removeNumbers=TRUE,removePunctuation=TRUE,minDocFreq=100,WordLengths=c(2,Inf),weighting=weightTf)
#创建词条-文本矩阵，可绘制词频图
wf_q.tdm=TermDocumentMatrix(text_corpus,control)
wf_q.dtm=DocumentTermMatrix(text_corpus,control)
wf_q.dtm <- removeSparseTerms(x = wf_q.dtm, sparse = 0.9999)
wf_q.dtm
rowTotals<- apply(wf_q.dtm,1,sum) #Find the sum of words in each Document
dtm.new <- wf_q.dtm[rowTotals>0,]
df <- as.data.frame(inspect(wf_q.dtm))
df<-scale(df)
d<-dist(df)
fit1 <- hclust(d = d)
plot(fit1)
rect.hclust(tree = fit1, k = 2, border = 'red')
##kmeans聚类
library("tm")
text_corpus <- Corpus(x = VectorSource(myfile.words))
presebtation_seg <- Corpus(DataframeSource(myfile.words)) #转换到tm专用格式
presebtation_term <- TermDocumentMatrix(text_corpus, control = list(stopwords = TRUE)) #生成词频矩阵
presebtation_term <- t(as.matrix(presebtation_term)) #转换为matrix并转置
summary(presebtation_term)
presebtation_kmeans <- kmeans(presebtation_term, 5) #kmeans聚为7类
summary(presebtation_kmeans)
mode(presebtation_kmeans)
hlzj.kmeansRes <- list(content=myfile.words,type=presebtation_kmeans$cluster)
write.csv(hlzj.kmeansRes,"kmeansRes.csv")
#高频词统计
presentations$seg2 <- unique((strsplit(presentations$seg,split=" "))) #断词
all_key_words <- iconv(unlist(presentations$seg2), from="UTF-8", to="GBK") #转换到GBK编码
all_key_words_fre <- as.data.frame(table(all_key_words)) #统计词频
names(all_key_words_fre)
all_key_words_fre <- arrange(all_key_words_fre,desc(Freq)) #按词频排序
all_key_words_fre[1:20,]$all_key_words #100个高频词






#关联规则
install.packages("arulesViz")
library(arules)
library(arulesViz)
myfilepatient<-read.table("./myfile-patient.txt")
myfiledoctor<-read.table("./myfile.txt")
### read.transactions是arules包中读取数据集并创建为系数矩阵，仅存发生事物1，节约内存；
trans <- read.transactions("./myfile-patient.txt", format="basket", sep=",")
summary(trans)
basketSize<-size(trans)
itemFrequency(trans)
itemFreq <- sort(itemFrequency(trans),decreasing = T)
itemFrequencyPlot(trans, support=0.01)
itemFrequencyPlot(trans,topN=10,horiz=T)
trans_use <- trans[basketSize > 0.1]
summary(trans_use)
trans_rule <- apriori(trans_use, parameter = list(support = 0.006, confidence = 0.25, minlen = 2))
ordered_rule <- sort(groceryrules, by="lift")
fruitrules <- subset(trans_rule, items %pin% c("fruit") & lift>3)
inspect(fruitrules)
summary(trans_rule)
plot(rules_lift_lg3, measure="confidence", method="graph", shading = "lift")


freq_sets <- eclat(myfilepatient,parameter=list(support=0.01,maxlen=5))#求频集
freq_sets
inspect(freq_sets[1:4])#查看频繁项集的前十项
inspect(sort(freq_sets,by="support")[1:4]) #根据支持度对求得的频繁项集排序并查看，结果如图。
rules=apriori(myfiledoctor,parameter=list(support=0.001,confidence=0.0003))
rules
inspect(sort(rules,by="support")[1:175])#查看前十条关联规则
