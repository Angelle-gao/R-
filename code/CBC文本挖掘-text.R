setwd("E:/homework/news")
library(tidytext)
library(tidyr)
library(tidyverse)

library(pacman)
library(dplyr)
library(text2vec)
library(ggplot2)
data<-read.csv("E:/homework/news/news.csv")
data<-data[,1:7]
library(tm)
library(jiebaR,jiebaRD) 
library(wordcloud2)
library(rJava)
library(Rwordseg)
text<-read.csv("E:/homework/news/text.csv")
DXYArea<-read.csv("E:/homework/DXYArea.csv")
DXYNews<-read.csv("E:/homework/DXYNews.csv")
gwnews<-read.csv("E:/homework/news/gwnews.csv")
DXYOverall<-read.csv("E:/homework/DXYOverall.csv")
CBCstage1<-read.csv("E:/homework/news/CBCstage1.csv")
CBCstage2<-read.csv("E:/homework/news/CBCstage2.csv")
text<-data$text
text<-gwnews$summary
text<-CBCstage1$text
text<-CBCstage2$text
##海外-加拿大疫情的总体分布趋势##
View(DXYOverall)
DXYOverall$updateTime<-as.Date(as.character(DXYOverall$updateTime), format='%Y/%m/%d')
GWconfirmedByTime <- DXYOverall %>%
  group_by(updateTime) %>%
  summarise(确诊人数=max(gwconfirmedCount))
View(GWconfirmedByTime)
ggplot(GWconfirmedByTime,aes(x =updateTime, y =日确诊人数,group=1)) + geom_line(stat="identity",size=2)
GWcuredByTime <- DXYOverall %>%
  group_by(updateTime) %>%
  summarise(治愈人数=max(gwcuredCount))%>%
  arrange(desc(治愈人数))
GWdeadByTime <- DXYOverall %>%
  group_by(updateTime) %>%
  summarise(死亡人数=max(gwdeadCount))%>%
  arrange(desc(死亡人数))
glodeadByTime <- DXYOverall %>%
  group_by(updateTime) %>%
  summarise(全球死亡人数=max(glodeadCount))%>%
  arrange(desc(全球死亡人数))
glocuredByTime <- DXYOverall %>%
  group_by(updateTime) %>%
  summarise(全球治愈人数=max(glocuredCount))%>%
  arrange(desc(全球治愈人数))
gloconfirmedByTime <- DXYOverall %>%
  group_by(updateTime) %>%
  summarise(全球确诊人数=max(gloconfirmedCount))%>%
  arrange(desc(全球确诊人数))
foreign_num<-merge(GWconfirmedByTime,GWcuredByTime,by.x = "updateTime",by.y = "updateTime",all.x = T,all.y = F) %>%
  merge(GWdeadByTime,GWcuredByTime,by.x = "updateTime",by.y = "updateTime",by ="updateTime",all.x = T,all.y = F)
global_num<-merge(gloconfirmedByTime,glocuredByTime,by.x = "updateTime",by.y = "updateTime",all.x = T,all.y = F) %>%
  merge(glodeadByTime,glocuredByTime,by.x = "updateTime",by.y = "updateTime",all.x = T,all.y = F)
View(global_num)
write.csv(foreign_num,"forein_num.csv")
write.csv(global_num,"global_num.csv")




gw<-filter(DXYArea,countryName!="中国")
gw$updatedate<-as.Date(as.character(gw$updatedate), format='%Y/%m/%d')
canada<-filter(gw,countryName=="加拿大")
View(canada)
confirmedcount <- gw %>%
  group_by(continentName) %>%
  summarise(洲确诊人数=max(province_confirmedCount))
View(confirmedcountdate)
confirmedcountER <- gw %>%
  filter(continentName=="欧洲")%>%
  group_by(updatedate) %>%
  summarise(欧洲确诊人数=max(province_confirmedCount))
ggplot(confirmedcountER,aes(x =updatedate, y =欧洲确诊人数,group=1)) + geom_line(stat="identity",size=1)
View(confirmedcountNA)
View(confirmedcountER)


View(confirmedcountdate)
write.csv(confirmedcount,"confirmedcount.csv")
ggplot(confirmedcount,aes(x =continentEnglishName, y =洲确诊人数,group=1)) + geom_line(stat="identity",size=1)
canada$updatedate<-as.Date(as.character(canada$updatedate), format='%Y/%m/%d')
confirmedByTimeCanada <- canada %>%
  group_by(updatedate) %>%
  summarise(加拿大确诊人数=max(province_confirmedCount))
View(confirmedByTimeCanada)
ggplot(confirmedByTimeCanada,aes(x =updatedate, y =加拿大确诊人数,group=1)) + geom_line(stat="identity",size=1.3)+geom_point()
View(confirmedcountdate)
write.csv(confirmedcount,"confirmedcount.csv")
ggplot(confirmedcount,aes(x =continentEnglishName, y =洲确诊人数,group=1)) + geom_line(stat="identity",size=1)

USA<-filter(gw,countryName=="美国")
confirmedByTimeUSA <- USA %>%
  group_by(updatedate) %>%
  summarise(美国确诊人数=max(province_confirmedCount))
ggplot(confirmedByTimeUSA,aes(x =updatedate, y =美国确诊人数,group=1)) + geom_line(stat="identity",size=1.3)+geom_point()
##新闻报道的时间分布情况##
data$publish_date<-as.Date(as.character(data$publish_date), format='%Y/%m/%d')
gwnews$pubDate<-as.Date(as.character(gwnews$pubDate), format='%Y/%m/%d')
gwnewstxt<-gwnews
View(CBCbytime)
View(gwnewsbytime)
CBCbytime <- data %>%
  group_by(publish_date) %>%
  summarise(CBC报道数量=n())
gwnewsbytime <- gwnews %>%
  group_by(pubDate) %>%
  summarise(国外报道数量=n())
news_num<-merge(CBCbytime,gwnewsbytime,by.x = "publish_date",by.y = "pubDate")
View(news_num)
write.csv(news_num,"news_num.csv")
write.csv(CBCbytime,"CBCbytime.csv")
combine<-merge(confirmedByTimeCanada,CBCbytime,by.x = "updatedate",by.y = "publish_date")
View(combine)
cor(combine$确诊人数,combine$CBC报道数量)
cor.test(combine$确诊人数,combine$CBC报道数量)

##新闻报道的总词频情况##
text<-as.character(text)
mixseg<-worker("mix")#建立模型分词
text <- gsub(pattern="http:[a-zA-Z\\/\\.0-9]+"," ",text)
text <- gsub("\n"," ",text)
text<-gsub("[^a-zA-Z]"," ",text)#除去非字母字符
text  <- gsub("  "," ",text)
text<-tolower(text)#转化为小写
text<-na.omit(text)
a<-segment(text,mixseg) #开始分词
text<-strsplit(x = text,split = " ")#以空格为参考拆分
text
stopwords <- read.table("E:/homework/news/stopwords_en.txt",header = F,fill=TRUE)
class(stopwords) 
stopwords <- as.vector(stopwords[,1]) 
text
wordResult <- removeWords(a,stopwords)
write.table(wordResult,"segmentgw.txt")
freq<-table(wordResult) #词频统计
freq
write.csv(freq,"gw新闻词频.csv")
freq<-read.csv("E:/homework/news/gw新闻词频.csv")
freqCBC<-read.csv("E:/homework/news/CBC新闻select.csv")
wordcloud2(freq[1:100,],shape='star',fontFamily = 'Segoe UI') #绘制词云
library(wordcloud)
mycolors <- brewer.pal(8,"Dark2")
#设置字体
windowsFonts(myFont=windowsFont("华文中宋"))
wordcloud(freq[1:100,]$wordResult,freq[1:100,]$Freq,random.order=FALSE,random.color=FALSE,colors=mycolors,family="muFont")



library(tm)
library(wordcloud)
library(tmcn)
wordResult
text_corpus <- Corpus(x = VectorSource(text))
text_corpus
#设置矩阵条件
control=list(removeNumbers=TRUE,removePunctuation=TRUE,minDocFreq=100,WordLengths=c(2,Inf),weighting=weightTf)
#创建词条-文本矩阵，可绘制词频图
wf_q.tdm=TermDocumentMatrix(text_corpus,control)
#创建文本-词条矩阵，可创建主题模型
library(lda)#LDA模型包
library(LDAvis)#LDA可视化
library(topicmodels)
wf_q.dtm=DocumentTermMatrix(text_corpus,control)
wf_q.dtm <- removeSparseTerms(x = wf_q.dtm, sparse = 0.99)
wf_q.dtm
rowTotals<- apply(wf_q.dtm,1,sum) #Find the sum of words in each Document
dtm.new <- wf_q.dtm[rowTotals>0,]
dtm.new
Gibbs = LDA(dtm.new,k = 4,method = "Gibbs",control = list(seed = 2015,burnin = 1000,thin = 100,iter = 1000))
terms(Gibbs,10)
ldatermk<-terms(Gibbs,300)
CTM = CTM(dtm.new, k = 2,control = list(seed = 2015,var = list(tol = 10^-4), em = list(tol = 10^-3)))
VEM_fixed = LDA(dtm.new, k = 2,control = list(estimate.alpha = FALSE, seed = 2015))
terms(Gibbs,100)#查看Gibbs模型每个主题下的前10个词
ldatermk<-terms(VEM_fixed,300)
write.csv(ldatermk,"海外的主题分类.csv")
View(ldatermk2)
#分割长字符串形成列表并统计词频
rseg<-segmentCN(text1,returnType = "tm")
doc.list <- strsplit(text, "[[:space:]]+")
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)
#清洗不符合词频的词语
del <- names(term.table) %in% stop_words | term.table < 5
term.table <- term.table[!del]
vocab <- names(term.table)
#创建特征文集
get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents <- lapply(doc.list, get.terms)
documents
#特征文集的统计量
D <- length(documents) 
W <- length(vocab)  
doc.length <- sapply(documents, function(x) sum(x[2, ]))  # 
N <- sum(doc.length) 
term.frequency <- as.integer(term.table) 
#训练模型
K <- 10
G <- 3000
alpha <- 0.02
eta <- 0.02
set.seed(357)
t1 <- Sys.time()
fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, num.iterations = G, alpha = alpha, eta = eta, initial = NULL, burnin = 0,compute.log.likelihood = TRUE)
t2 <- Sys.time()
t2 - t1  #查看训练模型的时间
theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))
MovieReviews <- list(phi = phi,theta = theta,doc.length = doc.length,vocab = vocab,term.frequency = term.frequency)
json <- createJSON(phi = MovieReviews$phi, 
                   theta = MovieReviews$theta, 
                   doc.length = MovieReviews$doc.length, 
                   vocab = MovieReviews$vocab, 
                   term.frequency = MovieReviews$term.frequency)
serVis(json, out.dir = 'vis', open.browser = TRUE)

fold_num = 10  
kv_num = c(5, 10*c(1:5, 10))  
seed_num = 2003  
smp<-function(cross=fold_num,n,seed)  
{  
  set.seed(seed)  
  dd=list()  
  aa0=sample(rep(1:cross,ceiling(n/cross))[1:n],n)  
  for (i in 1:cross) dd[[i]]=(1:n)[aa0==i]  
  return(dd)  
}  
selectK<-function(dtm,kv=kv_num,SEED=seed_num,cross=fold_num,sp) # change 60 to 15  
{  
  per_ctm=NULL  
  log_ctm=NULL  
  for (k in kv)  
  {  
    per=NULL  
    loglik=NULL  
    for (i in 1:3)  #only run for 3 replications#   
    {  
      cat("R is running for", "topic", k, "fold", i,  
          as.character(as.POSIXlt(Sys.time(), "Asia/Shanghai")),"\n")  
      te=sp[[i]]  
      tr=setdiff(1:nrow(dtm),te)  
      #这里的算法还能选择Gibbs,LDA
      CTM = CTM(dtm[tr,], k = k,   
                control = list(seed = SEED, var = list(tol = 10^-4), em = list(tol = 10^-3)))    
      per=c(per,perplexity(CTM,newdata=dtm[te,]))  
      loglik=c(loglik,logLik(CTM,newdata=dtm[te,]))  
    }  
    per_ctm=rbind(per_ctm,per)  
    log_ctm=rbind(log_ctm,loglik)  
  }  
  return(list(perplex=per_ctm,loglik=log_ctm))  
}  
sp=smp(n=nrow(dtm),seed=seed_num)  
system.time((ctmK=selectK(dtm=wf_q.dtm,kv=kv_num,SEED=seed_num,cross=fold_num,sp=sp)))  
m_per=apply(ctmK[[1]],1,mean)  
m_log=apply(ctmK[[2]],1,mean)  
K[which.max(m_log)]#这是我们要的K值
#绘制复杂度曲线
k=c(kv_num)  
df = ctmK[[1]]  # perplexity matrix  
matplot(k, df, type = c("b"), xlab = "Number of topics",   
        ylab = "Perplexity", pch=1:5,col = 1, main = '')         
legend("bottomright", legend = paste("fold", 1:5), col=1, pch=1:5)


library(pacman)
p_load(rlang,data.table,tidyverse)
unnest_dt <- function(tbl, col) {
  tbl <- as.data.table(tbl)
  col <- ensyms(col)
  clnms <- syms(setdiff(colnames(tbl), as.character(col)))
  tbl <- as.data.table(tbl)
  tbl <- eval(
    expr(tbl[, as.character(unlist(!!!col)), by = list(!!!clnms)])
  )
  colnames(tbl) <- c(as.character(clnms), as.character(col))
  tbl
}
corpus<-read.csv("E:/homework/news/corpus.csv")
View(corpus)
f_table<-corpus %>% 
  unnest_dt(word) %>% 
  count(id,word)
f_table
f_table %>%
  bind_tf_idf(term = word,document = id,n = n) -> tf_idf_table
View(tf_idf_table)
tf_idf_table %>% 
  group_by(id) %>% 
  top_n(15,tf_idf) %>% 
  ungroup() -> top15
p_load(wordcloud2)
View(top15)
write.csv(tf_idf_table,file="tf_idf_table.csv",quote=F,row.names = F)
write.csv(top15,file="top15.csv",quote=F,row.names = F)
top15 %>% 
  count(word) %>% 
  top_n(200) %>% #只显示出现次数最多的200个关键词
  wordcloud2(size = 2, fontFamily = "微软雅黑",color = "random-light", backgroundColor = "grey")




#创建文档矩阵
dtm <- DocumentTermMatrix(x = text_corpus, control = list(wordLengths = c(2,Inf)))
dtm <- removeSparseTerms(x = dtm, sparse = 0.9999)
df <- as.data.frame(inspect(wf_q.dtm))
df<-scale(df)
d<-dist(df)
fit1 <- hclust(d = d, method = 'complete')
plot(fit1)
rect.hclust(tree = fit1, k = 4, border = 'red')


x<-read.table("E:/homework/大学专业分类表.txt",fill=T,header=F)
x<-is.na(x)
View(x)
