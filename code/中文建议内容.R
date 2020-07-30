library(Rwordseg)#分词处理工具
library(tm)#文本整理工具，创建矩阵
library(tmcn)
library(lda)#LDA模型包
library(LDAvis)#LDA可视化
library(topicmodels)

evaluaton<-readLines("C:/Users/Lenovo/Desktop/中文建议内容.txt",encoding = "UTF-8")
evaluaton1<-gsub('[a-zA-Z0-9]','',evaluaton) #去除所有英文字母和数字
evaluaton<-gsub("[的|和|了|来|与|到|由|等|从|以|一|为|在|上|各|去|对|侧|多|并|千|万|年|更|向|这是]","",evaluaton1)
#读入停用词
stop_words<-readLines("C:/Users/Lenovo/Desktop/中文停用词.txt",encoding = "UTF-8")
#分词
rseg<-segmentCN(evaluaton,returnType = "tm")#返还tm格式
#数据清洗
vid<-VCorpus(VectorSource(rseg))#getSource()查看预设的来源信息
wf_q<-tm_map(vid,stripWhitespace)#清洗空白
###tm_map(reuters, removeWords, stopwordCN(stop_words)),清洗停用词,但是好像对中文支持有点差
#设置矩阵条件
control<-list(removeNumbers=TRUE,removePunctuation=TRUE,minDocFreq=5,WordLengths=c(2,Inf),weighting=weightTf)
#创建词条-文本矩阵，可绘制词频图
wf_q.tdm<-TermDocumentMatrix(wf_q,control)
#创建文本-词条矩阵，可创建主题模型
wf_q.dtm<-DocumentTermMatrix(wf_q,control)


#文本聚类
data<-t(wf_q.tdm[,1:50])
data.scale <- scale(data)
d <- dist(data.scale, method = "euclidean")
fit <- hclust(d, method="complete")
plot(fit,main="文本聚类")

#主题分析
rowTotals<- apply(wf_q.dtm,1,sum) #Find the sum of words in each Document
dtm.new <- wf_q.dtm[rowTotals>0,]
dtm.new
Gibbs<- LDA(dtm.new,k = 5,method = "Gibbs",control = list(seed = 2015,burnin = 1000,thin = 100,iter = 1000))
terms(Gibbs,10)
CTM <-CTM(dtm.new, k = 5,control = list(seed = 2015,var = list(tol = 10^-4), em = list(tol = 10^-3)))
VEM_fixed<- LDA(dtm.new, k = 5,control = list(estimate.alpha = FALSE, seed = 2015))
terms(Gibbs,10)#查看Gibbs模型每个主题下的前10个词

#分割长字符串形成列表并统计词频
doc.list <- strsplit(rseg, "[[:space:]]+")
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
#特征文集的统计量
D <- length(documents) 
W <- length(vocab)  
doc.length <- sapply(documents, function(x) sum(x[2, ]))  # 
N <- sum(doc.length) 
term.frequency <- as.integer(term.table) 

#训练模型
K <- 3
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

library(servr)
serVis(json, out.dir = './vis', open.browser = T)
writeLines(iconv(readLines("./vis/lda.json"), from = "GBK", to = "UTF8"), 
           file("./vis/lda.json", encoding="UTF-8"))

