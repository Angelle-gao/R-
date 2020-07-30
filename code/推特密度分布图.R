library(tidyverse)
library(data.table)
library(ggplot2)
library(ggridges)
library(RColorBrewer)
library(ggthemes)
library(lubridate)
library(magrittr)


setwd("C:/Users/Lenovo/Desktop")

#----------------------#
#--转发微博时间分布图--#
#----------------------#

Retweet<-read_tsv("2020-3-26-dataset.tsv") %>% 
  subset(date=="2020-3-22") %>% 
  mutate(time=hms(time)) %>% 
  mutate(hour=hour(time)) %>% 
  mutate(minute=minute(time)) %>% 
  mutate(Time=paste(hour,minute,sep = "-")) 


#小时级频数
data_h<-Retweet %>% 
  data.table() %>% 
  .[,.N,by=hour]

#这个小时级图数据不行，没有区分度
ggplot(data_h,aes(x=hour,y=N))+
  geom_line()



#分钟级频数
data_m<-Retweet %>% 
  data.table() %>% 
  .[,.N,by=Time]  %>% 
  mutate(minute=gsub("-",":",Time))

data_m$minute<-strptime(data_m$minute,"%H:%M") 

df<-data_m[,3:2] %>% 
  mutate(minute=as.POSIXct(minute))

ggplot(df,aes(x=minute,y=N))+
  geom_line()+
  scale_x_datetime(date_labels = "%H",
                   date_breaks = "1 hour",
                   date_minor_breaks = "1 hour")+
  ylab("Frequency")+
  xlab("Hour")

ggsave("Retweet.png",width=20, height=10,units="cm",dpi = 600)




#------------------------#
#--未转发微博时间分布图--#
#------------------------#

NotRetweet<-read_tsv("clean-dataset.tsv")%>% 
  subset(date=="2020-3-22") %>% 
  mutate(time=hms(time)) %>% 
  mutate(hour=hour(time)) %>% 
  mutate(minute=minute(time)) %>% 
  mutate(Time=paste(hour,minute,sep = "-")) 


#小时级频数
data_h<-NotRetweet %>% 
  data.table() %>% 
  .[,.N,by=hour]

#这个小时级图数据还行，没有区分度
ggplot(data_h,aes(x=hour,y=N))+
  geom_line()



#分钟级频数
data_m<-NotRetweet %>% 
  data.table() %>% 
  .[,.N,by=Time] %>% 
  mutate(minute=gsub("-",":",Time))

data_m$minute<-strptime(data_m$minute,"%H:%M") 

df<-data_m[,3:2] %>% 
  mutate(minute=as.POSIXct(minute))

ggplot(df,aes(x=minute,y=N))+
  geom_line()+
  scale_x_datetime(date_labels = "%H",
                   date_breaks = "1 hour",
                   date_minor_breaks = "1 hour")+
  ylab("Frequency")+
  xlab("Hour")

ggsave("NotRetweet.png",width=20, height=10,units="cm",dpi = 600)














