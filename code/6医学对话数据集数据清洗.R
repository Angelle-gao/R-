setwd("E:/homework/news")
library(dplyr)
data<-read.csv("E:/homework/news/医学对话.csv")
View(data)
new_data=bind_rows(merge_data,data[15,])
merge_data=new_data
match<-rowSums(data=="医生:")
match
length(match)
for(i in 16:length(match)){
  if(match[i]==1){
    new_data=rbind(data[i,],data[i+1,])
    merge_data=rbind(merge_data,new_data)
  }
}
doctor_dialog<-merge_data
doctor_dialog<-filter(doctor_dialog,doctor_dialog$content!="医生:")
View(merge_data)
View(doctor_dialog)
write.csv(doctor_dialog,"中文医生对话内容.csv")
match<-rowSums(data=="病人:")
match
length(match)
merge_data<-data[18,]
merge_data
new_data=bind_rows(merge_data,data[19,])
merge_data=new_data
for(i in 19:length(match)){
  if(match[i]==1){
    new_data=rbind(data[i,],data[i+1,])
    merge_data=rbind(merge_data,new_data)
  }
}
patient_dialog<-merge_data
View(patient_dialog)
patient_dialog<-filter(patient_dialog,patient_dialog$content!="病人:")
write.csv(patient_dialog,"中文病人对话内容.csv")

match<-rowSums(data=="Diagnosis and suggestions")
match[202]
length(match)
merge_data<-data[171,]
merge_data
new_data=bind_rows(merge_data,data[173,])
merge_data=new_data
for(i in 174:length(match)){
  if(match[i]==1){
    new_data=rbind(data[i,],data[i+2,])
    merge_data=rbind(merge_data,new_data)
  }
}
conclusion<-merge_data
View(conclusion)
conclusion<-filter(conclusion,conclusion$content!="Diagnosis and suggestions")
write.csv(conclusion,"中文总结内容.csv")

match<-rowSums(data=="Description")
View(data)
match
length(match)
merge_data<-data[3,]
new_data=bind_rows(merge_data,data[5:6,])
merge_data=new_data
merge_data
for(i in 8:length(match)){
  if(match[i]==1){
    new_data=rbind(data[i,],data[i+2,])
    merge_data=rbind(merge_data,new_data)
  }
}
symptom<-merge_data
View(symptom)
symptom<-filter(symptom,symptom$content!="Dialog")
write.csv(symptom,"中文病情描述.csv")
