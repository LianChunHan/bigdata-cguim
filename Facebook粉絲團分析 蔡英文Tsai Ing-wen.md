---
title:"Facebook�����Τ��R ���^��Tsai Ing-wen"
output: github_document
���R���^��ɵ��M���A��Ƥ��R�϶���2016/01/01�� 2016/04/11
---
if(!require('Rfacebook'))
{ install.packages("Rfacebook") 
library(Rfacebook) }

# Ū�����^�寻���θ��
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
token<-'CAACEdEose0cBAH3feqC73pX9grU9aOQWZCIKkEPQqOQd1pZCWOHDWsHWQyaxXMSptQFV2LCjJFHAqtkRaMS6UNVjNuh1XeYR4MZAnUtnxXufT8MGqYs0spRaFKnQLkrpi26LfZAT8wDwJ2mMpGV69n7KMPNAXkd94wIpXdGSJpZBT5KwjuFXK8o3E8QZBCkSOx7SOXIWbu5AZDZD'
totalPage<-NULL
lastDate<-Sys.Date()
DateVectorStr<-as.character(seq(as.Date("2016-01-01"),lastDate,by="5 days"))
for(i in 1:(length(DateVectorStr)-1)){
    tempPage<-getPage("TsaiIngwen", token,
                      since = DateVectorStr[i],until = DateVectorStr[i+1])
    totalPage<-rbind(totalPage,tempPage)
}
nrow(totalPage)
```
2016/01/01 �� 2016/04/11�A���^�寻���Τ@�@�� 212 �g�峹

# �C��o��Ƥ��R
���R���^�寻���ΨC��o��ơA�ѩ����榡���@�G���N���ର�x�W�ɰϫ�A���έp
```{r cars}
totalPage$datetime <- as.POSIXct(totalPage$created_time, 
                                 format = "%Y-%m-%dT%H:%M:%S+0000", 
                                 tz = "GMT") 
totalPage$dateTPE <- format(totalPage$datetime, "%Y-%m-%d", 
                            tz = "Asia/Taipei") 
totalPage$weekdays <-weekdays(as.Date(totalPage$dateTPE))
PostCount<-aggregate(id~dateTPE,totalPage,length)
library(knitr)
kable(head(PostCount[order(PostCount$id,decreasing = T),]))
```
|   |dateTPE    | id|
|:--|:----------|--:|
|15 |2016-01-15 |  8|
|11 |2016-01-11 |  7|
|14 |2016-01-14 |  7|
|8  |2016-01-08 |  6|
|10 |2016-01-10 |  6|
|13 |2016-01-13 |  6|
�Q��:1/15�o��Ƴ̦h(1)���i�ҥi�H�J�A�غاx���A�_�A�ڸ�j�a�]�i�H�@�_�J�A
(2)�]�\�A�]���ܦ��A�ҥH���`�^�a�A�]���`�Q���a�m�C���O�A
(3)�i���ʹ�p�����T���j �u�I�G�x�W�C�@���v�����Ө�̫�@��
��1/08~1/15���X�Ѧ]���O�`�Τj��e�i�ҥH�񥭱`�h

# �C��likes�Ƥ��R
���R���^�寻���ΨC�骺�I�g�ƨòέp
```{r pressure, echo=FALSE}
likescount<-aggregate(likes_count~dateTPE,totalPage,sum)
library(knitr)
kable(head(likescount[order(likescount$likes_count,decreasing = T),]))
```
|   |dateTPE    | likes_count|
|:--|:----------|-----------:|
|15 |2016-01-15 |      545547|
|14 |2016-01-14 |      340963|
|10 |2016-01-10 |      278127|
|13 |2016-01-13 |      202274|
|11 |2016-01-11 |      187555|
|12 |2016-01-12 |      168502|
�Q��:1/15���Q�h�U�g�̦h,�䦸�O1/14��30�h�U�g

# �C��comments�Ƥ��R
���R���^�寻���ΨC�骺�Q�׼ƨòέp
```{r pressure, echo=FALSE}
commentscount<-aggregate(comments_count~dateTPE,totalPage,sum)
library(knitr)
kable(head(commentscount[order(commentscount$comments_count,decreasing = T),]))
```
|   |dateTPE    | comments_count|
|:--|:----------|--------------:|
|15 |2016-01-15 |          32716|
|14 |2016-01-14 |           8261|
|10 |2016-01-10 |           5300|
|13 |2016-01-13 |           4907|
|12 |2016-01-12 |           4405|
|11 |2016-01-11 |           4326|
�Q��:1/15�d��3�U2�d�h��,�䦸�O1/14��8000�h��


# �C��shares�Ƥ��R
���R���^�寻���ΨC�骺���ɼƨð��έp
```{r pressure, echo=FALSE}
sharescount<-aggregate(shares_count~dateTPE,totalPage,sum)
library(knitr)
kable(head(sharescount[order(sharescount$shares_count,decreasing = T),]))
```
|   |dateTPE    | shares_count|
|:--|:----------|------------:|
|15 |2016-01-15 |        18248|
|14 |2016-01-14 |        14334|
|10 |2016-01-10 |         9258|
|13 |2016-01-13 |         5816|
|11 |2016-01-11 |         4985|
|6  |2016-01-06 |         4973|
�Q��:1/15���ɼƳ̦h��1�U8�d�h��,�䦸�O1/14��1�U4�d�h��

#�����d���W�v���R
���R���^�寻���ΨC�骺�����d���W�v
```{r pressure, echo=FALSE}
totalComment<-NULL
for(i in 1:10){
post<-getPost(totalPage$id[i],token,n.comments=totalPage$comments_count)
tempComment<-cbind(post$post$id,post$comments$from_name)
totalComment<-rbind(totalComment,tempComment)
}
totalComment<-data.frame(totalComment)
colnames(totalComment)<-c("postID","commentName")
NameCount<-aggregate(postID~commentName,totalComment,FUN=length)
library(knitr)
kable(head(NameCount[order(NameCount$postID,decreasing=T),],10))
```
|     |commentName | postID|
|:----|:-----------|------:|
|1629 |�d����      |    136|
|3165 |�B���W      |     90|
|422  |Gao Gary    |     81|
|1812 |������      |     63|
|3442 |�鮶��      |     61|
|1246 |Whanny Lin  |     53|
|3093 |�Q�婾      |     43|
|2850 |�����x      |     41|
|866  |Mike  Liu   |     40|
|122  |Amanda Wang |     37|
�Q��:�ھڪ����ܡA�d���ת��d�����Ƴ̦h

�d�����ƲĤG�h���O�u�B���W�v

�d�����ƲĤT�h���O�uGao Gary �v
