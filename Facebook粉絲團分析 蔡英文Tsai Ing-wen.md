---
title:"Facebook粉絲團分析 蔡英文Tsai Ing-wen"
output: github_document
分析蔡英文紛絲專頁，資料分析區間為2016/01/01至 2016/04/11
---
if(!require('Rfacebook'))
{ install.packages("Rfacebook") 
library(Rfacebook) }

# 讀取蔡英文粉絲團資料
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
2016/01/01 至 2016/04/11，蔡英文粉絲團一共有 212 篇文章

# 每日發文數分析
分析蔡英文粉絲團每日發文數，由於日期格式不一故先將其轉為台灣時區後再做統計
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
討論:1/15發文數最多(1)民進黨可以克服種種困難再起，我跟大家也可以一起克服
(2)也許你因為很忙，所以不常回家，也不常想念家鄉。但是，
(3)【活動實況直播訊息】 「點亮台灣每一里」車掃來到最後一哩
而1/08~1/15那幾天因為是總統大選前夕所以比平常多

# 每日likes數分析
分析蔡英文粉絲團每日的點讚數並統計
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
討論:1/15五十多萬讚最多,其次是1/14的30多萬讚

# 每日comments數分析
分析蔡英文粉絲團每日的討論數並統計
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
討論:1/15留言3萬2千多筆,其次是1/14的8000多筆


# 每日shares數分析
分析蔡英文粉絲團每日的分享數並做統計
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
討論:1/15分享數最多有1萬8千多筆,其次是1/14的1萬4千多筆

#粉絲留言頻率分析
分析蔡英文粉絲團每日的粉絲留言頻率
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
|1629 |吳志豐      |    136|
|3165 |劉金柱      |     90|
|422  |Gao Gary    |     81|
|1812 |李雲方      |     63|
|3442 |鍾振平      |     61|
|1246 |Whanny Lin  |     53|
|3093 |鄒賢忠      |     43|
|2850 |曾景釗      |     41|
|866  |Mike  Liu   |     40|
|122  |Amanda Wang |     37|
討論:根據表格顯示，吳志豐的留言次數最多

留言次數第二多的是「劉金柱」

留言次數第三多的是「Gao Gary 」
