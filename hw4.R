
if (!require('Rfacebook')){
  install.packages("Rfacebook")
  library(Rfacebook)
}
token<-'EAAYQEFcNfeoBABgRsZAGwaYhiIQOo4Ntx1amAV559BICMD0BZAm3VhUwtDz4qPHP9N8ZCYYpydrgI23RKMWeQtrzqDbdTMcHZCQLRHlJZADZAfWw8x17jw6ZCQqE0VbuCZCtxRsLNlEsQMIWb4rsj1LF5uXS95fjjm8UG2NCnbzhqRUuKZC30z3L0JILIA2OPVyoZD'
totalPage<-NULL
lastDate<-Sys.Date()
DateVectorStr<-as.character(seq(as.Date("2016-01-01"),lastDate,by="11 days"))
for(i in 1:(length(DateVectorStr)-1)){
  tempPage<-getPage("llchu", token,
                    since = DateVectorStr[i],until = DateVectorStr[i+1])
  totalPage<-rbind(totalPage,tempPage)
}
nrow(totalPage)



#postcount--------------------------------------------------------------
totalPage$datetime <- as.POSIXct(totalPage$created_time, 
                                 format = "%Y-%m-%dT%H:%M:%S+0000", 
                                 tz = "GMT") 
totalPage$dateTPE <- format(totalPage$datetime, "%Y-%m-%d", 
                            tz = "Asia/Taipei")
totalPage$weekdays <-weekdays(as.Date(totalPage$dateTPE))
PostCount<-aggregate(id~dateTPE,totalPage,length)
library(knitr)
kable(head(PostCount[order(PostCount$id,decreasing = T),]))




#likescount---------------------------------------------------------------
totalPage$datetime <- as.POSIXct(totalPage$created_time, 
                                 format = "%Y-%m-%dT%H:%M:%S+0000", 
                                 tz = "GMT")
totalPage$dateTPE <- format(totalPage$datetime, "%Y-%m-%d", 
                            tz = "Asia/Taipei") 
totalPage$weekdays <-weekdays(as.Date(totalPage$dateTPE))
likeCount<-aggregate(likes_count~dateTPE,totalPage,mean)
library(knitr)
kable(head(likeCount[order(likeCount$likes_count,decreasing = T),]))


#commentscount-------------------------------------------------------------
totalPage$datetime <- as.POSIXct(totalPage$created_time, 
                                 format = "%Y-%m-%dT%H:%M:%S+0000", 
                                 tz = "GMT")
totalPage$dateTPE <- format(totalPage$datetime, "%Y-%m-%d", 
                            tz = "Asia/Taipei") 
totalPage$weekdays <-weekdays(as.Date(totalPage$dateTPE))
commentCount<-aggregate(comments_count~dateTPE,totalPage,mean)
library(knitr)
kable(head(commentCount[order(commentCount$comments_count,decreasing = T),]))





#sharescount---------------------------------------------------------------
totalPage$datetime <- as.POSIXct(totalPage$created_time, 
                                 format = "%Y-%m-%dT%H:%M:%S+0000", 
                                 tz = "GMT")
totalPage$dateTPE <- format(totalPage$datetime, "%Y-%m-%d", 
                            tz = "Asia/Taipei") 
totalPage$weekdays <-weekdays(as.Date(totalPage$dateTPE))
sharesCount<-aggregate(shares_count~dateTPE,totalPage,mean)
library(knitr)
kable(head(sharesCount[order(sharesCount$shares_count,decreasing = T),]))















