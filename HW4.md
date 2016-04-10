Facebook粉絲團分析
================

程式說明
========

本程式目的為摘錄FB的粉絲專頁統計資料，舉例說明:每日發文數、likes數、comments數與分享數，來做分析與討論，本篇第一段程式碼目的為獲得網頁上資料，依序向下為統計並篩選出最高數量的日期，並試著與時事結合，分析討論造成結果的原因。

讀取朱立倫粉絲團資料
====================

``` r
if (!require('Rfacebook')){
  install.packages("Rfacebook")
  library(Rfacebook)
}
```

    ## Loading required package: Rfacebook

    ## Loading required package: httr

    ## Loading required package: rjson

    ## Loading required package: httpuv

    ## 
    ## Attaching package: 'Rfacebook'

    ## The following object is masked from 'package:methods':
    ## 
    ##     getGroup

``` r
token<-'EAAYQEFcNfeoBADMpsZB1RPDeaXjUyP6mSMoaP79ntnZCJxHCJyMrYokROTdQ3ZBmvCoPZCiyW9YSWoWk2ZC7g6O55NT6xQLJLYd0kXVFtx2cQHJCC8ZBtbJziR0y0STZAxFETGwHQ830yRqrMZAYYawzizoDgE5EcJEeqXxSztxNGyHZA4rWZB9I7YVijoUJiZBEjAZD'
totalPage<-NULL
lastDate<-Sys.Date()
DateVectorStr<-as.character(seq(as.Date("2016-01-01"),lastDate,by="11 days"))
for(i in 1:(length(DateVectorStr)-1)){
  tempPage<-getPage("llchu", token,
                    since = DateVectorStr[i],until = DateVectorStr[i+1])
  totalPage<-rbind(totalPage,tempPage)
}
```

    ## 30 posts 28 posts 11 posts 10 posts 11 posts 8 posts 10 posts 15 posts 10 posts

``` r
#從20160101到20160408 總共貼文數量
nrow(totalPage)
```

    ## [1] 133

每日發文數分析
==============

``` r
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

|     | dateTPE    |   id|
|-----|:-----------|----:|
| 12  | 2016-01-12 |    7|
| 13  | 2016-01-13 |    5|
| 14  | 2016-01-14 |    5|
| 15  | 2016-01-15 |    5|
| 65  | 2016-03-20 |    4|
| 1   | 2016-01-01 |    3|

### 分析:2016/1/12 當日貼文數最多高達7篇，多為鼓勵群眾參予投票的相關文章，而前四多貼文數的日期分別是2016/01/12~2016/01/15，接著便是2016/03/20，依此推論，政治人物於選舉前會積極發文刷存在感

每日likes數分析
===============

``` r
totalPage$datetime <- as.POSIXct(totalPage$created_time, 
                                 format = "%Y-%m-%dT%H:%M:%S+0000", 
                                 tz = "GMT")
totalPage$dateTPE <- format(totalPage$datetime, "%Y-%m-%d", 
                            tz = "Asia/Taipei") 
totalPage$weekdays <-weekdays(as.Date(totalPage$dateTPE))
likeCount<-aggregate(likes_count~dateTPE,totalPage,mean)
library(knitr)
kable(head(likeCount[order(likeCount$likes_count,decreasing = T),]))
```

|     | dateTPE    |  likes\_count|
|-----|:-----------|-------------:|
| 16  | 2016-01-16 |       83386.0|
| 34  | 2016-02-06 |       57639.0|
| 9   | 2016-01-09 |       52729.5|
| 15  | 2016-01-15 |       49404.6|
| 17  | 2016-01-18 |       46132.0|
| 36  | 2016-02-08 |       41877.0|

### 分析:2016/01/16當日的平均like數，最多此日為總統大選。當日朱立倫粉專總共發佈兩篇貼文，1篇與周子於風波相關，此文獲得3萬多like數，而當日敗選後發布敗選感言，此文獲得13萬like數，亦是朱先生粉專最高讚數的一篇，想必此文相當的有道理且引起廣大民眾共鳴

每日comments數分析
==================

``` r
totalPage$datetime <- as.POSIXct(totalPage$created_time, 
                                 format = "%Y-%m-%dT%H:%M:%S+0000", 
                                 tz = "GMT")
totalPage$dateTPE <- format(totalPage$datetime, "%Y-%m-%d", 
                            tz = "Asia/Taipei") 
totalPage$weekdays <-weekdays(as.Date(totalPage$dateTPE))
commentCount<-aggregate(comments_count~dateTPE,totalPage,mean)
library(knitr)
kable(head(commentCount[order(commentCount$comments_count,decreasing = T),]))
```

|     | dateTPE    |  comments\_count|
|-----|:-----------|----------------:|
| 16  | 2016-01-16 |          10605.5|
| 15  | 2016-01-15 |           7843.6|
| 17  | 2016-01-18 |           3629.0|
| 9   | 2016-01-09 |           1883.0|
| 18  | 2016-01-19 |           1649.0|
| 34  | 2016-02-06 |           1377.0|

### 分析:2016/01/16獲得日均評論數最高，當日發文數2篇，因當日為總統大選，敗選感言的文章有相當多人留言。而有趣的是，最多評論數的文章卻不是敗選感言當篇，而是2016/01/15關於周子瑜風波的貼文，一句"對於一個16歲的年輕人，這樣太殘忍了"，感動了大眾，讓大家紛紛留言表態，評論數多達3萬則，足足是敗選感言的兩倍之多。

每日shares數分析
================

``` r
totalPage$datetime <- as.POSIXct(totalPage$created_time, 
                                 format = "%Y-%m-%dT%H:%M:%S+0000", 
                                 tz = "GMT")
totalPage$dateTPE <- format(totalPage$datetime, "%Y-%m-%d", 
                            tz = "Asia/Taipei") 
totalPage$weekdays <-weekdays(as.Date(totalPage$dateTPE))
sharesCount<-aggregate(shares_count~dateTPE,totalPage,mean)
library(knitr)
kable(head(sharesCount[order(sharesCount$shares_count,decreasing = T),]))
```

|     | dateTPE    |  shares\_count|
|-----|:-----------|--------------:|
| 15  | 2016-01-15 |       2342.600|
| 1   | 2016-01-01 |       1521.000|
| 16  | 2016-01-16 |       1363.500|
| 34  | 2016-02-06 |       1264.000|
| 12  | 2016-01-12 |       1000.429|
| 9   | 2016-01-09 |        937.000|

### 分析:2016/01/15獲得的日均分享數最高，而原因亦是周子瑜風波的相關貼文，與上述分析同篇，獲得本段時間最高的分享數，共7800多次，由此反覆證明朱先生的這篇貼文，真的是非常的優秀，讓台灣人民不禁為之瘋狂
