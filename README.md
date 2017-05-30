長庚大學 大數據分析方法 作業六
================

國內旅遊人數分析
----------------

組員姓名:李旻臻、毛臆菱

分析議題背景
------------

一個國家觀光業的發展通常對於國際與國內總體環境的變化具有高程度的敏感性， 所以觀光業已成為國家與地方經濟發展的重要角色。

分析動機
--------

由於旅遊已成為大多數人的休閒活動之一，所以我們想了解旅遊次數是否和職業或月收入有交互影響的關係。 並且更進一步分析年齡、教育程度、婚姻狀況和居住地區是否對旅遊次數有影響。

使用資料
--------

交通部觀光局opendata

載入使用資料們

``` r
library(readr)
traveldata <- read.csv("C:/Users/Eileen/Desktop/taiwantravel.csv",fileEncoding ="Big5")
View(traveldata)
```

資料處理與清洗
--------------

step1 先將居住地區分類 step2 將分好的居住地區再細分個人每月平均所得 step3 藉由所得來觀察職業

在處理資料的過程中發現北部旅遊人數最多的所得竟然是無收入，所以我們在下面的圖表做了整個資料表無收入最多的職業是家庭管理 處理資料

``` r
traveldatan <- traveldata[grepl("北部",traveldata$`居住地區`),] #北部旅遊的表單

library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
group_by(traveldatan,Revenue=`個人每月平均所得`) %>%
  summarise(`個人每月平均所得人數(北)`=n()) %>%
  arrange(desc(`個人每月平均所得人數(北)`)) #北部月所得旅遊表單
```

    ## # A tibble: 10 × 2
    ##                   Revenue `個人每月平均所得人數(北)`
    ##                    <fctr>                      <int>
    ## 1                  無收入                       1747
    ## 2              未滿一萬元                       1547
    ## 3      二萬元至未滿三萬元                       1528
    ## 4      三萬元至未滿四萬元                       1469
    ## 5      五萬元至未滿七萬元                        985
    ## 6      四萬元至未滿五萬元                        921
    ## 7  一萬元至未滿一萬五千元                        499
    ## 8  一萬五千元至未滿二萬元                        498
    ## 9      七萬元至未滿十萬元                        356
    ## 10           十萬元及以上                        343

``` r
northmoneytd<-traveldatan[grepl("無收入",traveldatan$`個人每月平均所得`),] #北部旅遊無收入的表單

library(dplyr)
group_by(northmoneytd,Career=`職業`) %>%
  summarise(`無收入職業人數(北)`=n()) %>%
  arrange(desc(`無收入職業人數(北)`)) #北部無收入職業旅遊表單
```

    ## # A tibble: 7 × 2
    ##                                               Career `無收入職業人數(北)`
    ##                                               <fctr>                <int>
    ## 1                                 家庭管理(料理家務)                  839
    ## 2                                               學生                  546
    ## 3                                           退休人員                  198
    ## 4 軍公教人員民意代表、行政主管、企業主管、及經理人員                  158
    ## 5                                 服務及銷售工作人員                    3
    ## 6                             機械設備操作及組裝人員                    2
    ## 7                             農、林、漁、牧生產人員                    1

``` r
northmoneyjobtd<-northmoneytd[grepl("家庭管理",northmoneytd$`職業`),]

library(dplyr)
group_by(northmoneyjobtd,Years=`年齡`) %>%
  summarise(`無收入家庭管理年齡人數(北)`=n()) %>%
  arrange(desc(`無收入家庭管理年齡人數(北)`)) #北部無收入家庭管理年齡旅遊表單
```

    ## # A tibble: 8 × 2
    ##        Years `無收入家庭管理年齡人數(北)`
    ##       <fctr>                        <int>
    ## 1   50～59歲                          239
    ## 2   60～64歲                          192
    ## 3   40～49歲                          149
    ## 4   30～39歲                          109
    ## 5   65～69歲                           75
    ## 6 70歲及以上                           57
    ## 7   25～29歲                           13
    ## 8   20～24歲                            5

``` r
traveldatas <- traveldata[grepl("南部",traveldata$`居住地區`),] #南部旅遊的表單

library(dplyr)
group_by(traveldatas,Revenue=`個人每月平均所得`) %>%
  summarise(`個人每月平均所得人數(南)`=n()) %>%
  arrange(desc(`個人每月平均所得人數(南)`)) #南部月所得旅遊表單
```

    ## # A tibble: 10 × 2
    ##                   Revenue `個人每月平均所得人數(南)`
    ##                    <fctr>                      <int>
    ## 1      二萬元至未滿三萬元                       1179
    ## 2              未滿一萬元                       1164
    ## 3                  無收入                        843
    ## 4      三萬元至未滿四萬元                        789
    ## 5      四萬元至未滿五萬元                        483
    ## 6      五萬元至未滿七萬元                        460
    ## 7  一萬五千元至未滿二萬元                        415
    ## 8  一萬元至未滿一萬五千元                        350
    ## 9      七萬元至未滿十萬元                        177
    ## 10           十萬元及以上                        114

``` r
southmoneytd<-traveldatas[grepl("二萬元至未滿三萬元",traveldatas$`個人每月平均所得`),]

library(dplyr)
group_by(southmoneytd,Career=`職業`) %>%
  summarise(`二萬元至未滿三萬元職業人數(南)`=n()) %>%
  arrange(desc(`二萬元至未滿三萬元職業人數(南)`)) #南部二萬~三萬職業旅遊表單
```

    ## # A tibble: 14 × 2
    ##                                                      Career
    ##                                                      <fctr>
    ## 1                                        服務及銷售工作人員
    ## 2  事務支援人員(辦公室事務人員、資料輸入人員、銀行櫃員....)
    ## 3                                      技術員及助理專業人員
    ## 4                                    機械設備操作及組裝人員
    ## 5                      基層技術工及勞力工(清潔工、幫工....)
    ## 6                                                  退休人員
    ## 7                                        家庭管理(料理家務)
    ## 8       技藝有關人員(營造、砌磚、手工藝、印刷...等工作人員)
    ## 9                                    農、林、漁、牧生產人員
    ## 10       軍公教人員民意代表、行政主管、企業主管、及經理人員
    ## 11                 專業人員(醫師、律師、建築師、會計師....)
    ## 12                                               軍公教人員
    ## 13                                                     學生
    ## 14                 民意代表、行政主管、企業主管、及經理人員
    ## # ... with 1 more variables: `二萬元至未滿三萬元職業人數(南)` <int>

``` r
southmoneyjobtd<-southmoneytd[grepl("服務及銷售工作人員",southmoneytd$`職業`),]

library(dplyr)
group_by(southmoneyjobtd,Years=`年齡`) %>%
  summarise(`二萬元至未滿三萬元服務及銷售工作人員年齡人數(南)`=n()) %>%
  arrange(desc(`二萬元至未滿三萬元服務及銷售工作人員年齡人數(南)`)) #南部二萬~三萬服務及銷售工作人員年齡旅遊表單
```

    ## # A tibble: 9 × 2
    ##        Years `二萬元至未滿三萬元服務及銷售工作人員年齡人數(南)`
    ##       <fctr>                                              <int>
    ## 1   40～49歲                                                 65
    ## 2   50～59歲                                                 51
    ## 3   25～29歲                                                 45
    ## 4   30～39歲                                                 45
    ## 5   20～24歲                                                 32
    ## 6   60～64歲                                                 20
    ## 7   65～69歲                                                 11
    ## 8 70歲及以上                                                  6
    ## 9   15～19歲                                                  3

``` r
traveldatae <- traveldata[grepl("東部",traveldata$`居住地區`),] #東部旅遊的表單

library(dplyr)
group_by(traveldatae,Revenue=`個人每月平均所得`) %>%
  summarise(`個人每月平均所得人數(東)`=n()) %>%
  arrange(desc(`個人每月平均所得人數(東)`)) #東部月所得旅遊表單
```

    ## # A tibble: 10 × 2
    ##                   Revenue `個人每月平均所得人數(東)`
    ##                    <fctr>                      <int>
    ## 1              未滿一萬元                        107
    ## 2      二萬元至未滿三萬元                         85
    ## 3                  無收入                         72
    ## 4      三萬元至未滿四萬元                         56
    ## 5      五萬元至未滿七萬元                         56
    ## 6      四萬元至未滿五萬元                         51
    ## 7  一萬元至未滿一萬五千元                         35
    ## 8  一萬五千元至未滿二萬元                         31
    ## 9      七萬元至未滿十萬元                         21
    ## 10           十萬元及以上                         14

``` r
eastmoneytd<-traveldatae[grepl("未滿一萬元",traveldatae$`個人每月平均所得`),]

library(dplyr)
group_by(eastmoneytd,Career=`職業`) %>%
  summarise(`未滿一萬元職業人數(東)`=n()) %>%
  arrange(desc(`未滿一萬元職業人數(東)`))
```

    ## # A tibble: 9 × 2
    ##                                                Career
    ##                                                <fctr>
    ## 1                                  家庭管理(料理家務)
    ## 2                                            退休人員
    ## 3                                                學生
    ## 4                              農、林、漁、牧生產人員
    ## 5                                  服務及銷售工作人員
    ## 6  軍公教人員民意代表、行政主管、企業主管、及經理人員
    ## 7                                          軍公教人員
    ## 8                基層技術工及勞力工(清潔工、幫工....)
    ## 9 技藝有關人員(營造、砌磚、手工藝、印刷...等工作人員)
    ## # ... with 1 more variables: `未滿一萬元職業人數(東)` <int>

``` r
eastmoneyjobtd<-eastmoneytd[grepl("家庭管理",eastmoneytd$`職業`),]

library(dplyr)
group_by(eastmoneyjobtd,Years=`年齡`) %>%
  summarise(`未滿一萬元家庭管理年齡人數(東)`=n()) %>%
  arrange(desc(`未滿一萬元家庭管理年齡人數(東)`))
```

    ## # A tibble: 6 × 2
    ##        Years `未滿一萬元家庭管理年齡人數(東)`
    ##       <fctr>                            <int>
    ## 1 70歲及以上                               19
    ## 2   60～64歲                                6
    ## 3   50～59歲                                4
    ## 4   65～69歲                                3
    ## 5   30～39歲                                2
    ## 6   40～49歲                                2

``` r
traveldatam <- traveldata[grepl("中部",traveldata$`居住地區`),] #中部旅遊的表單

library(dplyr)
group_by(traveldatam,Revenue=`個人每月平均所得`) %>%
  summarise(`個人每月平均所得人數(中)`=n()) %>%
  arrange(desc(`個人每月平均所得人數(中)`)) #中部月所得旅遊表單
```

    ## # A tibble: 10 × 2
    ##                   Revenue `個人每月平均所得人數(中)`
    ##                    <fctr>                      <int>
    ## 1      二萬元至未滿三萬元                       1096
    ## 2              未滿一萬元                        985
    ## 3                  無收入                        896
    ## 4      三萬元至未滿四萬元                        746
    ## 5      五萬元至未滿七萬元                        447
    ## 6      四萬元至未滿五萬元                        422
    ## 7  一萬五千元至未滿二萬元                        323
    ## 8  一萬元至未滿一萬五千元                        323
    ## 9      七萬元至未滿十萬元                        151
    ## 10           十萬元及以上                        113

``` r
middlemoneytd<-traveldatam[grepl("二萬元至未滿三萬元",traveldatam$`個人每月平均所得`),]

library(dplyr)
group_by(middlemoneytd,Career=`職業`) %>%
  summarise(`二萬元至未滿三萬元職業人數(中)`=n()) %>%
  arrange(desc(`二萬元至未滿三萬元職業人數(中)`))
```

    ## # A tibble: 14 × 2
    ##                                                      Career
    ##                                                      <fctr>
    ## 1                                        服務及銷售工作人員
    ## 2  事務支援人員(辦公室事務人員、資料輸入人員、銀行櫃員....)
    ## 3                                      技術員及助理專業人員
    ## 4                                    機械設備操作及組裝人員
    ## 5                                                  退休人員
    ## 6                      基層技術工及勞力工(清潔工、幫工....)
    ## 7                                        家庭管理(料理家務)
    ## 8       技藝有關人員(營造、砌磚、手工藝、印刷...等工作人員)
    ## 9                                    農、林、漁、牧生產人員
    ## 10                                               軍公教人員
    ## 11       軍公教人員民意代表、行政主管、企業主管、及經理人員
    ## 12                 專業人員(醫師、律師、建築師、會計師....)
    ## 13                 民意代表、行政主管、企業主管、及經理人員
    ## 14                                                     學生
    ## # ... with 1 more variables: `二萬元至未滿三萬元職業人數(中)` <int>

``` r
middlemoneyjobtd<-middlemoneytd[grepl("服務及銷售工作人員",middlemoneytd$`職業`),]

library(dplyr)
group_by(middlemoneyjobtd,Years=`年齡`) %>%
  summarise(`二萬元至未滿三萬元服務及銷售工作人員年齡人數(中)`=n()) %>%
  arrange(desc(`二萬元至未滿三萬元服務及銷售工作人員年齡人數(中)`))
```

    ## # A tibble: 9 × 2
    ##        Years `二萬元至未滿三萬元服務及銷售工作人員年齡人數(中)`
    ##       <fctr>                                              <int>
    ## 1   50～59歲                                                 51
    ## 2   30～39歲                                                 50
    ## 3   40～49歲                                                 47
    ## 4   25～29歲                                                 46
    ## 5   20～24歲                                                 30
    ## 6   60～64歲                                                 18
    ## 7   65～69歲                                                  7
    ## 8   15～19歲                                                  3
    ## 9 70歲及以上                                                  3

``` r
traveldatao <- traveldata[grepl("離島地區",traveldata$`居住地區`),] #離島旅遊的表單

library(dplyr)
group_by(traveldatao,Revenue=`個人每月平均所得`) %>%
  summarise(`個人每月平均所得人數(離島)`=n()) %>%
  arrange(desc(`個人每月平均所得人數(離島)`)) #離島月所得旅遊表單
```

    ## # A tibble: 10 × 2
    ##                   Revenue `個人每月平均所得人數(離島)`
    ##                    <fctr>                        <int>
    ## 1              未滿一萬元                           64
    ## 2      二萬元至未滿三萬元                           59
    ## 3                  無收入                           56
    ## 4      五萬元至未滿七萬元                           43
    ## 5      三萬元至未滿四萬元                           38
    ## 6      四萬元至未滿五萬元                           30
    ## 7  一萬元至未滿一萬五千元                           26
    ## 8      七萬元至未滿十萬元                           18
    ## 9  一萬五千元至未滿二萬元                           17
    ## 10           十萬元及以上                            9

``` r
outmoneytd<-traveldatao[grepl("未滿一萬元",traveldatao$`個人每月平均所得`),]

library(dplyr)
group_by(outmoneytd,Career=`職業`) %>%
  summarise(`未滿一萬元職業人數(離島)`=n()) %>%
  arrange(desc(`未滿一萬元職業人數(離島)`))
```

    ## # A tibble: 7 × 2
    ##                                                     Career
    ##                                                     <fctr>
    ## 1                                       家庭管理(料理家務)
    ## 2                                                 退休人員
    ## 3                                                     學生
    ## 4                                       服務及銷售工作人員
    ## 5                                   農、林、漁、牧生產人員
    ## 6 事務支援人員(辦公室事務人員、資料輸入人員、銀行櫃員....)
    ## 7                     基層技術工及勞力工(清潔工、幫工....)
    ## # ... with 1 more variables: `未滿一萬元職業人數(離島)` <int>

``` r
outmoneyjobtd<-outmoneytd[grepl("家庭管理",outmoneytd$`職業`),]

library(dplyr)
group_by(outmoneyjobtd,Years=`年齡`) %>%
  summarise(`未滿一萬元家庭管理年齡人數(離島)`=n()) %>%
  arrange(desc(`未滿一萬元家庭管理年齡人數(離島)`))
```

    ## # A tibble: 6 × 2
    ##        Years `未滿一萬元家庭管理年齡人數(離島)`
    ##       <fctr>                              <int>
    ## 1 70歲及以上                                 18
    ## 2   65～69歲                                  7
    ## 3   40～49歲                                  2
    ## 4   50～59歲                                  2
    ## 5   30～39歲                                  1
    ## 6   60～64歲                                  1

``` r
traveldatanomoney <- traveldata[grepl("無收入",traveldata$`個人每月平均所得`),] #無收入旅遊的表單

library(dplyr)
nmtd<-group_by(traveldatanomoney,Career=`職業`) %>%
  summarise(`無收入職業人數`=n()) %>%
  arrange(desc(`無收入職業人數`)) #無收入旅遊的職業
```

探索式資料分析
--------------

圖文並茂圖文並茂

``` r
library(ggplot2)
ggplot()+geom_bar(data=nmtd,
                  aes(x=Career,y=`無收入職業人數`),
                  stat = "identity")
```

![](README_files/figure-markdown_github/unnamed-chunk-3-1.png)

期末專題分析規劃
----------------

1.性別和旅遊次數的關係 2.分析各個地區的人均所得是否和旅遊次數有相關 3.學生的旅遊季別是否都集中在寒暑假期間 4.何種職業的人們最常旅遊
