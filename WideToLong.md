WideToLong
================
Anne Margit
5/26/2020

``` r
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
library(tidyr)
library(anytime)
library(arsenal)
library(reshape2)
```

    ## 
    ## Attaching package: 'reshape2'

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     smiths

``` r
library(knitr)
library(papaja)
```

``` r
data.all <- read.csv("RMD06_Anne Margit Reitsema_2020-05-26 10-04 CEST.csv", header=TRUE)
```

``` r
data.all$StartDate <- anydate(data.all$StartDate)
data.all$w1_StartDate <- anydate(data.all$w1_StartDate)
data.all$w2_StartDate <- anydate(data.all$w2_StartDate)
data.all$w3_StartDate <- anydate(data.all$w3_StartDate)
data.all$w4_StartDate <- anydate(data.all$w4_StartDate)
data.all$w5_StartDate <- anydate(data.all$w5_StartDate)
data.all$w6_StartDate <- anydate(data.all$w6_StartDate)
data.all$w7_StartDate <- anydate(data.all$w7_StartDate)

data.all$RecordedDate <- anydate(data.all$RecordedDate)
data.all$w1_RecordedDate <- anydate(data.all$w1_RecordedDate)
data.all$w2_RecordedDate <- anydate(data.all$w2_RecordedDate)
data.all$w3_RecordedDate <- anydate(data.all$w3_RecordedDate)
data.all$w4_RecordedDate <- anydate(data.all$w4_RecordedDate)
data.all$w5_RecordedDate <- anydate(data.all$w5_RecordedDate)
data.all$w6_RecordedDate <- anydate(data.all$w6_RecordedDate)
data.all$w7_RecordedDate <- anydate(data.all$w7_RecordedDate)
```

``` r
apa_table(data.all[1:5,1:10])
```

<caption>

(\#tab:unnamed-chunk-1)

</caption>

<div data-custom-style="Table Caption">

\*\*

</div>

| X | StartDate  | w1\_StartDate | w2\_StartDate | w3\_StartDate | w4\_StartDate | w5\_StartDate | w6\_StartDate | w7\_StartDate | RecordedDate |
| :- | :--------- | :------------ | :------------ | :------------ | :------------ | :------------ | :------------ | :------------ | :----------- |
| 1 | 2020-03-25 | NA            | NA            | NA            | NA            | NA            | NA            | NA            | 2020-03-25   |
| 2 | 2020-05-01 | NA            | NA            | NA            | NA            | NA            | NA            | NA            | 2020-05-01   |
| 3 | 2020-03-27 | NA            | NA            | NA            | NA            | NA            | NA            | NA            | 2020-03-27   |
| 4 | 2020-03-26 | NA            | NA            | NA            | NA            | NA            | NA            | NA            | 2020-03-26   |
| 5 | 2020-04-18 | NA            | NA            | NA            | NA            | NA            | NA            | NA            | 2020-04-18   |

``` r
data_Date <- melt(data.all,
    id.vars=c("X", "gender", "coded_country", "age","countryCitizen", "Citizen"),
    measure.vars=c("RecordedDate", "w1_RecordedDate", "w2_RecordedDate", "w3_RecordedDate", "w4_RecordedDate", "w5_RecordedDate", "w6_RecordedDate", "w7_RecordedDate"),
    variable.name="Wave",
    value.name="RecordedDate")
```

``` r
data_Date <- data_Date[with(data_Date, order(X, Wave)),]
```

``` r
apa_table(data_Date[1:10, 1:8])
```

<caption>

(\#tab:unnamed-chunk-4)

</caption>

<div data-custom-style="Table Caption">

\*\*

</div>

| X | gender | coded\_country | age | countryCitizen | Citizen | Wave             | RecordedDate |
| :- | :----- | :------------- | :-- | :------------- | :------ | :--------------- | :----------- |
| 1 | 1      | Saudi Arabia   | 1   | 1              | 1       | RecordedDate     | 2020-03-25   |
| 1 | 1      | Saudi Arabia   | 1   | 1              | 1       | w1\_RecordedDate | NA           |
| 1 | 1      | Saudi Arabia   | 1   | 1              | 1       | w2\_RecordedDate | NA           |
| 1 | 1      | Saudi Arabia   | 1   | 1              | 1       | w3\_RecordedDate | NA           |
| 1 | 1      | Saudi Arabia   | 1   | 1              | 1       | w4\_RecordedDate | NA           |
| 1 | 1      | Saudi Arabia   | 1   | 1              | 1       | w5\_RecordedDate | NA           |
| 1 | 1      | Saudi Arabia   | 1   | 1              | 1       | w6\_RecordedDate | NA           |
| 1 | 1      | Saudi Arabia   | 1   | 1              | 1       | w7\_RecordedDate | NA           |
| 2 | 1      | Egypt          | 1   | NA             | NA      | RecordedDate     | 2020-05-01   |
| 2 | 1      | Egypt          | 1   | NA             | NA      | w1\_RecordedDate | NA           |

``` r
data_Date$Wave <- data_Date$Wave %>%
  recode(RecordedDate = "w0",
         w1_RecordedDate = "w1",
         w2_RecordedDate = "w2",
         w3_RecordedDate = "w3",
         w4_RecordedDate = "w4",
         w5_RecordedDate = "w5",
         w6_RecordedDate = "w6",
         w7_RecordedDate = "w7"
         )
```

``` r
apa_table(data_Date[1:5, 1:5])
```

<caption>

(\#tab:unnamed-chunk-6)

</caption>

<div data-custom-style="Table Caption">

\*\*

</div>

| X | gender | coded\_country | age | countryCitizen |
| :- | :----- | :------------- | :-- | :------------- |
| 1 | 1      | Saudi Arabia   | 1   | 1              |
| 1 | 1      | Saudi Arabia   | 1   | 1              |
| 1 | 1      | Saudi Arabia   | 1   | 1              |
| 1 | 1      | Saudi Arabia   | 1   | 1              |
| 1 | 1      | Saudi Arabia   | 1   | 1              |

``` r
data_Anx <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c("affAnx", "w1_affAnx", "w2_affAnx", "w3_affAnx", "w4_affAnx", "w5_affAnx", "w6_affAnx", "w7_affAnx"),
    variable.name="Wave",
    value.name="Anxiety")

data_Anx$Wave <- data_Anx$Wave %>%
  recode(affAnx = "w0",
         w1_affAnx = "w1",
         w2_affAnx = "w2",
         w3_affAnx = "w3",
         w4_affAnx = "w4",
         w5_affAnx = "w5",
         w6_affAnx = "w6",
         w7_affAnx = "w7"
         )
```

``` r
data_new <- left_join(data_Date, data_Anx, by=c("X", "Wave"))
```

``` r
apa_table(data_Date[1:5, 1:5])
```

<caption>

(\#tab:unnamed-chunk-9)

</caption>

<div data-custom-style="Table Caption">

\*\*

</div>

| X | gender | coded\_country | age | countryCitizen |
| :- | :----- | :------------- | :-- | :------------- |
| 1 | 1      | Saudi Arabia   | 1   | 1              |
| 1 | 1      | Saudi Arabia   | 1   | 1              |
| 1 | 1      | Saudi Arabia   | 1   | 1              |
| 1 | 1      | Saudi Arabia   | 1   | 1              |
| 1 | 1      | Saudi Arabia   | 1   | 1              |

``` r
data_Bored <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c("affBor", "w5_affBor", "w6_affBor", "w7_affBor"),
    variable.name="Wave",
    value.name="Bored")

data_Bored$Wave <- data_Bored$Wave %>%
  recode(affBor = "w0",
         w1_affBor = "w1",
         w5_affBor = "w5",
         w6_affBor = "w6",
         w7_affBor = "w7"
         )
```

``` r
data_new2 <- left_join(data_new, data_Bored, by=c("X", "Wave"))
```

    ## Warning: Column `Wave` joining factors with different levels, coercing to
    ## character vector

``` r
data_Calm <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c("affCalm", "w1_affCalm", "w2_affCalm", "w3_affCalm", "w4_affCalm", "w5_affCalm", "w6_affCalm", "w7_affCalm"),
    variable.name="Wave",
    value.name="Calm")

data_Calm$Wave <- data_Calm$Wave %>%
  recode(affCalm = "w0",
         w1_affCalm = "w1",
         w2_affCalm = "w2",
         w3_affCalm = "w3",
         w4_affCalm = "w4",
         w5_affCalm = "w5",
         w6_affCalm = "w6",
         w7_affCalm = "w7"
         )
```

``` r
data_new3 <- left_join(data_new2, data_Calm, by=c("X", "Wave"))
```

    ## Warning: Column `Wave` joining character vector and factor, coercing into
    ## character vector

``` r
data_Cont <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c("affContent"),
    variable.name="Wave",
    value.name="Content")

data_Cont$Wave <- data_Cont$Wave %>%
  recode(affContent = "w0"
         )
```

``` r
data_new4 <- left_join(data_new3, data_Cont, by=c("X", "Wave"))
```

    ## Warning: Column `Wave` joining character vector and factor, coercing into
    ## character vector

``` r
data_Depr <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c("affDepr", "w1_affDepr", "w2_affDepr", "w3_affDepr", "w4_affDepr", "w5_affDepr", "w6_affDepr", "w7_affDepr"),
    variable.name="Wave",
    value.name="Depr")

data_Depr$Wave <- data_Depr$Wave %>%
  recode(affDepr = "w0",
         w1_affDepr = "w1",
         w2_affDepr = "w2",
         w3_affDepr = "w3",
         w4_affDepr = "w4",
         w5_affDepr = "w5",
         w6_affDepr = "w6",
         w7_affDepr = "w7"
         )
```

``` r
data_new5 <- left_join(data_new4, data_Depr, by=c("X", "Wave"))
```

    ## Warning: Column `Wave` joining character vector and factor, coercing into
    ## character vector

``` r
data_Energ <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c("affEnerg", "w1_affEnerg", "w2_affEnerg", "w3_affEnerg", "w4_affEnerg", "w5_affEnerg", "w6_affEnerg", "w7_affEnerg"),
    variable.name="Wave",
    value.name="Energ")

data_Energ$Wave <- data_Energ$Wave %>%
  recode(affEnerg = "w0",
         w1_affEnerg = "w1",
         w2_affEnerg = "w2",
         w3_affEnerg = "w3",
         w4_affEnerg = "w4",
         w5_affEnerg = "w5",
         w6_affEnerg = "w6",
         w7_affEnerg = "w7"
         )
```

``` r
data_new5 <- left_join(data_new4, data_Energ, by=c("X", "Wave"))
```

    ## Warning: Column `Wave` joining character vector and factor, coercing into
    ## character vector

``` r
data_Exc <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c("affExc"),
    variable.name="Wave",
    value.name="Exc")

data_Exc$Wave <- data_Exc$Wave %>%
  recode(affExc = "w0")
```

``` r
data_new6 <- left_join(data_new5, data_Exc, by=c("X", "Wave"))
```

    ## Warning: Column `Wave` joining character vector and factor, coercing into
    ## character vector

``` r
data_Nerv <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c("affNerv", "w1_affNerv", "w2_affNerv", "w3_affNerv", "w4_affNerv", "w5_affNerv", "w6_affNerv", "w7_affNerv"),
    variable.name="Wave",
    value.name="Nerv")

data_Nerv$Wave <- data_Nerv$Wave %>%
  recode(affNerv = "w0",
         w1_affNerv = "w1",
         w2_affNerv = "w2",
         w3_affNerv = "w3",
         w4_affNerv = "w4",
         w5_affNerv = "w5",
         w6_affNerv = "w6",
         w7_affNerv = "w7"
         )
```

``` r
data_new7 <- left_join(data_new6, data_Nerv, by=c("X", "Wave"))
```

    ## Warning: Column `Wave` joining character vector and factor, coercing into
    ## character vector

``` r
data_Exh <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c("affExh", "w1_affExh", "w2_affExh", "w3_affExh", "w4_affExh", "w5_affExh", "w6_affExh", "w7_affExh"),
    variable.name="Wave",
    value.name="Exh")

data_Exh$Wave <- data_Exh$Wave %>%
  recode(affExh = "w0",
         w1_affExh = "w1",
         w2_affExh = "w2",
         w3_affExh = "w3",
         w4_affExh = "w4",
         w5_affExh = "w5",
         w6_affExh = "w6",
         w7_affExh = "w7"
         )
```

``` r
data_new8 <- left_join(data_new7, data_Exh, by=c("X", "Wave"))
```

    ## Warning: Column `Wave` joining character vector and factor, coercing into
    ## character vector

``` r
data_Insp <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c("affInsp", "w1_affInsp", "w2_affInsp", "w3_affInsp", "w4_affInsp", "w5_affInsp", "w6_affInsp", "w7_affInsp"),
    variable.name="Wave",
    value.name="Insp")

data_Insp$Wave <- data_Insp$Wave %>%
  recode(affInsp = "w0",
         w1_affInsp = "w1",
         w2_affInsp = "w2",
         w3_affInsp = "w3",
         w4_affInsp = "w4",
         w5_affInsp = "w5",
         w6_affInsp = "w6",
         w7_affInsp = "w7"
         )
```

``` r
data_new9 <- left_join(data_new8, data_Insp, by=c("X", "Wave"))
```

    ## Warning: Column `Wave` joining character vector and factor, coercing into
    ## character vector

``` r
data_Rel <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c("affRel", "w1_affRel", "w2_affRel", "w3_affRel", "w4_affRel", "w5_affRel", "w6_affRel", "w7_affRel"),
    variable.name="Wave",
    value.name="Rel")

data_Rel$Wave <- data_Rel$Wave %>%
  recode(affRel = "w0",
         w1_affRel = "w1",
         w2_affRel = "w2",
         w3_affRel = "w3",
         w4_affRel = "w4",
         w5_affRel = "w5",
         w6_affRel = "w6",
         w7_affRel = "w7"
         )
```

``` r
data_new10 <- left_join(data_new9, data_Rel, by=c("X", "Wave"))
```

    ## Warning: Column `Wave` joining character vector and factor, coercing into
    ## character vector

``` r
data_Ang <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c("w1_affAng", "w2_affAng", "w3_affAng", "w4_affAng", "w5_affAng", "w6_affAng", "w7_affAng"),
    variable.name="Wave",
    value.name="Ang")

data_Ang$Wave <- data_Ang$Wave %>%
  recode(w1_affAng = "w1",
         w2_affAng = "w2",
         w3_affAng = "w3",
         w4_affAng = "w4",
         w5_affAng = "w5",
         w6_affAng = "w6",
         w7_affAng = "w7"
         )
```

``` r
data_new11 <- left_join(data_new10, data_Ang, by=c("X", "Wave"))
```

    ## Warning: Column `Wave` joining character vector and factor, coercing into
    ## character vector

``` r
data_Lov <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c("w1_affLov", "w2_affLov", "w3_affLov", "w4_affLov", "w5_affLov", "w6_affLov", "w7_affLov"),
    variable.name="Wave",
    value.name="Lov")

data_Lov$Wave <- data_Lov$Wave %>%
  recode(w1_affLov = "w1",
         w2_affLov = "w2",
         w3_affLov = "w3",
         w4_affLov = "w4",
         w5_affLov = "w5",
         w6_affLov = "w6",
         w7_affLov = "w7"
         )
```

``` r
data_new12 <- left_join(data_new11, data_Lov, by=c("X", "Wave"))
```

    ## Warning: Column `Wave` joining character vector and factor, coercing into
    ## character vector

``` r
data_Close1 <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c("coronaClose_1", "w1_coronaClose_1", "w2_coronaClose_1", "w3_coronaClose_1", "w4_coronaClose_1", "w5_coronaClose_1", "w6_coronaClose_1", "w7_coronaClose_1"),
    variable.name="Wave",
    value.name="Close1")

data_Close1$Wave <- data_Close1$Wave %>%
  recode(coronaClose_1 = "w0",
         w1_coronaClose_1 = "w1",
         w2_coronaClose_1 = "w2",
         w3_coronaClose_1 = "w3",
         w4_coronaClose_1 = "w4",
         w5_coronaClose_1 = "w5",
         w6_coronaClose_1 = "w6",
         w7_coronaClose_1 = "w7"
         )
```

``` r
data_new13 <- left_join(data_new12, data_Close1, by=c("X", "Wave"))
```

    ## Warning: Column `Wave` joining character vector and factor, coercing into
    ## character vector

``` r
data_Close2 <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c("coronaClose_2", "w1_coronaClose_2", "w2_coronaClose_2", "w3_coronaClose_2", "w4_coronaClose_2", "w5_coronaClose_2", "w6_coronaClose_2", "w7_coronaClose_2"),
    variable.name="Wave",
    value.name="Close2")

data_Close2$Wave <- data_Close2$Wave %>%
  recode(coronaClose_2 = "w0",
         w1_coronaClose_2 = "w1",
         w2_coronaClose_2 = "w2",
         w3_coronaClose_2 = "w3",
         w4_coronaClose_2 = "w4",
         w5_coronaClose_2 = "w5",
         w6_coronaClose_2 = "w6",
         w7_coronaClose_2 = "w7"
         )
```

``` r
data_new14 <- left_join(data_new13, data_Close2, by=c("X", "Wave"))
```

    ## Warning: Column `Wave` joining character vector and factor, coercing into
    ## character vector

``` r
data_Close3 <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c("coronaClose_3", "w1_coronaClose_3", "w2_coronaClose_3", "w3_coronaClose_3", "w4_coronaClose_3", "w5_coronaClose_3", "w6_coronaClose_3", "w7_coronaClose_3"),
    variable.name="Wave",
    value.name="Close3")

data_Close3$Wave <- data_Close3$Wave %>%
  recode(coronaClose_3 = "w0",
         w1_coronaClose_3 = "w1",
         w2_coronaClose_3 = "w2",
         w3_coronaClose_3 = "w3",
         w4_coronaClose_3 = "w4",
         w5_coronaClose_3 = "w5",
         w6_coronaClose_3 = "w6",
         w7_coronaClose_3 = "w7"
         )
```

``` r
data_new15 <- left_join(data_new14, data_Close3, by=c("X", "Wave"))
```

    ## Warning: Column `Wave` joining character vector and factor, coercing into
    ## character vector

``` r
data_Close4 <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c("coronaClose_4", "w1_coronaClose_4", "w2_coronaClose_4", "w3_coronaClose_4", "w4_coronaClose_4", "w5_coronaClose_4", "w6_coronaClose_4", "w7_coronaClose_4"),
    variable.name="Wave",
    value.name="Close4")

data_Close4$Wave <- data_Close4$Wave %>%
  recode(coronaClose_4 = "w0",
         w1_coronaClose_4 = "w1",
         w2_coronaClose_4 = "w2",
         w3_coronaClose_4 = "w3",
         w4_coronaClose_4 = "w4",
         w5_coronaClose_4 = "w5",
         w6_coronaClose_4 = "w6",
         w7_coronaClose_4 = "w7"
         )
```

``` r
data_new16 <- left_join(data_new15, data_Close4, by=c("X", "Wave"))
```

    ## Warning: Column `Wave` joining character vector and factor, coercing into
    ## character vector

``` r
data_Close5 <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c("coronaClose_5", "w1_coronaClose_5", "w2_coronaClose_5", "w3_coronaClose_5", "w4_coronaClose_5", "w5_coronaClose_5", "w6_coronaClose_5", "w7_coronaClose_5"),
    variable.name="Wave",
    value.name="Close5")

data_Close5$Wave <- data_Close5$Wave %>%
  recode(coronaClose_5 = "w0",
         w1_coronaClose_5 = "w1",
         w2_coronaClose_5 = "w2",
         w3_coronaClose_5 = "w3",
         w4_coronaClose_5 = "w4",
         w5_coronaClose_5 = "w5",
         w6_coronaClose_5 = "w6",
         w7_coronaClose_5 = "w7"
         )
```

``` r
data_new17 <- left_join(data_new16, data_Close5, by=c("X", "Wave"))
```

    ## Warning: Column `Wave` joining character vector and factor, coercing into
    ## character vector

``` r
data_Close6 <- melt(data.all,
    id.vars=c("X"),
    measure.vars=c("coronaClose_6", "w1_coronaClose_6", "w2_coronaClose_6", "w3_coronaClose_6", "w4_coronaClose_6", "w5_coronaClose_6", "w6_coronaClose_6", "w7_coronaClose_6"),
    variable.name="Wave",
    value.name="Close6")

data_Close6$Wave <- data_Close6$Wave %>%
  recode(coronaClose_6 = "w0",
         w1_coronaClose_6 = "w1",
         w2_coronaClose_6 = "w2",
         w3_coronaClose_6 = "w3",
         w4_coronaClose_6 = "w4",
         w5_coronaClose_6 = "w5",
         w6_coronaClose_6 = "w6",
         w7_coronaClose_6 = "w7"
         )
```

``` r
data_new18 <- left_join(data_new17, data_Close6, by=c("X", "Wave"))
```

    ## Warning: Column `Wave` joining character vector and factor, coercing into
    ## character vector

``` r
apa_table(data_new18[1:5,1:10])
```

<caption>

(\#tab:unnamed-chunk-46)

</caption>

<div data-custom-style="Table Caption">

\*\*

</div>

| X | gender | coded\_country | age | countryCitizen | Citizen | Wave | RecordedDate | Anxiety | Bored |
| :- | :----- | :------------- | :-- | :------------- | :------ | :--- | :----------- | :------ | :---- |
| 1 | 1      | Saudi Arabia   | 1   | 1              | 1       | w0   | 2020-03-25   | 4       | 3     |
| 1 | 1      | Saudi Arabia   | 1   | 1              | 1       | w1   | NA           | NA      | NA    |
| 1 | 1      | Saudi Arabia   | 1   | 1              | 1       | w2   | NA           | NA      | NA    |
| 1 | 1      | Saudi Arabia   | 1   | 1              | 1       | w3   | NA           | NA      | NA    |
| 1 | 1      | Saudi Arabia   | 1   | 1              | 1       | w4   | NA           | NA      | NA    |

``` r
data_long <- data_new18
rm(data_Ang, data_Anx, data_Bored, data_Calm, data_Close1, data_Close2, data_Close3, data_Close4, data_Close5, data_Close6,
   data_Cont, data_Date, data_Depr, data_Energ, data_Exc, data_Exh, data_Insp, data_Lov, data_Nerv, data_Rel,
   data_new, data_new1, data_new2, data_new3, data_new4, data_new5, data_new6, data_new7, data_new8, data_new9, data_new10,
   data_new11, data_new12, data_new13, data_new14, data_new15, data_new16, data_new17, data_new18)
```

    ## Warning in rm(data_Ang, data_Anx, data_Bored, data_Calm, data_Close1,
    ## data_Close2, : object 'data_new1' not found

``` r
data_long <- as_tibble(data_long)
data_long <- data_long[,c("X", "Wave", "RecordedDate", "age", "gender", "coded_country", "countryCitizen", "Citizen",
                           "Ang", "Anxiety", "Bored", "Calm", "Content", "Energ", "Exc", "Exh", "Insp", "Lov", "Nerv", "Rel",
                           "Close1", "Close2", "Close3", "Close4", "Close5", "Close6")]
save(data_long, file="data_long.Rdata")
```

``` r
apa_table(data_long[1:5,1:10])
```

<caption>

(\#tab:unnamed-chunk-49)

</caption>

<div data-custom-style="Table Caption">

\*\*

</div>

| X | Wave | RecordedDate | age | gender | coded\_country | countryCitizen | Citizen | Ang | Anxiety |
| :- | :--- | :----------- | :-- | :----- | :------------- | :------------- | :------ | :-- | :------ |
| 1 | w0   | 2020-03-25   | 1   | 1      | Saudi Arabia   | 1              | 1       | NA  | 4       |
| 1 | w1   | NA           | 1   | 1      | Saudi Arabia   | 1              | 1       | NA  | NA      |
| 1 | w2   | NA           | 1   | 1      | Saudi Arabia   | 1              | 1       | NA  | NA      |
| 1 | w3   | NA           | 1   | 1      | Saudi Arabia   | 1              | 1       | NA  | NA      |
| 1 | w4   | NA           | 1   | 1      | Saudi Arabia   | 1              | 1       | NA  | NA      |
