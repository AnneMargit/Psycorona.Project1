Data preparation for multilevel modeling
================
Anne Margit
07/07/2020

``` r
library(ggplot2)
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
library(knitr)
library(lme4)
```

    ## Loading required package: Matrix

``` r
library(arsenal)
```

    ## 
    ## Attaching package: 'arsenal'

    ## The following objects are masked from 'package:Matrix':
    ## 
    ##     head, tail

``` r
library(lmerTest)
```

    ## 
    ## Attaching package: 'lmerTest'

    ## The following object is masked from 'package:lme4':
    ## 
    ##     lmer

    ## The following object is masked from 'package:stats':
    ## 
    ##     step

``` r
library(tidyverse)
```

    ## ── Attaching packages ──────────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ tibble  3.0.1     ✓ purrr   0.3.4
    ## ✓ tidyr   1.1.0     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.5.0

    ## ── Conflicts ─────────────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## x tidyr::expand() masks Matrix::expand()
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()
    ## x tidyr::pack()   masks Matrix::pack()
    ## x tidyr::unpack() masks Matrix::unpack()

``` r
library(anytime)
library(rockchalk)
```

    ## 
    ## Attaching package: 'rockchalk'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     summarize

``` r
load("data_long.Rdata")
oxforddata <- read.csv("OxCGRT_latest.csv", header=TRUE)
```

To check whether country names are different in both datasets:

``` r
levels1 <- levels(data_long$coded_country)
levels2 <- levels(oxforddata$CountryName)

levels1<- as.data.frame(levels1)
levels2<- as.data.frame(levels2)

levels1$levels1 <-as.character(levels1$levels1)
levels2$levels2 <-as.character(levels2$levels2)

check <- comparedf(levels1, levels2, by.x="levels1", by.y="levels2")
summary(check)
```

Some data preparation

``` r
data_long$coded_country <- 
  plyr::revalue(data_long$coded_country , c("Hong Kong S.A.R."="Hong Kong", 
                                       "Kyrgyzstan" = "Kyrgyz Republic",
                                       "United Republic of Tanzania" = "Tanzania", 
                                       "United States of America" = "United States",
                                       "Republic of Serbia" = "Serbia",
                                       "Slovakia" = "Slovak Republic"))
```

Renaming and recoding variables:

``` r
data_long$age <- as.factor(data_long$age)
names(data_long)[names(data_long) == "coded_country"] <- "Country"
names(data_long)[names(data_long) == "X"] <- "ID"
names(data_long)[names(data_long) == "RecordedDate"] <- "Date"
```

Recoding NA into zero’s:

``` r
data_long$Close1[is.na(data_long$Close1)] <- 0
data_long$Close2[is.na(data_long$Close2)] <- 0
data_long$Close3[is.na(data_long$Close3)] <- 0
data_long$Close4[is.na(data_long$Close4)] <- 0
data_long$Close5[is.na(data_long$Close5)] <- 0
data_long$Close6[is.na(data_long$Close6)] <- 0
```

Delete participants with missing data on which country they currently
live:

``` r
data_long$Country[data_long$Country == ""] <- NA

data_long <- data_long %>%
  filter(!is.na(Country))
```

Delete participants that are not residents of the country they currently
live in:

``` r
data_long <- data_long %>%
  filter(countryCitizen == 1)
```

Delete participants with missing data on age

``` r
data_long <-data_long %>%
filter(!is.na(age))
```

Delete participants that reported their gender as other or did not
provide information:

``` r
data_long <- data_long %>%
  filter(gender == 1 | gender == 2)
```

Recode gender into male = 0 and female = 1

``` r
data_long$gender[data_long$gender == "2"] <- 0
```

Add number of measurements

``` r
data_long <- data_long %>% group_by(ID) %>% add_tally(wt = !is.na(Date))
```

Filter participants with at least 3 measurements

``` r
data_long_min3 <- data_long %>% filter(n > 2)
```

``` r
save(data_long_min3, file="data_long_min3.Rdata")
```

Calculate number of participants per country & select countries with
\>20 participants:

``` r
data_long_min3$Country <- as.factor(data_long_min3$Country)
data_long_min3 <- as_tibble(data_long_min3)

Country_N <- data_long_min3 %>%
  filter(Wave == "w0") %>%
  group_by(Country) %>%
  dplyr::summarise(N = n())
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
data_long_min3n <- left_join(data_long_min3, Country_N, by="Country")
```

``` r
data_long_min3_20 <- data_long_min3n %>%
  filter(N>20)
```

``` r
data_long_min3_20 <- data_long_min3_20 %>%
  select(-Citizen)

summary(data_long_min3_20)
```

    ##        ID             Wave            Date                 age       
    ##  Min.   :   44   w0     :10343   Min.   :2020-03-19   2      :23208  
    ##  1st Qu.:14101   w1     :10343   1st Qu.:2020-04-18   3      :22512  
    ##  Median :30027   w2     :10343   Median :2020-05-04   4      :22080  
    ##  Mean   :29290   w3     :10343   Mean   :2020-05-05   5      :21480  
    ##  3rd Qu.:42836   w4     :10343   3rd Qu.:2020-05-23   6      :16512  
    ##  Max.   :61889   w5     :10343   Max.   :2020-06-23   1      :15972  
    ##                  (Other):62058   NA's   :65836        (Other): 2352  
    ##      gender                Country      countryCitizen      Ang       
    ##  Min.   :0.0000   United States:28080   Min.   :1      Min.   :1.00   
    ##  1st Qu.:0.0000   Spain        :12252   1st Qu.:1      1st Qu.:1.00   
    ##  Median :1.0000   Greece       : 8088   Median :1      Median :2.00   
    ##  Mean   :0.6689   Netherlands  : 8088   Mean   :1      Mean   :1.98   
    ##  3rd Qu.:1.0000   Serbia       : 6072   3rd Qu.:1      3rd Qu.:3.00   
    ##  Max.   :1.0000   Italy        : 5712   Max.   :1      Max.   :5.00   
    ##                   (Other)      :55824                  NA's   :76349  
    ##     Anxiety          Bored            Calm          Content      
    ##  Min.   :1.00    Min.   :1.00    Min.   :1.00    Min.   :1.00    
    ##  1st Qu.:1.00    1st Qu.:1.00    1st Qu.:2.00    1st Qu.:2.00    
    ##  Median :2.00    Median :2.00    Median :3.00    Median :3.00    
    ##  Mean   :2.37    Mean   :2.13    Mean   :3.07    Mean   :2.65    
    ##  3rd Qu.:3.00    3rd Qu.:3.00    3rd Qu.:4.00    3rd Qu.:3.00    
    ##  Max.   :5.00    Max.   :5.00    Max.   :5.00    Max.   :5.00    
    ##  NA's   :66045   NA's   :81635   NA's   :66033   NA's   :113845  
    ##       Depr           Energ            Exc              Exh       
    ##  Min.   :1       Min.   :1.00    Min.   :1        Min.   :1.00   
    ##  1st Qu.:1       1st Qu.:2.00    1st Qu.:1        1st Qu.:1.00   
    ##  Median :2       Median :3.00    Median :2        Median :2.00   
    ##  Mean   :2       Mean   :2.65    Mean   :2        Mean   :2.34   
    ##  3rd Qu.:3       3rd Qu.:3.00    3rd Qu.:3        3rd Qu.:3.00   
    ##  Max.   :5       Max.   :5.00    Max.   :5        Max.   :5.00   
    ##  NA's   :66072   NA's   :66098   NA's   :113857   NA's   :66097  
    ##       Insp            Lov             Nerv            Rel       
    ##  Min.   :1.00    Min.   :1.00    Min.   :1.00    Min.   :1.00   
    ##  1st Qu.:2.00    1st Qu.:3.00    1st Qu.:1.00    1st Qu.:2.00   
    ##  Median :2.00    Median :4.00    Median :2.00    Median :3.00   
    ##  Mean   :2.48    Mean   :3.44    Mean   :2.25    Mean   :2.88   
    ##  3rd Qu.:3.00    3rd Qu.:4.00    3rd Qu.:3.00    3rd Qu.:4.00   
    ##  Max.   :5.00    Max.   :5.00    Max.   :5.00    Max.   :5.00   
    ##  NA's   :66123   NA's   :76336   NA's   :66073   NA's   :66078  
    ##      Close1             Close2            Close3            Close4       
    ##  Min.   :0.000000   Min.   :0.00000   Min.   :0.00000   Min.   :0.00000  
    ##  1st Qu.:0.000000   1st Qu.:0.00000   1st Qu.:0.00000   1st Qu.:0.00000  
    ##  Median :0.000000   Median :0.00000   Median :0.00000   Median :0.00000  
    ##  Mean   :0.002441   Mean   :0.01691   Mean   :0.01984   Mean   :0.06133  
    ##  3rd Qu.:0.000000   3rd Qu.:0.00000   3rd Qu.:0.00000   3rd Qu.:0.00000  
    ##  Max.   :1.000000   Max.   :1.00000   Max.   :1.00000   Max.   :1.00000  
    ##                                                                          
    ##      Close5            Close6             n                N         
    ##  Min.   :0.00000   Min.   :0.0000   Min.   : 3.000   Min.   :  21.0  
    ##  1st Qu.:0.00000   1st Qu.:0.0000   1st Qu.: 3.000   1st Qu.: 251.0  
    ##  Median :0.00000   Median :0.0000   Median : 5.000   Median : 506.0  
    ##  Mean   :0.04305   Mean   :0.3533   Mean   : 5.635   Mean   : 877.6  
    ##  3rd Qu.:0.00000   3rd Qu.:1.0000   3rd Qu.: 8.000   3rd Qu.:1021.0  
    ##  Max.   :1.00000   Max.   :1.0000   Max.   :12.000   Max.   :2340.0  
    ## 

``` r
summary(data_long_min3_20$Country)
```

    ##          United States                  Spain                 Greece 
    ##                  28080                  12252                   8088 
    ##            Netherlands                 Serbia                  Italy 
    ##                   8088                   6072                   5712 
    ##         United Kingdom                Germany                 France 
    ##                   5592                   4764                   4284 
    ##                Romania                 Canada                Ukraine 
    ##                   3684                   3648                   3012 
    ##           South Africa                 Brazil              Argentina 
    ##                   2616                   2604                   2508 
    ##              Australia                 Russia                Hungary 
    ##                   2436                   2412                   2340 
    ##              Indonesia                 Turkey                Croatia 
    ##                   2220                   2148                   1692 
    ##                 Poland            Philippines               Malaysia 
    ##                   1512                   1500                   1236 
    ##                  Chile             Kazakhstan                  Japan 
    ##                   1008                    984                    912 
    ##           Saudi Arabia              Singapore                   Peru 
    ##                    852                    504                    420 
    ##                 Kosovo                Vietnam            South Korea 
    ##                    348                    336                    252 
    ##                                       Albania                Algeria 
    ##                      0                      0                      0 
    ##                Andorra                Armenia                Austria 
    ##                      0                      0                      0 
    ##             Azerbaijan                Bahrain             Bangladesh 
    ##                      0                      0                      0 
    ##                Belarus                Belgium                  Benin 
    ##                      0                      0                      0 
    ## Bosnia and Herzegovina               Botswana                 Brunei 
    ##                      0                      0                      0 
    ##               Bulgaria               Cambodia               Cameroon 
    ##                      0                      0                      0 
    ##                  China               Colombia             Costa Rica 
    ##                      0                      0                      0 
    ##                 Cyprus         Czech Republic                Denmark 
    ##                      0                      0                      0 
    ##     Dominican Republic                Ecuador                  Egypt 
    ##                      0                      0                      0 
    ##            El Salvador                Estonia               Ethiopia 
    ##                      0                      0                      0 
    ##                Finland                Georgia              Guatemala 
    ##                      0                      0                      0 
    ##              Hong Kong                Iceland                  India 
    ##                      0                      0                      0 
    ##                   Iran                   Iraq                Ireland 
    ##                      0                      0                      0 
    ##                 Israel                Jamaica                 Jordan 
    ##                      0                      0                      0 
    ##                  Kenya                 Kuwait        Kyrgyz Republic 
    ##                      0                      0                      0 
    ##                   Laos                 Latvia                Lebanon 
    ##                      0                      0                      0 
    ##                  Libya              Lithuania             Luxembourg 
    ##                      0                      0                      0 
    ##                   Mali                  Malta              Mauritius 
    ##                      0                      0                      0 
    ##                 Mexico                Moldova               Mongolia 
    ##                      0                      0                      0 
    ##             Montenegro                Morocco                Myanmar 
    ##                      0                      0                      0 
    ##                  Nepal            New Zealand                Nigeria 
    ##                      0                      0                      0 
    ##                 Norway                   Oman               Pakistan 
    ##                      0                      0                      0 
    ##                (Other) 
    ##                      0

Save

``` r
save(data_long_min3_20, file="data_long_min3_20.Rdata")
```

Recode oxford data:

``` r
oxforddata$Date <-anydate(oxforddata$Date)
oxforddata$Country <- oxforddata$CountryName
```

Drop not used
variables:

``` r
Stringency_data <- oxforddata %>% select(Date, Country, StringencyIndex, ConfirmedCases, ConfirmedDeaths)
```

Impute Stringency Index, fill empty rows with last previously reported
index score: Create new Stringency Index variable first

``` r
Stringency_data$StringencyIndex_imp <- Stringency_data$StringencyIndex
```

First join, then
impute

``` r
data_long_min3_str <- left_join(data_long_min3_20, Stringency_data, by=c("Country", "Date"))
```

Impute

``` r
data_long_min3_str<-data_long_min3_str[with(data_long_min3_str, order(Country, Date)),]

data_long_min3_str2 <- data_long_min3_str %>%
  group_by(Country) %>%
  fill(StringencyIndex_imp, .direction = "down")

data_long_min3_str <- data_long_min3_str2
```

Check if StringencyIndex and StringencyIndex\_imp are identical (they
shouldnt
be)

``` r
identical(data_long_min3_str[["StringencyIndex"]],data_long_min3_str[["StringencyIndex_imp"]])
```

    ## [1] FALSE

Save: This dataset includes measurements from participants that (1)
provided at least 3 measurements, (2) that are residents of the country
they currently live in, (3) from countries with at least 20
participants, (4) provided data on age, and (5) with imputed Stringency
index values, (6) for baseline through wave 11

``` r
save(data_long_min3_str, file ="data_long_min3_str.Rdata")
```

Centering Stringency
Index

``` r
data_long_min3_strc <- gmc(data_long_min3_str, "StringencyIndex_imp", "Country", FUN = mean, suffix = c("_mn", "_dev"),
    fulldataframe = TRUE)
```

This dataset also adds the country mean centered stringency index

``` r
save(data_long_min3_strc, file="data_long_min3_strc.Rdata")
```

``` r
data_means <- data_long_min3_str %>%
    dplyr::group_by(ID) %>%
    dplyr::summarise_each(funs(mean(., na.rm=TRUE)), 
                          Ang, Anxiety, Bored, Calm, Depr, Energ, Exh, Insp, Lov, Nerv, Rel)
```

    ## Warning: `summarise_each_()` is deprecated as of dplyr 0.7.0.
    ## Please use `across()` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_warnings()` to see where this warning was generated.

    ## Warning: `funs()` is deprecated as of dplyr 0.8.0.
    ## Please use a list of either functions or lambdas: 
    ## 
    ##   # Simple named list: 
    ##   list(mean = mean, median = median)
    ## 
    ##   # Auto named with `tibble::lst()`: 
    ##   tibble::lst(mean, median)
    ## 
    ##   # Using lambdas
    ##   list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_warnings()` to see where this warning was generated.

``` r
save(data_means, file="data_means.Rdata")
```
