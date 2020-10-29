Data preparation for multilevel modeling
================
Anne Margit
10/27/2020

``` r
library(ggplot2)
library(dplyr)
library(knitr)
library(arsenal)
```

    ## Warning: package 'arsenal' was built under R version 4.0.3

``` r
library(tidyverse)
library(anytime)
```

    ## Warning: package 'anytime' was built under R version 4.0.3

``` r
library(rockchalk)
```

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
names(data_long)[names(data_long) == "age"] <- "Age"
names(data_long)[names(data_long) == "coded_country"] <- "Country"
names(data_long)[names(data_long) == "X"] <- "ID"
names(data_long)[names(data_long) == "RecordedDate"] <- "Date"
names(data_long)[names(data_long) == "gender"] <- "Gender"
names(data_long)[names(data_long) == "edu"] <- "Edu"
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

Delete participants that are not citizens of the country they currently
live in: Note: Citizen = Have you been a citizen of this country since
birth? countryCitizen = Are you a citizen of this country?

``` r
data_long <- data_long %>%
  filter(countryCitizen == 1)
```

Delete participants with missing data on age

``` r
data_long <-data_long %>%
filter(!is.na(Age))
```

Delete participants that reported their gender as other or did not
provide information:

``` r
data_long <- data_long %>%
  filter(Gender == 1 | Gender == 2)
```

Recode gender into male = 0 and female = 1

``` r
data_long$Gender[data_long$Gender == "2"] <- 0
```

Delete participants with missing data on education:

``` r
data_long <- data_long %>%
  filter(!is.na(Edu))
```

Delete emotion variables we don’t study (Bored, Loved, Content, Excited)

``` r
data_long <- data_long %>%
  select(-c(Bored, Lov, Content, Exc))
```

Add number of measurements

``` r
data_long$Date <- as.Date(data_long$Date)
data_long <- data_long %>% group_by(ID) %>% add_tally(wt = !is.na(Date))
```

Filter participants with at least 3 measurements

``` r
data_long_min3 <- data_long %>% filter(n > 2)
```

Rename Wave column into Time ordered 1-12

``` r
data_long_min3 <- data_long_min3 %>%
  rename(Time = Wave)

data_long_min3$Time <- data_long_min3$Time %>%
  plyr::revalue(c("w0"="1","w1"="2","w2"="3","w3"="4","w4"="5","w5"="6","w6"="7","w7"="8","w8"="9","w9"="10","w10"="11","w11"="12"))
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
  filter(Time == "1") %>%
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
  select(-c(Citizen, countryCitizen))

summary(data_long_min3_20)
```

    ##        ID             Time            Date                 Age       
    ##  Min.   :   13   1      :10457   Min.   :2020-03-19   2      :23448  
    ##  1st Qu.:14139   2      :10457   1st Qu.:2020-04-18   3      :22812  
    ##  Median :30295   3      :10457   Median :2020-05-04   4      :22404  
    ##  Mean   :29557   4      :10457   Mean   :2020-05-05   5      :21660  
    ##  3rd Qu.:43512   5      :10457   3rd Qu.:2020-05-23   6      :16632  
    ##  Max.   :62491   6      :10457   Max.   :2020-07-07   1      :16176  
    ##                  (Other):62742   NA's   :66111        (Other): 2352  
    ##      Gender            Edu                 Country           Ang       
    ##  Min.   :0.0000   Min.   :1.000   United States:28308   Min.   :1.00   
    ##  1st Qu.:0.0000   1st Qu.:4.000   Spain        :12312   1st Qu.:1.00   
    ##  Median :1.0000   Median :5.000   Greece       : 8148   Median :2.00   
    ##  Mean   :0.6693   Mean   :4.511   Netherlands  : 8136   Mean   :1.98   
    ##  3rd Qu.:1.0000   3rd Qu.:6.000   Serbia       : 6120   3rd Qu.:3.00   
    ##  Max.   :1.0000   Max.   :7.000   Italy        : 5760   Max.   :5.00   
    ##                                   (Other)      :56700   NA's   :76747  
    ##     Anxiety           Calm            Depr           Energ      
    ##  Min.   :1.00    Min.   :1.00    Min.   :1       Min.   :1.00   
    ##  1st Qu.:1.00    1st Qu.:2.00    1st Qu.:1       1st Qu.:2.00   
    ##  Median :2.00    Median :3.00    Median :2       Median :3.00   
    ##  Mean   :2.37    Mean   :3.07    Mean   :2       Mean   :2.65   
    ##  3rd Qu.:3.00    3rd Qu.:4.00    3rd Qu.:3       3rd Qu.:3.00   
    ##  Max.   :5.00    Max.   :5.00    Max.   :5       Max.   :5.00   
    ##  NA's   :66328   NA's   :66318   NA's   :66358   NA's   :66385  
    ##       Exh             Insp            Nerv            Rel       
    ##  Min.   :1.00    Min.   :1.00    Min.   :1.00    Min.   :1.00   
    ##  1st Qu.:1.00    1st Qu.:2.00    1st Qu.:1.00    1st Qu.:2.00   
    ##  Median :2.00    Median :2.00    Median :2.00    Median :3.00   
    ##  Mean   :2.34    Mean   :2.48    Mean   :2.25    Mean   :2.88   
    ##  3rd Qu.:3.00    3rd Qu.:3.00    3rd Qu.:3.00    3rd Qu.:4.00   
    ##  Max.   :5.00    Max.   :5.00    Max.   :5.00    Max.   :5.00   
    ##  NA's   :66381   NA's   :66411   NA's   :66358   NA's   :66363  
    ##      Close1             Close2           Close3            Close4       
    ##  Min.   :0.000000   Min.   :0.0000   Min.   :0.00000   Min.   :0.00000  
    ##  1st Qu.:0.000000   1st Qu.:0.0000   1st Qu.:0.00000   1st Qu.:0.00000  
    ##  Median :0.000000   Median :0.0000   Median :0.00000   Median :0.00000  
    ##  Mean   :0.002439   Mean   :0.0169   Mean   :0.01991   Mean   :0.06181  
    ##  3rd Qu.:0.000000   3rd Qu.:0.0000   3rd Qu.:0.00000   3rd Qu.:0.00000  
    ##  Max.   :1.000000   Max.   :1.0000   Max.   :1.00000   Max.   :1.00000  
    ##                                                                         
    ##      Close5            Close6             n                N         
    ##  Min.   :0.00000   Min.   :0.0000   Min.   : 3.000   Min.   :  22.0  
    ##  1st Qu.:0.00000   1st Qu.:0.0000   1st Qu.: 3.000   1st Qu.: 262.0  
    ##  Median :0.00000   Median :0.0000   Median : 5.000   Median : 510.0  
    ##  Mean   :0.04338   Mean   :0.3561   Mean   : 5.678   Mean   : 882.5  
    ##  3rd Qu.:0.00000   3rd Qu.:1.0000   3rd Qu.: 8.000   3rd Qu.:1026.0  
    ##  Max.   :1.00000   Max.   :1.0000   Max.   :12.000   Max.   :2359.0  
    ## 

``` r
summary(data_long_min3_20$Country)
```

    ##                Albania                Algeria                Andorra 
    ##                      0                      0                      0 
    ##              Argentina                Armenia              Australia 
    ##                   2556                      0                   2472 
    ##                Austria             Bangladesh                Belgium 
    ##                      0                      0                      0 
    ## Bosnia and Herzegovina                 Brazil               Bulgaria 
    ##                      0                   2640                      0 
    ##                 Canada                  Chile                  China 
    ##                   3696                   1044                      0 
    ##               Colombia             Costa Rica                Croatia 
    ##                      0                      0                   1692 
    ##                 Cyprus         Czech Republic                Ecuador 
    ##                      0                      0                      0 
    ##                  Egypt            El Salvador                Estonia 
    ##                      0                      0                      0 
    ##                Finland                 France                Georgia 
    ##                      0                   4296                      0 
    ##                Germany                 Greece              Hong Kong 
    ##                   4824                   8148                      0 
    ##                Hungary                  India              Indonesia 
    ##                   2352                      0                   2232 
    ##                   Iran                   Iraq                Ireland 
    ##                      0                      0                      0 
    ##                 Israel                  Italy                Jamaica 
    ##                      0                   5760                      0 
    ##                  Japan                 Jordan             Kazakhstan 
    ##                    924                      0                    996 
    ##                 Kosovo        Kyrgyz Republic                Lebanon 
    ##                    348                      0                      0 
    ##              Lithuania             Luxembourg               Malaysia 
    ##                      0                      0                   1296 
    ##                   Mali                  Malta                 Mexico 
    ##                      0                      0                      0 
    ##             Montenegro                Morocco                  Nepal 
    ##                      0                      0                      0 
    ##            Netherlands            New Zealand                 Norway 
    ##                   8136                      0                      0 
    ##               Pakistan                   Peru            Philippines 
    ##                      0                    432                   1524 
    ##                 Poland               Portugal                Romania 
    ##                   1572                      0                   3744 
    ##                 Russia           Saudi Arabia                 Serbia 
    ##                   2424                    852                   6120 
    ##              Singapore        Slovak Republic           South Africa 
    ##                    516                      0                   2664 
    ##            South Korea                  Spain                 Sweden 
    ##                    264                  12312                      0 
    ##            Switzerland                 Taiwan               Thailand 
    ##                      0                      0                      0 
    ##    Trinidad and Tobago                Tunisia                 Turkey 
    ##                      0                      0                   2208 
    ##                Ukraine   United Arab Emirates         United Kingdom 
    ##                   3144                      0                   5628 
    ##          United States                Uruguay              Venezuela 
    ##                  28308                      0                      0 
    ##                Vietnam 
    ##                    360

Save

``` r
save(data_long_min3_20, file="data_long_min3_20.Rdata")
```

Recode oxford data:

``` r
oxforddata$Date <-anydate(oxforddata$Date)
oxforddata$Country <- oxforddata$CountryName
```

Drop not used variables:

``` r
Stringency_data <- oxforddata %>% select(Date, Country, StringencyIndex, ConfirmedCases, ConfirmedDeaths)
```

Impute Stringency Index, fill empty rows with last previously reported
index score: Create new Stringency Index variable first

``` r
Stringency_data$StringencyIndex_imp <- Stringency_data$StringencyIndex
```

First join, then impute

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
shouldnt be)

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

Centering Stringency Index

``` r
data_long_min3_strc <- gmc(data_long_min3_str, "StringencyIndex_imp", "Country", FUN = mean, suffix = c("_mn", "_dev"),
    fulldataframe = TRUE)
```

This dataset also adds the country mean centered stringency index

``` r
save(data_long_min3_strc, file="data_long_min3_strc.Rdata")
```

Make new age groups: Youth: aged 18-24 coded as 1 \<- 0 Young adults:
aged 25-44 coded as 2 or 3 \<- 1 Middle-aged adults: aged 45-64 coded as
4 or 5 \<- 2 Older-aged adults: aged 65+ coded as 6, 7, or 8 \<- 3

``` r
data_long_min3_str_age <- data_long_min3_str 
data_long_min3_str_age$Age_new <- data_long_min3_str_age$Age
```

``` r
data_long_min3_str_age$Age_new <- data_long_min3_str_age$Age_new %>%
  plyr::revalue(c("1"="0", "2"= "1", "3"="1", "4"="2", "5"="2", "6"="3", "7"="3", "8"="3"))
```

Create new variable that indicates sum of missings:

``` r
data_long_min3_str_age <- data_long_min3_str_age %>%
  group_by(ID, Time) %>%
mutate(Nmiss = sum(is.na(Ang)) + sum(is.na(Anxiety)) + sum(is.na(Nerv)) + sum(is.na(Depr)) + sum(is.na(Exh)) + 
               sum(is.na(Energ)) + sum(is.na(Insp)) + sum(is.na(Calm)) + sum(is.na(Rel))) %>%
  ungroup()
```

save

``` r
save(data_long_min3_str_age, file="data_long_min3_str_age.Rdata")
```

``` r
data_means <- data_long_min3_str %>%
    dplyr::group_by(ID) %>%
    dplyr::summarise_each(funs(mean(., na.rm=TRUE)), 
                          Ang, Anxiety, Calm, Depr, Energ, Exh, Insp, Nerv, Rel)
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

Next = data imputation
