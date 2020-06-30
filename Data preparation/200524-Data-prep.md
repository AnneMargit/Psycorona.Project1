Data prep
================
A.M. Reitsema
5/24/2020

``` r
coronadata <- read.csv("Data/RMD06_Anne Margit Reitsema_2020-05-08 11-48 CEST.csv", header=TRUE)
oxforddata <- read.csv("Data/OxCGRT_latest.csv", header=TRUE)
```

To check whether country names are different in both datasets:

``` r
levels1 <- levels(coronadata$coded_country)
levels2 <- levels(oxforddata$CountryName)

levels1<- as.data.frame(levels1)
levels2<- as.data.frame(levels2)

levels1$levels1 <-as.character(levels1$levels1)
levels2$levels2 <-as.character(levels2$levels2)

check <- comparedf(levels1, levels2, by.x="levels1", by.y="levels2")
summary(check)
```

Country names that need to be renamed in PsyCorona:  
Hong Kong S.A.R. -\> Hong Kong  
Kyrgyzstan -\> Kyrgyz Republic  
United Republic of Tanzania -\> Tanzania  
United States of America -\> United States  
Republic of Serbia -\> Serbia

``` r
coronadata$coded_country <- 
  revalue(coronadata$coded_country , c("Hong Kong S.A.R."="Hong Kong", 
                                       "Kyrgyzstan" = "Kyrgyz Republic",
                                       "United Republic of Tanzania" = "Tanzania", 
                                       "United States of America" = "United States",
                                       "Republic of Serbia" = "Serbia"))
```

Renaming and recoding variables:

``` r
coronadata$gender <- as.factor(coronadata$gender)
coronadata$age <- as.factor(coronadata$age)
coronadata$gender <- as.factor(coronadata$gender)
coronadata$dateB<-anydate(coronadata$StartDate)
coronadata$dateW1 <- anydate(coronadata$w1_StartDate)

names(coronadata)[names(coronadata) == "coded_country"] <- "country"
names(coronadata)[names(coronadata) == "X"] <- "ID"
```

Recoding NA into zero’s:

``` r
coronadata$coronaClose_1[is.na(coronadata$coronaClose_1)] <- 0
coronadata$coronaClose_2[is.na(coronadata$coronaClose_2)] <- 0
coronadata$coronaClose_3[is.na(coronadata$coronaClose_3)] <- 0
coronadata$coronaClose_4[is.na(coronadata$coronaClose_4)] <- 0
coronadata$coronaClose_5[is.na(coronadata$coronaClose_5)] <- 0
coronadata$coronaClose_6[is.na(coronadata$coronaClose_6)] <- 0
```

Delete participants with missing data on which country they currently
live:

``` r
coronadata$country[coronadata$country == ""] <- NA

coronadata <- coronadata %>%
  filter(!is.na(country))
```

Delete participants that are not residents of the country they currently
live in:

``` r
coronadata <- coronadata %>%
  filter(countryCitizen == 1)
```

Select baseline
data:

``` r
coronadataB <- coronadata %>% select(ID, affAnx, affBor, affCalm, affContent, affDepr, affEnerg, affExc, affNerv,
                                     affExh, affInsp, affRel, happy, coronaClose_1, coronaClose_2, coronaClose_3,
                                     coronaClose_4, coronaClose_5, coronaClose_6, countryCitizen, Citizen,
                                     gender, age, country, dateB)
coronadataB<- as_tibble(coronadataB)
head(coronadataB, 5)
```

    ## # A tibble: 5 x 25
    ##      ID affAnx affBor affCalm affContent affDepr affEnerg affExc affNerv affExh
    ##   <int>  <int>  <int>   <int>      <int>   <int>    <int>  <int>   <int>  <int>
    ## 1     2      3      4       2          2       1        3      1       1      2
    ## 2     3      4      3       3          3       2        3      2       4      3
    ## 3     6      2      3       1          2       1        3      1       1      2
    ## 4     7      3      4       3          3       4        2      3       4      4
    ## 5     8      4      4       3          2       2        2      2       2      2
    ## # … with 15 more variables: affInsp <int>, affRel <int>, happy <int>,
    ## #   coronaClose_1 <dbl>, coronaClose_2 <dbl>, coronaClose_3 <dbl>,
    ## #   coronaClose_4 <dbl>, coronaClose_5 <dbl>, coronaClose_6 <dbl>,
    ## #   countryCitizen <int>, Citizen <int>, gender <fct>, age <fct>,
    ## #   country <fct>, dateB <date>

Select wave 1
data:

``` r
coronadataW1 <- coronadata %>% select(ID, w1_affAnx, w1_affBor, w1_affCalm, w1_affDepr, w1_affEnerg, w1_affNerv,
                                      w1_affExh, w1_affInsp, w1_affRel, w1_affAng, w1_affLov, w1_coronaClose_1, 
                                      w1_coronaClose_2, w1_coronaClose_3, w1_coronaClose_4, w1_coronaClose_5, 
                                      w1_coronaClose_6, countryCitizen, Citizen, gender, age, country, dateW1)
coronadataW1<- as_tibble(coronadataW1)
head(coronadataW1, 5)
```

    ## # A tibble: 5 x 24
    ##      ID w1_affAnx w1_affBor w1_affCalm w1_affDepr w1_affEnerg w1_affNerv
    ##   <int>     <int>     <int>      <int>      <int>       <int>      <int>
    ## 1     2        NA        NA         NA         NA          NA         NA
    ## 2     3        NA        NA         NA         NA          NA         NA
    ## 3     6        NA        NA         NA         NA          NA         NA
    ## 4     7        NA        NA         NA         NA          NA         NA
    ## 5     8        NA        NA         NA         NA          NA         NA
    ## # … with 17 more variables: w1_affExh <int>, w1_affInsp <int>, w1_affRel <int>,
    ## #   w1_affAng <int>, w1_affLov <int>, w1_coronaClose_1 <int>,
    ## #   w1_coronaClose_2 <int>, w1_coronaClose_3 <int>, w1_coronaClose_4 <int>,
    ## #   w1_coronaClose_5 <int>, w1_coronaClose_6 <int>, countryCitizen <int>,
    ## #   Citizen <int>, gender <fct>, age <fct>, country <fct>, dateW1 <date>

Recode oxford data:

``` r
#Recode 
oxforddata$dateB <-anydate(oxforddata$Date)
oxforddata$dateW1 <- anydate(oxforddata$Date)
oxforddata$country <- oxforddata$CountryName
```

Drop not used variables
Baseline:

``` r
Stringency_dataB <- oxforddata %>% select(dateB, country, StringencyIndex, ConfirmedCases, ConfirmedDeaths)
```

Drop not used variables
W1:

``` r
Stringency_dataW1 <- oxforddata %>% select(dateW1, country, StringencyIndex, ConfirmedCases, ConfirmedDeaths)
```

Join with Baseline data by country and date, rename, and
save:

``` r
coronadataBS <- left_join(coronadataB, Stringency_dataB, by=c("country", "dateB"))
```

    ## Warning: Column `country` joining factors with different levels, coercing to
    ## character vector

``` r
coronadataBS <- coronadataBS %>%
  dplyr::rename(
    StringencyIndexB1 = StringencyIndex,
    ConfirmedCasesB1 = ConfirmedCases,
    ConfirmedDeathsB1 = ConfirmedDeaths)

save(coronadataBS, file ="coronadataBS.Rdata")
```

First 10 rows and 8
columns:

``` r
apa_table(coronadataBS[1:10,1:8])
```

<caption>

(\#tab:unnamed-chunk-10)

</caption>

<div data-custom-style="Table Caption">

\*\*

</div>

| ID | affAnx | affBor | affCalm | affContent | affDepr | affEnerg | affExc |
| :- | :----- | :----- | :------ | :--------- | :------ | :------- | :----- |
| 2  | 3      | 4      | 2       | 2          | 1       | 3        | 1      |
| 3  | 4      | 3      | 3       | 3          | 2       | 3        | 2      |
| 6  | 2      | 3      | 1       | 2          | 1       | 3        | 1      |
| 7  | 3      | 4      | 3       | 3          | 4       | 2        | 3      |
| 8  | 4      | 4      | 3       | 2          | 2       | 2        | 2      |
| 9  | 3      | 4      | 3       | 3          | 3       | 3        | 3      |
| 11 | 2      | 3      | 3       | 3          | 1       | 3        | 3      |
| 12 | 2      | 3      | 3       | 4          | 2       | 4        | 4      |
| 13 | 5      | 2      | 5       | 4          | 1       | 3        | 3      |
| 15 | 3      | 5      | 4       | 1          | 5       | 2        | 2      |

Join with Wave 1 data, rename and
save:

``` r
coronadataW1S <- left_join(coronadataW1, Stringency_dataW1, by=c("country", "dateW1"))
```

    ## Warning: Column `country` joining factors with different levels, coercing to
    ## character vector

``` r
coronadataW1S <- coronadataW1S %>%
  dplyr::rename(
    StringencyIndexW1 = StringencyIndex,
    ConfirmedCasesW1 = ConfirmedCases,
    ConfirmedDeathsW1 = ConfirmedDeaths)

save(coronadataW1S, file ="coronadataW1S.Rdata")
```

First 10 rows and 8 columns (still a lot of missing data of participants
that only completed
baseline):

``` r
apa_table(coronadataW1S[1:10,1:8])
```

<caption>

(\#tab:unnamed-chunk-12)

</caption>

<div data-custom-style="Table Caption">

\*\*

</div>

| ID | w1\_affAnx | w1\_affBor | w1\_affCalm | w1\_affDepr | w1\_affEnerg | w1\_affNerv | w1\_affExh |
| :- | :--------- | :--------- | :---------- | :---------- | :----------- | :---------- | :--------- |
| 2  | NA         | NA         | NA          | NA          | NA           | NA          | NA         |
| 3  | NA         | NA         | NA          | NA          | NA           | NA          | NA         |
| 6  | NA         | NA         | NA          | NA          | NA           | NA          | NA         |
| 7  | NA         | NA         | NA          | NA          | NA           | NA          | NA         |
| 8  | NA         | NA         | NA          | NA          | NA           | NA          | NA         |
| 9  | NA         | NA         | NA          | NA          | NA           | NA          | NA         |
| 11 | NA         | NA         | NA          | NA          | NA           | NA          | NA         |
| 12 | NA         | NA         | NA          | NA          | NA           | NA          | NA         |
| 13 | NA         | NA         | NA          | NA          | NA           | NA          | NA         |
| 15 | NA         | NA         | NA          | NA          | NA           | NA          | NA         |

Merge Baseline and W1, drop duplicates, reorder and save:

``` r
coronadataBW1S <- merge(coronadataBS, coronadataW1S, by = "ID")

coronadataBW1S <- coronadataBW1S %>% select(-c(countryCitizen.y, Citizen.y, country.y, gender.y, age.y))

coronadataBW1S <- coronadataBW1S[,c("ID", "dateB", "dateW1", "countryCitizen.x", "Citizen.x", "country.x", "gender.x", "age.x",
                                    "StringencyIndexB1", "StringencyIndexW1", "affAnx", "affBor", "affCalm", "affContent", 
                                    "affDepr", "affEnerg", "affExc", "affNerv", "affExh", "affInsp", "affRel", "happy",
                                     "coronaClose_1", "coronaClose_2", "coronaClose_3", "coronaClose_4", "coronaClose_5", 
                                    "coronaClose_6",
                                     "w1_affAnx", "w1_affBor", "w1_affCalm", "w1_affDepr", "w1_affEnerg", "w1_affNerv",
                                    "w1_affExh", "w1_affInsp", "w1_affRel", "w1_affAng", "w1_affLov", "w1_coronaClose_1", 
                                    "w1_coronaClose_2", "w1_coronaClose_3", "w1_coronaClose_4", "w1_coronaClose_5", 
                                    "w1_coronaClose_6")]

save(coronadataBW1S, file ="coronadataBW1S.Rdata")
```

First 10 rows and 8
columns:

``` r
apa_table(coronadataBW1S[1:10,1:8])
```

<caption>

(\#tab:unnamed-chunk-14)

</caption>

<div data-custom-style="Table Caption">

\*\*

</div>

| ID | dateB      | dateW1 | countryCitizen.x | Citizen.x | country.x    | gender.x | age.x |
| :- | :--------- | :----- | :--------------- | :-------- | :----------- | :------- | :---- |
| 2  | 2020-03-23 | NA     | 1                | 0         | Israel       | 1        | 4     |
| 3  | 2020-03-25 | NA     | 1                | 1         | Saudi Arabia | 1        | 2     |
| 6  | 2020-03-24 | NA     | 1                | 1         | Saudi Arabia | 1        | 1     |
| 7  | 2020-04-19 | NA     | 1                | 1         | Saudi Arabia | 1        | 1     |
| 8  | 2020-03-25 | NA     | 1                | 1         | Saudi Arabia | 1        | 4     |
| 9  | 2020-04-22 | NA     | 1                | 1         | Saudi Arabia | 2        | 4     |
| 11 | 2020-04-23 | NA     | 1                | 1         | Saudi Arabia | 2        | 4     |
| 12 | 2020-04-19 | NA     | 1                | 1         | Saudi Arabia | 1        | 1     |
| 13 | 2020-04-22 | NA     | 1                | 1         | Saudi Arabia | 2        | 3     |
| 15 | 2020-05-04 | NA     | 1                | NA        | Egypt        | 1        | 1     |
