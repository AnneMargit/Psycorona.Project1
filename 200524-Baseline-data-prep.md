Baseline data prep
================
true
5/24/2020

Calculate number of participants per country & select countries with
\>500 participants:

``` r
load("coronadataBS.Rdata")

coronadataBS$country <- as.factor(coronadataBS$country)

coronadataBS <- as_tibble(coronadataBS)

countrytableN <- coronadataBS %>%
  dplyr::group_by(country) %>%
  dplyr::mutate(N=n())

coronadataBS500 <- countrytableN %>%
  dplyr::filter(N>500)

coronadataBS500 <- coronadataBS500 %>%
  select(-Citizen)

summary(coronadataBS500)
```

    ##        ID            affAnx          affBor         affCalm     
    ##  Min.   :    3   Min.   :1.000   Min.   :1.000   Min.   :1.000  
    ##  1st Qu.:15231   1st Qu.:2.000   1st Qu.:2.000   1st Qu.:2.000  
    ##  Median :29798   Median :3.000   Median :3.000   Median :3.000  
    ##  Mean   :29841   Mean   :2.729   Mean   :2.713   Mean   :2.911  
    ##  3rd Qu.:44425   3rd Qu.:4.000   3rd Qu.:4.000   3rd Qu.:4.000  
    ##  Max.   :58711   Max.   :5.000   Max.   :5.000   Max.   :5.000  
    ##                  NA's   :480     NA's   :502     NA's   :489    
    ##    affContent       affDepr         affEnerg         affExc     
    ##  Min.   :1.000   Min.   :1.000   Min.   :1.000   Min.   :1.000  
    ##  1st Qu.:2.000   1st Qu.:1.000   1st Qu.:2.000   1st Qu.:1.000  
    ##  Median :3.000   Median :2.000   Median :3.000   Median :2.000  
    ##  Mean   :2.635   Mean   :2.229   Mean   :2.548   Mean   :2.123  
    ##  3rd Qu.:3.000   3rd Qu.:3.000   3rd Qu.:3.000   3rd Qu.:3.000  
    ##  Max.   :5.000   Max.   :5.000   Max.   :5.000   Max.   :5.000  
    ##  NA's   :579     NA's   :575     NA's   :628     NA's   :670    
    ##     affNerv          affExh         affInsp          affRel     
    ##  Min.   :1.000   Min.   :1.000   Min.   :1.000   Min.   :1.000  
    ##  1st Qu.:2.000   1st Qu.:1.000   1st Qu.:1.000   1st Qu.:2.000  
    ##  Median :2.000   Median :2.000   Median :2.000   Median :3.000  
    ##  Mean   :2.583   Mean   :2.456   Mean   :2.414   Mean   :2.732  
    ##  3rd Qu.:4.000   3rd Qu.:3.000   3rd Qu.:3.000   3rd Qu.:4.000  
    ##  Max.   :5.000   Max.   :5.000   Max.   :5.000   Max.   :5.000  
    ##  NA's   :535     NA's   :619     NA's   :656     NA's   :568    
    ##      happy        coronaClose_1     coronaClose_2    coronaClose_3    
    ##  Min.   : 1.000   Min.   :0.00000   Min.   :0.0000   Min.   :0.00000  
    ##  1st Qu.: 5.000   1st Qu.:0.00000   1st Qu.:0.0000   1st Qu.:0.00000  
    ##  Median : 7.000   Median :0.00000   Median :0.0000   Median :0.00000  
    ##  Mean   : 6.362   Mean   :0.01435   Mean   :0.0314   Mean   :0.03923  
    ##  3rd Qu.: 8.000   3rd Qu.:0.00000   3rd Qu.:0.0000   3rd Qu.:0.00000  
    ##  Max.   :10.000   Max.   :1.00000   Max.   :1.0000   Max.   :1.00000  
    ##  NA's   :632                                                          
    ##  coronaClose_4    coronaClose_5    coronaClose_6    countryCitizen  gender     
    ##  Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :1      1   :30393  
    ##  1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:1      2   :19870  
    ##  Median :0.0000   Median :0.0000   Median :1.0000   Median :1      3   :  224  
    ##  Mean   :0.1182   Mean   :0.1103   Mean   :0.7395   Mean   :1      NA's:  115  
    ##  3rd Qu.:0.0000   3rd Qu.:0.0000   3rd Qu.:1.0000   3rd Qu.:1                  
    ##  Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1                  
    ##                                                                                
    ##       age                 country          dateB            StringencyIndexB1
    ##  2      :12014   United States:10248   Min.   :2020-03-19   Min.   : 24.07   
    ##  1      : 9968   Spain        : 3045   1st Qu.:2020-04-02   1st Qu.: 68.41   
    ##  3      : 9959   Greece       : 2724   Median :2020-04-11   Median : 81.75   
    ##  4      : 7731   Romania      : 2645   Mean   :2020-04-08   Mean   : 79.54   
    ##  5      : 6298   Netherlands  : 2603   3rd Qu.:2020-04-17   3rd Qu.: 89.41   
    ##  (Other): 4492   Indonesia    : 2350   Max.   :2020-05-06   Max.   :100.00   
    ##  NA's   :  140   (Other)      :26987   NA's   :583          NA's   :588      
    ##  ConfirmedCasesB1  ConfirmedDeathsB1       N        
    ##  Min.   :    158   Min.   :    0     Min.   :  545  
    ##  1st Qu.:   5202   1st Qu.:  109     1st Qu.: 1378  
    ##  Median :  14250   Median :  437     Median : 2068  
    ##  Mean   :  69361   Mean   : 3333     Mean   : 3557  
    ##  3rd Qu.:  85991   3rd Qu.: 3601     3rd Qu.: 3045  
    ##  Max.   :1180634   Max.   :68934     Max.   :10248  
    ##  NA's   :583       NA's   :583

Impute Stringency Index, fill empty rows with last previously reported
index
score:

``` r
coronadataBS500<-coronadataBS500[with(coronadataBS500, order(country, dateB)),]

coronadataBSfill <- coronadataBS500 %>%
  group_by(country) %>%
  fill(StringencyIndexB1, .direction = "down")
```

Which countries are there in the dataset?

``` r
coronadataBSfill$country <- as_factor(coronadataBSfill$country)
levels(coronadataBSfill$country)
```

    ##   [1] "Albania"                "Algeria"                "Andorra"               
    ##   [4] "Argentina"              "Armenia"                "Australia"             
    ##   [7] "Austria"                "Azerbaijan"             "Bangladesh"            
    ##  [10] "Belarus"                "Belgium"                "Benin"                 
    ##  [13] "Bosnia and Herzegovina" "Brazil"                 "Brunei"                
    ##  [16] "Bulgaria"               "Cameroon"               "Canada"                
    ##  [19] "Chile"                  "China"                  "Colombia"              
    ##  [22] "Costa Rica"             "Croatia"                "Cyprus"                
    ##  [25] "Czech Republic"         "Denmark"                "Dominican Republic"    
    ##  [28] "Ecuador"                "Egypt"                  "El Salvador"           
    ##  [31] "Estonia"                "Finland"                "France"                
    ##  [34] "Georgia"                "Germany"                "Greece"                
    ##  [37] "Guatemala"              "Hong Kong"              "Hungary"               
    ##  [40] "Iceland"                "India"                  "Indonesia"             
    ##  [43] "Iran"                   "Iraq"                   "Ireland"               
    ##  [46] "Israel"                 "Italy"                  "Jamaica"               
    ##  [49] "Japan"                  "Jordan"                 "Kazakhstan"            
    ##  [52] "Kenya"                  "Kyrgyz Republic"        "Latvia"                
    ##  [55] "Lebanon"                "Libya"                  "Lithuania"             
    ##  [58] "Luxembourg"             "Malaysia"               "Mali"                  
    ##  [61] "Malta"                  "Mauritius"              "Mexico"                
    ##  [64] "Moldova"                "Mongolia"               "Montenegro"            
    ##  [67] "Morocco"                "Nepal"                  "Netherlands"           
    ##  [70] "New Zealand"            "Nigeria"                "Norway"                
    ##  [73] "Oman"                   "Pakistan"               "Palestine"             
    ##  [76] "Peru"                   "Philippines"            "Poland"                
    ##  [79] "Portugal"               "Qatar"                  "Romania"               
    ##  [82] "Russia"                 "Saudi Arabia"           "Serbia"                
    ##  [85] "Singapore"              "Slovakia"               "Slovenia"              
    ##  [88] "South Africa"           "South Korea"            "Spain"                 
    ##  [91] "Sweden"                 "Switzerland"            "Taiwan"                
    ##  [94] "Thailand"               "Trinidad and Tobago"    "Tunisia"               
    ##  [97] "Turkey"                 "Ukraine"                "United Arab Emirates"  
    ## [100] "United Kingdom"         "United States"          "Uruguay"               
    ## [103] "Uzbekistan"             "Venezuela"              "Vietnam"

How much missing data is left?

``` r
#Missing data
missing.values <- coronadataBSfill %>%
  gather(key = "key", value = "val") %>%
  dplyr::mutate(is.missing = is.na(val)) %>%
  dplyr::group_by(key, is.missing) %>%
  dplyr::summarise(num.missing = n()) %>%
  dplyr::filter(is.missing==T) %>%
  dplyr::select(-is.missing) %>%
  dplyr::arrange(desc(num.missing))
```

    ## Warning: attributes are not identical across measure variables;
    ## they will be dropped

``` r
missing.values
```

    ## # A tibble: 17 x 2
    ## # Groups:   key [17]
    ##    key               num.missing
    ##    <chr>                   <int>
    ##  1 affExc                    670
    ##  2 affInsp                   656
    ##  3 happy                     632
    ##  4 affEnerg                  628
    ##  5 affExh                    619
    ##  6 ConfirmedCasesB1          583
    ##  7 ConfirmedDeathsB1         583
    ##  8 dateB                     583
    ##  9 affContent                579
    ## 10 affDepr                   575
    ## 11 affRel                    568
    ## 12 affNerv                   535
    ## 13 affBor                    502
    ## 14 affCalm                   489
    ## 15 affAnx                    480
    ## 16 age                       140
    ## 17 gender                    115

Centering Stringency Index per
country

``` r
coronadataBSfill <- gmc(coronadataBSfill, "StringencyIndexB1", "country", FUN = mean, suffix = c("_mn", "_dev"),
    fulldataframe = TRUE)

save(coronadataBSfill, file="coronadataBSfill.Rdata")
```
