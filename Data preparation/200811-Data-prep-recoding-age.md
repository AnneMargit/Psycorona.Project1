200811 Data prep recoding age
================
Anne Margit
8/11/2020

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
library(knitr)
library(tidyverse)
```

    ## ── Attaching packages ──────────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.1     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.1     ✓ stringr 1.4.0
    ## ✓ tidyr   1.1.0     ✓ forcats 0.5.0
    ## ✓ readr   1.3.1

    ## ── Conflicts ─────────────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

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
load("data_long_min3_str.Rdata")
```

Make new age groups: Youth: aged 18-24 coded as 1 \<- 0 Young adults:
aged 25-44 coded as 2 or 3 \<- 1 Middle-aged adults: aged 45-64 coded as
4 or 5 \<- 2 Older-aged adults: aged 65+ coded as 6, 7, or 8 \<- 3

``` r
data_long_min3_str_age <- data_long_min3_str 
data_long_min3_str_age$age_new <- data_long_min3_str_age$age
```

``` r
data_long_min3_str_age$age_new <- data_long_min3_str_age$age_new %>%
  plyr::revalue(c("1"="0", "2"= "1", "3"="1", "4"="2", "5"="2", "6"="3", "7"="3", "8"="3"))
```

save

``` r
save(data_long_min3_str_age, file="data_long_min3_str_age.Rdata")
```
