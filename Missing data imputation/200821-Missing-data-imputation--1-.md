Missing data imputation
================
Anne Margit
10/26/2020

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
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.0 --

    ## v tibble  3.0.3     v purrr   0.3.4
    ## v tidyr   1.1.2     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.5.0

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(anytime)
```

    ## Warning: package 'anytime' was built under R version 4.0.3

``` r
library(missMDA)
```

    ## Warning: package 'missMDA' was built under R version 4.0.3

``` r
library(psych)
```

    ## Warning: package 'psych' was built under R version 4.0.3

    ## 
    ## Attaching package: 'psych'

    ## The following objects are masked from 'package:ggplot2':
    ## 
    ##     %+%, alpha

``` r
load("data_long_min3_str_age.Rdata")
```

order

``` r
data_long_min3_str_age<-data_long_min3_str_age[with(data_long_min3_str_age, order(ID, Time)),]
```

Select the variables: items for PA and NA, and the grouping variable ID:

``` r
selectie <- data_long_min3_str_age %>%
  group_by(ID) %>%
  select(Ang, Anxiety, Calm, Depr, Energ, Exh, Insp, Nerv, Rel)
```

    ## Adding missing grouping variables: `ID`

``` r
selectie2 <- data_long_min3_str_age %>%
  group_by(ID) %>%
  select(Ang, Anxiety, Calm, Depr, Energ, Exh, Insp, Nerv, Rel, Time)
```

    ## Adding missing grouping variables: `ID`

Grouping variable into factor

``` r
class(selectie$ID)
```

    ## [1] "integer"

``` r
selectie$ID <- factor(selectie$ID)
is.factor(selectie$ID)
```

    ## [1] TRUE

Impute with imputeMultilevel, with 4 4 between- and 4 within factors

``` r
selectieImputed <- imputeMultilevel(selectie, ifac=1, ncpB=4, ncpW=4)
View(selectieImputed$completeObs)
dim(selectieImputed$completeObs)
```

    ## [1] 125484     10

Create time variable

``` r
selectieImputed$completeObs<-selectieImputed$completeObs %>% group_by(ID) %>%
  mutate(Time = 1:n()) %>% 
  ungroup()
View(selectieImputed$completeObs)
View(selectie2)
```

``` r
selectieImputed2<-selectieImputed$completeObs
selectieImputed2$Time <- as.factor(selectieImputed2$Time)
```

Check de imputaties met wat plotjes:

``` r
p <- ggplot(selectie2, aes(x=Time, y=Ang)) 
p + stat_summary(geom = "point", fun = mean, size = 1) 
```

    ## Warning: Removed 76747 rows containing non-finite values (stat_summary).

![](200821-Missing-data-imputation--1-_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
p2 <- ggplot(selectieImputed2, aes(x=Time, y=Ang)) 
p2 + stat_summary(geom = "point", fun = mean, size = 1) 
```

![](200821-Missing-data-imputation--1-_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

Vergelijk de imputaties met de oorspronkelijke data:

``` r
summary(selectie2[which(selectie2$Time==1), ]$Ang)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##      NA      NA      NA     NaN      NA      NA   10457

``` r
summary(selectieImputed2[which(selectieImputed2$Time==1), ]$Ang)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.4684  1.5465  2.0076  2.0959  2.5751  5.1198

Door de imputatie zijn er nu niet-mogelijke scores, bv. negatieve scores
en scores \> 5. Is dit een probleem?

Maak wat plotjes met lijnen per id, op een selectie van de eerste 10
personen:

``` r
class(selectieImputed2$ID)
```

    ## [1] "factor"

``` r
selectieImputed2$ID2 <- as.numeric(selectieImputed2$ID)
describe(selectieImputed2$ID2)
```

    ##    vars      n mean      sd median trimmed     mad min   max range skew
    ## X1    1 125484 5229 3018.69   5229    5229 3875.52   1 10457 10456    0
    ##    kurtosis   se
    ## X1     -1.2 8.52

Bekijk nu hoe het eerste datapunt is geimputeerd:

``` r
ggplot(data = selectieImputed2[which(selectieImputed2$ID2 <10), ], aes(x = Time, y = Ang, color=as.factor(ID2))) + geom_line() + geom_point() + facet_grid(. ~ ID2)
```

    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?

![](200821-Missing-data-imputation--1-_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
head(selectie2[which(selectie2$Time==1), ]$Ang, n=20L)
```

    ##  [1] NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA

``` r
head(selectieImputed2[which(selectieImputed2$Time==1), ]$Ang, n=20L)
```

    ##  [1] 2.5709701 2.9838064 1.8653325 2.7604218 1.2169757 2.1250567 1.4979091
    ##  [8] 1.2034100 0.9741964 1.2784021 2.7855291 2.5842363 1.9253040 1.1895384
    ## [15] 1.8420204 1.1689674 3.4127949 2.8254615 0.9364671 2.0501404

``` r
save(selectieImputed2, file="selectieImputed2.Rdata")
save(selectie2, file="selectie2.Rdata")
```

\!\! Imputeert ook hele meetmomenten als ALLE items missen op die meting
\!\! Alleen losse items terugplaatsen op meetmomenten waarop
participanten andere items wel beantwoord hebben
