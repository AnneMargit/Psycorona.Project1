200824 Merging imputed data
================
Anne Margit
8/24/2020

``` r
library(dplyr)
library(knitr)
library(tidyverse)
library(ggplot2)
library(psych)
```

``` r
load("data_long_min3_str_age.Rdata")
load("selectieImputed2.Rdata")
```

Rename imputed variables:

``` r
selectieNew <- selectieImputed2 %>%
  rename("Ang_imp" = "Ang",
         "Anxiety_imp" = "Anxiety",
         "Calm_imp" = "Calm",
         "Depr_imp" = "Depr",
         "Energ_imp" = "Energ",
         "Exh_imp" = "Exh",
         "Insp_imp" = "Insp",
         "Nerv_imp" = "Nerv",
         "Rel_imp" = "Rel") 
```

Merge imputed dataset with original dataset:

``` r
data_long_min3_str_age$ID <- as.factor(data_long_min3_str_age$ID)
selectieNew$ID <- as.factor(selectieNew$ID)

data_merge1 <- left_join(data_long_min3_str_age, selectieNew, by=c("ID", "Time"))
```

Create new variable that indicates sum of missings:

``` r
data_merge1 <- data_merge1 %>%
  group_by(ID, Time) %>%
mutate(Nmiss = sum(is.na(Ang)) + sum(is.na(Anxiety)) + sum(is.na(Nerv)) + sum(is.na(Depr)) + sum(is.na(Exh)) + 
               sum(is.na(Energ)) + sum(is.na(Insp)) + sum(is.na(Calm)) + sum(is.na(Rel))) %>%
  ungroup()
```

``` r
describe(data_merge1$Nmiss)
```

    ##    vars      n mean   sd median trimmed mad min max range  skew kurtosis   se
    ## X1    1 124116 4.87 4.41      9    4.97   0   0   9     9 -0.14    -1.97 0.01

``` r
data_merge1 %>%
  summarize(n= sum(Nmiss == 8))
```

    ## # A tibble: 1 x 1
    ##       n
    ##   <int>
    ## 1    23

``` r
data_merge1 %>%
  summarize(n= sum(Nmiss == 7))
```

    ## # A tibble: 1 x 1
    ##       n
    ##   <int>
    ## 1   130

``` r
data_merge1 %>%
  summarize(n= sum(Nmiss == 6))
```

    ## # A tibble: 1 x 1
    ##       n
    ##   <int>
    ## 1    78

``` r
data_merge1 %>%
  summarize(n= sum(Nmiss == 5))
```

    ## # A tibble: 1 x 1
    ##       n
    ##   <int>
    ## 1    22

Create new variable coded as missing if sum of missings = 9 (if all
scores are
missing)

``` r
data_merge1$Ang_imp2 <- ifelse(data_merge1$Nmiss == 9, NA, data_merge1$Ang_imp)

data_merge1$Anxiety_imp2 <- ifelse(data_merge1$Nmiss == 9, NA, data_merge1$Anxiety_imp)

data_merge1$Nerv_imp2 <- ifelse(data_merge1$Nmiss == 9, NA, data_merge1$Nerv_imp)

data_merge1$Depr_imp2 <- ifelse(data_merge1$Nmiss == 9, NA, data_merge1$Depr_imp)

data_merge1$Exh_imp2 <- ifelse(data_merge1$Nmiss == 9, NA, data_merge1$Exh_imp)

data_merge1$Energ_imp2 <- ifelse(data_merge1$Nmiss == 9, NA, data_merge1$Energ_imp)

data_merge1$Insp_imp2 <- ifelse(data_merge1$Nmiss == 9, NA, data_merge1$Insp_imp)

data_merge1$Calm_imp2 <- ifelse(data_merge1$Nmiss == 9, NA, data_merge1$Calm_imp)

data_merge1$Rel_imp2 <- ifelse(data_merge1$Nmiss == 9, NA, data_merge1$Rel_imp)
```

Drop other emotion variables (original items and imputed items)

``` r
data_merge2 <- data_merge1 %>%
  select(-c("Ang", "Anxiety", "Calm", "Depr", "Energ", "Exh", "Insp", "Nerv", "Rel",
            "Ang_imp", "Anxiety_imp", "Calm_imp", "Depr_imp", "Energ_imp", "Exh_imp", "Insp_imp", "Nerv_imp", "Rel_imp"))
```

Rename variables:

``` r
data_imputed <- data_merge2 %>%
  rename("Ang" = "Ang_imp2",
         "Anxiety" = "Anxiety_imp2",
         "Calm" = "Calm_imp2",
         "Depr" = "Depr_imp2",
         "Energ" = "Energ_imp2",
         "Exh" = "Exh_imp2",
         "Insp" = "Insp_imp2",
         "Nerv" = "Nerv_imp2",
         "Rel" = "Rel_imp2") 
```

Check original data

``` r
summary(data_long_min3_str_age[,7:15])
```

    ##       Ang           Anxiety           Calm            Depr      
    ##  Min.   :1.00    Min.   :1.00    Min.   :1.00    Min.   :1      
    ##  1st Qu.:1.00    1st Qu.:1.00    1st Qu.:2.00    1st Qu.:1      
    ##  Median :2.00    Median :2.00    Median :3.00    Median :2      
    ##  Mean   :1.98    Mean   :2.37    Mean   :3.07    Mean   :2      
    ##  3rd Qu.:3.00    3rd Qu.:3.00    3rd Qu.:4.00    3rd Qu.:3      
    ##  Max.   :5.00    Max.   :5.00    Max.   :5.00    Max.   :5      
    ##  NA's   :76349   NA's   :66045   NA's   :66033   NA's   :66072  
    ##      Energ            Exh             Insp            Nerv      
    ##  Min.   :1.00    Min.   :1.00    Min.   :1.00    Min.   :1.00   
    ##  1st Qu.:2.00    1st Qu.:1.00    1st Qu.:2.00    1st Qu.:1.00   
    ##  Median :3.00    Median :2.00    Median :2.00    Median :2.00   
    ##  Mean   :2.65    Mean   :2.34    Mean   :2.48    Mean   :2.25   
    ##  3rd Qu.:3.00    3rd Qu.:3.00    3rd Qu.:3.00    3rd Qu.:3.00   
    ##  Max.   :5.00    Max.   :5.00    Max.   :5.00    Max.   :5.00   
    ##  NA's   :66098   NA's   :66097   NA's   :66123   NA's   :66073  
    ##       Rel       
    ##  Min.   :1.00   
    ##  1st Qu.:2.00   
    ##  Median :3.00   
    ##  Mean   :2.88   
    ##  3rd Qu.:4.00   
    ##  Max.   :5.00   
    ##  NA's   :66078

Compare original data and imputed data

``` r
summary(data_imputed[,20:28])
```

    ##      Nmiss            Ang           Anxiety           Nerv      
    ##  Min.   :0.000   Min.   :0.46    Min.   :0.62    Min.   :0.57   
    ##  1st Qu.:0.000   1st Qu.:1.00    1st Qu.:1.00    1st Qu.:1.00   
    ##  Median :9.000   Median :2.00    Median :2.00    Median :2.00   
    ##  Mean   :4.874   Mean   :2.00    Mean   :2.37    Mean   :2.25   
    ##  3rd Qu.:9.000   3rd Qu.:2.96    3rd Qu.:3.00    3rd Qu.:3.00   
    ##  Max.   :9.000   Max.   :5.12    Max.   :5.00    Max.   :5.00   
    ##                  NA's   :65870   NA's   :65870   NA's   :65870  
    ##       Depr            Exh            Energ            Insp      
    ##  Min.   :0.41    Min.   :0.69    Min.   :0.75    Min.   :0.60   
    ##  1st Qu.:1.00    1st Qu.:1.00    1st Qu.:2.00    1st Qu.:2.00   
    ##  Median :2.00    Median :2.00    Median :3.00    Median :2.00   
    ##  Mean   :2.00    Mean   :2.34    Mean   :2.65    Mean   :2.48   
    ##  3rd Qu.:3.00    3rd Qu.:3.00    3rd Qu.:3.00    3rd Qu.:3.00   
    ##  Max.   :5.00    Max.   :5.00    Max.   :5.00    Max.   :5.00   
    ##  NA's   :65870   NA's   :65870   NA's   :65870   NA's   :65870  
    ##       Calm      
    ##  Min.   :1.00   
    ##  1st Qu.:2.00   
    ##  Median :3.00   
    ##  Mean   :3.06   
    ##  3rd Qu.:4.00   
    ##  Max.   :5.00   
    ##  NA's   :65870

Nu hebben alle emoties evenveel missings. Is dit logisch?

Maak wat plotjes met lijnen per id, op een selectie van de eerste 15
personen:

``` r
class(data_long_min3_str_age$ID)
```

    ## [1] "factor"

``` r
data_long_min3_str_age$ID2 <- as.numeric(data_long_min3_str_age$ID)
describe(data_long_min3_str_age$ID2)
```

    ##    vars      n mean      sd median trimmed  mad min   max range skew kurtosis
    ## X1    1 124116 5172 2985.78   5172    5172 3834   1 10343 10342    0     -1.2
    ##      se
    ## X1 8.48

``` r
class(data_imputed$ID)
```

    ## [1] "factor"

``` r
data_imputed$ID2 <- as.numeric(data_imputed$ID)
describe(data_imputed$ID2)
```

    ##    vars      n mean      sd median trimmed  mad min   max range skew kurtosis
    ## X1    1 124116 5172 2985.78   5172    5172 3834   1 10343 10342    0     -1.2
    ##      se
    ## X1 8.48

Originele
data:

``` r
plot1<- ggplot(data = data_long_min3_str_age[which(data_long_min3_str_age$ID2 <16), ], aes(x = Time, y = Ang, color=as.factor(ID2))) + geom_line() + geom_point() + facet_wrap(. ~ ID2, nrow=3)

plot1
```

![](200824-Merging-imputed-data_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

Originele data gemerged met geimputeerde
data:

``` r
plot2<- ggplot(data = data_imputed[which(data_imputed$ID2 <16), ], aes(x = Time, y = Ang, color=as.factor(ID2))) + geom_line() + geom_point() + facet_wrap(. ~ ID2, nrow=3)

plot2
```

![](200824-Merging-imputed-data_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
save(data_imputed, file="data_imputed.Rdata")
```
