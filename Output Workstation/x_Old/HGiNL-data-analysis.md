HGis data analysis
================
Anne Margit
10/29/2020

``` r
load("data_HGis.Rdata")
```

``` r
library(dplyr)
library(tidyverse)
library(psych)
```

    ## Warning: package 'psych' was built under R version 4.0.3

``` r
library(lme4)
library(lmerTest)
```

    ## Warning: package 'lmerTest' was built under R version 4.0.3

Sample size per age group

``` r
data_HGis$Age_new <- as.factor(data_HGis$Age_new)

data_HGis %>% 
group_by(Age_new) %>%
summarize(NAge = n())
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## # A tibble: 4 x 2
    ##   Age_new  NAge
    ##   <fct>   <int>
    ## 1 0         402
    ## 2 1        6092
    ## 3 2        8102
    ## 4 3        4538

Mean emotion ratings + sd per timepoint

``` r
data_HGis %>% 
group_by(Time) %>% 
summarise_at(
  .vars = vars(NAA, NAD, PAA),
  .funs = c(mean="mean", sd="sd"), na.rm=TRUE)
```

    ## # A tibble: 2 x 7
    ##   Time  NAA_mean NAD_mean PAA_mean NAA_sd NAD_sd PAA_sd
    ##   <fct>    <dbl>    <dbl>    <dbl>  <dbl>  <dbl>  <dbl>
    ## 1 0         2.12     2.22     3.09  0.848  1.03   0.932
    ## 2 1         2.01     2.02     2.91  0.827  0.889  0.873

Means per age group per timepoint

``` r
data_HGis %>% 
group_by(Age_new, Time) %>% 
summarise_at(
  .vars = vars(NAA, NAD, PAA),
  .funs = c(mean="mean", sd="sd"), na.rm=TRUE)
```

    ## # A tibble: 8 x 8
    ## # Groups:   Age_new [4]
    ##   Age_new Time  NAA_mean NAD_mean PAA_mean NAA_sd NAD_sd PAA_sd
    ##   <fct>   <fct>    <dbl>    <dbl>    <dbl>  <dbl>  <dbl>  <dbl>
    ## 1 0       0         2.63     2.65     3.04  0.961  1.14   0.996
    ## 2 0       1         2.27     2.8      2.7   0.760  0.975  0.447
    ## 3 1       0         2.34     2.43     2.99  0.873  1.05   0.939
    ## 4 1       1         2.36     2.34     2.79  0.866  0.892  0.921
    ## 5 2       0         2.04     2.17     3.11  0.812  1.01   0.939
    ## 6 2       1         1.86     1.87     2.97  0.729  0.852  0.850
    ## 7 3       0         1.91     2.00     3.19  0.778  0.957  0.887
    ## 8 3       1         1.70     1.72     3.02  0.740  0.769  0.832

NAA

``` r
lmer_NAA <- lmer(NAA ~ Time + Age_new + (1|profile_id), data=data_HGis)
summary(lmer_NAA)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: NAA ~ Time + Age_new + (1 | profile_id)
    ##    Data: data_HGis
    ## 
    ## REML criterion at convergence: 24655.9
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.0265 -0.6768 -0.1858  0.4270  3.2481 
    ## 
    ## Random effects:
    ##  Groups     Name        Variance Std.Dev.
    ##  profile_id (Intercept) 0.2232   0.4724  
    ##  Residual               0.4587   0.6772  
    ## Number of obs: 10057, groups:  profile_id, 9560
    ## 
    ## Fixed effects:
    ##               Estimate Std. Error         df t value Pr(>|t|)    
    ## (Intercept)    2.62493    0.05788 9722.01216  45.350  < 2e-16 ***
    ## Time1         -0.09185    0.03511 1803.27181  -2.616  0.00896 ** 
    ## Age_new1      -0.27685    0.05974 9702.02531  -4.634 3.63e-06 ***
    ## Age_new2      -0.58354    0.05928 9708.00602  -9.843  < 2e-16 ***
    ## Age_new3      -0.72148    0.06037 9702.10229 -11.952  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##          (Intr) Time1  Ag_nw1 Ag_nw2
    ## Time1    -0.011                     
    ## Age_new1 -0.969 -0.015              
    ## Age_new2 -0.976 -0.012  0.946       
    ## Age_new3 -0.959 -0.009  0.929  0.936

``` r
anova(lmer_NAA)
```

    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##          Sum Sq Mean Sq NumDF  DenDF  F value    Pr(>F)    
    ## Time      3.139   3.139     1 1803.3   6.8447  0.008965 ** 
    ## Age_new 234.912  78.304     3 9485.1 170.7267 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

NAD

``` r
lmer_NAD <- lmer(NAD ~ Time + Age_new + (1|profile_id), data=data_HGis)
summary(lmer_NAD)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: NAD ~ Time + Age_new + (1 | profile_id)
    ##    Data: data_HGis
    ## 
    ## REML criterion at convergence: 28632
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -1.8749 -0.7452 -0.1331  0.4551  2.8936 
    ## 
    ## Random effects:
    ##  Groups     Name        Variance Std.Dev.
    ##  profile_id (Intercept) 0.3558   0.5965  
    ##  Residual               0.6578   0.8110  
    ## Number of obs: 10057, groups:  profile_id, 9560
    ## 
    ## Fixed effects:
    ##               Estimate Std. Error         df t value Pr(>|t|)    
    ## (Intercept)    2.65734    0.07059 9667.03201  37.642  < 2e-16 ***
    ## Time1         -0.20637    0.04240 1468.67935  -4.868 1.25e-06 ***
    ## Age_new1      -0.22612    0.07286 9645.47157  -3.104  0.00192 ** 
    ## Age_new2      -0.49098    0.07230 9651.91155  -6.790 1.18e-11 ***
    ## Age_new3      -0.65810    0.07363 9645.53641  -8.938  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##          (Intr) Time1  Ag_nw1 Ag_nw2
    ## Time1    -0.011                     
    ## Age_new1 -0.969 -0.015              
    ## Age_new2 -0.976 -0.012  0.946       
    ## Age_new3 -0.959 -0.009  0.929  0.936

``` r
anova(lmer_NAD)
```

    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##          Sum Sq Mean Sq NumDF  DenDF F value    Pr(>F)    
    ## Time     15.586  15.586     1 1468.7  23.696  1.25e-06 ***
    ## Age_new 196.771  65.590     3 9414.6  99.716 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

PAA

``` r
lmer_PAA <- lmer(PAA ~ Time + Age_new + (1|profile_id), data=data_HGis)
summary(lmer_PAA)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: PAA ~ Time + Age_new + (1 | profile_id)
    ##    Data: data_HGis
    ## 
    ## REML criterion at convergence: 26930.5
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.54379 -0.52493  0.01372  0.68768  2.53636 
    ## 
    ## Random effects:
    ##  Groups     Name        Variance Std.Dev.
    ##  profile_id (Intercept) 0.3207   0.5663  
    ##  Residual               0.5358   0.7320  
    ## Number of obs: 10057, groups:  profile_id, 9560
    ## 
    ## Fixed effects:
    ##               Estimate Std. Error         df t value Pr(>|t|)    
    ## (Intercept)    3.03385    0.06492 9743.36065  46.735  < 2e-16 ***
    ## Time1         -0.16730    0.03858 1669.67617  -4.337 1.53e-05 ***
    ## Age_new1      -0.04991    0.06700 9727.15632  -0.745   0.4564    
    ## Age_new2       0.08040    0.06649 9731.99012   1.209   0.2266    
    ## Age_new3       0.16146    0.06771 9727.19092   2.385   0.0171 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##          (Intr) Time1  Ag_nw1 Ag_nw2
    ## Time1    -0.011                     
    ## Age_new1 -0.969 -0.015              
    ## Age_new2 -0.976 -0.012  0.946       
    ## Age_new3 -0.959 -0.009  0.929  0.936

``` r
anova(lmer_PAA)
```

    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##         Sum Sq Mean Sq NumDF  DenDF F value    Pr(>F)    
    ## Time    10.077  10.077     1 1669.7  18.808 1.532e-05 ***
    ## Age_new 39.942  13.314     3 9554.3  24.848 5.222e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
