Second analysis NAA Phase 1 (Age only)
================
Anne Margit
10/28/2020

    ## [1] ""

``` r
load("data_analyse2_p1.Rdata")
```

This dataset includes:

1.  Data from all weekly measurement waves (baseline through wave 11,
    Time 1 through 12)
2.  Participants who provided at least 3 measurements
3.  Participants who are residents of the country they currently live in
4.  Participants who provided info on age
5.  Participants who provided info on gender (either male or female)
6.  Data from countries with at least 20 participants
7.  Pooled age groups
8.  Imputed missing emotion scores
9.  Combined emotion scores (NAA, NAD, PAA, PAD)
10. An imputed Stringency index (StringencyIndex\_imp)
11. A variable indicating the number of days before and after the day on
    which maximum stringency was reached for the respective country
    (DaysMax)
12. A variable indicating the number of weeks before and after the day
    on which maximum stringency was reached for the respective country
    (WeeksMax)
13. A variable indicating the date on which maximum Stringency was
    reached for that country (DateMaxStr)
14. A dummy Str\_dummy with 0 = before the peaj, 1 = during peak, 2 =
    after peak
15. Observations during which there was a second peak are excluded
    (N=583)

> My comments are in block quotes such as this.

``` r
library(dplyr)
library(tidyverse)
library(ggpubr)
library(ggplot2)
library(rockchalk)
library(effects)
library(nlme)
library(lattice)
library(broom.mixed)
library(purrr)
```

# Descriptives

**Number of participants per age group**

``` r
data_analyse2_p1 %>%
  group_by(Age_new) %>%
  summarise(NAge = n())
```

    # A tibble: 4 x 2
      Age_new  NAge
      <fct>   <int>
    1 0         695
    2 1        1893
    3 2        1274
    4 3         293

**Plots** **Mean NAA against max stringency in WEEKS**

``` r
plot_NAA <- ggplot(data_analyse2_p1, aes(x=WeeksMax_p1, y=NAA, group = Age_new, color = Age_new))

plot_NAA + stat_summary(fun.y=mean, geom="line", size=1)  + geom_errorbar(stat="summary", fun.data="mean_se", width=0) + scale_colour_discrete(name = "Age", labels = c("18-24", "25-44", "45-64", "65+")) + expand_limits(y=c(1, 5))
```

![](Second-analysis-NAA-Phase-1_Age-only_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

**Mean NAA against max stringency in DAYS**

``` r
plot_NAA <- ggplot(data_analyse2_p1, aes(x=DaysMax_p1, y=NAA, group = Age_new, color = Age_new))

plot_NAA + stat_summary(fun.y=mean, geom="line", size=1)  + geom_errorbar(stat="summary", fun.data="mean_se", width=0) + scale_colour_discrete(name = "Age", labels = c("18-24", "25-44", "45-64", "65+")) + expand_limits(y=c(1, 5))
```

![](Second-analysis-NAA-Phase-1_Age-only_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

# Regression models phase 1

**Negative affect low arousal**

*Predictor: Age, Random: IC for Country*

``` r
model_NAA1 <- lme(fixed = NAA ~  Age_new,
                   random = ~1 | Country, 
                  data = data_analyse2_p1, 
                  na.action = na.omit)

summary(model_NAA1)
```

    Linear mixed-effects model fit by REML
     Data: data_analyse2_p1 
           AIC      BIC    logLik
      11161.43 11199.42 -5574.716
    
    Random effects:
     Formula: ~1 | Country
            (Intercept)  Residual
    StdDev:   0.2601364 0.9191224
    
    Fixed effects: NAA ~ Age_new 
                     Value  Std.Error   DF   t-value p-value
    (Intercept)  2.7604138 0.07148790 4126  38.61372  0.0000
    Age_new1    -0.0870514 0.04146790 4126  -2.09925  0.0359
    Age_new2    -0.4102959 0.04505196 4126  -9.10717  0.0000
    Age_new3    -0.7275805 0.06683493 4126 -10.88623  0.0000
     Correlation: 
             (Intr) Ag_nw1 Ag_nw2
    Age_new1 -0.425              
    Age_new2 -0.394  0.690       
    Age_new3 -0.275  0.456  0.454
    
    Standardized Within-Group Residuals:
            Min          Q1         Med          Q3         Max 
    -2.07727880 -0.79397883 -0.09676078  0.72227472  2.90924015 
    
    Number of Observations: 4155
    Number of Groups: 26 

*Predictors: Age, Random: IC for ID*

``` r
model_NAA2 <- lme(fixed = NAA ~ Age_new,
                  random = ~1 | ID, 
                 data = data_analyse2_p1, 
                 na.action = na.omit)

summary(model_NAA2)
```

    Linear mixed-effects model fit by REML
     Data: data_analyse2_p1 
           AIC      BIC    logLik
      10540.81 10578.79 -5264.404
    
    Random effects:
     Formula: ~1 | ID
            (Intercept)  Residual
    StdDev:   0.7239131 0.5894974
    
    Fixed effects: NAA ~ Age_new 
                     Value  Std.Error   DF  t-value p-value
    (Intercept)  2.6827752 0.04280179 2743 62.67905   0.000
    Age_new1    -0.0872200 0.04964213 2743 -1.75698   0.079
    Age_new2    -0.3804005 0.05259262 2743 -7.23296   0.000
    Age_new3    -0.6038113 0.07318136 2743 -8.25089   0.000
     Correlation: 
             (Intr) Ag_nw1 Ag_nw2
    Age_new1 -0.862              
    Age_new2 -0.814  0.702       
    Age_new3 -0.585  0.504  0.476
    
    Standardized Within-Group Residuals:
            Min          Q1         Med          Q3         Max 
    -2.91874850 -0.53112493 -0.03894578  0.51521209  2.94049461 
    
    Number of Observations: 4155
    Number of Groups: 2747 

*Random: IC for ID and Country*

``` r
model_NAA3 <- lme(fixed = NAA ~ Age_new,
                  random = ~1 | Country/ID, 
                  data = data_analyse2_p1, 
                  na.action = na.omit)

summary(model_NAA3)
```

    Linear mixed-effects model fit by REML
     Data: data_analyse2_p1 
           AIC      BIC    logLik
      10420.69 10465.01 -5203.346
    
    Random effects:
     Formula: ~1 | Country
            (Intercept)
    StdDev:   0.2612806
    
     Formula: ~1 | ID %in% Country
            (Intercept)  Residual
    StdDev:   0.6945817 0.5879342
    
    Fixed effects: NAA ~ Age_new 
                     Value  Std.Error   DF  t-value p-value
    (Intercept)  2.7721220 0.07534534 2718 36.79222  0.0000
    Age_new1    -0.0989310 0.04904546 2718 -2.01713  0.0438
    Age_new2    -0.4082593 0.05266002 2718 -7.75274  0.0000
    Age_new3    -0.7247089 0.07436072 2718 -9.74586  0.0000
     Correlation: 
             (Intr) Ag_nw1 Ag_nw2
    Age_new1 -0.482              
    Age_new2 -0.450  0.704       
    Age_new3 -0.324  0.485  0.483
    
    Standardized Within-Group Residuals:
            Min          Q1         Med          Q3         Max 
    -2.86808221 -0.54363266 -0.04838165  0.51193755  2.98240561 
    
    Number of Observations: 4155
    Number of Groups: 
            Country ID %in% Country 
                 26            2747 

> Model NAA3 is best, with random IC for ID and Country.

*QQ plot of residuals*

``` r
par(mfrow = c(1,2))
lims <- c(-3.5,3.5)
hist(resid(model_NAA3, type = "normalized"),
freq = FALSE, xlim = lims, ylim =  c(0,.7),main = "Histogram of Standardized Residuals")
lines(density(scale(resid(model_NAA3))))
qqnorm(resid(model_NAA3, type = "normalized"),
xlim = lims, ylim = lims,main = "QQ plot")
abline(0,1, col = "red", lty = 2)
```

![](Second-analysis-NAA-Phase-1_Age-only_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

*Residuals vs fitted*

``` r
plot(fitted(model_NAA3, level=2), residuals(model_NAA3, level=2), 
     main="residuals vs fitted at ID level")
abline(a=0, b=0,col="red")
```

![](Second-analysis-NAA-Phase-1_Age-only_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
plot(fitted(model_NAA3, level=1), residuals(model_NAA3, level=1), 
    main="residuals vs fitted at Country level")
abline(a=0, b=0,col="red")
```

![](Second-analysis-NAA-Phase-1_Age-only_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->

> Residuen zien er allemaal goed uit

*Plot random intercepts and slopes*

``` r
plot(ranef(model_NAA3, level = 1))
```

![](Second-analysis-NAA-Phase-1_Age-only_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
plot(ranef(model_NAA3, level = 2))
```

![](Second-analysis-NAA-Phase-1_Age-only_files/figure-gfm/unnamed-chunk-11-2.png)<!-- -->

*Confidence intervals*

``` r
intervals(model_NAA3)
```

    Approximate 95% confidence intervals
    
     Fixed effects:
                     lower        est.        upper
    (Intercept)  2.6243820  2.77212198  2.919861921
    Age_new1    -0.1951012 -0.09893103 -0.002760864
    Age_new2    -0.5115170 -0.40825928 -0.305001555
    Age_new3    -0.8705181 -0.72470888 -0.578899618
    attr(,"label")
    [1] "Fixed effects:"
    
     Random Effects:
      Level: Country 
                       lower      est.     upper
    sd((Intercept)) 0.176626 0.2612806 0.3865092
      Level: ID 
                        lower      est.     upper
    sd((Intercept)) 0.6646964 0.6945817 0.7258106
    
     Within-group standard error:
        lower      est.     upper 
    0.5673554 0.5879342 0.6092593 

*Plot of predicted values*

``` r
ef_NAA <- effect("Age_new", model_NAA3)

plot_NAA <- ggplot(as.data.frame(ef_NAA), 
       aes(Age_new, fit, color=Age_new)) + geom_line() + 
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=1) + theme_bw(base_size=12) + scale_color_discrete(name="Age", labels = c("18-24", "25-44", "45-64", "65+")) + expand_limits(y=c(1, 5))
```

``` r
plot_NAA
```

![](Second-analysis-NAA-Phase-1_Age-only_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

*Effect sizes* **Within person SD and average within person SD of NAA**

``` r
ISDs <- data_analyse2_p1 %>% 
  group_by(ID) %>%
  summarize_at(c("NAA"), sd, na.rm=TRUE) %>%
  ungroup()

ISDs_av <- ISDs %>%
  summarize_at(c("NAA"), mean, na.rm=TRUE) %>%
  stack() %>%
  rename(sd=values) 
```

> Effect sizes for intercept and main effect of age = regression
> coefficient / average ISD of NAA Effect size for main effect of
> DaysMax = (regression coefficient \* 28)/ average ISD of NAA Effect
> sizes for interaction effects = (regression coefficient \* 28)/
> average ISD of NAA

> The effect sizes for main effect of DaysMax and the interaction
> effects reflect the increase in SD of NAA over 4 weeks (28 days)

``` r
coef_NAA = tidy(model_NAA3, 
               effects = "fixed")

coef_NAA <- coef_NAA %>%
  mutate(e_size = estimate/0.4673061) %>%
  mutate(across(2:7, round, 4))
```

``` r
coef_NAA
```

    ## # A tibble: 4 x 7
    ##   term        estimate std.error    df statistic p.value e_size
    ##   <chr>          <dbl>     <dbl> <dbl>     <dbl>   <dbl>  <dbl>
    ## 1 (Intercept)   2.77      0.0753  2718     36.8   0       5.93 
    ## 2 Age_new1     -0.0989    0.049   2718     -2.02  0.0438 -0.212
    ## 3 Age_new2     -0.408     0.0527  2718     -7.75  0      -0.874
    ## 4 Age_new3     -0.725     0.0744  2718     -9.75  0      -1.55

> Older age groups report lower NAA compared with the youngest age
> group.
