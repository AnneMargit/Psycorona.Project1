First analyses winning models
================
Anne Margit
12/03/2020

    ## [1] ""

``` r
load("data_analyse1_fc.Rdata")
```

``` r
library(dplyr)
library(tidyverse)
library(papaja)
library(ggpubr)
library(ggplot2)
library(rockchalk)
library(effects)
library(nlme)
library(lattice)
library(broom)
library(purrr)
library(stargazer)
```

    ## Warning: package 'stargazer' was built under R version 4.0.3

**Negative affect high arousal**

``` r
data_analyse1_fc <- data_analyse1_fc[with(data_analyse1_fc, order(Country, ID, Time)),]
data_analyse1_fc$Time <- as.numeric(data_analyse1_fc$Time)

model_NAA10 <- lme(fixed = NAA ~ StringencyIndex_dev + Str_dummy +  StringencyIndex_dev*Str_dummy,
                   random = list (Country = pdDiag(~StringencyIndex_dev), ID = pdDiag(~StringencyIndex_dev)),
                  data = data_analyse1_fc, 
                  na.action = na.omit,
                  correlation = corAR1(form = ~ Time | Country/ID))

summary(model_NAA10)
```

    Linear mixed-effects model fit by REML
     Data: data_analyse1_fc 
           AIC      BIC    logLik
      122122.7 122230.4 -61049.33
    
    Random effects:
     Formula: ~StringencyIndex_dev | Country
     Structure: Diagonal
            (Intercept) StringencyIndex_dev
    StdDev:    0.219093           0.0018857
    
     Formula: ~StringencyIndex_dev | ID %in% Country
     Structure: Diagonal
            (Intercept) StringencyIndex_dev  Residual
    StdDev:   0.7310759          0.00666666 0.5943753
    
    Correlation Structure: ARMA(1,0)
     Formula: ~Time | Country/ID 
     Parameter estimate(s):
         Phi1 
    0.3381581 
    Fixed effects: NAA ~ StringencyIndex_dev + Str_dummy + StringencyIndex_dev *      Str_dummy 
                                        Value  Std.Error    DF   t-value p-value
    (Intercept)                     2.4678709 0.04170346 48257  59.17664    0.00
    StringencyIndex_dev            -0.0037733 0.00162227 48257  -2.32594    0.02
    Str_dummy1                     -0.2077604 0.01766026 48257 -11.76429    0.00
    Str_dummy2                     -0.2142938 0.01357030 48257 -15.79138    0.00
    StringencyIndex_dev:Str_dummy1  0.0125782 0.00210399 48257   5.97828    0.00
    StringencyIndex_dev:Str_dummy2  0.0071435 0.00169938 48257   4.20357    0.00
     Correlation: 
                                   (Intr) StrnI_ Str_d1 Str_d2 SI_:S_1
    StringencyIndex_dev             0.067                             
    Str_dummy1                     -0.175 -0.308                      
    Str_dummy2                     -0.252 -0.189  0.495               
    StringencyIndex_dev:Str_dummy1 -0.059 -0.568 -0.332  0.257        
    StringencyIndex_dev:Str_dummy2 -0.065 -0.906  0.231  0.247  0.636 
    
    Standardized Within-Group Residuals:
           Min         Q1        Med         Q3        Max 
    -4.9238783 -0.5660415 -0.1338120  0.5004555  5.0031833 
    
    Number of Observations: 58719
    Number of Groups: 
            Country ID %in% Country 
                 33           10457 

> Random ICs and slopes for country and ID, no correlation between ICS
> and slopes, AR structure

**Negative affect low arousal**

``` r
data_analyse1_fc <- data_analyse1_fc[with(data_analyse1_fc, order(Country, ID, Time)),]
data_analyse1_fc$Time <- as.numeric(data_analyse1_fc$Time)

model_NAD10 <- lme(fixed = NAD ~ StringencyIndex_dev + Str_dummy +  StringencyIndex_dev*Str_dummy,
                   random = list (Country = pdDiag(~StringencyIndex_dev), ID = pdDiag(~StringencyIndex_dev)),
                  data = data_analyse1_fc, 
                  na.action = na.omit,
                  correlation = corAR1(form = ~ Time | Country/ID))

summary(model_NAD10)
```

    Linear mixed-effects model fit by REML
     Data: data_analyse1_fc 
         AIC      BIC    logLik
      130514 130621.7 -65244.98
    
    Random effects:
     Formula: ~StringencyIndex_dev | Country
     Structure: Diagonal
            (Intercept) StringencyIndex_dev
    StdDev:   0.2116749         0.004059112
    
     Formula: ~StringencyIndex_dev | ID %in% Country
     Structure: Diagonal
            (Intercept) StringencyIndex_dev  Residual
    StdDev:   0.7734115         0.008690452 0.6284581
    
    Correlation Structure: ARMA(1,0)
     Formula: ~Time | Country/ID 
     Parameter estimate(s):
        Phi1 
    0.287679 
    Fixed effects: NAD ~ StringencyIndex_dev + Str_dummy + StringencyIndex_dev *      Str_dummy 
                                        Value  Std.Error    DF  t-value p-value
    (Intercept)                     2.2375002 0.04095914 48257 54.62762  0.0000
    StringencyIndex_dev             0.0027233 0.00198099 48257  1.37473  0.1692
    Str_dummy1                     -0.0063811 0.02042530 48257 -0.31241  0.7547
    Str_dummy2                      0.0072746 0.01456295 48257  0.49953  0.6174
    StringencyIndex_dev:Str_dummy1  0.0009454 0.00245675 48257  0.38482  0.7004
    StringencyIndex_dev:Str_dummy2 -0.0022820 0.00198032 48257 -1.15235  0.2492
     Correlation: 
                                   (Intr) StrnI_ Str_d1 Str_d2 SI_:S_1
    StringencyIndex_dev             0.064                             
    Str_dummy1                     -0.190 -0.297                      
    Str_dummy2                     -0.276 -0.156  0.461               
    StringencyIndex_dev:Str_dummy1 -0.047 -0.502 -0.390  0.223        
    StringencyIndex_dev:Str_dummy2 -0.061 -0.841  0.180  0.218  0.657 
    
    Standardized Within-Group Residuals:
           Min         Q1        Med         Q3        Max 
    -4.9698654 -0.5531552 -0.1427467  0.5040818  4.9728334 
    
    Number of Observations: 58719
    Number of Groups: 
            Country ID %in% Country 
                 33           10457 

> This model has random slopes for Stringency at the ID and Country
> level, assumes no correlation between random slopes and intercepts,
> and assumes autoregressive correlation structure at the measurement
> level.

**Positive affect high arousal**

``` r
data_analyse1_fc <- data_analyse1_fc[with(data_analyse1_fc, order(Country, ID, Time)),]
data_analyse1_fc$Time <- as.numeric(data_analyse1_fc$Time)

model_PAA10 <- lme(fixed = PAA ~ StringencyIndex_dev + Str_dummy +  StringencyIndex_dev*Str_dummy,
                   random = list (Country = pdDiag(~StringencyIndex_dev), ID = pdDiag(~StringencyIndex_dev)),
                  data = data_analyse1_fc, 
                  na.action = na.omit,
                  correlation = corAR1(form = ~ Time | Country/ID))

summary(model_PAA10)
```

    Linear mixed-effects model fit by REML
     Data: data_analyse1_fc 
           AIC      BIC    logLik
      128654.8 128762.6 -64315.41
    
    Random effects:
     Formula: ~StringencyIndex_dev | Country
     Structure: Diagonal
            (Intercept) StringencyIndex_dev
    StdDev:   0.2357815         0.004752035
    
     Formula: ~StringencyIndex_dev | ID %in% Country
     Structure: Diagonal
            (Intercept) StringencyIndex_dev  Residual
    StdDev:   0.6937351         0.007834257 0.6266687
    
    Correlation Structure: ARMA(1,0)
     Formula: ~Time | Country/ID 
     Parameter estimate(s):
         Phi1 
    0.2730126 
    Fixed effects: PAA ~ StringencyIndex_dev + Str_dummy + StringencyIndex_dev *      Str_dummy 
                                        Value  Std.Error    DF  t-value p-value
    (Intercept)                     2.4507808 0.04451137 48257 55.05966   0e+00
    StringencyIndex_dev             0.0082810 0.00204576 48257  4.04788   1e-04
    Str_dummy1                      0.1499298 0.02084464 48257  7.19273   0e+00
    Str_dummy2                      0.2217428 0.01452230 48257 15.26913   0e+00
    StringencyIndex_dev:Str_dummy1 -0.0152654 0.00250151 48257 -6.10249   0e+00
    StringencyIndex_dev:Str_dummy2 -0.0139779 0.00200346 48257 -6.97689   0e+00
     Correlation: 
                                   (Intr) StrnI_ Str_d1 Str_d2 SI_:S_1
    StringencyIndex_dev             0.057                             
    Str_dummy1                     -0.174 -0.289                      
    Str_dummy2                     -0.254 -0.149  0.455               
    StringencyIndex_dev:Str_dummy1 -0.038 -0.479 -0.411  0.212        
    StringencyIndex_dev:Str_dummy2 -0.053 -0.818  0.163  0.210  0.660 
    
    Standardized Within-Group Residuals:
             Min           Q1          Med           Q3          Max 
    -5.114790432 -0.570915193  0.002241026  0.578437865  4.778311930 
    
    Number of Observations: 58719
    Number of Groups: 
            Country ID %in% Country 
                 33           10457 

> This model has random slopes for Stringency at the ID and Country
> level, assumes no correlation between random slopes and intercepts,
> and assumes autoregressive correlation structure at the measurement
> level.

**Positive affect low arousal**

``` r
data_analyse1_fc <- data_analyse1_fc[with(data_analyse1_fc, order(Country, ID, Time)),]
data_analyse1_fc$Time <- as.numeric(data_analyse1_fc$Time)

model_PAD10 <- lme(fixed = PAD ~ StringencyIndex_dev + Str_dummy +  StringencyIndex_dev*Str_dummy,
                   random = list (Country = pdDiag(~StringencyIndex_dev), ID = pdDiag(~StringencyIndex_dev)),
                  data = data_analyse1_fc, 
                  na.action = na.omit,
                  correlation = corAR1(form = ~ Time | Country/ID))

summary(model_PAD10)
```

    Linear mixed-effects model fit by REML
     Data: data_analyse1_fc 
           AIC      BIC   logLik
      132137.4 132245.2 -66056.7
    
    Random effects:
     Formula: ~StringencyIndex_dev | Country
     Structure: Diagonal
            (Intercept) StringencyIndex_dev
    StdDev:   0.2045257          0.00232971
    
     Formula: ~StringencyIndex_dev | ID %in% Country
     Structure: Diagonal
            (Intercept) StringencyIndex_dev  Residual
    StdDev:   0.7246956         0.008764282 0.6407487
    
    Correlation Structure: ARMA(1,0)
     Formula: ~Time | Country/ID 
     Parameter estimate(s):
       Phi1 
    0.25693 
    Fixed effects: PAD ~ StringencyIndex_dev + Str_dummy + StringencyIndex_dev *      Str_dummy 
                                        Value  Std.Error    DF  t-value p-value
    (Intercept)                     2.8277745 0.03964000 48257 71.33640  0.0000
    StringencyIndex_dev             0.0046247 0.00180771 48257  2.55831  0.0105
    Str_dummy1                      0.1661866 0.01941400 48257  8.56015  0.0000
    Str_dummy2                      0.1633678 0.01462096 48257 11.17354  0.0000
    StringencyIndex_dev:Str_dummy1 -0.0104280 0.00232046 48257 -4.49396  0.0000
    StringencyIndex_dev:Str_dummy2 -0.0050879 0.00188284 48257 -2.70226  0.0069
     Correlation: 
                                   (Intr) StrnI_ Str_d1 Str_d2 SI_:S_1
    StringencyIndex_dev             0.074                             
    Str_dummy1                     -0.200 -0.307                      
    Str_dummy2                     -0.287 -0.184  0.489               
    StringencyIndex_dev:Str_dummy1 -0.063 -0.561 -0.341  0.250        
    StringencyIndex_dev:Str_dummy2 -0.073 -0.897  0.221  0.244  0.643 
    
    Standardized Within-Group Residuals:
            Min          Q1         Med          Q3         Max 
    -5.16979793 -0.55326273  0.06289692  0.58358609  4.25539186 
    
    Number of Observations: 58719
    Number of Groups: 
            Country ID %in% Country 
                 33           10457 

> This model has random slopes for Stringency at the ID and Country
> level, assumes no correlation between random slopes and intercepts,
> and assumes autoregressive correlation structure at the measurement
> level.

stargazer(model\_NAA10, model\_NAD10, model\_PAA10, model\_PAD10,
type=“html”, out=“star\_NAA10.doc”, single.row=TRUE, digits = 2, align
= TRUE, order = c(“Intercept”,“SI”, “Max SI (during)”, “Max SI (after)”,
“SI x Max SI (during)”, “SI x Max SI (after)”), covariate.labels =
c(“Intercept”,“SI”, “Max SI (during)”, “Max SI (after)”, “SI x Max SI
(during)”, “SI x Max SI (after)”))
