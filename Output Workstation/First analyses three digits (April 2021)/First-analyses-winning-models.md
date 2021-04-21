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

``` r
VarCorr(model_NAA10)
```

``` 
                    Variance                    StdDev    
Country =           pdDiag(StringencyIndex_dev)           
(Intercept)         0.048001738121              0.21909299
StringencyIndex_dev 0.000003555864              0.00188570
ID =                pdDiag(StringencyIndex_dev)           
(Intercept)         0.534471938641              0.73107588
StringencyIndex_dev 0.000044444358              0.00666666
Residual            0.353281976781              0.59437528
```

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

``` r
VarCorr(model_NAD10)
```

``` 
                    Variance                    StdDev     
Country =           pdDiag(StringencyIndex_dev)            
(Intercept)         0.04480625857               0.211674889
StringencyIndex_dev 0.00001647639               0.004059112
ID =                pdDiag(StringencyIndex_dev)            
(Intercept)         0.59816530411               0.773411471
StringencyIndex_dev 0.00007552396               0.008690452
Residual            0.39495957281               0.628458092
```

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
    (Intercept)                     2.4507808 0.04451137 48257 55.05966  0.0000
    StringencyIndex_dev             0.0082810 0.00204576 48257  4.04788  0.0001
    Str_dummy1                      0.1499298 0.02084464 48257  7.19273  0.0000
    Str_dummy2                      0.2217428 0.01452230 48257 15.26913  0.0000
    StringencyIndex_dev:Str_dummy1 -0.0152654 0.00250151 48257 -6.10249  0.0000
    StringencyIndex_dev:Str_dummy2 -0.0139779 0.00200346 48257 -6.97689  0.0000
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

``` r
VarCorr(model_PAA10)
```

``` 
                    Variance                    StdDev     
Country =           pdDiag(StringencyIndex_dev)            
(Intercept)         0.05559292611               0.235781522
StringencyIndex_dev 0.00002258183               0.004752035
ID =                pdDiag(StringencyIndex_dev)            
(Intercept)         0.48126843083               0.693735130
StringencyIndex_dev 0.00006137559               0.007834257
Residual            0.39271362508               0.626668672
```

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

``` r
VarCorr(model_PAD10)
```

``` 
                    Variance                    StdDev     
Country =           pdDiag(StringencyIndex_dev)            
(Intercept)         0.041830764285              0.204525706
StringencyIndex_dev 0.000005427551              0.002329710
ID =                pdDiag(StringencyIndex_dev)            
(Intercept)         0.525183642908              0.724695552
StringencyIndex_dev 0.000076812639              0.008764282
Residual            0.410558920652              0.640748719
```

> This model has random slopes for Stringency at the ID and Country
> level, assumes no correlation between random slopes and intercepts,
> and assumes autoregressive correlation structure at the measurement
> level.

``` r
stargazer(model_NAA10, model_NAD10, model_PAA10, model_PAD10,
type="html", df = TRUE, out="star_firstanalyses.doc",  single.row=TRUE, digits = 3, align = TRUE, 
order = c("Intercept","SI", "Max SI (during)", "Max SI (after)", "SI x Max SI (during)", "SI x Max SI (after)"), 
covariate.labels = c("Intercept","SI", "Max SI (during)", "Max SI (after)", 
"SI x Max SI (during)", "SI x Max SI (after)"))
```

``` 

<table style="text-align:center"><tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td colspan="4"><em>Dependent variable:</em></td></tr>
<tr><td></td><td colspan="4" style="border-bottom: 1px solid black"></td></tr>
<tr><td style="text-align:left"></td><td>NAA</td><td>NAD</td><td>PAA</td><td>PAD</td></tr>
<tr><td style="text-align:left"></td><td>(1)</td><td>(2)</td><td>(3)</td><td>(4)</td></tr>
<tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Intercept</td><td>-0.004<sup>**</sup> (0.002)</td><td>0.003 (0.002)</td><td>0.008<sup>***</sup> (0.002)</td><td>0.005<sup>**</sup> (0.002)</td></tr>
<tr><td style="text-align:left">SI</td><td>-0.208<sup>***</sup> (0.018)</td><td>-0.006 (0.020)</td><td>0.150<sup>***</sup> (0.021)</td><td>0.166<sup>***</sup> (0.019)</td></tr>
<tr><td style="text-align:left">Max SI (during)</td><td>-0.214<sup>***</sup> (0.014)</td><td>0.007 (0.015)</td><td>0.222<sup>***</sup> (0.015)</td><td>0.163<sup>***</sup> (0.015)</td></tr>
<tr><td style="text-align:left">Max SI (after)</td><td>0.013<sup>***</sup> (0.002)</td><td>0.001 (0.002)</td><td>-0.015<sup>***</sup> (0.003)</td><td>-0.010<sup>***</sup> (0.002)</td></tr>
<tr><td style="text-align:left">SI x Max SI (during)</td><td>0.007<sup>***</sup> (0.002)</td><td>-0.002 (0.002)</td><td>-0.014<sup>***</sup> (0.002)</td><td>-0.005<sup>***</sup> (0.002)</td></tr>
<tr><td style="text-align:left">SI x Max SI (after)</td><td>2.468<sup>***</sup> (0.042)</td><td>2.238<sup>***</sup> (0.041)</td><td>2.451<sup>***</sup> (0.045)</td><td>2.828<sup>***</sup> (0.040)</td></tr>
<tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observations</td><td>58,719</td><td>58,719</td><td>58,719</td><td>58,719</td></tr>
<tr><td style="text-align:left">Log Likelihood</td><td>-61,049.330</td><td>-65,244.980</td><td>-64,315.410</td><td>-66,056.700</td></tr>
<tr><td style="text-align:left">Akaike Inf. Crit.</td><td>122,122.700</td><td>130,514.000</td><td>128,654.800</td><td>132,137.400</td></tr>
<tr><td style="text-align:left">Bayesian Inf. Crit.</td><td>122,230.400</td><td>130,621.700</td><td>128,762.600</td><td>132,245.200</td></tr>
<tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td colspan="4" style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr>
</table>
```
