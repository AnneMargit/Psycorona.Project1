Factor analysis within-person
================
Anne Margit
6/15/2020

This is the confirmatory factor analysis of within-person emotion
scores, using person-mean centered scores

``` r
load("data_long_min3_str.Rdata")
load("data_means.Rdata")
```

“data\_means” file contains person centered mean scores

``` r
data_long_min3_str <- as_tibble(data_long_min3_str)
data_factor <- data_long_min3_str %>% select(ID, Wave, Ang, Anxiety, Bored, Calm, Content, Depr, Energ, Exc, Exh, Insp, Lov, Nerv, Rel)
```

``` r
data_factor_new <- left_join(data_factor, data_means, by="ID")
```

This dataframe data\_within contains the raw scores - person means
Excited and Content are only measured on baseline, so better to drop
those \`\`

Without excited and content

``` r
data_within2 <- data_factor_new %>%
group_by(ID) %>%
  transmute(Wave = Wave,
    Ang = Ang.x - Ang.y,
         Bored = Bored.x - Bored.y,
         Anxiety = Anxiety.x - Anxiety.y,
         Calm = Calm.x - Calm.y,
         Depr = Depr.x - Depr.y,
         Energ = Energ.x - Energ.y,
         Exh = Exh.x - Exh.y,
         Insp = Insp.x - Insp.y,
         Lov = Lov.x - Lov.y,
         Nerv = Nerv.x - Nerv.y,
         Rel = Rel.x - Rel.y)
```

``` r
save(data_within2, file = "data_within2.Rdata")
```

``` r
round(cor(data_within2[,c("Ang", "Anxiety", "Bored", "Depr","Exh","Nerv","Calm","Energ", "Insp", "Lov","Rel")],use= "pairwise.complete.obs"),2)
```

    ##           Ang Anxiety Bored  Depr   Exh  Nerv  Calm Energ  Insp   Lov   Rel
    ## Ang      1.00    0.22  0.18  0.26  0.17  0.24 -0.16 -0.09 -0.09 -0.08 -0.16
    ## Anxiety  0.22    1.00  0.13  0.33  0.22  0.47 -0.28 -0.13 -0.12 -0.06 -0.27
    ## Bored    0.18    0.13  1.00  0.19  0.11  0.12 -0.06 -0.14 -0.13 -0.05 -0.07
    ## Depr     0.26    0.33  0.19  1.00  0.27  0.32 -0.22 -0.19 -0.18 -0.12 -0.21
    ## Exh      0.17    0.22  0.11  0.27  1.00  0.23 -0.16 -0.15 -0.11 -0.05 -0.19
    ## Nerv     0.24    0.47  0.12  0.32  0.23  1.00 -0.28 -0.11 -0.11 -0.07 -0.27
    ## Calm    -0.16   -0.28 -0.06 -0.22 -0.16 -0.28  1.00  0.24  0.23  0.15  0.46
    ## Energ   -0.09   -0.13 -0.14 -0.19 -0.15 -0.11  0.24  1.00  0.35  0.17  0.26
    ## Insp    -0.09   -0.12 -0.13 -0.18 -0.11 -0.11  0.23  0.35  1.00  0.15  0.25
    ## Lov     -0.08   -0.06 -0.05 -0.12 -0.05 -0.07  0.15  0.17  0.15  1.00  0.16
    ## Rel     -0.16   -0.27 -0.07 -0.21 -0.19 -0.27  0.46  0.26  0.25  0.16  1.00

``` r
corrplot(cor(data_within2[,c("Ang", "Anxiety", "Bored", "Depr","Exh","Nerv","Calm","Energ", "Insp", "Lov","Rel")], use="pairwise.complete.obs"), order = "original", tl.col='black', tl.cex=.75)
```

![](Within-person-factor-analysis_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
model <- '
f1 =~ Ang + Anxiety + Bored + Depr + Exh + Nerv 
f2 =~ Calm + Energ + Insp + Lov + Rel 

# latent variable variances
f1 ~~ 1*f1
f2 ~~ 1*f2

# latent variable covariances
f1 ~~ f2

# manifest variable variances (uniquenesses)
  Ang ~~ Ang
  Anxiety ~~ Anxiety
  Bored ~~ Bored
  Depr ~~ Depr
  Exh ~~ Exh
  Nerv ~~ Nerv
  Calm ~~ Calm
  Energ ~~ Energ
  Insp ~~ Insp
  Lov ~~ Lov
  Rel ~~ Rel

#manifest variable means 
  Ang ~ 1
  Anxiety ~ 1
  Bored ~ 1
  Depr ~ 1
  Exh ~ 1
  Nerv ~ 1
  Calm ~ 1
  Energ ~ 1
  Insp ~ 1
  Lov ~ 1
  Rel ~ 1
'
```

``` r
fit <- cfa(model, data = data_within2, std.lv=TRUE, missing="fiml")
```

``` r
summary(fit, standardized=TRUE, fit.measures=TRUE)
```

    ## lavaan 0.6-6 ended normally after 26 iterations
    ## 
    ##   Estimator                                         ML
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                         34
    ##                                                       
    ##                                                   Used       Total
    ##   Number of observations                         39118       71672
    ##   Number of missing patterns                       164            
    ##                                                                   
    ## Model Test User Model:
    ##                                                       
    ##   Test statistic                              4786.458
    ##   Degrees of freedom                                43
    ##   P-value (Chi-square)                           0.000
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                             54666.130
    ##   Degrees of freedom                                55
    ##   P-value                                        0.000
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.913
    ##   Tucker-Lewis Index (TLI)                       0.889
    ## 
    ## Loglikelihood and Information Criteria:
    ## 
    ##   Loglikelihood user model (H0)            -358496.541
    ##   Loglikelihood unrestricted model (H1)    -356103.312
    ##                                                       
    ##   Akaike (AIC)                              717061.082
    ##   Bayesian (BIC)                            717352.609
    ##   Sample-size adjusted Bayesian (BIC)       717244.557
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.053
    ##   90 Percent confidence interval - lower         0.052
    ##   90 Percent confidence interval - upper         0.054
    ##   P-value RMSEA <= 0.05                          0.000
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.036
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                             Standard
    ##   Information                                 Observed
    ##   Observed information based on                Hessian
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   f1 =~                                                                 
    ##     Ang               0.263    0.004   61.327    0.000    0.263    0.424
    ##     Anxiety           0.438    0.004  118.063    0.000    0.438    0.651
    ##     Bored             0.146    0.005   31.861    0.000    0.146    0.231
    ##     Depr              0.339    0.004   95.283    0.000    0.339    0.541
    ##     Exh               0.279    0.004   68.118    0.000    0.279    0.395
    ##     Nerv              0.438    0.004  117.873    0.000    0.438    0.651
    ##   f2 =~                                                                 
    ##     Calm              0.409    0.004  114.520    0.000    0.409    0.650
    ##     Energ             0.274    0.004   72.642    0.000    0.274    0.432
    ##     Insp              0.278    0.004   70.168    0.000    0.278    0.418
    ##     Lov               0.137    0.004   38.671    0.000    0.137    0.276
    ##     Rel               0.432    0.004  117.359    0.000    0.432    0.666
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   f1 ~~                                                                 
    ##     f2               -0.611    0.006 -104.796    0.000   -0.611   -0.611
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .Ang               0.022    0.004    6.317    0.000    0.022    0.036
    ##    .Anxiety           0.000    0.003    0.037    0.971    0.000    0.000
    ##    .Bored            -0.006    0.004   -1.496    0.135   -0.006   -0.010
    ##    .Depr              0.000    0.003    0.065    0.948    0.000    0.000
    ##    .Exh               0.000    0.004    0.069    0.945    0.000    0.000
    ##    .Nerv              0.000    0.003    0.074    0.941    0.000    0.000
    ##    .Calm             -0.000    0.003   -0.065    0.948   -0.000   -0.000
    ##    .Energ            -0.000    0.003   -0.039    0.969   -0.000   -0.000
    ##    .Insp             -0.000    0.003   -0.052    0.959   -0.000   -0.000
    ##    .Lov              -0.011    0.003   -3.799    0.000   -0.011   -0.022
    ##    .Rel              -0.000    0.003   -0.099    0.921   -0.000   -0.001
    ##     f1                0.000                               0.000    0.000
    ##     f2                0.000                               0.000    0.000
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##     f1                1.000                               1.000    1.000
    ##     f2                1.000                               1.000    1.000
    ##    .Ang               0.315    0.003  111.804    0.000    0.315    0.820
    ##    .Anxiety           0.261    0.003   98.259    0.000    0.261    0.576
    ##    .Bored             0.380    0.004  106.488    0.000    0.380    0.947
    ##    .Depr              0.278    0.002  115.177    0.000    0.278    0.707
    ##    .Exh               0.420    0.003  129.045    0.000    0.420    0.844
    ##    .Nerv              0.261    0.003   98.103    0.000    0.261    0.576
    ##    .Calm              0.228    0.002   94.569    0.000    0.228    0.578
    ##    .Energ             0.326    0.003  124.006    0.000    0.326    0.813
    ##    .Insp              0.364    0.003  125.340    0.000    0.364    0.825
    ##    .Lov               0.228    0.002  118.186    0.000    0.228    0.924
    ##    .Rel               0.234    0.003   90.833    0.000    0.234    0.556

Factor loadings for within-person factor analysis based on person-mean
centered deviation scores

``` r
parameterEstimates(fit, standardized=TRUE) %>% 
  filter(op == "=~") %>% 
  select('Latent Factor'=lhs, Indicator=rhs, B=est, SE=se, Z=z, 'p-value'=pvalue, Beta=std.all) %>% 
  kable(digits = 3, format="pandoc", caption="Factor Loadings")
```

| Latent Factor | Indicator |     B |    SE |       Z | p-value |  Beta |
| :------------ | :-------- | ----: | ----: | ------: | ------: | ----: |
| f1            | Ang       | 0.263 | 0.004 |  61.327 |       0 | 0.424 |
| f1            | Anxiety   | 0.438 | 0.004 | 118.063 |       0 | 0.651 |
| f1            | Bored     | 0.146 | 0.005 |  31.861 |       0 | 0.231 |
| f1            | Depr      | 0.339 | 0.004 |  95.283 |       0 | 0.541 |
| f1            | Exh       | 0.279 | 0.004 |  68.118 |       0 | 0.395 |
| f1            | Nerv      | 0.438 | 0.004 | 117.873 |       0 | 0.651 |
| f2            | Calm      | 0.409 | 0.004 | 114.520 |       0 | 0.650 |
| f2            | Energ     | 0.274 | 0.004 |  72.642 |       0 | 0.432 |
| f2            | Insp      | 0.278 | 0.004 |  70.168 |       0 | 0.418 |
| f2            | Lov       | 0.137 | 0.004 |  38.671 |       0 | 0.276 |
| f2            | Rel       | 0.432 | 0.004 | 117.359 |       0 | 0.666 |

Factor Loadings

With Lavaan multilevel model

``` r
model2 <- '
level: 1
f1 =~ Ang + Anxiety + Bored + Depr + Exh + Nerv 
f2 =~ Calm + Energ + Insp + Lov + Rel 

level: 2
f1 =~ Ang + Anxiety + Bored + Depr + Exh + Nerv 
f2 =~ Calm + Energ + Insp + Lov + Rel 
'
```

``` r
fit2 <- cfa(model2, data = data_long_min3_str, std.lv=TRUE, missing="fiml", cluster="ID")
```

``` r
summary(fit2, standardized=TRUE, fit.measures=TRUE)
```

    ## lavaan 0.6-6 ended normally after 40 iterations
    ## 
    ##   Estimator                                         ML
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                         57
    ##                                                       
    ##                                                   Used       Total
    ##   Number of observations                         14758       71672
    ##   Number of clusters [ID]                         7146            
    ##                                                                   
    ## Model Test User Model:
    ##                                                       
    ##   Test statistic                              5280.215
    ##   Degrees of freedom                                86
    ##   P-value (Chi-square)                           0.000
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                             53083.142
    ##   Degrees of freedom                               110
    ##   P-value                                        0.000
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.902
    ##   Tucker-Lewis Index (TLI)                       0.875
    ## 
    ## Loglikelihood and Information Criteria:
    ## 
    ##   Loglikelihood user model (H0)            -201078.364
    ##   Loglikelihood unrestricted model (H1)    -198438.256
    ##                                                       
    ##   Akaike (AIC)                              402270.728
    ##   Bayesian (BIC)                            402703.902
    ##   Sample-size adjusted Bayesian (BIC)       402522.761
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.064
    ##   90 Percent confidence interval - lower         0.063
    ##   90 Percent confidence interval - upper         0.065
    ##   P-value RMSEA <= 0.05                          0.000
    ## 
    ## Standardized Root Mean Square Residual (corr metric):
    ## 
    ##   SRMR (within covariance matrix)                0.032
    ##   SRMR (between covariance matrix)               0.070
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                             Standard
    ##   Information                                 Observed
    ##   Observed information based on                Hessian
    ## 
    ## 
    ## Level 1 [within]:
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   f1 =~                                                                 
    ##     Ang               0.329    0.009   34.730    0.000    0.329    0.474
    ##     Anxiety           0.388    0.010   38.021    0.000    0.388    0.571
    ##     Bored             0.190    0.011   17.463    0.000    0.190    0.268
    ##     Depr              0.363    0.010   37.644    0.000    0.363    0.560
    ##     Exh               0.289    0.010   28.239    0.000    0.289    0.390
    ##     Nerv              0.384    0.011   35.109    0.000    0.384    0.561
    ##   f2 =~                                                                 
    ##     Calm              0.384    0.011   33.688    0.000    0.384    0.581
    ##     Energ             0.324    0.014   23.077    0.000    0.324    0.482
    ##     Insp              0.317    0.014   21.923    0.000    0.317    0.453
    ##     Lov               0.164    0.008   19.695    0.000    0.164    0.296
    ##     Rel               0.406    0.011   37.623    0.000    0.406    0.594
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   f1 ~~                                                                 
    ##     f2               -0.518    0.016  -33.204    0.000   -0.518   -0.518
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .Ang               0.000                               0.000    0.000
    ##    .Anxiety           0.000                               0.000    0.000
    ##    .Bored             0.000                               0.000    0.000
    ##    .Depr              0.000                               0.000    0.000
    ##    .Exh               0.000                               0.000    0.000
    ##    .Nerv              0.000                               0.000    0.000
    ##    .Calm              0.000                               0.000    0.000
    ##    .Energ             0.000                               0.000    0.000
    ##    .Insp              0.000                               0.000    0.000
    ##    .Lov               0.000                               0.000    0.000
    ##    .Rel               0.000                               0.000    0.000
    ##     f1                0.000                               0.000    0.000
    ##     f2                0.000                               0.000    0.000
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .Ang               0.375    0.007   53.174    0.000    0.375    0.776
    ##    .Anxiety           0.312    0.007   45.471    0.000    0.312    0.674
    ##    .Bored             0.464    0.008   59.173    0.000    0.464    0.928
    ##    .Depr              0.288    0.006   45.913    0.000    0.288    0.686
    ##    .Exh               0.464    0.008   56.896    0.000    0.464    0.848
    ##    .Nerv              0.321    0.007   45.625    0.000    0.321    0.686
    ##    .Calm              0.290    0.007   38.745    0.000    0.290    0.662
    ##    .Energ             0.346    0.008   44.711    0.000    0.346    0.768
    ##    .Insp              0.389    0.008   47.338    0.000    0.389    0.795
    ##    .Lov               0.279    0.005   58.516    0.000    0.279    0.912
    ##    .Rel               0.302    0.008   39.324    0.000    0.302    0.647
    ##     f1                1.000                               1.000    1.000
    ##     f2                1.000                               1.000    1.000
    ## 
    ## 
    ## Level 2 [ID]:
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   f1 =~                                                                 
    ##     Ang               0.657    0.012   54.669    0.000    0.657    0.733
    ##     Anxiety           0.888    0.012   76.308    0.000    0.888    0.920
    ##     Bored             0.479    0.014   33.574    0.000    0.479    0.495
    ##     Depr              0.779    0.011   69.043    0.000    0.779    0.861
    ##     Exh               0.722    0.012   57.787    0.000    0.722    0.770
    ##     Nerv              0.850    0.011   75.741    0.000    0.850    0.938
    ##   f2 =~                                                                 
    ##     Calm              0.797    0.010   78.424    0.000    0.797    0.972
    ##     Energ             0.602    0.013   46.045    0.000    0.602    0.723
    ##     Insp              0.573    0.014   40.777    0.000    0.573    0.661
    ##     Lov               0.447    0.014   31.258    0.000    0.447    0.446
    ##     Rel               0.809    0.010   78.850    0.000    0.809    0.966
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   f1 ~~                                                                 
    ##     f2               -0.759    0.008  -96.018    0.000   -0.759   -0.759
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .Ang               1.997    0.012  163.140    0.000    1.997    2.227
    ##    .Anxiety           2.313    0.013  179.546    0.000    2.313    2.396
    ##    .Bored             2.163    0.013  165.912    0.000    2.163    2.237
    ##    .Depr              1.994    0.012  164.383    0.000    1.994    2.203
    ##    .Exh               2.338    0.013  182.089    0.000    2.338    2.496
    ##    .Nerv              2.193    0.012  178.836    0.000    2.193    2.421
    ##    .Calm              3.107    0.011  275.774    0.000    3.107    3.792
    ##    .Energ             2.698    0.011  234.983    0.000    2.698    3.240
    ##    .Insp              2.531    0.012  211.608    0.000    2.531    2.919
    ##    .Lov               3.423    0.013  266.331    0.000    3.423    3.413
    ##    .Rel               2.921    0.012  252.995    0.000    2.921    3.488
    ##     f1                0.000                               0.000    0.000
    ##     f2                0.000                               0.000    0.000
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .Ang               0.372    0.011   33.887    0.000    0.372    0.463
    ##    .Anxiety           0.143    0.008   18.093    0.000    0.143    0.153
    ##    .Bored             0.707    0.017   41.370    0.000    0.707    0.755
    ##    .Depr              0.212    0.008   25.937    0.000    0.212    0.259
    ##    .Exh               0.357    0.012   30.043    0.000    0.357    0.407
    ##    .Nerv              0.098    0.007   13.714    0.000    0.098    0.119
    ##    .Calm              0.037    0.006    5.892    0.000    0.037    0.055
    ##    .Energ             0.331    0.010   32.919    0.000    0.331    0.477
    ##    .Insp              0.423    0.012   35.587    0.000    0.423    0.563
    ##    .Lov               0.806    0.017   47.972    0.000    0.806    0.801
    ##    .Rel               0.047    0.006    7.552    0.000    0.047    0.067
    ##     f1                1.000                               1.000    1.000
    ##     f2                1.000                               1.000    1.000

``` r
parameterEstimates(fit2, standardized=TRUE) %>% 
  filter(op == "=~") %>% 
  select('Latent Factor'=lhs, Indicator=rhs, B=est, SE=se, Z=z, 'p-value'=pvalue, Beta=std.all) %>% 
  kable(digits = 3, format="pandoc", caption="Factor Loadings")
```

| Latent Factor | Indicator |     B |    SE |      Z | p-value |  Beta |
| :------------ | :-------- | ----: | ----: | -----: | ------: | ----: |
| f1            | Ang       | 0.329 | 0.009 | 34.730 |       0 | 0.474 |
| f1            | Anxiety   | 0.388 | 0.010 | 38.021 |       0 | 0.571 |
| f1            | Bored     | 0.190 | 0.011 | 17.463 |       0 | 0.268 |
| f1            | Depr      | 0.363 | 0.010 | 37.644 |       0 | 0.560 |
| f1            | Exh       | 0.289 | 0.010 | 28.239 |       0 | 0.390 |
| f1            | Nerv      | 0.384 | 0.011 | 35.109 |       0 | 0.561 |
| f2            | Calm      | 0.384 | 0.011 | 33.688 |       0 | 0.581 |
| f2            | Energ     | 0.324 | 0.014 | 23.077 |       0 | 0.482 |
| f2            | Insp      | 0.317 | 0.014 | 21.923 |       0 | 0.453 |
| f2            | Lov       | 0.164 | 0.008 | 19.695 |       0 | 0.296 |
| f2            | Rel       | 0.406 | 0.011 | 37.623 |       0 | 0.594 |
| f1            | Ang       | 0.657 | 0.012 | 54.669 |       0 | 0.733 |
| f1            | Anxiety   | 0.888 | 0.012 | 76.308 |       0 | 0.920 |
| f1            | Bored     | 0.479 | 0.014 | 33.574 |       0 | 0.495 |
| f1            | Depr      | 0.779 | 0.011 | 69.043 |       0 | 0.861 |
| f1            | Exh       | 0.722 | 0.012 | 57.787 |       0 | 0.770 |
| f1            | Nerv      | 0.850 | 0.011 | 75.741 |       0 | 0.938 |
| f2            | Calm      | 0.797 | 0.010 | 78.424 |       0 | 0.972 |
| f2            | Energ     | 0.602 | 0.013 | 46.045 |       0 | 0.723 |
| f2            | Insp      | 0.573 | 0.014 | 40.777 |       0 | 0.661 |
| f2            | Lov       | 0.447 | 0.014 | 31.258 |       0 | 0.446 |
| f2            | Rel       | 0.809 | 0.010 | 78.850 |       0 | 0.966 |

Factor Loadings
