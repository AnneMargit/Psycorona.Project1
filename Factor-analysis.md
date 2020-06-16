Factor analysis between-person
================
Anne Margit
6/8/2020

This is the confirmatory factor analysis of between-person emotion
scores (i.e., mean scores across waves)

``` r
load("data_long_min3.Rdata")
```

``` r
data_long_min3 <- as_tibble(data_long_min3)
data_factor <- data_long_min3 %>% select(X, Wave, Ang, Anxiety, Bored, Calm, Content, Depr, Energ, Exc, Exh, Insp, Lov, Nerv, Rel)
```

``` r
data_means <- data_factor %>%
    dplyr::group_by(X) %>%
    dplyr::summarise_each(funs(mean(., na.rm=TRUE)), 
                          Ang, Anxiety, Bored, Calm, Content, Depr, Energ, Exc, Exh, Insp, Lov, Nerv, Rel)
```

    ## Warning: `summarise_each_()` is deprecated as of dplyr 0.7.0.
    ## Please use `across()` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_warnings()` to see where this warning was generated.

    ## Warning: `funs()` is deprecated as of dplyr 0.8.0.
    ## Please use a list of either functions or lambdas: 
    ## 
    ##   # Simple named list: 
    ##   list(mean = mean, median = median)
    ## 
    ##   # Auto named with `tibble::lst()`: 
    ##   tibble::lst(mean, median)
    ## 
    ##   # Using lambdas
    ##   list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_warnings()` to see where this warning was generated.

``` r
save(data_means, file="data_means.Rdata")
```

``` r
round(cor(data_means[,c("Ang", "Anxiety", "Bored", "Depr","Exh","Nerv","Calm", "Content","Energ","Exc", "Insp", "Lov","Rel")],use= "complete.obs"),2)
```

    ##           Ang Anxiety Bored  Depr   Exh  Nerv  Calm Content Energ   Exc  Insp
    ## Ang      1.00    0.54  0.31  0.58  0.48  0.59 -0.44   -0.24 -0.26  0.00 -0.21
    ## Anxiety  0.54    1.00  0.33  0.69  0.60  0.84 -0.67   -0.37 -0.38 -0.09 -0.34
    ## Bored    0.31    0.33  1.00  0.41  0.28  0.32 -0.24   -0.23 -0.26 -0.12 -0.27
    ## Depr     0.58    0.69  0.41  1.00  0.62  0.70 -0.58   -0.36 -0.43 -0.12 -0.37
    ## Exh      0.48    0.60  0.28  0.62  1.00  0.62 -0.52   -0.27 -0.41 -0.05 -0.28
    ## Nerv     0.59    0.84  0.32  0.70  0.62  1.00 -0.67   -0.34 -0.35 -0.06 -0.30
    ## Calm    -0.44   -0.67 -0.24 -0.58 -0.52 -0.67  1.00    0.50  0.58  0.21  0.53
    ## Content -0.24   -0.37 -0.23 -0.36 -0.27 -0.34  0.50    1.00  0.42  0.32  0.42
    ## Energ   -0.26   -0.38 -0.26 -0.43 -0.41 -0.35  0.58    0.42  1.00  0.33  0.70
    ## Exc      0.00   -0.09 -0.12 -0.12 -0.05 -0.06  0.21    0.32  0.33  1.00  0.40
    ## Insp    -0.21   -0.34 -0.27 -0.37 -0.28 -0.30  0.53    0.42  0.70  0.40  1.00
    ## Lov     -0.20   -0.19 -0.20 -0.33 -0.20 -0.18  0.34    0.27  0.37  0.18  0.37
    ## Rel     -0.41   -0.61 -0.20 -0.53 -0.53 -0.60  0.84    0.50  0.59  0.23  0.54
    ##           Lov   Rel
    ## Ang     -0.20 -0.41
    ## Anxiety -0.19 -0.61
    ## Bored   -0.20 -0.20
    ## Depr    -0.33 -0.53
    ## Exh     -0.20 -0.53
    ## Nerv    -0.18 -0.60
    ## Calm     0.34  0.84
    ## Content  0.27  0.50
    ## Energ    0.37  0.59
    ## Exc      0.18  0.23
    ## Insp     0.37  0.54
    ## Lov      1.00  0.35
    ## Rel      0.35  1.00

``` r
corrplot(cor(data_means[,c("Ang", "Anxiety", "Bored", "Depr","Exh","Nerv","Calm", "Content","Energ","Exc", "Insp", "Lov","Rel")], use="complete.obs"), order = "original", tl.col='black', tl.cex=.75)
```

![](Factor-analysis_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
model <- '
f1 =~ Ang + Anxiety + Bored + Depr + Exh + Nerv 
f2 =~ Calm + Content + Energ + Exc + Insp + Lov + Rel 

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
  Content ~~ Content
  Energ ~~ Energ
  Exc ~~ Exc
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
  Content ~ 1
  Energ ~ 1
  Exc ~ 1
  Insp ~ 1
  Lov ~ 1
  Rel ~ 1
'
```

``` r
fit <- cfa(model, data = data_means, std.lv=TRUE, missing="fiml")
summary(fit, standardized=TRUE, fit.measures=TRUE)
```

    ## lavaan 0.6-6 ended normally after 41 iterations
    ## 
    ##   Estimator                                         ML
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                         40
    ##                                                       
    ##   Number of observations                          9752
    ##   Number of missing patterns                        13
    ##                                                       
    ## Model Test User Model:
    ##                                                       
    ##   Test statistic                              8959.250
    ##   Degrees of freedom                                64
    ##   P-value (Chi-square)                           0.000
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                             72242.926
    ##   Degrees of freedom                                78
    ##   P-value                                        0.000
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.877
    ##   Tucker-Lewis Index (TLI)                       0.850
    ## 
    ## Loglikelihood and Information Criteria:
    ## 
    ##   Loglikelihood user model (H0)            -143435.167
    ##   Loglikelihood unrestricted model (H1)    -138955.542
    ##                                                       
    ##   Akaike (AIC)                              286950.335
    ##   Bayesian (BIC)                            287237.744
    ##   Sample-size adjusted Bayesian (BIC)       287110.630
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.119
    ##   90 Percent confidence interval - lower         0.117
    ##   90 Percent confidence interval - upper         0.121
    ##   P-value RMSEA <= 0.05                          0.000
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.064
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
    ##     Ang               0.619    0.009   69.512    0.000    0.619    0.646
    ##     Anxiety           0.895    0.008  111.154    0.000    0.895    0.893
    ##     Bored             0.430    0.011   39.049    0.000    0.430    0.394
    ##     Depr              0.756    0.008   93.051    0.000    0.756    0.800
    ##     Exh               0.686    0.009   77.597    0.000    0.686    0.703
    ##     Nerv              0.865    0.008  113.437    0.000    0.865    0.904
    ##   f2 =~                                                                 
    ##     Calm              0.782    0.007  114.973    0.000    0.782    0.914
    ##     Content           0.618    0.011   58.617    0.000    0.618    0.565
    ##     Energ             0.582    0.008   73.261    0.000    0.582    0.677
    ##     Exc               0.301    0.011   27.507    0.000    0.301    0.286
    ##     Insp              0.564    0.008   66.901    0.000    0.564    0.632
    ##     Lov               0.417    0.010   40.162    0.000    0.417    0.405
    ##     Rel               0.785    0.007  112.282    0.000    0.785    0.900
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   f1 ~~                                                                 
    ##     f2               -0.750    0.005 -139.381    0.000   -0.750   -0.750
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .Ang               1.999    0.010  206.033    0.000    1.999    2.087
    ##    .Anxiety           2.475    0.010  243.984    0.000    2.475    2.471
    ##    .Bored             2.282    0.011  206.258    0.000    2.282    2.090
    ##    .Depr              2.068    0.010  216.295    0.000    2.068    2.190
    ##    .Exh               2.378    0.010  240.666    0.000    2.378    2.437
    ##    .Nerv              2.344    0.010  241.949    0.000    2.344    2.450
    ##    .Calm              3.028    0.009  349.554    0.000    3.028    3.540
    ##    .Content           2.639    0.011  237.547    0.000    2.639    2.411
    ##    .Energ             2.600    0.009  298.406    0.000    2.600    3.022
    ##    .Exc               1.998    0.011  186.498    0.000    1.998    1.896
    ##    .Insp              2.437    0.009  269.242    0.000    2.437    2.727
    ##    .Lov               3.404    0.010  326.594    0.000    3.404    3.308
    ##    .Rel               2.840    0.009  321.622    0.000    2.840    3.257
    ##     f1                0.000                               0.000    0.000
    ##     f2                0.000                               0.000    0.000
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##     f1                1.000                               1.000    1.000
    ##     f2                1.000                               1.000    1.000
    ##    .Ang               0.534    0.008   65.734    0.000    0.534    0.582
    ##    .Anxiety           0.203    0.004   48.040    0.000    0.203    0.202
    ##    .Bored             1.007    0.015   68.746    0.000    1.007    0.845
    ##    .Depr              0.320    0.005   58.787    0.000    0.320    0.360
    ##    .Exh               0.481    0.008   64.145    0.000    0.481    0.506
    ##    .Nerv              0.166    0.004   44.851    0.000    0.166    0.182
    ##    .Calm              0.121    0.003   39.946    0.000    0.121    0.165
    ##    .Content           0.815    0.012   66.850    0.000    0.815    0.681
    ##    .Energ             0.401    0.006   63.932    0.000    0.401    0.542
    ##    .Exc               1.019    0.015   68.965    0.000    1.019    0.918
    ##    .Insp              0.480    0.007   65.071    0.000    0.480    0.601
    ##    .Lov               0.885    0.013   68.651    0.000    0.885    0.836
    ##    .Rel               0.145    0.003   45.410    0.000    0.145    0.191

``` r
parameterEstimates(fit, standardized=TRUE) %>% 
  filter(op == "=~") %>% 
  select('Latent Factor'=lhs, Indicator=rhs, B=est, SE=se, Z=z, 'p-value'=pvalue, Beta=std.all) %>% 
  kable(digits = 3, format="pandoc", caption="Factor Loadings")
```

| Latent Factor | Indicator |     B |    SE |       Z | p-value |  Beta |
| :------------ | :-------- | ----: | ----: | ------: | ------: | ----: |
| f1            | Ang       | 0.619 | 0.009 |  69.512 |       0 | 0.646 |
| f1            | Anxiety   | 0.895 | 0.008 | 111.154 |       0 | 0.893 |
| f1            | Bored     | 0.430 | 0.011 |  39.049 |       0 | 0.394 |
| f1            | Depr      | 0.756 | 0.008 |  93.051 |       0 | 0.800 |
| f1            | Exh       | 0.686 | 0.009 |  77.597 |       0 | 0.703 |
| f1            | Nerv      | 0.865 | 0.008 | 113.437 |       0 | 0.904 |
| f2            | Calm      | 0.782 | 0.007 | 114.973 |       0 | 0.914 |
| f2            | Content   | 0.618 | 0.011 |  58.617 |       0 | 0.565 |
| f2            | Energ     | 0.582 | 0.008 |  73.261 |       0 | 0.677 |
| f2            | Exc       | 0.301 | 0.011 |  27.507 |       0 | 0.286 |
| f2            | Insp      | 0.564 | 0.008 |  66.901 |       0 | 0.632 |
| f2            | Lov       | 0.417 | 0.010 |  40.162 |       0 | 0.405 |
| f2            | Rel       | 0.785 | 0.007 | 112.282 |       0 | 0.900 |

Factor Loadings
