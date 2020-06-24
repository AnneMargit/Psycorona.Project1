Within person exploratory factor analysis
================
Anne Margit
6/23/2020

This is the principal component analysis of within-person emotion
scores, using person-mean centered scores.

``` r
load("data_within2.Rdata")
```

install.packages(“GPArotation”)

``` r
library(psych)
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
library(GPArotation)
```

``` r
data_within3 <- data_within2 %>%
  select(-c("ID", "Wave"))
```

    ## Adding missing grouping variables: `ID`

Parallel analysis. Choose the number of factors by simulating a random
data set, and choosing the point where the eigenvalues of the real data
fall below the simulated
data.

``` r
within_cor <- cor(data_within3[,-1], use="complete.obs")
```

``` r
parallel <- fa.parallel(data_within3[,-1], fm="ml", main="Scree plot", fa= "pc", n.iter=50, SMC=TRUE, quant = .95 )
```

![](Within-person-exploratory-factor-analysis_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

    ## Parallel analysis suggests that the number of factors =  NA  and the number of components =  3

``` r
parallel
```

    ## Call: fa.parallel(x = data_within3[, -1], fm = "ml", fa = "pc", main = "Scree plot", 
    ##     n.iter = 50, SMC = TRUE, quant = 0.95)
    ## Parallel analysis suggests that the number of factors =  NA  and the number of components =  3 
    ## 
    ##  Eigen Values of 
    ## 
    ##  eigen values of factors
    ##  [1]  2.17  0.50  0.26  0.01 -0.01 -0.04 -0.12 -0.13 -0.17 -0.18 -0.19
    ## 
    ##  eigen values of simulated factors
    ## [1] NA
    ## 
    ##  eigen values of components 
    ##  [1] 2.95 1.30 1.08 0.90 0.85 0.79 0.75 0.66 0.65 0.54 0.52
    ## 
    ##  eigen values of simulated components
    ##  [1] 1.02 1.01 1.01 1.01 1.00 1.00 1.00 0.99 0.99 0.99 0.98

This suggests 3 within-person
components

``` r
threepca <- principal(data_within3[,-1], nfactors = 3, rotate = "varimax")
threepca
```

    ## Principal Components Analysis
    ## Call: principal(r = data_within3[, -1], nfactors = 3, rotate = "varimax")
    ## Standardized loadings (pattern matrix) based upon correlation matrix
    ##           RC3   RC1   RC2   h2   u2 com
    ## Ang      0.60 -0.09 -0.03 0.37 0.63 1.1
    ## Bored    0.65  0.34 -0.25 0.60 0.40 1.8
    ## Anxiety  0.48 -0.57  0.07 0.56 0.44 2.0
    ## Calm    -0.05  0.67  0.36 0.58 0.42 1.6
    ## Depr     0.62 -0.23 -0.15 0.46 0.54 1.4
    ## Energ   -0.14  0.09  0.72 0.54 0.46 1.1
    ## Exh      0.48 -0.22 -0.07 0.28 0.72 1.4
    ## Insp    -0.10  0.09  0.71 0.52 0.48 1.1
    ## Lov     -0.03  0.07  0.49 0.25 0.75 1.0
    ## Nerv     0.48 -0.59  0.10 0.59 0.41 2.0
    ## Rel     -0.05  0.65  0.40 0.58 0.42 1.7
    ## 
    ##                        RC3  RC1  RC2
    ## SS loadings           1.89 1.79 1.65
    ## Proportion Var        0.17 0.16 0.15
    ## Cumulative Var        0.17 0.33 0.48
    ## Proportion Explained  0.35 0.34 0.31
    ## Cumulative Proportion 0.35 0.69 1.00
    ## 
    ## Mean item complexity =  1.5
    ## Test of the hypothesis that 3 components are sufficient.
    ## 
    ## The root mean square of the residuals (RMSR) is  0.1 
    ##  with the empirical chi square  78636.4  with prob <  0 
    ## 
    ## Fit based upon off diagonal values = 0.77

Component A: Anger, Boredom, Anxiety, Depressed, Exhausted, Nervous

Component B: Calm, Relaxed

Component C: Energetic, Inspired, Loved

Fit of .77, is this high enough?
