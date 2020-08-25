Data prep averaging emotion items into factors
================
Anne Margit
8/25/2020

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
library(tidyverse)
```

    ## ── Attaching packages ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.1     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.1     ✓ stringr 1.4.0
    ## ✓ tidyr   1.1.0     ✓ forcats 0.5.0
    ## ✓ readr   1.3.1

    ## ── Conflicts ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(knitr)
library(papaja)
```

``` r
load("data_imputed.Rdata")
```

Create variable negative affect high arousal (Negative Affect Activated
= NAA)

``` r
data_emomeans <- data_imputed %>%
  mutate(NAA = (Ang + Anxiety + Nerv) / 3)
```

Create variable negative affect low arousal (Negative Affect Deactivated
= NAD)

``` r
data_emomeans2 <- data_emomeans %>%
  mutate(NAD = (Depr + Exh) / 2)
```

Create variable positive affect high arousal (PAA)

``` r
data_emomeans3 <- data_emomeans2 %>%
  mutate(PAA = (Energ + Insp) / 2)
```

Create variable positive affect low arousal (PAD)

``` r
data_emomeans4 <- data_emomeans3 %>%
  mutate(PAD = (Calm + Rel) / 2) 
```

``` r
data_imputed_emomeans <- data_emomeans4
```

``` r
emomeansN <- data_imputed_emomeans %>%
  group_by(Time) %>%
  summarise(across(NAA:PAD, .fns=list(Mean = mean, SD = sd, Median = median), na.rm=TRUE,
                   .names="{col}_{fn}"))
```

``` r
apa_table(emomeansN, caption="Mean scores of averaged emotion scores per wave")
```

<caption>

(\#tab:unnamed-chunk-9)

</caption>

<div data-custom-style="Table Caption">

*Mean scores of averaged emotion scores per
wave*

</div>

| Time | NAA\_Mean | NAA\_SD | NAA\_Median | NAD\_Mean | NAD\_SD | NAD\_Median | PAA\_Mean | PAA\_SD | PAA\_Median | PAD\_Mean | PAD\_SD | PAD\_Median |
| :--- | :-------- | :------ | :---------- | :-------- | :------ | :---------- | :-------- | :------ | :---------- | :-------- | :------ | :---------- |
| 1    | 2.43      | 0.95    | 2.33        | 2.24      | 1.01    | 2.00        | 2.41      | 0.96    | 2.50        | 2.81      | 1.01    | 3.00        |
| 2    | 2.36      | 0.95    | 2.33        | 2.16      | 0.95    | 2.00        | 2.26      | 0.92    | 2.00        | 2.78      | 0.97    | 3.00        |
| 3    | 2.30      | 0.94    | 2.00        | 2.26      | 1.00    | 2.00        | 2.47      | 0.94    | 2.50        | 2.94      | 0.95    | 3.00        |
| 4    | 2.28      | 0.96    | 2.00        | 2.29      | 1.00    | 2.00        | 2.50      | 0.94    | 2.50        | 2.97      | 0.96    | 3.00        |
| 5    | 2.19      | 0.96    | 2.00        | 2.17      | 1.01    | 2.00        | 2.54      | 0.96    | 2.50        | 3.00      | 0.99    | 3.00        |
| 6    | 2.18      | 0.98    | 2.00        | 2.16      | 1.01    | 2.00        | 2.59      | 0.98    | 2.50        | 3.01      | 0.99    | 3.00        |
| 7    | 2.15      | 0.97    | 2.00        | 2.14      | 1.01    | 2.00        | 2.61      | 0.99    | 2.50        | 3.02      | 0.99    | 3.00        |
| 8    | 2.13      | 0.98    | 2.00        | 2.12      | 1.01    | 2.00        | 2.65      | 0.99    | 2.50        | 3.04      | 1.01    | 3.00        |
| 9    | 2.09      | 0.98    | 2.00        | 2.12      | 1.01    | 2.00        | 2.65      | 0.99    | 2.50        | 3.05      | 1.01    | 3.00        |
| 10   | 2.10      | 0.97    | 2.00        | 2.11      | 1.01    | 2.00        | 2.68      | 1.00    | 2.50        | 3.04      | 1.00    | 3.00        |
| 11   | 2.06      | 0.98    | 2.00        | 2.08      | 1.01    | 2.00        | 2.68      | 0.99    | 2.50        | 3.03      | 1.02    | 3.00        |
| 12   | 2.05      | 0.97    | 2.00        | 2.09      | 1.00    | 2.00        | 2.73      | 1.00    | 3.00        | 3.06      | 1.00    | 3.00        |

``` r
save(data_imputed_emomeans, file="data_imputed_emomeans.Rdata")
```
