Descriptives
================
true
5/22/2020

``` r
source("Code/Helper.R")
load("Data/coronadataBSfill.Rdata")
coronadataBSfill <- as.data.frame(coronadataBSfill)
ordered <- emotion.descriptives(coronadataBSfill)
```

    ## Warning: funs() is soft deprecated as of dplyr 0.8.0
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
    ## This warning is displayed once per session.

``` r
head(ordered, 5)
```

    ## # A tibble: 5 x 25
    ##   country affAnx_mean affAnx_sd affBor_mean affBor_sd affCalm_mean affCalm_sd
    ##   <fct>         <dbl>     <dbl>       <dbl>     <dbl>        <dbl>      <dbl>
    ## 1 Argent…        2.83      1.25        2.71      1.33         2.91       1.1 
    ## 2 Austra…        2.49      1.24        2.66      1.33         2.93       1.12
    ## 3 Brazil         3.07      1.3         2.58      1.23         2.64       1.12
    ## 4 Canada         2.76      1.22        2.78      1.3          2.72       1.06
    ## 5 France         2.46      1.18        2.34      1.2          3.07       1.06
    ## # … with 18 more variables: affContent_mean <dbl>, affContent_sd <dbl>,
    ## #   affDepr_mean <dbl>, affDepr_sd <dbl>, affEnerg_mean <dbl>,
    ## #   affEnerg_sd <dbl>, affExc_mean <dbl>, affExc_sd <dbl>, affNerv_mean <dbl>,
    ## #   affNerv_sd <dbl>, affExh_mean <dbl>, affExh_sd <dbl>, affInsp_mean <dbl>,
    ## #   affInsp_sd <dbl>, affRel_mean <dbl>, affRel_sd <dbl>, happy_mean <dbl>,
    ## #   happy_sd <dbl>

``` r
# apa_table(ordered, caption="ordered data")
```

``` r
partial.combine <- function(name) {
  func.template <- "function(row) paste0(row$%s_mean, \" (\", row$%s_sd, \")\")"
  func.def <- sprintf(func.template, name, name)
  func <- eval(parse(text=func.def))
  return(func)
}

combine.m.sd <- function(row) paste0(row$affAnx_mean, " (", row$affAnx_sd, ")")

func <- partial.combine("affAnx")
ordered$affAnx <- ordered %>% (partial.combine("affAnx"))
ordered$affBor <- ordered %>% (partial.combine("affBor"))
ordered$affCalm <- ordered%>% (partial.combine("affCalm"))
ordered$affContent <- ordered%>% (partial.combine("affContent"))
ordered$affDepr <- ordered%>% (partial.combine("affDepr"))
ordered$affEnerg <- ordered%>% (partial.combine("affEnerg"))
ordered$affExc <- ordered%>% (partial.combine("affExc"))
ordered$affNerv <- ordered%>% (partial.combine("affNerv"))
ordered$affExh <- ordered%>% (partial.combine("affExh"))
ordered$affInsp <- ordered%>% (partial.combine("affInsp"))
ordered$affRel <- ordered%>% (partial.combine("affRel"))
ordered$happy <- ordered%>% (partial.combine("happy"))


ordered.small <- ordered %>% dplyr::select(country, affAnx, affBor, affCalm, affContent, affDepr, affEnerg, affExc, affNerv, affExh, affInsp, affRel, happy)

apa_table(ordered.small, caption="ordered data")
```

<caption>

(\#tab:unnamed-chunk-5)

</caption>

<div data-custom-style="Table Caption">

*ordered
data*

</div>

| country        | affAnx      | affBor      | affCalm     | affContent  | affDepr     | affEnerg    | affExc      | affNerv     | affExh      | affInsp     | affRel      | happy       |
| :------------- | :---------- | :---------- | :---------- | :---------- | :---------- | :---------- | :---------- | :---------- | :---------- | :---------- | :---------- | :---------- |
| Argentina      | 2.83 (1.25) | 2.71 (1.33) | 2.91 (1.1)  | 2.59 (1.11) | 2 (1.13)    | 2.22 (1.07) | 2.19 (1.12) | 2.45 (1.21) | 2.29 (1.18) | 2.5 (1.12)  | 2.64 (1.13) | 6.76 (1.97) |
| Australia      | 2.49 (1.24) | 2.66 (1.33) | 2.93 (1.12) | 2.68 (1.13) | 2.16 (1.21) | 2.45 (1.11) | 1.94 (1.04) | 2.25 (1.2)  | 2.42 (1.23) | 2.19 (1.09) | 2.8 (1.15)  | 6.14 (2.16) |
| Brazil         | 3.07 (1.3)  | 2.58 (1.23) | 2.64 (1.12) | 2.5 (1.14)  | 2.26 (1.24) | 2.38 (1.13) | 2.19 (1.18) | 2.65 (1.25) | 2.46 (1.22) | 2.37 (1.18) | 2.42 (1.14) | 6.94 (2.17) |
| Canada         | 2.76 (1.22) | 2.78 (1.3)  | 2.72 (1.06) | 2.49 (1.03) | 2.22 (1.17) | 2.3 (1.01)  | 1.88 (1.01) | 2.61 (1.19) | 2.52 (1.25) | 2.1 (1.08)  | 2.56 (1.07) | 6.1 (2)     |
| France         | 2.46 (1.18) | 2.34 (1.2)  | 3.07 (1.06) | 2.56 (1.09) | 1.99 (1.11) | 2.67 (1.05) | 2.39 (1.07) | 2.29 (1.14) | 2.18 (1.16) | 2.47 (1.08) | 2.74 (1.09) | 6.23 (2.18) |
| Germany        | 2.18 (1.08) | 2.28 (1.22) | 3.06 (1)    | 2.82 (1)    | 2.17 (1.1)  | 2.82 (1.03) | 2.25 (1.03) | 2.15 (1.09) | 2.32 (1.12) | 2.45 (1.06) | 2.88 (1.04) | 6.25 (1.92) |
| Greece         | 2.89 (1.19) | 2.95 (1.26) | 3.15 (1.05) | 2.59 (1.06) | 2.57 (1.19) | 2.91 (1.03) | 2.04 (1.05) | 2.64 (1.18) | 2.27 (1.18) | 2.66 (1.14) | 3.01 (1.09) | 6.31 (1.81) |
| Indonesia      | 2.97 (1.26) | 3.34 (1.35) | 3.12 (1.16) | 2.56 (1.17) | 2.11 (1.17) | 3.03 (1.19) | 2.84 (1.21) | 2.3 (1.16)  | 2.58 (1.22) | 2.97 (1.22) | 3.04 (1.14) | 6.97 (1.87) |
| Italy          | 2.62 (1.24) | 2.72 (1.3)  | 2.64 (1.09) | 2.07 (1.06) | 2.14 (1.16) | 2.31 (1.05) | 1.65 (0.96) | 2.62 (1.21) | 2.37 (1.21) | 2.2 (1.11)  | 2.4 (1.1)   | 5.85 (1.87) |
| Japan          | 3.19 (1.21) | 2.72 (1.22) | 2.9 (1.02)  | 2.34 (1.03) | 2.28 (1.11) | 2.65 (1.02) | 1.85 (0.98) | 2.66 (1.18) | 2.43 (1.13) | 2.2 (1.03)  | 2.71 (1.02) | 6.04 (2.03) |
| Kazakhstan     | 2.34 (1.09) | 2.48 (1.25) | 3.06 (1.03) | 2.73 (1.07) | 2.02 (1.13) | 2.63 (1.03) | 1.87 (1.02) | 2.34 (1.1)  | 2.03 (1.09) | 2.32 (1.08) | 2.75 (1.08) | 6.76 (2.02) |
| Malaysia       | 2.5 (1.19)  | 2.81 (1.32) | 3.26 (1)    | 2.93 (1)    | 2.07 (1.13) | 2.74 (0.97) | 2.24 (1.04) | 2.26 (1.13) | 2.6 (1.19)  | 2.7 (1.1)   | 3.24 (1.01) | 6.18 (1.87) |
| Netherlands    | 2 (1.06)    | 2.13 (1.21) | 3.35 (1.02) | 3.19 (1.01) | 2.08 (1.09) | 2.98 (1.04) | 2.13 (1.09) | 2.11 (1.11) | 2.14 (1.17) | 2.67 (1.12) | 3.11 (1.03) | 6.88 (1.61) |
| Philippines    | 2.77 (1.23) | 3.04 (1.35) | 3.13 (1.06) | 2.8 (1.12)  | 2.14 (1.17) | 2.86 (1.05) | 2.25 (1.19) | 2.6 (1.23)  | 2.51 (1.16) | 2.78 (1.21) | 3 (1.1)     | 6.32 (1.94) |
| Poland         | 2.81 (1.23) | 2.59 (1.28) | 2.72 (1.1)  | 2.56 (1.06) | 2.69 (1.32) | 2.24 (1.07) | 1.83 (1.02) | 2.85 (1.24) | 2.86 (1.27) | 2.2 (1.14)  | 2.42 (1.09) | 5.67 (1.96) |
| Romania        | 2.28 (1.23) | 2.93 (1.39) | 3.28 (1.04) | 2.88 (1.11) | 1.97 (1.18) | 2.83 (1.09) | 2.47 (1.11) | 2.49 (1.21) | 2.51 (1.25) | 2.74 (1.14) | 3.08 (1.12) | 6.54 (1.94) |
| Russia         | 2.54 (1.21) | 2.46 (1.25) | 2.86 (1.07) | 2.75 (1.04) | 2.2 (1.19)  | 2.56 (1.05) | 1.92 (1.04) | 2.57 (1.2)  | 2.19 (1.17) | 2.31 (1.11) | 2.49 (1.01) | 6.4 (2.05)  |
| Saudi Arabia   | 2.87 (1.21) | 3.15 (1.28) | 3.15 (1.07) | 3.17 (1.09) | 2.39 (1.26) | 2.82 (1.03) | 2.6 (1.1)   | 2.74 (1.24) | 2.63 (1.21) | 2.72 (1.1)  | 2.91 (1.12) | 6.62 (2.46) |
| Serbia         | 2.85 (1.15) | 2.53 (1.25) | 2.63 (1.11) | 2.71 (1.06) | 2.12 (1.16) | 2.68 (1.03) | 2.21 (1.03) | 2.75 (1.18) | 2.56 (1.17) | 2.47 (1.1)  | 2.81 (1.08) | 6.67 (2.14) |
| South Africa   | 2.78 (1.31) | 2.93 (1.43) | 2.98 (1.17) | 2.71 (1.11) | 2.27 (1.29) | 2.67 (1.13) | 2.12 (1.14) | 2.64 (1.3)  | 2.5 (1.27)  | 2.54 (1.22) | 2.87 (1.19) | 6.16 (2.08) |
| South Korea    | 2.37 (1.14) | 2.91 (1.19) | 2.73 (1.05) | 2.59 (1.02) | 2.36 (1.13) | 2.33 (1.01) | 2.31 (1.04) | 2.49 (1.14) | 2.44 (1.15) | 2.33 (1)    | 2.77 (1.05) | 6.2 (1.82)  |
| Spain          | 2.6 (1.21)  | 2.43 (1.29) | 2.8 (1.06)  | 2.62 (1.09) | 2.09 (1.11) | 1.83 (0.97) | 2.37 (1.16) | 2.62 (1.17) | 2.37 (1.21) | 2.27 (1.1)  | 2.57 (1.1)  | 6.59 (1.77) |
| Turkey         | 3.23 (1.18) | 3.28 (1.24) | 2.75 (1.03) | 2.33 (1.06) | 2.64 (1.22) | 2.48 (1.03) | 2.19 (1.06) | 2.97 (1.18) | 2.72 (1.15) | 2.44 (1.14) | 2.49 (1.07) | 5.83 (2.24) |
| Ukraine        | 2.36 (1.11) | 2.43 (1.22) | 3.01 (1.04) | 2.75 (1.05) | 2.08 (1.13) | 2.71 (1.03) | 2.16 (1.06) | 2.34 (1.12) | 2.14 (1.11) | 2.44 (1.09) | 2.63 (1.04) | 6.3 (2.01)  |
| United Kingdom | 2.79 (1.28) | 2.48 (1.35) | 2.79 (1.15) | 2.58 (1.12) | 2.17 (1.22) | 2.37 (1.12) | 1.76 (1.02) | 2.56 (1.24) | 2.44 (1.3)  | 2.09 (1.1)  | 2.64 (1.15) | 6.29 (2.01) |
| United States  | 3.03 (1.24) | 2.74 (1.32) | 2.72 (1.09) | 2.54 (1.09) | 2.37 (1.23) | 2.38 (1.08) | 1.9 (1.05)  | 2.85 (1.24) | 2.65 (1.26) | 2.18 (1.12) | 2.54 (1.1)  | 6.22 (2.05) |
