Emotion means per country during peak restrictions
================
Anne Margit
04/23/2021

    ## [1] ""

``` r
load("data_analyse2_p2.Rdata")
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
library(broom.mixed)
library(purrr)
library(stargazer)
```

    ## Warning: package 'stargazer' was built under R version 4.0.3

``` r
library("viridis")  
```

    ## Warning: package 'viridis' was built under R version 4.0.3

NAA

``` r
country_means_NAA <- data_analyse2_p2 %>%
  group_by(Country) %>%
  summarise(NAA_mean= mean(NAA,na.rm=TRUE), NAA_SD = sd(NAA,na.rm=TRUE), NAA_min = min(NAA,na.rm=TRUE), NAA_max = max(NAA,na.rm=TRUE)) %>%
  mutate(across(2:5, round, 3)) 
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
apa_table(country_means_NAA)
```

<caption>

(\#tab:unnamed-chunk-4)

</caption>

<div data-custom-style="Table Caption">

\*\*

</div>

| Country        | NAA\_mean | NAA\_SD | NAA\_min | NAA\_max |
| :------------- | :-------- | :------ | :------- | :------- |
| Argentina      | 2.32      | 0.95    | 0.92     | 4.74     |
| Australia      | 2.64      | 0.92    | 1.49     | 4.33     |
| Brazil         | 2.43      | 1.07    | 1.00     | 5.00     |
| Canada         | 2.69      | 1.03    | 1.04     | 4.57     |
| Chile          | 2.55      | 0.94    | 1.00     | 5.00     |
| Croatia        | 2.40      | 0.92    | 0.96     | 5.00     |
| France         | 2.11      | 0.90    | 0.89     | 5.04     |
| Germany        | 1.90      | 0.81    | 0.92     | 5.00     |
| Greece         | 2.47      | 0.91    | 0.90     | 5.00     |
| Hungary        | 2.44      | 0.99    | 0.87     | 5.00     |
| Indonesia      | 2.34      | 0.99    | 1.00     | 5.00     |
| Italy          | 2.28      | 0.95    | 0.94     | 5.00     |
| Japan          | 2.56      | 0.78    | 1.00     | 5.00     |
| Kazakhstan     | 2.22      | 0.95    | 0.93     | 5.00     |
| Kosovo         | 2.04      | 0.83    | 1.00     | 3.90     |
| Malaysia       | 2.03      | 0.98    | 1.00     | 5.00     |
| Netherlands    | 1.71      | 0.77    | 0.87     | 5.00     |
| Peru           | 2.73      | 0.70    | 1.51     | 3.47     |
| Philippines    | 2.73      | 0.94    | 0.98     | 5.00     |
| Poland         | 2.56      | 0.98    | 1.00     | 5.00     |
| Romania        | 2.22      | 1.00    | 0.64     | 5.00     |
| Russia         | 2.47      | 0.89    | 1.52     | 4.43     |
| Saudi Arabia   | 2.56      | 0.90    | 1.00     | 4.33     |
| Serbia         | 2.56      | 0.85    | 0.82     | 5.00     |
| Singapore      | 2.55      | 0.93    | 1.00     | 4.35     |
| South Africa   | 2.37      | 1.06    | 0.94     | 5.00     |
| South Korea    | 2.62      | 1.22    | 1.66     | 3.99     |
| Spain          | 2.26      | 0.98    | 0.95     | 5.00     |
| Turkey         | 2.65      | 0.98    | 1.00     | 5.00     |
| Ukraine        | 2.17      | 0.94    | 0.93     | 5.00     |
| United Kingdom | 2.02      | 0.94    | 0.85     | 5.00     |
| United States  | 2.33      | 0.99    | 0.84     | 5.00     |
| Vietnam        | 1.90      | 0.34    | 1.67     | 2.14     |

``` r
country_means_NAA %>% summarise(min=min(NAA_mean, na.rm=TRUE), max=max(NAA_mean, na.rm=TRUE))
```

    ## # A tibble: 1 x 2
    ##     min   max
    ##   <dbl> <dbl>
    ## 1  1.71  2.73

NAD

``` r
country_means_NAD <- data_analyse2_p2 %>%
  group_by(Country) %>%
  summarise(NAD_mean= mean(NAD,na.rm=TRUE), NAD_SD = sd(NAD,na.rm=TRUE), NAA_min = min(NAD,na.rm=TRUE), NAD_max = max(NAD,na.rm=TRUE)) %>%
   mutate(across(2:5, round, 3))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
apa_table(country_means_NAD)
```

<caption>

(\#tab:unnamed-chunk-6)

</caption>

<div data-custom-style="Table Caption">

\*\*

</div>

| Country        | NAD\_mean | NAD\_SD | NAA\_min | NAD\_max |
| :------------- | :-------- | :------ | :------- | :------- |
| Argentina      | 2.05      | 0.99    | 0.84     | 5.00     |
| Australia      | 2.37      | 0.95    | 1.50     | 4.50     |
| Brazil         | 2.21      | 1.07    | 1.00     | 5.00     |
| Canada         | 2.91      | 1.30    | 1.00     | 4.50     |
| Chile          | 2.60      | 1.01    | 1.00     | 5.00     |
| Croatia        | 2.28      | 0.94    | 1.00     | 5.00     |
| France         | 2.01      | 0.92    | 1.00     | 5.00     |
| Germany        | 2.06      | 0.92    | 1.00     | 5.00     |
| Greece         | 2.28      | 0.97    | 0.84     | 5.00     |
| Hungary        | 2.60      | 1.03    | 1.00     | 5.00     |
| Indonesia      | 2.46      | 1.11    | 1.00     | 5.00     |
| Italy          | 2.13      | 0.99    | 0.96     | 5.00     |
| Japan          | 2.17      | 1.00    | 1.00     | 5.00     |
| Kazakhstan     | 2.12      | 1.07    | 1.00     | 5.00     |
| Kosovo         | 2.39      | 0.90    | 1.00     | 4.00     |
| Malaysia       | 2.20      | 1.02    | 1.00     | 5.00     |
| Netherlands    | 1.93      | 0.90    | 1.00     | 5.00     |
| Peru           | 2.83      | 0.52    | 2.00     | 3.50     |
| Philippines    | 2.42      | 0.97    | 1.00     | 5.00     |
| Poland         | 2.62      | 1.11    | 1.00     | 5.00     |
| Romania        | 2.16      | 1.04    | 0.55     | 5.00     |
| Russia         | 2.62      | 1.19    | 1.00     | 4.50     |
| Saudi Arabia   | 2.33      | 1.00    | 1.00     | 4.50     |
| Serbia         | 2.32      | 0.95    | 1.00     | 5.00     |
| Singapore      | 2.63      | 0.98    | 1.00     | 5.00     |
| South Africa   | 2.24      | 1.09    | 1.00     | 5.00     |
| South Korea    | 3.67      | 1.89    | 1.50     | 5.00     |
| Spain          | 2.18      | 1.00    | 1.00     | 5.00     |
| Turkey         | 2.49      | 1.04    | 1.00     | 5.00     |
| Ukraine        | 2.09      | 0.99    | 0.93     | 5.00     |
| United Kingdom | 1.97      | 0.96    | 1.00     | 5.00     |
| United States  | 2.31      | 1.03    | 1.00     | 5.00     |
| Vietnam        | 3.00      | 0.71    | 2.50     | 3.50     |

``` r
country_means_NAD %>% summarise(min=min(NAD_mean, na.rm=TRUE), max=max(NAD_mean, na.rm=TRUE))
```

    ## # A tibble: 1 x 2
    ##     min   max
    ##   <dbl> <dbl>
    ## 1  1.93  3.67

PAA

``` r
country_means_PAA <- data_analyse2_p2 %>%
  group_by(Country) %>%
  summarise(PAA_mean= mean(PAA,na.rm=TRUE), PAA_SD = sd(PAA,na.rm=TRUE), PAA_min = min(PAA,na.rm=TRUE), PAA_max = max(PAA,na.rm=TRUE)) %>%
   mutate(across(2:5, round, 3))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
apa_table(country_means_PAA)
```

<caption>

(\#tab:unnamed-chunk-8)

</caption>

<div data-custom-style="Table Caption">

\*\*

</div>

| Country        | PAA\_mean | PAA\_SD | PAA\_min | PAA\_max |
| :------------- | :-------- | :------ | :------- | :------- |
| Argentina      | 2.38      | 0.97    | 1.00     | 5.00     |
| Australia      | 2.60      | 1.00    | 1.00     | 4.00     |
| Brazil         | 2.38      | 1.03    | 1.00     | 5.00     |
| Canada         | 1.97      | 0.77    | 1.00     | 3.00     |
| Chile          | 2.31      | 0.89    | 1.00     | 5.00     |
| Croatia        | 2.70      | 0.90    | 1.00     | 5.00     |
| France         | 2.59      | 0.87    | 1.00     | 5.00     |
| Germany        | 2.76      | 0.88    | 1.00     | 5.00     |
| Greece         | 2.91      | 0.93    | 1.00     | 5.00     |
| Hungary        | 2.70      | 0.96    | 1.00     | 5.00     |
| Indonesia      | 3.05      | 1.14    | 1.00     | 5.00     |
| Italy          | 2.30      | 0.90    | 1.00     | 5.00     |
| Japan          | 2.44      | 0.66    | 1.00     | 4.50     |
| Kazakhstan     | 2.50      | 0.97    | 1.00     | 5.00     |
| Kosovo         | 2.80      | 0.95    | 1.50     | 5.00     |
| Malaysia       | 2.86      | 0.99    | 1.00     | 5.00     |
| Netherlands    | 2.94      | 0.92    | 1.00     | 5.00     |
| Peru           | 2.25      | 0.61    | 1.50     | 3.00     |
| Philippines    | 2.61      | 0.93    | 1.00     | 5.00     |
| Poland         | 2.41      | 0.93    | 1.00     | 5.00     |
| Romania        | 2.92      | 0.96    | 1.00     | 5.00     |
| Russia         | 2.38      | 1.09    | 1.00     | 4.50     |
| Saudi Arabia   | 2.89      | 0.70    | 1.50     | 4.50     |
| Serbia         | 2.59      | 0.86    | 1.00     | 5.00     |
| Singapore      | 2.46      | 0.81    | 1.00     | 4.00     |
| South Africa   | 2.53      | 0.99    | 1.00     | 5.00     |
| South Korea    | 1.33      | 0.58    | 1.00     | 2.00     |
| Spain          | 2.23      | 0.94    | 0.68     | 5.00     |
| Turkey         | 2.38      | 0.89    | 1.00     | 4.00     |
| Ukraine        | 2.59      | 0.94    | 1.00     | 5.00     |
| United Kingdom | 2.34      | 0.94    | 1.00     | 5.00     |
| United States  | 2.36      | 0.94    | 1.00     | 5.00     |
| Vietnam        | 2.75      | 0.35    | 2.50     | 3.00     |

``` r
country_means_PAA %>% summarise(min=min(PAA_mean, na.rm=TRUE), max=max(PAA_mean, na.rm=TRUE))
```

    ## # A tibble: 1 x 2
    ##     min   max
    ##   <dbl> <dbl>
    ## 1  1.33  3.05

PAD

``` r
country_means_PAD <- data_analyse2_p2 %>%
  group_by(Country) %>%
  summarise(PAD_mean= mean(PAD,na.rm=TRUE), PAD_SD = sd(PAD,na.rm=TRUE), PAD_min = min(PAD,na.rm=TRUE), PAD_max = max(PAD,na.rm=TRUE)) %>%
   mutate(across(2:5, round, 3))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
apa_table(country_means_PAD)
```

<caption>

(\#tab:unnamed-chunk-10)

</caption>

<div data-custom-style="Table Caption">

\*\*

</div>

| Country        | PAD\_mean | PAD\_SD | PAD\_min | PAD\_max |
| :------------- | :-------- | :------ | :------- | :------- |
| Argentina      | 2.82      | 0.99    | 1.00     | 5.00     |
| Australia      | 2.60      | 1.12    | 1.00     | 4.00     |
| Brazil         | 2.65      | 1.06    | 1.00     | 5.00     |
| Canada         | 2.49      | 0.73    | 1.37     | 3.50     |
| Chile          | 2.50      | 0.95    | 1.00     | 5.00     |
| Croatia        | 3.13      | 0.87    | 1.00     | 5.00     |
| France         | 3.02      | 0.92    | 1.00     | 5.00     |
| Germany        | 3.11      | 0.88    | 1.00     | 5.00     |
| Greece         | 3.22      | 0.94    | 1.00     | 5.00     |
| Hungary        | 3.05      | 0.98    | 1.00     | 5.00     |
| Indonesia      | 3.34      | 1.02    | 1.00     | 5.00     |
| Italy          | 2.68      | 0.98    | 1.00     | 5.00     |
| Japan          | 2.98      | 0.89    | 1.00     | 5.00     |
| Kazakhstan     | 2.88      | 0.89    | 1.00     | 5.00     |
| Kosovo         | 3.01      | 0.93    | 1.00     | 5.00     |
| Malaysia       | 3.24      | 0.94    | 1.00     | 5.00     |
| Netherlands    | 3.39      | 0.88    | 1.00     | 5.00     |
| Peru           | 2.25      | 0.69    | 1.00     | 3.00     |
| Philippines    | 2.86      | 0.89    | 1.00     | 5.00     |
| Poland         | 2.67      | 0.95    | 1.00     | 5.00     |
| Romania        | 3.20      | 0.95    | 1.00     | 5.00     |
| Russia         | 3.00      | 1.24    | 1.00     | 5.00     |
| Saudi Arabia   | 3.06      | 0.71    | 1.00     | 4.00     |
| Serbia         | 2.79      | 0.94    | 1.00     | 5.00     |
| Singapore      | 2.73      | 0.86    | 1.00     | 5.00     |
| South Africa   | 3.01      | 1.04    | 1.00     | 5.00     |
| South Korea    | 2.50      | 0.87    | 1.50     | 3.00     |
| Spain          | 2.91      | 1.00    | 1.00     | 5.00     |
| Turkey         | 2.93      | 0.77    | 1.00     | 4.00     |
| Ukraine        | 2.83      | 0.85    | 1.00     | 5.00     |
| United Kingdom | 3.13      | 1.06    | 1.00     | 5.00     |
| United States  | 2.79      | 1.00    | 1.00     | 5.00     |
| Vietnam        | 3.00      | 1.41    | 2.00     | 4.00     |

``` r
country_means_PAD %>% summarise(min=min(PAD_mean, na.rm=TRUE), max=max(PAD_mean, na.rm=TRUE))
```

    ## # A tibble: 1 x 2
    ##     min   max
    ##   <dbl> <dbl>
    ## 1  2.25  3.39

``` r
N_Country <- data_analyse2_p2 %>%
  group_by(Country) %>%
  summarise(Ncountry = n_distinct(ID))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
apa_table(N_Country)
```

<caption>

(\#tab:unnamed-chunk-12)

</caption>

<div data-custom-style="Table Caption">

\*\*

</div>

| Country        | Ncountry |
| :------------- | :------- |
| Argentina      | 206      |
| Australia      | 13       |
| Brazil         | 191      |
| Canada         | 11       |
| Chile          | 78       |
| Croatia        | 138      |
| France         | 358      |
| Germany        | 397      |
| Greece         | 668      |
| Hungary        | 195      |
| Indonesia      | 102      |
| Italy          | 454      |
| Japan          | 72       |
| Kazakhstan     | 83       |
| Kosovo         | 19       |
| Malaysia       | 65       |
| Netherlands    | 661      |
| Peru           | 6        |
| Philippines    | 127      |
| Poland         | 131      |
| Romania        | 311      |
| Russia         | 12       |
| Saudi Arabia   | 27       |
| Serbia         | 475      |
| Singapore      | 41       |
| South Africa   | 218      |
| South Korea    | 3        |
| Spain          | 1001     |
| Turkey         | 75       |
| Ukraine        | 262      |
| United Kingdom | 455      |
| United States  | 2359     |
| Vietnam        | 1        |

``` r
country_means_NAA <- as.matrix(country_means_NAA)
country_means_NAD <- as.matrix(country_means_NAD) 
country_means_PAA <- as.matrix(country_means_PAA)
country_means_PAD <- as.matrix(country_means_PAD)
stargazer(country_means_NAA, country_means_NAD, country_means_PAA,
          country_means_PAD, df= TRUE, type="html", out="country_means.doc")
```

    ## 
    ## <table style="text-align:center"><tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Country</td><td>NAA_mean</td><td>NAA_SD</td><td>NAA_min</td><td>NAA_max</td></tr>
    ## <tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Argentina</td><td>2.323</td><td>0.954</td><td>0.920</td><td>4.738</td></tr>
    ## <tr><td style="text-align:left">Australia</td><td>2.636</td><td>0.916</td><td>1.486</td><td>4.333</td></tr>
    ## <tr><td style="text-align:left">Brazil</td><td>2.434</td><td>1.068</td><td>1.000</td><td>5.000</td></tr>
    ## <tr><td style="text-align:left">Canada</td><td>2.694</td><td>1.034</td><td>1.043</td><td>4.566</td></tr>
    ## <tr><td style="text-align:left">Chile</td><td>2.548</td><td>0.944</td><td>1.000</td><td>5.000</td></tr>
    ## <tr><td style="text-align:left">Croatia</td><td>2.405</td><td>0.916</td><td>0.961</td><td>5.000</td></tr>
    ## <tr><td style="text-align:left">France</td><td>2.112</td><td>0.901</td><td>0.893</td><td>5.040</td></tr>
    ## <tr><td style="text-align:left">Germany</td><td>1.901</td><td>0.808</td><td>0.919</td><td>5.000</td></tr>
    ## <tr><td style="text-align:left">Greece</td><td>2.470</td><td>0.914</td><td>0.900</td><td>5.000</td></tr>
    ## <tr><td style="text-align:left">Hungary</td><td>2.441</td><td>0.989</td><td>0.868</td><td>5.000</td></tr>
    ## <tr><td style="text-align:left">Indonesia</td><td>2.344</td><td>0.994</td><td>1.000</td><td>5.000</td></tr>
    ## <tr><td style="text-align:left">Italy</td><td>2.278</td><td>0.948</td><td>0.943</td><td>5.000</td></tr>
    ## <tr><td style="text-align:left">Japan</td><td>2.561</td><td>0.780</td><td>1.000</td><td>5.000</td></tr>
    ## <tr><td style="text-align:left">Kazakhstan</td><td>2.225</td><td>0.950</td><td>0.929</td><td>5.000</td></tr>
    ## <tr><td style="text-align:left">Kosovo</td><td>2.039</td><td>0.832</td><td>1.000</td><td>3.899</td></tr>
    ## <tr><td style="text-align:left">Malaysia</td><td>2.027</td><td>0.977</td><td>1.000</td><td>5.000</td></tr>
    ## <tr><td style="text-align:left">Netherlands</td><td>1.710</td><td>0.766</td><td>0.874</td><td>5.000</td></tr>
    ## <tr><td style="text-align:left">Peru</td><td>2.732</td><td>0.702</td><td>1.514</td><td>3.472</td></tr>
    ## <tr><td style="text-align:left">Philippines</td><td>2.728</td><td>0.944</td><td>0.979</td><td>5.000</td></tr>
    ## <tr><td style="text-align:left">Poland</td><td>2.555</td><td>0.981</td><td>1.000</td><td>5.000</td></tr>
    ## <tr><td style="text-align:left">Romania</td><td>2.223</td><td>0.997</td><td>0.641</td><td>5.000</td></tr>
    ## <tr><td style="text-align:left">Russia</td><td>2.466</td><td>0.886</td><td>1.516</td><td>4.435</td></tr>
    ## <tr><td style="text-align:left">Saudi Arabia</td><td>2.559</td><td>0.904</td><td>1.000</td><td>4.333</td></tr>
    ## <tr><td style="text-align:left">Serbia</td><td>2.564</td><td>0.854</td><td>0.823</td><td>5.000</td></tr>
    ## <tr><td style="text-align:left">Singapore</td><td>2.547</td><td>0.929</td><td>1.000</td><td>4.352</td></tr>
    ## <tr><td style="text-align:left">South Africa</td><td>2.367</td><td>1.062</td><td>0.935</td><td>5.000</td></tr>
    ## <tr><td style="text-align:left">South Korea</td><td>2.618</td><td>1.216</td><td>1.660</td><td>3.986</td></tr>
    ## <tr><td style="text-align:left">Spain</td><td>2.259</td><td>0.985</td><td>0.946</td><td>5.000</td></tr>
    ## <tr><td style="text-align:left">Turkey</td><td>2.655</td><td>0.975</td><td>1.000</td><td>5.000</td></tr>
    ## <tr><td style="text-align:left">Ukraine</td><td>2.175</td><td>0.936</td><td>0.934</td><td>5.000</td></tr>
    ## <tr><td style="text-align:left">United Kingdom</td><td>2.017</td><td>0.940</td><td>0.846</td><td>5.000</td></tr>
    ## <tr><td style="text-align:left">United States</td><td>2.325</td><td>0.987</td><td>0.838</td><td>5.000</td></tr>
    ## <tr><td style="text-align:left">Vietnam</td><td>1.903</td><td>0.335</td><td>1.667</td><td>2.140</td></tr>
    ## <tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr></table>
    ## 
    ## <table style="text-align:center"><tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Country</td><td>NAD_mean</td><td>NAD_SD</td><td>NAA_min</td><td>NAD_max</td></tr>
    ## <tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Argentina</td><td>2.050</td><td>0.994</td><td>0.843</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Australia</td><td>2.367</td><td>0.954</td><td>1.500</td><td>4.5</td></tr>
    ## <tr><td style="text-align:left">Brazil</td><td>2.208</td><td>1.068</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Canada</td><td>2.909</td><td>1.300</td><td>1.000</td><td>4.5</td></tr>
    ## <tr><td style="text-align:left">Chile</td><td>2.596</td><td>1.014</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Croatia</td><td>2.276</td><td>0.945</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">France</td><td>2.013</td><td>0.924</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Germany</td><td>2.056</td><td>0.915</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Greece</td><td>2.283</td><td>0.972</td><td>0.836</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Hungary</td><td>2.598</td><td>1.033</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Indonesia</td><td>2.458</td><td>1.106</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Italy</td><td>2.127</td><td>0.990</td><td>0.959</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Japan</td><td>2.166</td><td>1.002</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Kazakhstan</td><td>2.115</td><td>1.069</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Kosovo</td><td>2.389</td><td>0.902</td><td>1.000</td><td>4.0</td></tr>
    ## <tr><td style="text-align:left">Malaysia</td><td>2.204</td><td>1.019</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Netherlands</td><td>1.930</td><td>0.902</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Peru</td><td>2.833</td><td>0.516</td><td>2.000</td><td>3.5</td></tr>
    ## <tr><td style="text-align:left">Philippines</td><td>2.425</td><td>0.970</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Poland</td><td>2.620</td><td>1.115</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Romania</td><td>2.162</td><td>1.045</td><td>0.551</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Russia</td><td>2.625</td><td>1.189</td><td>1.000</td><td>4.5</td></tr>
    ## <tr><td style="text-align:left">Saudi Arabia</td><td>2.333</td><td>1.000</td><td>1.000</td><td>4.5</td></tr>
    ## <tr><td style="text-align:left">Serbia</td><td>2.315</td><td>0.951</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Singapore</td><td>2.630</td><td>0.980</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">South Africa</td><td>2.243</td><td>1.093</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">South Korea</td><td>3.667</td><td>1.893</td><td>1.500</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Spain</td><td>2.177</td><td>1.001</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Turkey</td><td>2.486</td><td>1.037</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Ukraine</td><td>2.086</td><td>0.991</td><td>0.930</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">United Kingdom</td><td>1.966</td><td>0.964</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">United States</td><td>2.312</td><td>1.034</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Vietnam</td><td>3.000</td><td>0.707</td><td>2.500</td><td>3.5</td></tr>
    ## <tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr></table>
    ## 
    ## <table style="text-align:center"><tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Country</td><td>PAA_mean</td><td>PAA_SD</td><td>PAA_min</td><td>PAA_max</td></tr>
    ## <tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Argentina</td><td>2.379</td><td>0.970</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Australia</td><td>2.600</td><td>1.004</td><td>1.000</td><td>4.0</td></tr>
    ## <tr><td style="text-align:left">Brazil</td><td>2.382</td><td>1.028</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Canada</td><td>1.971</td><td>0.769</td><td>1.000</td><td>3.0</td></tr>
    ## <tr><td style="text-align:left">Chile</td><td>2.308</td><td>0.889</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Croatia</td><td>2.702</td><td>0.903</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">France</td><td>2.587</td><td>0.868</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Germany</td><td>2.755</td><td>0.878</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Greece</td><td>2.914</td><td>0.933</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Hungary</td><td>2.702</td><td>0.964</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Indonesia</td><td>3.051</td><td>1.135</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Italy</td><td>2.299</td><td>0.895</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Japan</td><td>2.436</td><td>0.657</td><td>1.000</td><td>4.5</td></tr>
    ## <tr><td style="text-align:left">Kazakhstan</td><td>2.495</td><td>0.967</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Kosovo</td><td>2.795</td><td>0.953</td><td>1.500</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Malaysia</td><td>2.863</td><td>0.992</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Netherlands</td><td>2.940</td><td>0.922</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Peru</td><td>2.250</td><td>0.612</td><td>1.500</td><td>3.0</td></tr>
    ## <tr><td style="text-align:left">Philippines</td><td>2.608</td><td>0.929</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Poland</td><td>2.412</td><td>0.929</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Romania</td><td>2.923</td><td>0.962</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Russia</td><td>2.375</td><td>1.090</td><td>1.000</td><td>4.5</td></tr>
    ## <tr><td style="text-align:left">Saudi Arabia</td><td>2.889</td><td>0.698</td><td>1.500</td><td>4.5</td></tr>
    ## <tr><td style="text-align:left">Serbia</td><td>2.594</td><td>0.865</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Singapore</td><td>2.457</td><td>0.806</td><td>1.000</td><td>4.0</td></tr>
    ## <tr><td style="text-align:left">South Africa</td><td>2.534</td><td>0.989</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">South Korea</td><td>1.333</td><td>0.577</td><td>1.000</td><td>2.0</td></tr>
    ## <tr><td style="text-align:left">Spain</td><td>2.226</td><td>0.937</td><td>0.677</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Turkey</td><td>2.375</td><td>0.886</td><td>1.000</td><td>4.0</td></tr>
    ## <tr><td style="text-align:left">Ukraine</td><td>2.590</td><td>0.939</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">United Kingdom</td><td>2.337</td><td>0.941</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">United States</td><td>2.357</td><td>0.940</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Vietnam</td><td>2.750</td><td>0.354</td><td>2.500</td><td>3.0</td></tr>
    ## <tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr></table>
    ## 
    ## <table style="text-align:center"><tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Country</td><td>PAD_mean</td><td>PAD_SD</td><td>PAD_min</td><td>PAD_max</td></tr>
    ## <tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Argentina</td><td>2.815</td><td>0.987</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Australia</td><td>2.600</td><td>1.121</td><td>1.000</td><td>4.0</td></tr>
    ## <tr><td style="text-align:left">Brazil</td><td>2.649</td><td>1.057</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Canada</td><td>2.488</td><td>0.726</td><td>1.372</td><td>3.5</td></tr>
    ## <tr><td style="text-align:left">Chile</td><td>2.496</td><td>0.950</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Croatia</td><td>3.127</td><td>0.872</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">France</td><td>3.021</td><td>0.915</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Germany</td><td>3.111</td><td>0.882</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Greece</td><td>3.221</td><td>0.944</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Hungary</td><td>3.054</td><td>0.982</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Indonesia</td><td>3.336</td><td>1.018</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Italy</td><td>2.682</td><td>0.983</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Japan</td><td>2.980</td><td>0.887</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Kazakhstan</td><td>2.882</td><td>0.888</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Kosovo</td><td>3.012</td><td>0.928</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Malaysia</td><td>3.243</td><td>0.945</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Netherlands</td><td>3.393</td><td>0.885</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Peru</td><td>2.250</td><td>0.689</td><td>1.000</td><td>3.0</td></tr>
    ## <tr><td style="text-align:left">Philippines</td><td>2.860</td><td>0.890</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Poland</td><td>2.674</td><td>0.949</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Romania</td><td>3.205</td><td>0.953</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Russia</td><td>3.000</td><td>1.243</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Saudi Arabia</td><td>3.056</td><td>0.712</td><td>1.000</td><td>4.0</td></tr>
    ## <tr><td style="text-align:left">Serbia</td><td>2.792</td><td>0.944</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Singapore</td><td>2.732</td><td>0.859</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">South Africa</td><td>3.011</td><td>1.036</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">South Korea</td><td>2.500</td><td>0.866</td><td>1.500</td><td>3.0</td></tr>
    ## <tr><td style="text-align:left">Spain</td><td>2.914</td><td>1.001</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Turkey</td><td>2.933</td><td>0.767</td><td>1.000</td><td>4.0</td></tr>
    ## <tr><td style="text-align:left">Ukraine</td><td>2.825</td><td>0.850</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">United Kingdom</td><td>3.128</td><td>1.056</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">United States</td><td>2.786</td><td>0.999</td><td>1.000</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Vietnam</td><td>3.000</td><td>1.414</td><td>2.000</td><td>4.0</td></tr>
    ## <tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr></table>
