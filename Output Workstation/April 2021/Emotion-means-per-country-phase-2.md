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
  mutate(across(2:5, round, 2)) 
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
| Croatia        | 2.41      | 0.92    | 0.96     | 5.00     |
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
| South Africa   | 2.37      | 1.06    | 0.93     | 5.00     |
| South Korea    | 2.62      | 1.22    | 1.66     | 3.99     |
| Spain          | 2.26      | 0.98    | 0.95     | 5.00     |
| Turkey         | 2.66      | 0.97    | 1.00     | 5.00     |
| Ukraine        | 2.17      | 0.94    | 0.93     | 5.00     |
| United Kingdom | 2.02      | 0.94    | 0.85     | 5.00     |
| United States  | 2.33      | 0.99    | 0.84     | 5.00     |
| Vietnam        | 1.90      | 0.33    | 1.67     | 2.14     |

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
   mutate(across(2:5, round, 2))
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
| Philippines    | 2.43      | 0.97    | 1.00     | 5.00     |
| Poland         | 2.62      | 1.12    | 1.00     | 5.00     |
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
   mutate(across(2:5, round, 2))
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
| Germany        | 2.75      | 0.88    | 1.00     | 5.00     |
| Greece         | 2.91      | 0.93    | 1.00     | 5.00     |
| Hungary        | 2.70      | 0.96    | 1.00     | 5.00     |
| Indonesia      | 3.05      | 1.13    | 1.00     | 5.00     |
| Italy          | 2.30      | 0.90    | 1.00     | 5.00     |
| Japan          | 2.44      | 0.66    | 1.00     | 4.50     |
| Kazakhstan     | 2.49      | 0.97    | 1.00     | 5.00     |
| Kosovo         | 2.79      | 0.95    | 1.50     | 5.00     |
| Malaysia       | 2.86      | 0.99    | 1.00     | 5.00     |
| Netherlands    | 2.94      | 0.92    | 1.00     | 5.00     |
| Peru           | 2.25      | 0.61    | 1.50     | 3.00     |
| Philippines    | 2.61      | 0.93    | 1.00     | 5.00     |
| Poland         | 2.41      | 0.93    | 1.00     | 5.00     |
| Romania        | 2.92      | 0.96    | 1.00     | 5.00     |
| Russia         | 2.38      | 1.09    | 1.00     | 4.50     |
| Saudi Arabia   | 2.89      | 0.70    | 1.50     | 4.50     |
| Serbia         | 2.59      | 0.87    | 1.00     | 5.00     |
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
   mutate(across(2:5, round, 2))
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
| Argentina      | 2.81      | 0.99    | 1.00     | 5.00     |
| Australia      | 2.60      | 1.12    | 1.00     | 4.00     |
| Brazil         | 2.65      | 1.06    | 1.00     | 5.00     |
| Canada         | 2.49      | 0.73    | 1.37     | 3.50     |
| Chile          | 2.50      | 0.95    | 1.00     | 5.00     |
| Croatia        | 3.13      | 0.87    | 1.00     | 5.00     |
| France         | 3.02      | 0.91    | 1.00     | 5.00     |
| Germany        | 3.11      | 0.88    | 1.00     | 5.00     |
| Greece         | 3.22      | 0.94    | 1.00     | 5.00     |
| Hungary        | 3.05      | 0.98    | 1.00     | 5.00     |
| Indonesia      | 3.34      | 1.02    | 1.00     | 5.00     |
| Italy          | 2.68      | 0.98    | 1.00     | 5.00     |
| Japan          | 2.98      | 0.89    | 1.00     | 5.00     |
| Kazakhstan     | 2.88      | 0.89    | 1.00     | 5.00     |
| Kosovo         | 3.01      | 0.93    | 1.00     | 5.00     |
| Malaysia       | 3.24      | 0.95    | 1.00     | 5.00     |
| Netherlands    | 3.39      | 0.89    | 1.00     | 5.00     |
| Peru           | 2.25      | 0.69    | 1.00     | 3.00     |
| Philippines    | 2.86      | 0.89    | 1.00     | 5.00     |
| Poland         | 2.67      | 0.95    | 1.00     | 5.00     |
| Romania        | 3.21      | 0.95    | 1.00     | 5.00     |
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
    ## <tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Argentina</td><td>2.32</td><td>0.95</td><td>0.92</td><td>4.74</td></tr>
    ## <tr><td style="text-align:left">Australia</td><td>2.64</td><td>0.92</td><td>1.49</td><td>4.33</td></tr>
    ## <tr><td style="text-align:left">Brazil</td><td>2.43</td><td>1.07</td><td>1.00</td><td>5.00</td></tr>
    ## <tr><td style="text-align:left">Canada</td><td>2.69</td><td>1.03</td><td>1.04</td><td>4.57</td></tr>
    ## <tr><td style="text-align:left">Chile</td><td>2.55</td><td>0.94</td><td>1.00</td><td>5.00</td></tr>
    ## <tr><td style="text-align:left">Croatia</td><td>2.41</td><td>0.92</td><td>0.96</td><td>5.00</td></tr>
    ## <tr><td style="text-align:left">France</td><td>2.11</td><td>0.90</td><td>0.89</td><td>5.04</td></tr>
    ## <tr><td style="text-align:left">Germany</td><td>1.90</td><td>0.81</td><td>0.92</td><td>5.00</td></tr>
    ## <tr><td style="text-align:left">Greece</td><td>2.47</td><td>0.91</td><td>0.90</td><td>5.00</td></tr>
    ## <tr><td style="text-align:left">Hungary</td><td>2.44</td><td>0.99</td><td>0.87</td><td>5.00</td></tr>
    ## <tr><td style="text-align:left">Indonesia</td><td>2.34</td><td>0.99</td><td>1.00</td><td>5.00</td></tr>
    ## <tr><td style="text-align:left">Italy</td><td>2.28</td><td>0.95</td><td>0.94</td><td>5.00</td></tr>
    ## <tr><td style="text-align:left">Japan</td><td>2.56</td><td>0.78</td><td>1.00</td><td>5.00</td></tr>
    ## <tr><td style="text-align:left">Kazakhstan</td><td>2.22</td><td>0.95</td><td>0.93</td><td>5.00</td></tr>
    ## <tr><td style="text-align:left">Kosovo</td><td>2.04</td><td>0.83</td><td>1.00</td><td>3.90</td></tr>
    ## <tr><td style="text-align:left">Malaysia</td><td>2.03</td><td>0.98</td><td>1.00</td><td>5.00</td></tr>
    ## <tr><td style="text-align:left">Netherlands</td><td>1.71</td><td>0.77</td><td>0.87</td><td>5.00</td></tr>
    ## <tr><td style="text-align:left">Peru</td><td>2.73</td><td>0.70</td><td>1.51</td><td>3.47</td></tr>
    ## <tr><td style="text-align:left">Philippines</td><td>2.73</td><td>0.94</td><td>0.98</td><td>5.00</td></tr>
    ## <tr><td style="text-align:left">Poland</td><td>2.56</td><td>0.98</td><td>1.00</td><td>5.00</td></tr>
    ## <tr><td style="text-align:left">Romania</td><td>2.22</td><td>1.00</td><td>0.64</td><td>5.00</td></tr>
    ## <tr><td style="text-align:left">Russia</td><td>2.47</td><td>0.89</td><td>1.52</td><td>4.43</td></tr>
    ## <tr><td style="text-align:left">Saudi Arabia</td><td>2.56</td><td>0.90</td><td>1.00</td><td>4.33</td></tr>
    ## <tr><td style="text-align:left">Serbia</td><td>2.56</td><td>0.85</td><td>0.82</td><td>5.00</td></tr>
    ## <tr><td style="text-align:left">Singapore</td><td>2.55</td><td>0.93</td><td>1.00</td><td>4.35</td></tr>
    ## <tr><td style="text-align:left">South Africa</td><td>2.37</td><td>1.06</td><td>0.93</td><td>5.00</td></tr>
    ## <tr><td style="text-align:left">South Korea</td><td>2.62</td><td>1.22</td><td>1.66</td><td>3.99</td></tr>
    ## <tr><td style="text-align:left">Spain</td><td>2.26</td><td>0.98</td><td>0.95</td><td>5.00</td></tr>
    ## <tr><td style="text-align:left">Turkey</td><td>2.66</td><td>0.97</td><td>1.00</td><td>5.00</td></tr>
    ## <tr><td style="text-align:left">Ukraine</td><td>2.17</td><td>0.94</td><td>0.93</td><td>5.00</td></tr>
    ## <tr><td style="text-align:left">United Kingdom</td><td>2.02</td><td>0.94</td><td>0.85</td><td>5.00</td></tr>
    ## <tr><td style="text-align:left">United States</td><td>2.33</td><td>0.99</td><td>0.84</td><td>5.00</td></tr>
    ## <tr><td style="text-align:left">Vietnam</td><td>1.90</td><td>0.33</td><td>1.67</td><td>2.14</td></tr>
    ## <tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr></table>
    ## 
    ## <table style="text-align:center"><tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Country</td><td>NAD_mean</td><td>NAD_SD</td><td>NAA_min</td><td>NAD_max</td></tr>
    ## <tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Argentina</td><td>2.05</td><td>0.99</td><td>0.84</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Australia</td><td>2.37</td><td>0.95</td><td>1.50</td><td>4.5</td></tr>
    ## <tr><td style="text-align:left">Brazil</td><td>2.21</td><td>1.07</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Canada</td><td>2.91</td><td>1.30</td><td>1.00</td><td>4.5</td></tr>
    ## <tr><td style="text-align:left">Chile</td><td>2.60</td><td>1.01</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Croatia</td><td>2.28</td><td>0.94</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">France</td><td>2.01</td><td>0.92</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Germany</td><td>2.06</td><td>0.92</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Greece</td><td>2.28</td><td>0.97</td><td>0.84</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Hungary</td><td>2.60</td><td>1.03</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Indonesia</td><td>2.46</td><td>1.11</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Italy</td><td>2.13</td><td>0.99</td><td>0.96</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Japan</td><td>2.17</td><td>1.00</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Kazakhstan</td><td>2.12</td><td>1.07</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Kosovo</td><td>2.39</td><td>0.90</td><td>1.00</td><td>4.0</td></tr>
    ## <tr><td style="text-align:left">Malaysia</td><td>2.20</td><td>1.02</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Netherlands</td><td>1.93</td><td>0.90</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Peru</td><td>2.83</td><td>0.52</td><td>2.00</td><td>3.5</td></tr>
    ## <tr><td style="text-align:left">Philippines</td><td>2.43</td><td>0.97</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Poland</td><td>2.62</td><td>1.12</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Romania</td><td>2.16</td><td>1.04</td><td>0.55</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Russia</td><td>2.62</td><td>1.19</td><td>1.00</td><td>4.5</td></tr>
    ## <tr><td style="text-align:left">Saudi Arabia</td><td>2.33</td><td>1.00</td><td>1.00</td><td>4.5</td></tr>
    ## <tr><td style="text-align:left">Serbia</td><td>2.32</td><td>0.95</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Singapore</td><td>2.63</td><td>0.98</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">South Africa</td><td>2.24</td><td>1.09</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">South Korea</td><td>3.67</td><td>1.89</td><td>1.50</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Spain</td><td>2.18</td><td>1.00</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Turkey</td><td>2.49</td><td>1.04</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Ukraine</td><td>2.09</td><td>0.99</td><td>0.93</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">United Kingdom</td><td>1.97</td><td>0.96</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">United States</td><td>2.31</td><td>1.03</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Vietnam</td><td>3.00</td><td>0.71</td><td>2.50</td><td>3.5</td></tr>
    ## <tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr></table>
    ## 
    ## <table style="text-align:center"><tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Country</td><td>PAA_mean</td><td>PAA_SD</td><td>PAA_min</td><td>PAA_max</td></tr>
    ## <tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Argentina</td><td>2.38</td><td>0.97</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Australia</td><td>2.60</td><td>1.00</td><td>1.00</td><td>4.0</td></tr>
    ## <tr><td style="text-align:left">Brazil</td><td>2.38</td><td>1.03</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Canada</td><td>1.97</td><td>0.77</td><td>1.00</td><td>3.0</td></tr>
    ## <tr><td style="text-align:left">Chile</td><td>2.31</td><td>0.89</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Croatia</td><td>2.70</td><td>0.90</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">France</td><td>2.59</td><td>0.87</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Germany</td><td>2.75</td><td>0.88</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Greece</td><td>2.91</td><td>0.93</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Hungary</td><td>2.70</td><td>0.96</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Indonesia</td><td>3.05</td><td>1.13</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Italy</td><td>2.30</td><td>0.90</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Japan</td><td>2.44</td><td>0.66</td><td>1.00</td><td>4.5</td></tr>
    ## <tr><td style="text-align:left">Kazakhstan</td><td>2.49</td><td>0.97</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Kosovo</td><td>2.79</td><td>0.95</td><td>1.50</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Malaysia</td><td>2.86</td><td>0.99</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Netherlands</td><td>2.94</td><td>0.92</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Peru</td><td>2.25</td><td>0.61</td><td>1.50</td><td>3.0</td></tr>
    ## <tr><td style="text-align:left">Philippines</td><td>2.61</td><td>0.93</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Poland</td><td>2.41</td><td>0.93</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Romania</td><td>2.92</td><td>0.96</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Russia</td><td>2.38</td><td>1.09</td><td>1.00</td><td>4.5</td></tr>
    ## <tr><td style="text-align:left">Saudi Arabia</td><td>2.89</td><td>0.70</td><td>1.50</td><td>4.5</td></tr>
    ## <tr><td style="text-align:left">Serbia</td><td>2.59</td><td>0.87</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Singapore</td><td>2.46</td><td>0.81</td><td>1.00</td><td>4.0</td></tr>
    ## <tr><td style="text-align:left">South Africa</td><td>2.53</td><td>0.99</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">South Korea</td><td>1.33</td><td>0.58</td><td>1.00</td><td>2.0</td></tr>
    ## <tr><td style="text-align:left">Spain</td><td>2.23</td><td>0.94</td><td>0.68</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Turkey</td><td>2.38</td><td>0.89</td><td>1.00</td><td>4.0</td></tr>
    ## <tr><td style="text-align:left">Ukraine</td><td>2.59</td><td>0.94</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">United Kingdom</td><td>2.34</td><td>0.94</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">United States</td><td>2.36</td><td>0.94</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Vietnam</td><td>2.75</td><td>0.35</td><td>2.50</td><td>3.0</td></tr>
    ## <tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr></table>
    ## 
    ## <table style="text-align:center"><tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Country</td><td>PAD_mean</td><td>PAD_SD</td><td>PAD_min</td><td>PAD_max</td></tr>
    ## <tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Argentina</td><td>2.81</td><td>0.99</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Australia</td><td>2.60</td><td>1.12</td><td>1.00</td><td>4.0</td></tr>
    ## <tr><td style="text-align:left">Brazil</td><td>2.65</td><td>1.06</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Canada</td><td>2.49</td><td>0.73</td><td>1.37</td><td>3.5</td></tr>
    ## <tr><td style="text-align:left">Chile</td><td>2.50</td><td>0.95</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Croatia</td><td>3.13</td><td>0.87</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">France</td><td>3.02</td><td>0.91</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Germany</td><td>3.11</td><td>0.88</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Greece</td><td>3.22</td><td>0.94</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Hungary</td><td>3.05</td><td>0.98</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Indonesia</td><td>3.34</td><td>1.02</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Italy</td><td>2.68</td><td>0.98</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Japan</td><td>2.98</td><td>0.89</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Kazakhstan</td><td>2.88</td><td>0.89</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Kosovo</td><td>3.01</td><td>0.93</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Malaysia</td><td>3.24</td><td>0.95</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Netherlands</td><td>3.39</td><td>0.89</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Peru</td><td>2.25</td><td>0.69</td><td>1.00</td><td>3.0</td></tr>
    ## <tr><td style="text-align:left">Philippines</td><td>2.86</td><td>0.89</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Poland</td><td>2.67</td><td>0.95</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Romania</td><td>3.21</td><td>0.95</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Russia</td><td>3.00</td><td>1.24</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Saudi Arabia</td><td>3.06</td><td>0.71</td><td>1.00</td><td>4.0</td></tr>
    ## <tr><td style="text-align:left">Serbia</td><td>2.79</td><td>0.94</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Singapore</td><td>2.73</td><td>0.86</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">South Africa</td><td>3.01</td><td>1.04</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">South Korea</td><td>2.50</td><td>0.87</td><td>1.50</td><td>3.0</td></tr>
    ## <tr><td style="text-align:left">Spain</td><td>2.91</td><td>1.00</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Turkey</td><td>2.93</td><td>0.77</td><td>1.00</td><td>4.0</td></tr>
    ## <tr><td style="text-align:left">Ukraine</td><td>2.83</td><td>0.85</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">United Kingdom</td><td>3.13</td><td>1.06</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">United States</td><td>2.79</td><td>1.00</td><td>1.00</td><td>5.0</td></tr>
    ## <tr><td style="text-align:left">Vietnam</td><td>3.00</td><td>1.41</td><td>2.00</td><td>4.0</td></tr>
    ## <tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr></table>
