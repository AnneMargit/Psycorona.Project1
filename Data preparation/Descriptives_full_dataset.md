Descriptives full dataset
================
Anne Margit
1/17/2021

``` r
library(dplyr)
library(tidyverse)
library(stringr)
library(ggpubr)
library(ggplot2)
library(papaja)
```

``` r
load("data_long_happy.Rdata")
```

``` r
#Missing data
missing.values <- data_long_happy %>%
  gather(key = "key", value = "val") %>%
  dplyr::mutate(is.missing = is.na(val)) %>%
  dplyr::group_by(key, is.missing) %>%
  dplyr::summarise(num.missing = n()) %>%
  dplyr::filter(is.missing==T) %>%
  dplyr::select(-is.missing) %>%
  dplyr::arrange(desc(num.missing))
```

    ## Warning: attributes are not identical across measure variables;
    ## they will be dropped

``` r
apa_table(missing.values, caption="Missing data")
```

<caption>

(\#tab:unnamed-chunk-4)

</caption>

<div data-custom-style="Table Caption">

*Missing data*

</div>

| key            | num.missing |
| :------------- | :---------- |
| Close1         | 760809      |
| Close2         | 757859      |
| Close3         | 756945      |
| Close5         | 749281      |
| Close4         | 746268      |
| Ang            | 702224      |
| Lov            | 702220      |
| Exc            | 699512      |
| Content        | 699387      |
| Happy          | 673564      |
| Close6         | 670258      |
| Bored          | 660802      |
| RecordedDate   | 640605      |
| Insp           | 639801      |
| Energ          | 639735      |
| Exh            | 639716      |
| Depr           | 639649      |
| Rel            | 639643      |
| Nerv           | 639569      |
| Anxiety        | 639506      |
| Calm           | 639485      |
| Citizen        | 57696       |
| countryCitizen | 7116        |
| edu            | 5112        |
| age            | 4320        |
| gender         | 3936        |

**Number of participants and measurements per wave with original data**
Create new variable that indicates sum of missings:

``` r
data_long_happy <- data_long_happy %>%
  group_by(X, Wave) %>%
mutate(Nmiss = sum(is.na(Ang)) + sum(is.na(Anxiety)) + sum(is.na(Nerv)) + sum(is.na(Depr)) + sum(is.na(Exh)) + 
               sum(is.na(Energ)) + sum(is.na(Insp)) + sum(is.na(Calm)) + sum(is.na(Rel)) +  sum(is.na(Happy))) %>%
  ungroup()
```

``` r
data_long_happy$Wave <- as.factor(data_long_happy$Wave)

Wave_N <- data_long_happy %>% 
group_by(Wave)%>%
summarise(NParticipants = n_distinct(X), NWave = sum (!is.na(RecordedDate)), NAng= sum(!is.na(Ang)), NAnx= sum(!is.na(Anxiety)), NNerv= sum(!is.na(Nerv)), NDepr= sum(!is.na(Depr)), NExh= sum(!is.na(Exh)), NEnerg= sum(!is.na(Energ)),  NInsp= sum(!is.na(Insp)), NCalm= sum(!is.na(Calm)), NRel= sum(!is.na(Rel)), NHappy = sum(!is.na(Happy)))
```

``` r
apa_table(Wave_N, caption="Number of measurements per wave original data")
```

<caption>

(\#tab:unnamed-chunk-7)

</caption>

<div data-custom-style="Table Caption">

*Number of measurements per wave original
data*

</div>

| Wave | NParticipants | NWave | NAng | NAnx  | NNerv | NDepr | NExh  | NEnerg | NInsp | NCalm | NRel  | NHappy |
| :--- | :------------ | :---- | :--- | :---- | :---- | :---- | :---- | :----- | :---- | :---- | :---- | :----- |
| w0   | 63495         | 61385 | 0    | 62706 | 62647 | 62571 | 62519 | 62507  | 62466 | 62696 | 62570 | 62676  |
| w1   | 63495         | 1511  | 1508 | 1507  | 1507  | 1508  | 1507  | 1508   | 1506  | 1509  | 1507  | 0      |
| w2   | 63495         | 6269  | 6256 | 6258  | 6258  | 6257  | 6255  | 6255   | 6253  | 6259  | 6259  | 0      |
| w3   | 63495         | 5561  | 5542 | 5542  | 5544  | 5539  | 5542  | 5541   | 5538  | 5541  | 5541  | 0      |
| w4   | 63495         | 8031  | 7988 | 7996  | 7997  | 7993  | 7988  | 7989   | 7988  | 7999  | 7994  | 8030   |
| w5   | 63495         | 7370  | 7341 | 7345  | 7342  | 7344  | 7345  | 7343   | 7343  | 7354  | 7347  | 7364   |
| w6   | 63495         | 6564  | 6527 | 6537  | 6530  | 6531  | 6530  | 6522   | 6522  | 6539  | 6529  | 0      |
| w7   | 63495         | 5321  | 5289 | 5294  | 5292  | 5290  | 5288  | 5291   | 5285  | 5300  | 5291  | 0      |
| w8   | 63495         | 5361  | 5340 | 5335  | 5340  | 5342  | 5338  | 5342   | 5336  | 5342  | 5342  | 5356   |
| w9   | 63495         | 4858  | 4839 | 4837  | 4840  | 4839  | 4838  | 4836   | 4834  | 4843  | 4842  | 0      |
| w10  | 63495         | 4151  | 4148 | 4146  | 4146  | 4145  | 4145  | 4142   | 4142  | 4142  | 4143  | 0      |
| w11  | 63495         | 4953  | 4938 | 4931  | 4928  | 4932  | 4929  | 4929   | 4926  | 4931  | 4932  | 4950   |

Add number of measurements

``` r
data_long_happy$RecordedDate <- as.Date(data_long_happy$RecordedDate)
data_long_happy <- data_long_happy %>% group_by(X) %>% add_tally(wt = !is.na(RecordedDate))
```

Number of participants per number of complete assessments

``` r
N_measures <- data_long_happy %>%
group_by(n)%>%
summarize(n_distinct(X))
```

``` r
apa_table(N_measures, caption="Number of participants per number of complete assessments")
```

<caption>

(\#tab:unnamed-chunk-10)

</caption>

<div data-custom-style="Table Caption">

*Number of participants per number of complete assessments*

</div>

| n  | n\_distinct(X) |
| :- | :------------- |
| 0  | 2102           |
| 1  | 43438          |
| 2  | 6530           |
| 3  | 3246           |
| 4  | 1945           |
| 5  | 1303           |
| 6  | 953            |
| 7  | 921            |
| 8  | 944            |
| 9  | 940            |
| 10 | 470            |
| 11 | 509            |
| 12 | 194            |
