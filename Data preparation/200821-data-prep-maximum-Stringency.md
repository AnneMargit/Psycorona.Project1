Data prep maximum stringency
================
Anne Margit
8/21/2020

``` r
load("data_long_min3_str_age.Rdata")
```

``` r
library(pracma)
library(dplyr)
library(tidyverse)
library(anytime)
library(lubridate)
```

Order by Stringency and Date and select rows with first day of maximum
Stringency for each country

``` r
data_long_min3_str_age <- as_tibble(data_long_min3_str_age)

data_long_min3_str_age$Date <- as.Date(data_long_min3_str_age$Date)

data_long_min3_str_age$Country <- as.factor(data_long_min3_str_age$Country)

data_long_min3_str_age <- data_long_min3_str_age[with(data_long_min3_str_age, order(StringencyIndex, Date)),]

MaxStr <- data_long_min3_str_age %>% 
dplyr::group_by(Country) %>%
arrange(desc(c(StringencyIndex))) %>%
slice(1) %>%
ungroup()
```

Rename, select columns and merge with original data

``` r
MaxStr$MaxStr <- MaxStr$StringencyIndex

MaxStr <- as_tibble(MaxStr)

MaxStr2 <- MaxStr %>%
dplyr::mutate(NewDate = date(Date))

MaxStr2 <- MaxStr2 %>%
dplyr::select(c("ID", "MaxStr", "Date", "NewDate"))

data2 <- full_join(data_long_min3_str_age, MaxStr2, by=c("ID", "Date"))
```

Fill other rows of NewDate with date on which max Stringency was reached
for respective country

``` r
data3 <- data2 %>%
group_by(Country) %>%
fill(NewDate, .direction = "downup")
```

Calculate difference in days between date of maximum Stringency and
current date

``` r
data4 <- data3 %>%
group_by(Country) %>%
arrange(Date) %>%
mutate(Newest = difftime(Date, NewDate, units = "days"))
```

Drop not used columns

``` r
data_long_min3_str_age_max <- data4 %>%
  select(-c("MaxStr", "NewDate"))
```

Rename

``` r
data_long_min3_str_age_max <- data_long_min3_str_age_max %>%
  rename("DaysMax" = "Newest")
```

This file contains the new X axis on which day of maximum Stringency of
the country is coded as
0

``` r
data_long_min3_str_age_max$DaysMax <- as.numeric(data_long_min3_str_age_max$DaysMax)

save(data_long_min3_str_age_max, file = "data_long_min3_str_age_max.Rdata")
```