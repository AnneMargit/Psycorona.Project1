Second analyses data prep
================
Anne Margit
10/04/2020

    ## [1] ""

``` r
load("data_analyse1_fc.Rdata")
```

This dataset includes:

1.  Data from all weekly measurement waves (baseline through wave 11,
    Time 1 through 12)
2.  Participants who provided at least 3 measurements
3.  Participants who are residents of the country they currently live in
4.  Participants who provided info on age
5.  Participants who provided info on gender (either male or female)
6.  Data from countries with at least 20 participants
7.  Pooled age groups
8.  Imputed missing emotion scores
9.  Combined emotion scores (NAA, NAD, PAA, PAD)
10. An imputed Stringency index (StringencyIndex\_imp)
11. A variable indicating the number of days before and after the day on
    which maximum stringency was reached for the respective country
    (DaysMax)
12. A variable indicating the number of weeks before and after the day
    on which maximum stringency was reached for the respective country
    (WeeksMax)
13. A variable indicating the date on which maximum Stringency was
    reached for that country (DateMaxStr)
14. A dummy Str\_dummy with 0 = before the peaj, 1 = during peak, 2 =
    after peak
15. Observations during which there was a second peak are excluded
    (N=583)

> My comments are in block quotes such as this.

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
```

# Subset data based on phase of stringency

``` r
data_analyse2_p1 <- data_analyse1_fc %>%
  filter(Str_dummy == 0)

data_analyse2_p2 <- data_analyse1_fc %>%
  filter(Str_dummy == 1)

data_analyse2_p3 <- data_analyse1_fc %>%
  filter(Str_dummy == 2)
```

# Make new DaysMax and WeekMax variables for each phase so that they start at 1 on the first day of the phase and then increase until the end of the phase

**Phase 1**

``` r
data_analyse2_p1 <- data_analyse2_p1[with(data_analyse2_p1, order(Date)),]

data_analyse2_p1 <- data_analyse2_p1 %>%
  mutate(DaysMax_p1 = round(difftime(Date, DateMaxStr, units = "days"), digits=0))

data_analyse2_p1 <- data_analyse2_p1 %>%
  mutate(WeeksMax_p1 = round(difftime(Date, DateMaxStr, units = "weeks"), digits=0))

data_analyse2_p1$DaysMax_p1 <- as.numeric(data_analyse2_p1$DaysMax_p1)

data_analyse2_p1$Gender<- as_factor(data_analyse2_p1$Gender)
data_analyse2_p1$Edu<- as_factor(data_analyse2_p1$Edu)

save(data_analyse2_p1, file="data_analyse2_p1.Rdata")
```

**Phase 2**

``` r
data_analyse2_p2 <- data_analyse2_p2 %>%
  mutate(DaysMax_p2 = round(difftime(Date, DateMaxStr, units = "days"), digits=0))

data_analyse2_p2 <- data_analyse2_p2 %>%
  mutate(WeeksMax_p2 = round(difftime(Date, DateMaxStr, units = "weeks"), digits=0))

data_analyse2_p2$DaysMax_p2 <- as.numeric(data_analyse2_p2$DaysMax_p2)

data_analyse2_p2$Gender<- as_factor(data_analyse2_p2$Gender)
data_analyse2_p2$Edu<- as_factor(data_analyse2_p2$Edu)

save(data_analyse2_p2, file="data_analyse2_p2.Rdata")
```

**Phase 3**

``` r
StartPhase3 <- data_analyse2_p3 %>% 
group_by(Country) %>%
arrange(Date) %>%
slice(1) %>%
ungroup()

StartPhase3$StartPhase3 <- StartPhase3$Date

StartPhase3_2 <- StartPhase3 %>%
dplyr::select(c("ID", "Date", "StartPhase3"))

StartPhase3_3 <- left_join(data_analyse2_p3, StartPhase3_2, by=c("ID", "Date"))

StartPhase3_4 <- StartPhase3_3 %>%
group_by(Country) %>%
fill(StartPhase3, .direction = "downup")

StartPhase3_5 <- StartPhase3_4 %>%
mutate(DaysPhase3 = round(difftime(Date, StartPhase3, units = "days"), digits=0))

data_analyse2_p3 <- StartPhase3_5 %>%
mutate(WeeksPhase3 = round(difftime(Date, StartPhase3, units = "weeks"), digits=0))

data_analyse2_p3$DaysPhase3 <- as.numeric(data_analyse2_p3$DaysPhase3)

data_analyse2_p3$Gender<- as_factor(data_analyse2_p3$Gender)
data_analyse2_p3$Edu<- as_factor(data_analyse2_p3$Edu)

save(data_analyse2_p3, file="data_analyse2_p3.Rdata")
```
