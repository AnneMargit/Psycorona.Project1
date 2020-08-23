200821 Plots of emotions against stringency or time for different age
groups
================
Anne Margit
8/21/2020

*New output of plots to show emotions against time (date), and against
days before/ after maximum stringency, pooled for the 4 age groups*

This dataset includes measurements from participants that (1) provided
at least 3 measurements, (2) that are residents of the country they
currently live in, (3) from countries with at least 20 participants, (4)
provided data on age, and (5) with imputed Stringency index values that
are (6) centered around country means and (7) a variable called DaysMax
that indicates the days preceding or leading up to the day on which
maximum stringency was reached for the respective country, and (8) from
all waves (baseline through wave 11) for (9) the pooled age groups

\!\!\! DaysMax = 0: Day on which maximum stringency was reached for
country in which participant lives. This is based on available
Stringency Index data, not imputed scores \!\!\!

``` r
load("data_long_min3_str_age_max.Rdata")
```

# Anger

**Median anger against date**

``` r
plot_ang <- ggplot(data_long_min3_str_age_max, aes(x=Date, y=Ang))

plot_ang +  stat_summary(geom = "line", fun = median, size=1) + stat_summary(fun.data = median_hilow, 
                                                                    fun.args = (conf.int=.5))
```

![](200821-Plots-of-emotions-against-stringency-or-time-for-different-age-groups_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

**Median anger against wave**

``` r
plot_ang <- ggplot(data_long_min3_str_age_max, aes(x=Time, y=Ang))

plot_ang +  stat_summary(geom = "line", fun = median, size=1) + stat_summary(fun.data = median_hilow, 
                                                                    fun.args = (conf.int=.5))
```

![](200821-Plots-of-emotions-against-stringency-or-time-for-different-age-groups_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

**Median anger against maximum stringency**

``` r
plot_ang <- ggplot(data_long_min3_str_age_max, aes(x=DaysMax, y=Ang))

plot_ang +  stat_summary(geom = "line", fun = median, size=1) + stat_summary(fun.data = median_hilow, 
                                                                    fun.args = (conf.int=.5))
```

![](200821-Plots-of-emotions-against-stringency-or-time-for-different-age-groups_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

**Mean anger against date in different age
groups**

``` r
plot_ang <- ggplot(data_long_min3_str_age_max, aes(x=Date, y=Ang, group = Age_new, color = Age_new))

plot_ang +  stat_summary(geom = "line", fun = mean, size=1) + scale_colour_discrete(name = "Age", 
labels = c("18-24", "25-44", "45-64", "65+"))
```

![](200821-Plots-of-emotions-against-stringency-or-time-for-different-age-groups_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

**Mean anger against wave in different age
groups**

``` r
plot_ang <- ggplot(data_long_min3_str_age_max, aes(x=Time, y=Ang, group = Age_new, color = Age_new))

plot_ang +  stat_summary(geom = "line", fun = mean, size=1) + scale_colour_discrete(name = "Age", 
labels = c("18-24", "25-44", "45-64", "65+"))
```

![](200821-Plots-of-emotions-against-stringency-or-time-for-different-age-groups_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

**Mean anger against maximum stringency in different age
groups**

``` r
plot_ang <- ggplot(data_long_min3_str_age_max, aes(x=DaysMax, y=Ang, group = Age_new, color = Age_new))

plot_ang +  stat_summary(geom = "line", fun = mean, size=1) + scale_colour_discrete(name = "Age", 
labels = c("18-24", "25-44", "45-64", "65+"))
```

![](200821-Plots-of-emotions-against-stringency-or-time-for-different-age-groups_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

# Anxiety

**Median anxiety against date**

``` r
plot_anx <- ggplot(data_long_min3_str_age_max, aes(x=Date, y=Anxiety))

plot_anx +  stat_summary(geom = "line", fun = median, size=1) + stat_summary(fun.data = median_hilow, 
                                                                    fun.args = (conf.int=.5))
```

![](200821-Plots-of-emotions-against-stringency-or-time-for-different-age-groups_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

**Median anxiety against wave**

``` r
plot_anx <- ggplot(data_long_min3_str_age_max, aes(x=Time, y=Anxiety))

plot_anx +  stat_summary(geom = "line", fun = median, size=1) + stat_summary(fun.data = median_hilow, 
                                                                    fun.args = (conf.int=.5))
```

![](200821-Plots-of-emotions-against-stringency-or-time-for-different-age-groups_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

**Median anxiety against maximum
stringency**

``` r
plot_anx <- ggplot(data_long_min3_str_age_max, aes(x=DaysMax, y=Anxiety))

plot_anx +  stat_summary(geom = "line", fun = median, size=1) + stat_summary(fun.data = median_hilow, 
                                                                    fun.args = (conf.int=.5))
```

![](200821-Plots-of-emotions-against-stringency-or-time-for-different-age-groups_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

**Mean anxiety against date in different age
groups**

``` r
plot_anx <- ggplot(data_long_min3_str_age_max, aes(x=Date, y=Anxiety, group = Age_new, color = Age_new))

plot_anx +  stat_summary(geom = "line", fun = mean, size=1) + scale_colour_discrete(name = "Age", 
labels = c("18-24", "25-44", "45-64", "65+"))
```

![](200821-Plots-of-emotions-against-stringency-or-time-for-different-age-groups_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->
**Mean anxiety against wave in different age
groups**

``` r
plot_anx <- ggplot(data_long_min3_str_age_max, aes(x=Time, y=Anxiety, group = Age_new, color = Age_new))

plot_anx +  stat_summary(geom = "line", fun = mean, size=1) + scale_colour_discrete(name = "Age", 
labels = c("18-24", "25-44", "45-64", "65+"))
```

![](200821-Plots-of-emotions-against-stringency-or-time-for-different-age-groups_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

**Mean anxiety against maximum stringency in different age
groups**

``` r
plot_anx <- ggplot(data_long_min3_str_age_max, aes(x=DaysMax, y=Anxiety, group = Age_new, color = Age_new))

plot_anx +  stat_summary(geom = "line", fun = mean, size=1) + scale_colour_discrete(name = "Age", 
labels = c("18-24", "25-44", "45-64", "65+"))
```

![](200821-Plots-of-emotions-against-stringency-or-time-for-different-age-groups_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

# Nervous

**Median nervous against date**

``` r
plot_nerv <- ggplot(data_long_min3_str_age_max, aes(x=Date, y=Nerv))

plot_nerv +  stat_summary(geom = "line", fun = median, size=1) + stat_summary(fun.data = median_hilow, 
                                                                    fun.args = (conf.int=.5))
```

![](200821-Plots-of-emotions-against-stringency-or-time-for-different-age-groups_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

**Median nervous against wave**

``` r
plot_nerv <- ggplot(data_long_min3_str_age_max, aes(x=Time, y=Nerv))

plot_nerv +  stat_summary(geom = "line", fun = median, size=1) + stat_summary(fun.data = median_hilow, 
                                                                    fun.args = (conf.int=.5))
```

![](200821-Plots-of-emotions-against-stringency-or-time-for-different-age-groups_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

**Median nervous against maximum stringency**

``` r
plot_nerv <- ggplot(data_long_min3_str_age_max, aes(x=DaysMax, y=Nerv))

plot_nerv +  stat_summary(geom = "line", fun = median, size=1) + stat_summary(fun.data = median_hilow, 
                                                                    fun.args = (conf.int=.5))
```

![](200821-Plots-of-emotions-against-stringency-or-time-for-different-age-groups_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

**Mean nervous against date in different age
groups**

``` r
plot_nerv <- ggplot(data_long_min3_str_age_max, aes(x=Date, y=Nerv, group = Age_new, color = Age_new))

plot_nerv +  stat_summary(geom = "line", fun = mean, size=1) + scale_colour_discrete(name = "Age", 
labels = c("18-24", "25-44", "45-64", "65+"))
```

![](200821-Plots-of-emotions-against-stringency-or-time-for-different-age-groups_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

**Mean nervous against wave in different age
groups**

``` r
plot_nerv <- ggplot(data_long_min3_str_age_max, aes(x=Time, y=Nerv, group = Age_new, color = Age_new))

plot_nerv +  stat_summary(geom = "line", fun = mean, size=1) + scale_colour_discrete(name = "Age", 
labels = c("18-24", "25-44", "45-64", "65+"))
```

![](200821-Plots-of-emotions-against-stringency-or-time-for-different-age-groups_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

**Mean nervous against maximum stringency in different age
groups**

``` r
plot_nerv <- ggplot(data_long_min3_str_age_max, aes(x=DaysMax, y=Nerv, group = Age_new, color = Age_new))

plot_nerv +  stat_summary(geom = "line", fun = mean, size=1) + scale_colour_discrete(name = "Age", 
labels = c("18-24", "25-44", "45-64", "65+"))
```

![](200821-Plots-of-emotions-against-stringency-or-time-for-different-age-groups_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

# Depressed

**Median depressed against date**

``` r
plot_dep <- ggplot(data_long_min3_str_age_max, aes(x=Date, y=Depr))

plot_dep +  stat_summary(geom = "line", fun = median, size=1) + stat_summary(fun.data = median_hilow, 
                                                                    fun.args = (conf.int=.5))
```

![](200821-Plots-of-emotions-against-stringency-or-time-for-different-age-groups_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

**Median depressed against wave**

``` r
plot_dep <- ggplot(data_long_min3_str_age_max, aes(x=Time, y=Depr))

plot_dep +  stat_summary(geom = "line", fun = median, size=1) + stat_summary(fun.data = median_hilow, 
                                                                    fun.args = (conf.int=.5))
```

![](200821-Plots-of-emotions-against-stringency-or-time-for-different-age-groups_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

**Median depressed against maximum stringency**

``` r
plot_dep <- ggplot(data_long_min3_str_age_max, aes(x=DaysMax, y=Depr))

plot_dep +  stat_summary(geom = "line", fun = median, size=1) + stat_summary(fun.data = median_hilow, 
                                                                    fun.args = (conf.int=.5))
```

![](200821-Plots-of-emotions-against-stringency-or-time-for-different-age-groups_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

**Mean depressed against date in different age
groups**

``` r
plot_dep <- ggplot(data_long_min3_str_age_max, aes(x=Date, y=Depr, group = Age_new, color = Age_new))

plot_dep +  stat_summary(geom = "line", fun = mean, size=1) + scale_colour_discrete(name = "Age", 
labels = c("18-24", "25-44", "45-64", "65+"))
```

![](200821-Plots-of-emotions-against-stringency-or-time-for-different-age-groups_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

**Mean depressed against wave in different age
groups**

``` r
plot_dep <- ggplot(data_long_min3_str_age_max, aes(x=Time, y=Depr, group = Age_new, color = Age_new))

plot_dep +  stat_summary(geom = "line", fun = mean, size=1) + scale_colour_discrete(name = "Age", 
labels = c("18-24", "25-44", "45-64", "65+"))
```

![](200821-Plots-of-emotions-against-stringency-or-time-for-different-age-groups_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

**Mean depressed against maximum stringency in different age
groups**

``` r
plot_dep <- ggplot(data_long_min3_str_age_max, aes(x=DaysMax, y=Depr, group = Age_new, color = Age_new))

plot_dep +  stat_summary(geom = "line", fun = mean, size=1) + scale_colour_discrete(name = "Age", 
labels = c("18-24", "25-44", "45-64", "65+"))
```

![](200821-Plots-of-emotions-against-stringency-or-time-for-different-age-groups_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

# Exhausted

**Median exhausted against date**

``` r
plot_dep <- ggplot(data_long_min3_str_age_max, aes(x=Date, y=Exh))

plot_dep +  stat_summary(geom = "line", fun = median, size=1) + stat_summary(fun.data = median_hilow, 
                                                                    fun.args = (conf.int=.5))
```

![](200821-Plots-of-emotions-against-stringency-or-time-for-different-age-groups_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

**Median exhausted against wave**

``` r
plot_dep <- ggplot(data_long_min3_str_age_max, aes(x=Time, y=Exh))

plot_dep +  stat_summary(geom = "line", fun = median, size=1) + stat_summary(fun.data = median_hilow, 
                                                                    fun.args = (conf.int=.5))
```

![](200821-Plots-of-emotions-against-stringency-or-time-for-different-age-groups_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->

**Median exhausted against maximum stringency**

``` r
plot_exh <- ggplot(data_long_min3_str_age_max, aes(x=DaysMax, y=Exh))

plot_exh +  stat_summary(geom = "line", fun = median, size=1) + stat_summary(fun.data = median_hilow, 
                                                                    fun.args = (conf.int=.5))
```

![](200821-Plots-of-emotions-against-stringency-or-time-for-different-age-groups_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->

**Mean exhausted against date in different age
groups**

``` r
plot_exh <- ggplot(data_long_min3_str_age_max, aes(x=Date, y=Exh, group = Age_new, color = Age_new))

plot_exh +  stat_summary(geom = "line", fun = mean, size=1) + scale_colour_discrete(name = "Age", 
labels = c("18-24", "25-44", "45-64", "65+"))
```

![](200821-Plots-of-emotions-against-stringency-or-time-for-different-age-groups_files/figure-gfm/unnamed-chunk-30-1.png)<!-- -->

**Mean exhausted against wave in different age
groups**

``` r
plot_exh <- ggplot(data_long_min3_str_age_max, aes(x=Time, y=Exh, group = Age_new, color = Age_new))

plot_exh +  stat_summary(geom = "line", fun = mean, size=1) + scale_colour_discrete(name = "Age", 
labels = c("18-24", "25-44", "45-64", "65+"))
```

![](200821-Plots-of-emotions-against-stringency-or-time-for-different-age-groups_files/figure-gfm/unnamed-chunk-31-1.png)<!-- -->

**Mean exhausted against maximum stringency in different age
groups**

``` r
plot_exh <- ggplot(data_long_min3_str_age_max, aes(x=DaysMax, y=Exh, group = Age_new, color = Age_new))

plot_exh +  stat_summary(geom = "line", fun = mean, size=1) + scale_colour_discrete(name = "Age", 
labels = c("18-24", "25-44", "45-64", "65+"))
```

![](200821-Plots-of-emotions-against-stringency-or-time-for-different-age-groups_files/figure-gfm/unnamed-chunk-32-1.png)<!-- -->

# Energetic

**Median energetic against date**

``` r
plot_energ <- ggplot(data_long_min3_str_age_max, aes(x=Date, y=Energ))

plot_energ +  stat_summary(geom = "line", fun = median, size=1) + stat_summary(fun.data = median_hilow, 
                                                                    fun.args = (conf.int=.5))
```

![](200821-Plots-of-emotions-against-stringency-or-time-for-different-age-groups_files/figure-gfm/unnamed-chunk-33-1.png)<!-- -->

**Median energetic against wave**

``` r
plot_energ <- ggplot(data_long_min3_str_age_max, aes(x=Time, y=Energ))

plot_energ +  stat_summary(geom = "line", fun = median, size=1) + stat_summary(fun.data = median_hilow, 
                                                                    fun.args = (conf.int=.5))
```

![](200821-Plots-of-emotions-against-stringency-or-time-for-different-age-groups_files/figure-gfm/unnamed-chunk-34-1.png)<!-- -->

**Median energetic against maximum
stringency**

``` r
plot_energ <- ggplot(data_long_min3_str_age_max, aes(x=DaysMax, y=Energ))

plot_energ +  stat_summary(geom = "line", fun = median, size=1) + stat_summary(fun.data = median_hilow, 
                                                                    fun.args = (conf.int=.5))
```

![](200821-Plots-of-emotions-against-stringency-or-time-for-different-age-groups_files/figure-gfm/unnamed-chunk-35-1.png)<!-- -->

**Mean energetic against date in different age
groups**

``` r
plot_energ <- ggplot(data_long_min3_str_age_max, aes(x=Date, y=Energ, group = Age_new, color = Age_new))

plot_energ +  stat_summary(geom = "line", fun = mean, size=1) + scale_colour_discrete(name = "Age", 
labels = c("18-24", "25-44", "45-64", "65+"))
```

![](200821-Plots-of-emotions-against-stringency-or-time-for-different-age-groups_files/figure-gfm/unnamed-chunk-36-1.png)<!-- -->

**Mean energetic against wave in different age
groups**

``` r
plot_energ <- ggplot(data_long_min3_str_age_max, aes(x=Time, y=Energ, group = Age_new, color = Age_new))

plot_energ +  stat_summary(geom = "line", fun = mean, size=1) + scale_colour_discrete(name = "Age", 
labels = c("18-24", "25-44", "45-64", "65+"))
```

![](200821-Plots-of-emotions-against-stringency-or-time-for-different-age-groups_files/figure-gfm/unnamed-chunk-37-1.png)<!-- -->

**Mean energetic against maximum stringency in different age
groups**

``` r
plot_energ <- ggplot(data_long_min3_str_age_max, aes(x=DaysMax, y=Energ, group = Age_new, color = Age_new))

plot_energ +  stat_summary(geom = "line", fun = mean, size=1) + scale_colour_discrete(name = "Age", 
labels = c("18-24", "25-44", "45-64", "65+"))
```

![](200821-Plots-of-emotions-against-stringency-or-time-for-different-age-groups_files/figure-gfm/unnamed-chunk-38-1.png)<!-- -->

# Inspired

**Median inspired against date**

``` r
plot_insp <- ggplot(data_long_min3_str_age_max, aes(x=Date, y=Insp))

plot_insp +  stat_summary(geom = "line", fun = median, size=1) + stat_summary(fun.data = median_hilow, 
                                                                    fun.args = (conf.int=.5))
```

![](200821-Plots-of-emotions-against-stringency-or-time-for-different-age-groups_files/figure-gfm/unnamed-chunk-39-1.png)<!-- -->

**Median inspired against wave**

``` r
plot_insp <- ggplot(data_long_min3_str_age_max, aes(x=Time, y=Insp))

plot_insp +  stat_summary(geom = "line", fun = median, size=1) + stat_summary(fun.data = median_hilow, 
                                                                    fun.args = (conf.int=.5))
```

![](200821-Plots-of-emotions-against-stringency-or-time-for-different-age-groups_files/figure-gfm/unnamed-chunk-40-1.png)<!-- -->

**Median inspired against maximum stringency**

``` r
plot_insp <- ggplot(data_long_min3_str_age_max, aes(x=DaysMax, y=Insp))

plot_insp +  stat_summary(geom = "line", fun = median, size=1) + stat_summary(fun.data = median_hilow, 
                                                                    fun.args = (conf.int=.5))
```

![](200821-Plots-of-emotions-against-stringency-or-time-for-different-age-groups_files/figure-gfm/unnamed-chunk-41-1.png)<!-- -->

**Mean inspired against date in different age
groups**

``` r
plot_insp <- ggplot(data_long_min3_str_age_max, aes(x=Date, y=Insp, group = Age_new, color = Age_new))

plot_insp +  stat_summary(geom = "line", fun = mean, size=1) + scale_colour_discrete(name = "Age", 
labels = c("18-24", "25-44", "45-64", "65+"))
```

![](200821-Plots-of-emotions-against-stringency-or-time-for-different-age-groups_files/figure-gfm/unnamed-chunk-42-1.png)<!-- -->

**Mean inspired against wave in different age
groups**

``` r
plot_insp <- ggplot(data_long_min3_str_age_max, aes(x=Time, y=Insp, group = Age_new, color = Age_new))

plot_insp +  stat_summary(geom = "line", fun = mean, size=1) + scale_colour_discrete(name = "Age", 
labels = c("18-24", "25-44", "45-64", "65+"))
```

![](200821-Plots-of-emotions-against-stringency-or-time-for-different-age-groups_files/figure-gfm/unnamed-chunk-43-1.png)<!-- -->

**Mean inspired against maximum stringency in different age
groups**

``` r
plot_insp <- ggplot(data_long_min3_str_age_max, aes(x=DaysMax, y=Insp, group = Age_new, color = Age_new))

plot_insp +  stat_summary(geom = "line", fun = mean, size=1) + scale_colour_discrete(name = "Age", 
labels = c("18-24", "25-44", "45-64", "65+"))
```

![](200821-Plots-of-emotions-against-stringency-or-time-for-different-age-groups_files/figure-gfm/unnamed-chunk-44-1.png)<!-- -->

# Calm

**Median calm against date**

``` r
plot_calm <- ggplot(data_long_min3_str_age_max, aes(x=Date, y=Calm))

plot_calm +  stat_summary(geom = "line", fun = median, size=1) + stat_summary(fun.data = median_hilow, 
                                                                    fun.args = (conf.int=.5))
```

![](200821-Plots-of-emotions-against-stringency-or-time-for-different-age-groups_files/figure-gfm/unnamed-chunk-45-1.png)<!-- -->

**Median calm against wave**

``` r
plot_calm <- ggplot(data_long_min3_str_age_max, aes(x=Time, y=Calm))

plot_calm +  stat_summary(geom = "line", fun = median, size=1) + stat_summary(fun.data = median_hilow, 
                                                                    fun.args = (conf.int=.5))
```

![](200821-Plots-of-emotions-against-stringency-or-time-for-different-age-groups_files/figure-gfm/unnamed-chunk-46-1.png)<!-- -->

**Median calm against maximum stringency**

``` r
plot_calm <- ggplot(data_long_min3_str_age_max, aes(x=DaysMax, y=Calm))

plot_calm +  stat_summary(geom = "line", fun = median, size=1) + stat_summary(fun.data = median_hilow, 
                                                                    fun.args = (conf.int=.5))
```

![](200821-Plots-of-emotions-against-stringency-or-time-for-different-age-groups_files/figure-gfm/unnamed-chunk-47-1.png)<!-- -->

**Mean calm against date in different age
groups**

``` r
plot_calm <- ggplot(data_long_min3_str_age_max, aes(x=Date, y=Calm, group = Age_new, color = Age_new))

plot_calm +  stat_summary(geom = "line", fun = mean, size=1) + scale_colour_discrete(name = "Age", 
labels = c("18-24", "25-44", "45-64", "65+"))
```

![](200821-Plots-of-emotions-against-stringency-or-time-for-different-age-groups_files/figure-gfm/unnamed-chunk-48-1.png)<!-- -->

**Mean calm against wave in different age
groups**

``` r
plot_calm <- ggplot(data_long_min3_str_age_max, aes(x=Time, y=Calm, group = Age_new, color = Age_new))

plot_calm +  stat_summary(geom = "line", fun = mean, size=1) + scale_colour_discrete(name = "Age", 
labels = c("18-24", "25-44", "45-64", "65+"))
```

![](200821-Plots-of-emotions-against-stringency-or-time-for-different-age-groups_files/figure-gfm/unnamed-chunk-49-1.png)<!-- -->

**Mean calm against maximum stringency in different age
groups**

``` r
plot_calm <- ggplot(data_long_min3_str_age_max, aes(x=DaysMax, y=Calm, group = Age_new, color = Age_new))

plot_calm +  stat_summary(geom = "line", fun = mean, size=1) + scale_colour_discrete(name = "Age", 
labels = c("18-24", "25-44", "45-64", "65+"))
```

![](200821-Plots-of-emotions-against-stringency-or-time-for-different-age-groups_files/figure-gfm/unnamed-chunk-50-1.png)<!-- -->

# Relaxed

**Median relaxed against date**

``` r
plot_rel <- ggplot(data_long_min3_str_age_max, aes(x=Date, y=Rel))

plot_rel +  stat_summary(geom = "line", fun = median, size=1) + stat_summary(fun.data = median_hilow, 
                                                                    fun.args = (conf.int=.5))
```

![](200821-Plots-of-emotions-against-stringency-or-time-for-different-age-groups_files/figure-gfm/unnamed-chunk-51-1.png)<!-- -->

**Median relaxed against wave**

``` r
plot_rel <- ggplot(data_long_min3_str_age_max, aes(x=Time, y=Rel))

plot_rel +  stat_summary(geom = "line", fun = median, size=1) + stat_summary(fun.data = median_hilow, 
                                                                    fun.args = (conf.int=.5))
```

![](200821-Plots-of-emotions-against-stringency-or-time-for-different-age-groups_files/figure-gfm/unnamed-chunk-52-1.png)<!-- -->

**Median relaxed against maximum stringency**

``` r
plot_rel <- ggplot(data_long_min3_str_age_max, aes(x=DaysMax, y=Rel))

plot_rel +  stat_summary(geom = "line", fun = median, size=1) + stat_summary(fun.data = median_hilow, 
                                                                    fun.args = (conf.int=.5))
```

![](200821-Plots-of-emotions-against-stringency-or-time-for-different-age-groups_files/figure-gfm/unnamed-chunk-53-1.png)<!-- -->

**Mean relaxed against date in different age
groups**

``` r
plot_rel <- ggplot(data_long_min3_str_age_max, aes(x=Date, y=Rel, group = Age_new, color = Age_new))

plot_rel +  stat_summary(geom = "line", fun = mean, size=1) + scale_colour_discrete(name = "Age", 
labels = c("18-24", "25-44", "45-64", "65+"))
```

![](200821-Plots-of-emotions-against-stringency-or-time-for-different-age-groups_files/figure-gfm/unnamed-chunk-54-1.png)<!-- -->

**Mean relaxed against wave (= “Time”) in different age
groups**

``` r
plot_rel <- ggplot(data_long_min3_str_age_max, aes(x=Time, y=Rel, group = Age_new, color = Age_new))

plot_rel +  stat_summary(geom = "line", fun = mean, size=1) + scale_colour_discrete(name = "Age", 
labels = c("18-24", "25-44", "45-64", "65+"))
```

![](200821-Plots-of-emotions-against-stringency-or-time-for-different-age-groups_files/figure-gfm/unnamed-chunk-55-1.png)<!-- -->

**Mean relaxed against maximum stringency in different age
groups**

``` r
plot_rel <- ggplot(data_long_min3_str_age_max, aes(x=DaysMax, y=Rel, group = Age_new, color = Age_new))

plot_rel +  stat_summary(geom = "line", fun = mean, size=1) + scale_colour_discrete(name = "Age", 
labels = c("18-24", "25-44", "45-64", "65+"))
```

![](200821-Plots-of-emotions-against-stringency-or-time-for-different-age-groups_files/figure-gfm/unnamed-chunk-56-1.png)<!-- -->
