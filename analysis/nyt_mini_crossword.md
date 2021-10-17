---
title: "New York Time Mini Crossword"
author: "Stephen Kaluzny"
date: "17 October, 2021"
output:
  html_document:
    keep_md: true
---



## The Puzzle

Everyday the New York Times puts out a mini crossword.
A group of us have been recording our time to complete the puzzle everytime
we complete a puzzle.
Along with the date and time we also record the time of day that we did the
puzzle.

This is an analysis of that puzzle data.
We look at puzzle completion time across the various players,
across day of the week (are the puzzles harder later in the week?)
and time of day.
This is also an example data analysis done with R in a reproducible manner.
As an example document for learning about R,
all of the R code is shown.

## Setup

We explicitly install the required R packages from CRAN if they
are not already available.


```r
if(!requireNamespace("dplyr", quietly=TRUE)) {
  install.packages("dplyr")
}
if(!requireNamespace("tidyr", quietly=TRUE)) {
  install.packages("tidyr")
}
if(!requireNamespace("ggplot2", quietly=TRUE)) {
  install.packages("ggplot2")
}
if(!requireNamespace("lubridate", quietly=TRUE)) {
  install.packages("lubridate")
}
if(!requireNamespace("hms", quietly=TRUE)) {
  install.packages("hms")
}
if(!requireNamespace("here", quietly=TRUE)) {
  install.packages("here")
}
if(!requireNamespace("assertr", quietly=TRUE)) {
  install.packages("assertr")
}
library("assertr", quietly = TRUE, warn.conflicts = FALSE)
library("tidyr", quietly = TRUE, warn.conflicts = FALSE)
library("dplyr", quietly = TRUE, warn.conflicts = FALSE)
library("ggplot2", quietly = TRUE, warn.conflicts = FALSE)
```
  
## The Data

The `here` package allows us to find files (in this case the raw data file)
anywhere within the project tree.


```r
d <- read.csv(here::here("data", "nyt.csv"), stringsAsFactors=FALSE)
```

The `Date` and `TimeOfDay` are combined and converted to a `POSIX` variable `DateTime`.
The `WeekDay` is then computed from `DateTime`.
The puzzle time (`Time`) was recorded as minutes:seconds,
we convert that to a numeric `Seconds` variable.

```r
d$Date <- as.Date(d$Date)
d$DateTime <- as.POSIXct(with(d,
  paste(Date, ifelse(is.na(TimeOfDay), "00:00", TimeOfDay))))
d$WeekDay <- lubridate::wday(d$DateTime)
d$TimeOfDayOrig <- d$TimeOfDay
d$TimeOfDay <- hms::parse_hm(d$TimeOfDay)
d$Seconds <- with(d, as.numeric(lubridate::seconds(lubridate::ms(Time))))
```

Compute the time interval, in days, between puzzle playing by `Player`.

```r
d <- d %>%
  group_by(Player) %>%
  arrange(Date) %>%
  mutate(WaitingTime = as.numeric(Date - lag(Date), units="days")) %>%
  ungroup()
```

### Data Quality Check

New data is regularly added to the CSV data file.
In this section we do a quality check on the data to make sure no
data entry errors have occurred.

First, set some values that are expected in the data:


```r
# Current list of players:
players <- c("SPK", "JAK", "JIK", "BBK", "SKK", "AKK")
# Earliest date:
first_date <- as.Date("2017-09-25")
# Upper bound on time (10 minutes = 600 seconds):
time_bound <- 600
```

Check that Player is one of 5 possible values:

```r
d %>% assert(in_set(players), Player) %>% success_logical()
```

```
## [1] TRUE
```

There should be no missing values in the data:

```r
d %>% assert(not_na, DateTime, WeekDay, Seconds) %>% success_logical()
```

```
## [1] TRUE
```

Only one observation per player per date:

```r
d %>% group_by(Date) %>% count(Player) %>% verify(n == 1) %>% success_logical()
```

```
## [1] TRUE
```

Check for proper time values:

```r
d %>% assertr::verify(Seconds > 0 & Seconds < time_bound) %>% success_logical()
```

```
## [1] TRUE
```

Check range of dates

```r
first_date <- as.Date("2017-09-25")
today <- Sys.Date()
d %>% 
  assertr::verify(Date >= first_date & Date <= today) %>% success_logical()
```

```
## [1] TRUE
```

## Summary Statistics

Current data set has 1187 observations.


```r
d %>% group_by(Player) %>%
  summarise(Mean = mean(Seconds), Median = median(Seconds), Min = min(Seconds), Max = max(Seconds), N=n()) %>%
  arrange(Median)
```

```
## # A tibble: 6 × 6
##   Player  Mean Median   Min   Max     N
##   <chr>  <dbl>  <dbl> <dbl> <dbl> <int>
## 1 SKK     51       47    21    89     4
## 2 AKK     55       55    55    55     1
## 3 JIK     78.9     61    13   264   225
## 4 SPK    102.      85    18   455   477
## 5 JAK    103.      86    27   454   424
## 6 BBK    117.     102    42   350    56
```

## Subset the Data

Subset the data by only working with observations from
`SPK`, `JAK` and `JIK`.
Other players do not have enough observations to analyze.


```r
d <- d %>%
  filter(Player %in% c("SPK", "JAK", "JIK"))
```

Now have 1126 observations.

## Plots

An Initial plot of the data.


```r
d %>%
  ggplot(aes(x=WeekDay, y=Seconds)) +
    geom_jitter(position = position_jitter(width=.3)) +
    ggtitle("Time vs Day of the Week")
```

![](nyt_mini_crossword_files/figure-html/plot01-1.png)<!-- -->

Same plot with player identified.


```r
d %>%
  ggplot(aes(x=WeekDay, y=Seconds, color=Player)) +
    geom_jitter(position = position_jitter(width=.3)) +
    ggtitle("Time vs Day of the Week")
```

![](nyt_mini_crossword_files/figure-html/plot02-1.png)<!-- -->

The distribution of the times, summarised in a boxplot:

![](nyt_mini_crossword_files/figure-html/plot03-1.png)<!-- -->

## Days Between Puzzles


```r
d %>%
  ggplot(aes(x=Date, y=WaitingTime, color=Player)) +
    geom_point()
```

```
## Warning: Removed 3 rows containing missing values (geom_point).
```

![](nyt_mini_crossword_files/figure-html/plot04-1.png)<!-- -->


```r
d %>%
  ggplot(aes(x=Date, y=WaitingTime)) +
    geom_point() +
    facet_grid(Player ~ ., scales="free")
```

```
## Warning: Removed 3 rows containing missing values (geom_point).
```

![](nyt_mini_crossword_files/figure-html/plot05-1.png)<!-- -->

## Player vs Player
The function `spkjak` returns "SPKJAK" if a date contains an entry
for both "SPK" and "JAK".
It could also contain an entry for "JIK".
The `d2` object selects the observations with "SPKJAK" set and
then only keeps those where `Player` is "SPK" or "JAK".

```r
# spkjak -----
spkjak <- function(x) {
  m <- match(c("SPK","JAK"), x, nomatch=-1)
  if(all(m > 0)) {
      "SPKJAK"
  } else {
      NA
  }
}
d2 <- group_by(d, Date) %>%
  mutate(SPKJAK = spkjak(Player)) %>%
  ungroup() %>%
  filter(SPKJAK == "SPKJAK", Player %in% c("SPK", "JAK"))
```

```r
d2 %>% filter(Player %in% c("SPK", "JAK")) %>%
  ggplot(aes(x=Date, y=Seconds, color=Player)) + geom_point()
```

![](nyt_mini_crossword_files/figure-html/spk_and_jak-1.png)<!-- -->

```r
d3 <- d2 %>% select(Date, Player, Seconds) %>%
  group_by(Date) %>%
  spread(key=Player, value=Seconds) %>%
  ungroup()
```

## Appendix
Knowing the versions of R and the packages used in an analysis is
an important part of reproducibility.
The `sessionInfo` function will show this.

```r
sessionInfo()
```

```
## R version 4.1.1 (2021-08-10)
## Platform: x86_64-pc-linux-gnu (64-bit)
## Running under: Ubuntu 20.04.3 LTS
## 
## Matrix products: default
## BLAS:   /home/R/R-4.1.1/lib/R/lib/libRblas.so
## LAPACK: /home/R/R-4.1.1/lib/R/lib/libRlapack.so
## 
## locale:
##  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
##  [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
##  [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
##  [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
##  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
## [11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] ggplot2_3.3.5 dplyr_1.0.7   tidyr_1.1.4   assertr_2.8  
## 
## loaded via a namespace (and not attached):
##  [1] highr_0.9        pillar_1.6.3     bslib_0.3.1      compiler_4.1.1  
##  [5] jquerylib_0.1.4  tools_4.1.1      digest_0.6.28    lubridate_1.8.0 
##  [9] jsonlite_1.7.2   evaluate_0.14    lifecycle_1.0.1  tibble_3.1.5    
## [13] gtable_0.3.0     pkgconfig_2.0.3  rlang_0.4.11     cli_3.0.1       
## [17] DBI_1.1.1        yaml_2.2.1       xfun_0.26        fastmap_1.1.0   
## [21] withr_2.4.2      stringr_1.4.0    knitr_1.36       hms_1.1.1       
## [25] generics_0.1.0   vctrs_0.3.8      sass_0.4.0       rprojroot_2.0.2 
## [29] grid_4.1.1       tidyselect_1.1.1 here_1.0.1       glue_1.4.2      
## [33] R6_2.5.1         fansi_0.5.0      rmarkdown_2.11   farver_2.1.0    
## [37] purrr_0.3.4      magrittr_2.0.1   scales_1.1.1     ellipsis_0.3.2  
## [41] htmltools_0.5.2  assertthat_0.2.1 colorspace_2.0-2 labeling_0.4.2  
## [45] utf8_1.2.2       stringi_1.7.5    munsell_0.5.0    crayon_1.4.1
```
