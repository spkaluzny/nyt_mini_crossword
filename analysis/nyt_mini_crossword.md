---
title: "New York Time Mini Crossword"
author: "Stephen Kaluzny"
date: "27 May, 2024"
output:
  html_document: 
    keep_md: yes
    theme: cerulean
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


``` r
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


``` r
d <- read.csv(here::here("data", "nyt.csv"), stringsAsFactors=FALSE)
```

The `Date` and `TimeOfDay` are combined and converted to a `POSIX` variable `DateTime`.
The `WeekDay` is then computed from `DateTime`.
The puzzle time (`Time`) was recorded as minutes:seconds,
we convert that to a numeric `Seconds` variable.

``` r
d$Date <- as.Date(d$Date)
d$DateTime <- as.POSIXct(with(d,
  paste(Date, ifelse(is.na(TimeOfDay), "00:00", TimeOfDay))))
d$WeekDay <- lubridate::wday(d$DateTime)
d$TimeOfDayOrig <- d$TimeOfDay
d$TimeOfDay <- hms::parse_hm(d$TimeOfDay)
d$Seconds <- with(d, as.numeric(lubridate::seconds(lubridate::ms(Time))))
```

Compute the time interval, in days, between puzzle playing by `Player`.

``` r
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


``` r
# Current list of players:
players <- c("SPK", "JAK", "JIK", "BBK", "SKK", "AKK", "MBH")
# Earliest date:
first_date <- as.Date("2017-09-25")
# Upper bound on time (10 minutes = 600 seconds):
time_bound <- 700
```

Check that Player is one of 5 possible values:

``` r
d %>% assert(in_set(players), Player, success_fun=success_logical,
  error_fun=error_logical)
```

```
## [1] FALSE
```

There should be no missing values in the data:

``` r
d %>% assert(not_na, DateTime, WeekDay, Seconds) %>% success_logical()
```

```
## [1] TRUE
```

Only one observation per player per date:

``` r
d %>% group_by(Date) %>% count(Player) %>% verify(n == 1) %>% success_logical()
```

```
## [1] TRUE
```

Check for proper time values:

``` r
d %>% assertr::verify(Seconds > 0 & Seconds < time_bound,
  success_fun=success_logical, error_fun=error_logical)
```

```
## [1] FALSE
```

Check range of dates

``` r
first_date <- as.Date("2017-09-25")
today <- Sys.Date()
d %>% 
  assertr::verify(Date >= first_date & Date <= today, success_fun=success_logical,
    error_fun=error_logical)
```

```
## [1] TRUE
```

## Summary Statistics

Current data set has 2361 observations.


``` r
d %>% group_by(Player) %>%
  summarise(Mean = mean(Seconds), Median = median(Seconds), Min = min(Seconds), Max = max(Seconds), N=n()) %>%
  arrange(Median)
```

```
## # A tibble: 9 × 6
##   Player  Mean Median   Min   Max     N
##   <chr>  <dbl>  <dbl> <dbl> <dbl> <int>
## 1 SKK     60.1     45    21   146     7
## 2 JIK     63.6     52    12   264   538
## 3 AKK     55       55    55    55     1
## 4 SPK     94.9     79    16   455   982
## 5 MC     161.      87    74   432     5
## 6 DJK    116.      94    71   244     5
## 7 JAK    122.      95    27   849   659
## 8 BBK    113.     102    42   350    76
## 9 MBH    220.     164    51   827    88
```

## Subset the Data

Subset the data by only working with observations from
`SPK`, `JAK` and `JIK`.
Other players do not have enough observations to analyze.


``` r
d <- d %>%
  filter(Player %in% c("SPK", "JAK", "JIK"))
```

Now have 2179 observations.

## Plots

An Initial plot of the data.


``` r
d %>%
  ggplot(aes(x=WeekDay, y=Seconds)) +
    geom_jitter(position = position_jitter(width=.3)) +
    ggtitle("Time vs Day of the Week")
```

![](nyt_mini_crossword_files/figure-html/plot01-1.png)<!-- -->

Same plot with player identified.


``` r
d %>%
  ggplot(aes(x=WeekDay, y=Seconds, color=Player)) +
    geom_jitter(position = position_jitter(width=.3)) +
    ggtitle("Time vs Day of the Week")
```

![](nyt_mini_crossword_files/figure-html/plot02-1.png)<!-- -->

The distribution of the times, summarised in a boxplot:

![](nyt_mini_crossword_files/figure-html/plot03-1.png)<!-- -->

## Days Between Puzzles


``` r
d %>%
  ggplot(aes(x=Date, y=WaitingTime, color=Player)) +
    geom_point()
```

```
## Warning: Removed 3 rows containing missing values or values outside the scale range
## (`geom_point()`).
```

![](nyt_mini_crossword_files/figure-html/plot04-1.png)<!-- -->


``` r
d %>%
  ggplot(aes(x=Date, y=WaitingTime)) +
    geom_point() +
    facet_grid(Player ~ ., scales="free")
```

```
## Warning: Removed 3 rows containing missing values or values outside the scale range
## (`geom_point()`).
```

![](nyt_mini_crossword_files/figure-html/plot05-1.png)<!-- -->

## Player vs Player
The function `spkjak` returns "SPKJAK" if a date contains an entry
for both "SPK" and "JAK".
It could also contain an entry for "JIK".
The `d2` object selects the observations with "SPKJAK" set and
then only keeps those where `Player` is "SPK" or "JAK".

``` r
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

``` r
d2 %>% filter(Player %in% c("SPK", "JAK")) %>%
  ggplot(aes(x=Date, y=Seconds, color=Player)) + geom_point()
```

![](nyt_mini_crossword_files/figure-html/spk_and_jak-1.png)<!-- -->

``` r
d3 <- d2 %>% select(Date, Player, Seconds) %>%
  group_by(Date) %>%
  spread(key=Player, value=Seconds) %>%
  ungroup()
```

## Appendix
Knowing the versions of R and the packages used in an analysis is
an important part of reproducibility.
The `sessionInfo` function will show this.

``` r
sessionInfo()
```

```
## R version 4.4.0 (2024-04-24)
## Platform: x86_64-pc-linux-gnu
## Running under: Ubuntu 20.04.6 LTS
## 
## Matrix products: default
## BLAS:   /home/R/R-4.4.0/lib/R/lib/libRblas.so 
## LAPACK: /home/R/R-4.4.0/lib/R/lib/libRlapack.so;  LAPACK version 3.12.0
## 
## locale:
##  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
##  [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
##  [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
##  [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
##  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
## [11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       
## 
## time zone: America/Los_Angeles
## tzcode source: system (glibc)
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] ggplot2_3.5.1 dplyr_1.1.4   tidyr_1.3.1   assertr_3.0.1
## 
## loaded via a namespace (and not attached):
##  [1] gtable_0.3.5      jsonlite_1.8.8    highr_0.11        compiler_4.4.0   
##  [5] tidyselect_1.2.1  jquerylib_0.1.4   scales_1.3.0      yaml_2.3.8       
##  [9] fastmap_1.2.0     here_1.0.1        R6_2.5.1          labeling_0.4.3   
## [13] generics_0.1.3    knitr_1.46        tibble_3.2.1      rprojroot_2.0.4  
## [17] munsell_0.5.1     lubridate_1.9.3   bslib_0.7.0       pillar_1.9.0     
## [21] rlang_1.1.3       utf8_1.2.4        cachem_1.1.0      xfun_0.44        
## [25] sass_0.4.9        timechange_0.3.0  cli_3.6.2         withr_3.0.0      
## [29] magrittr_2.0.3    digest_0.6.35     grid_4.4.0        hms_1.1.3        
## [33] lifecycle_1.0.4   vctrs_0.6.5       evaluate_0.23     glue_1.7.0       
## [37] farver_2.1.2      fansi_1.0.6       colorspace_2.1-0  rmarkdown_2.27   
## [41] purrr_1.0.2       tools_4.4.0       pkgconfig_2.0.3   htmltools_0.5.8.1
```
