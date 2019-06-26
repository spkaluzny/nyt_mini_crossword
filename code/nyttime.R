library("lubridate")
library("dplyr")
library("ggplot2")
d <- read.csv("nyt.csv", stringsAsFactors=FALSE)
d <- d %>%
  filter(Player %in% c("SPK", "JAK", "JIK"))
d$Date <- as.Date(d$Date)
d$WeekDay <- lubridate::wday(d$Date)
d$Seconds <- with(d, seconds(ms(Time)))
d %>%
  ggplot(aes(x=WeekDay, y=Seconds)) +
  geom_point() +
  facet_grid(Player ~ .)
