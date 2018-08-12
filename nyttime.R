library("lubridate")
library("dplyr")
library("ggplot2")
d <- read.csv("nyt.csv", stringsAsFactors=FALSE)
d$Date <- as.Date(d$Date)
d$WeekDay <- lubridate::wday(d$Date)
d$Seconds <- with(d, seconds(ms(Time)))
d %>%
  ggplot(aes(x=WeekDay, y=Seconds)) +
  geom_point() +
  facet_wrap( ~ Player)
