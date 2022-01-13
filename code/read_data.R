d <- read.csv(here::here("data/nyt.csv"))
d$Date <- as.Date(d$Date)
d$WeekDay <- lubridate::wday(d$Date)
d$Seconds <- with(d, lubridate::seconds(lubridate::ms(Time)))
