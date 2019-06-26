# Reorder columns in the CSV, putting Player first
# 2019-03-06
d <- read.csv("nyt.csv", stringsAsFactors=FALSE)
d <- d[, c("Player", "Date", "Time", "TimeOfDay")]
write.table(d, "nyt.csv", sep=",", row.names=FALSE, quote=FALSE)
