# Adjust TimeOfDay for JAK and JIK to their timezone time of day
# 2019-03-06
d <- read.csv("nyt.csv", stringsAsFactors=FALSE)
todAdj <- ifelse(d[["Player"]] %in% c("JAK", "JIK"), 2, 0)
HM <- stringr::str_split_fixed(d[["TimeOfDay"]], ":", 2)
HOUR <- as.numeric(HM[,1]) + todAdj
MIN <- HM[,2]
d[["TimeOfDay"]] <- ifelse(is.na(HOUR), NA, paste(HOUR, MIN, sep=":"))
write.table(d, "nyt.csv", sep=",", row.names=FALSE, quote=FALSE)
