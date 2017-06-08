library(lubridate)

df <- read.csv('/home/larry/Downloads/lapd.csv', header = TRUE)
df$Date.Rptd <- mdy(df$Date.Rptd)
df$DATE.OCC <- mdy(df$DATE.OCC)
attach(df)


# The following is based on the analysis of open-source crime data for LA containing 243,750 reported crimes.
# 1)  Most crime occurs during lunch hour.  If you have worked in LA, this makes sense.  
#     The next peak is at 6PM when people are setting down for dinner.  
#     There is also a peak at 8AM, but it is not as pronounced, probably due to people skipping breakfast.
(crime.by.hour <- table(TIME.OCC %/% 100))
plot(0:23, crime.by.hour, type = 'b', col = 'blue', pch = 19)


# The afternoon crime wave is so bad that I've elected to adopt vampire hours and only interact with the public
# between the hours of 12AM and 7AM when crime is at the lowest.
tmp <- data.frame('PM' = x[13:20], 'AM' = x[1:8], 'PM Crime Increase' = x[13:20] / x[1:8])
rownames(tmp) <- paste0(c(12, 1:7), ':00')
tmp


