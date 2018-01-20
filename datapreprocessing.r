df = read.csv("Chicago_Crimes_2012_to_2017.csv")
library(dplyr)
library(chron)

CHECKING DUPLICATE ROWS
df<- df[order(df$ID),]
head(df)
nrow(df)
df <- subset(df, !duplicated(df$ID))
nrow(df)

REMOVING UNECESSARY COLUMNS
myvars <- c("Date", "Time","Primary.Type", "Arrest","Latitude","Longitude")
df<-df[myvars]

CHECKING  ROWS WITH NULL VALUES
sapply(df, function(x) sum(is.na(x)))

df<- na.omit(df)
sapply(df, function(x) sum(is.na(x)))
head(df)






