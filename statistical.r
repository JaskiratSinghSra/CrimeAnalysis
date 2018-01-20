df = read.csv("Chicago_Crimes_2012_to_2017.csv")
library(dplyr)
library(chron)
 
library(ggplot2); library(scales); library(grid); library(RColorBrewer)

myvars <- c("Date", "Time","Primary.Type", "Arrest","Latitude","Longitude")
df<-df[myvars]
sapply(df, function(x) sum(is.na(x)))

df<- na.omit(df)
sapply(df, function(x) sum(is.na(x)))

df$datetime <- paste(df$Date, paste(df$Time,":00",sep=''))
df$Incident_Date <- as.POSIXlt(df$datetime, format="%m/%d/%Y %H:%M")
df$Incident_Time <- times(format(df$Incident_Date, "%H:%M:%S"))
df$Incident_Date <- as.POSIXlt(strptime(df$Incident_Date, format="%Y-%m-%d")


df$Incident_Month <- months(df$Incident_Date, abbreviate=TRUE)
df$Incident_Month <- factor(df$Incident_Month, levels= c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

#PLOT FOR CRIME VS MONTH OF YEAR
qplot(df$Incident_Month, xlab= "Month of Incident", main= "Crimes by month") + scale_y_continuous("Number of crimes")

time.tag <- chron(times=c("00:00:00", "06:00:00", "12:00:00", "18:00:00", "23:59:59"))
df$Incident_Time_Tag <- cut(df$Incident_Time, breaks=time.tag, labels=c("Early Morning","Morning", "Evening", "Night"), include.lowest=TRUE)

#PLOT FOR CRIME VS TIME OF DAY
windows()
qplot(df$Incident_Time_Tag, xlab="Time of day", main = "Crimes by time of day") + scale_y_continuous("Number of crimes")




df$Crime_Category <- as.character(df$Primary.Type)
 
df$Crime_Category <- ifelse(df$Crime_Category %in% c("SEX OFFENSE","PROSTITUTION","OBSCENITY","CRIM SEXUAL ASSAULT","OFFENSE INVOLVING CHILDREN","HUMAN TRAFFICKING"),'SEX',df$Crime_Category)
 
df$Crime_Category <- ifelse(df$Crime_Category %in% c("LIQUOR LAW VIOLATION","NARCOTICS","NARCOTICS","OTHER NARCOTIC VIOLATION"),'DRUGS',df$Crime_Category)
 
df$Crime_Category <- ifelse(df$Crime_Category %in% c("DECEPTIVE PRACTICE"),'FRAUD',df$Crime_Category)
 
df$Crime_Category <- ifelse(df$Crime_Category %in% c("BURGLARY CONCEALED CARRY LICENSE VIOLATION","ROBBERY","CRIMINAL TRESPASS"),'ROBBERY',df$Crime_Category)
 
df$Crime_Category <- ifelse(df$Crime_Category %in% c("THEFT", "MOTOR VEHICLE THEFT" ),'THEFT',df$Crime_Category)
 
df$Crime_Category <- ifelse(df$Crime_Category %in% c("ARSON","CRIMINAL DAMAGE"),'ARSON',df$Crime_Category)

df$Crime_Category <- ifelse(df$Crime_Category %in% c("HOMICIDE","ASSAULT"),'MURDER',df$Crime_Category)
 
df$Crime_Category <- ifelse(df$Crime_Category %in% c("KIDNAPPING"),'KIDNAPPING',df$Crime_Category)

df$Crime_Category <- ifelse(df$Crime_Category %in% c("PUBLIC INDECENCY","PUBLIC PEACE VIOLATION","INTIMIDATION","INTERFERENCE WITH PUBLIC OFFICER"),'PUBLIC OFFENSE',df$Crime_Category)
 
df$Crime_Category <- ifelse(df$Crime_Category %in% c("GAMBLING","WEAPONS VIOLATION","OTHER OFFENSE","CONCEALED CARRY LICENSE VIOLATION"),'OTHER OFFENSES',df$Crime_Category)
 
df$Crime_Category <- ifelse(df$Crime_Category %in% c("NON-CRIMINAL","NON - CRIMINAL","NON-CRIMINAL (SUBJECT SPECIFIED)","STALKING"),'NON-CRIMINAL',df$Crime_Category)


table(df$ Crime_Category)
length(unique(df$ Crime_Category))

#PLOT FOR CRIME CATEGORIES
windows()
qplot(df$Crime_Category, xlab = "Crime Category", main = "Crimes in Chicago") + scale_y_continuous("# Crimes")
 


#Aggregate the Crime Category by different time buckets
incidents_by_time <- aggregate(df$Crime_Category, by = list(df$Crime_Category, df$Incident_Time_Tag), FUN = length)

#Name the columns of the new data frame
names(incidents_by_time) <- c("Crime_Category", "Incident_Time_Tag", "Count")





#HEAT MAP FOR CRIME CATEGORY VS TIME OF THE DAY
windows()
ggplot(incidents_by_time, aes(x= Crime_Category, y= factor(Incident_Time_Tag))) +
geom_tile(aes(fill= Count)) + scale_x_discrete("Crime", expand = c(0,0)) +
scale_y_discrete("Time of day", expand = c(0,-2)) +
scale_fill_gradient("Number of crimes", low = "gray", high = "red") +
theme_bw() + ggtitle("Crimes by time of day") +
theme(panel.grid.major = element_line(colour = NA), panel.grid.minor = element_line
(colour = NA))



#Aggregate the Crime Category by different time buckets

incidents_by_monthofyear <- aggregate(df$Crime_Category, by = list(df$Crime_Category, df$Incident_Month), FUN = length)
names(incidents_by_monthofyear) <- c("Crime_Category", "Incident_Month", "Count")
 
#HEAT MAP FOR CRIME CATEGORY VS MONTH
windows()
 
ggplot(incidents_by_monthofyear, aes(x= Crime_Category, y= factor(Incident_Month))) +
geom_tile(aes(fill= Count)) + scale_x_discrete("Crime", expand = c(0,0)) +
scale_y_discrete("Month of Year", expand = c(0,-2)) +
scale_fill_gradient("Number of crimes", low = "gray", high = "red") +
theme_bw() + ggtitle("Crimes by month of year") +
theme(panel.grid.major = element_line(colour = NA), panel.grid.minor = element_line
(colour = NA))





