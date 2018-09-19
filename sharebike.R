#share bike data in LA

library(ggplot2)
library(lubridate)
library(dplyr)

setwd("c:/practice")

bike<-read.csv("sharebike.csv")

str(bike)

table(bike$Passholder.Type)

qplot(bike$Passholder.Type)

table(bike$Trip.Route.Category)
qplot(bike$Trip.Route.Category)

#hour of use bike

head(bike$Start.Time)
bike$usetime<-ymd_hms(bike$End.Time)-ymd_hms(bike$Start.Time)
bike$usetime<-as.numeric(bike$usetime)

summary(bike$usetime)

boxplot(bike$usetime)

boxplot(bike$usetime)$stats

bike$usetime<-ifelse(bike$usetime>=50,NA,bike$usetime)
dim(bike)
bike<- bike %>% filter(!is.na(usetime))

table(is.na(bike$usetime))

summary(bike$usetime)

hist(bike$usetime, breaks = 60,
     main = "frequency of how they use", col = "yellow")

#group_by
ggplot(data=bike, aes(x=Passholder.Type, y=usetime))+geom_boxplot()


bike %>%
  group_by(Passholder.Type) %>%
  summarise(mean=mean(usetime))
