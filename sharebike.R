#share bike data in LA

library(ggplot2)
library(lubridate) #날짜 및 시간핸들링 패키지
library(dplyr)
library(plotly)

setwd("c:/practice")

bike<-read.csv("sharebike.csv")

str(bike)

table(bike$Passholder.Type)

qplot(bike$Passholder.Type)
table(bike$Trip.Route.Category)
qplot(bike$Trip.Route.Category)
head(bike)
#hour of use bike

head(bike$Start.Time)
bike$usetime<-ymd_hms(bike$End.Time)-ymd_hms(bike$Start.Time)
bike$usetime<-as.numeric(bike$usetime)

summary(bike$usetime)
#통계치 출력
boxplot(bike$usetime)$stats

#결측처리하기
bike$usetime<-ifelse(bike$usetime<=1|bike$usetime>=36,NA, bike$usetime)
table(is.na(bike$usetime))

bike<- bike %>% filter(!is.na(usetime))


ggplot(data=bike, aes(x=Passholder.Type, y=usetime))+geom_boxplot()+ggtitle("usetime by pass holder")


hist(bike$usetime, breaks = 30,
     main = "Using time", col = "yellow", xlab="time", ylab = "number")

#group_by 이용자 타입별 사용통계
ggplot(data=bike, aes(x=Passholder.Type, y=usetime))+geom_boxplot()+ggtitle("summary using time by Passholder Type")


#1. roundtrip 평균이용시간
round_journey<- bike %>%
  filter(Trip.Route.Category=="Round Trip")



round_journey %>%
  group_by(Passholder.Type) %>%
  summarise(mean=mean(usetime))

head(round_journey)
#1.1 round_journey의 passholder type별 사용통계
ggplot(data=round_journey, aes(x=Passholder.Type, y=usetime))+geom_boxplot()+ggtitle("round_journey")

#2. onewaytrip 평균이용시간
oneway_journey<- bike %>%
  filter(Trip.Route.Category=="One Way")

oneway_journey %>%
  group_by(Passholder.Type) %>%
  summarise(mean=mean(usetime))
#2.1 oneway_journey의 passholder type별 사용통계
ggplot(data=oneway_journey, aes(x=Passholder.Type, y=usetime))+geom_boxplot()+ggtitle("oneway_journey")

#3. oneway_journey에서 starting station & Ending station 추출
tail(oneway_journey)
table(oneway_journey$Starting.Station.ID)
table(oneway_journey$Ending.Station.ID)


oneway_starting_station_freq<-ggplot(data=oneway_journey,
                               aes(x=oneway_journey$Starting.Station.ID))+geom_bar(fill='#FF9900')+
                                coord_cartesian(xlim = c(3000, 3100))
ggplotly(oneway_starting_station_freq)


oneway_ending_station_freq<-ggplot(data=oneway_journey,
                                   aes(x=oneway_journey$Ending.Station.ID))+geom_bar()+coord_cartesian(xlim = c(3000, 3100))

ggplotly(oneway_ending_station_freq)
###################################
#데이터 나누지 않고 진행해보기#####
###################################

#oneway 중에서 starting.station id와 passholder type만을 추출.
oneway_starting_station_id<- bike %>%
  filter(Trip.Route.Category=="One Way") %>%
  select(Starting.Station.ID,Passholder.Type)

#추출한 데이터에서 starting station ID 횟수
oneway_starting_station_count<-oneway_starting_station_id %>%
  group_by(Starting.Station.ID) %>%
  tally()

#oneway중에서 ending station id와 passholder type만을 추출
oneway_ending_station_id<- bike %>%
  filter(Trip.Route.Category=="One Way") %>%
  select(Ending.Station.ID, Passholder.Type)
#추출한 데이터에서 ending station id 횟수
oneway_ending_station_count<-oneway_ending_station_id %>%
  group_by(Ending.Station.ID) %>%
  tally()

#데이터 통합을 위한 변수명 일치시키기
#시작스테이션데이터
starting_count_new<-oneway_starting_station_count #복사본 만들기. 

starting_count_new<-rename(starting_count_new, station = Starting.Station.ID) #변수명 변경
starting_count_new<-rename(starting_count_new, startcount = n)
#종료스테이션데이터
ending_count_new<-oneway_ending_station_count #복사본 만들기
#변수명 변경
ending_count_new<-rename(ending_count_new, station = Ending.Station.ID)
ending_count_new<-rename(ending_count_new, endingcount = n)
#불필요한 데이터 제거
starting_count_new<-starting_count_new[-c(65,66),]
ending_count_new<-ending_count_new[-c(65,66),]
#데이터합치기

total<-left_join(starting_count_new, ending_count_new, by="station")
#그림그리기!!!

ggplot(data=total, aes(x=station, fill= startcount,endingcount))+geom_bar()
m1<-ggplot(data=total, aes(x= station))
m2<-m1+geom_line(aes(y=startcount),colour="Red",size=1)

m3<-m2+geom_line(aes(y=endingcount), fill="blue", stat="identity")
m3

ggplotly(m3)
#####지도에 뿌리는건 다음시간에#####
