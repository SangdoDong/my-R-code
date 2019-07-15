getwd()
library(ggplot2)
library(dplyr)
library(dygraphs) #시계열을 만드는 라이브러리
library(xts) #데이터를 xts데이터 타입으로 만드는 라이브러리 
ticket<-read.csv("ticketprice.csv")

str(ticket)

table(is.na(ticket$price))

ticket<-ticket %>%
  filter(!is.na(price))

#마드리드 출발
madrid_origin<-ticket %>%
  filter(origin=="MADRID")


summary(madrid_origin)
#마드리드 출발 열차의 빈도 수
madrid_origin %>%
  ggplot(aes(x= destination))+
  geom_bar()
#마드리발 도착지별 가격 박스플롯
madrid_origin %>%
  filter(train_type=="AVE") %>%
  ggplot(aes(x=destination, y=price))+
  geom_boxplot()

madrid_origin %>%
  filter(train_type=="AVE") %>%
  ggplot(aes(x=destination, y=price))+
  geom_boxplot()+ggtitle("AVE의 좌석등급별 가격 박스플롯")+facet_grid(~train_class)

a<-madrid_origin %>%
  filter(train_type=="AVE") %>%
  filter(train_class=="Preferente") %>%
  filter(destination=="VALENCIA") 
b<-madrid_origin %>%
  filter(train_type=="AVE") %>%
  filter(train_class=="Turista") %>%
  filter(destination=="VALENCIA")
  
  
#날짜 데이터 변환. as.POSIXct는 factor형식의 날짜 사용가능
madrid_origin2<-xts(a$price, order.by =  as.POSIXct(a$start_date))
head(madrid_origin2)

madrid_origin3<-xts(b$price, order.by =  as.POSIXct(b$start_date))

#데이터 결합
a_b<-cbind(madrid_origin2,madrid_origin3)
#컬럼이름지정
colnames(a_b)<-c("preferente","Turista")
#마드리드 출발, 발렌시아 도착, AVE,preferente의 시간별 가격.
dygraph(madrid_origin2) %>%
  dyRangeSelector()

dygraph(a_b) %>%
  dyRangeSelector()

g
madrid_origin %>%
  filter(train_type=="AVE") %>%
  filter(train_class=="Preferente") %>%
  filter(destination=="VALENCIA") %>% 
  ggplot(aes(x=start_date, y=price))+geom_line()


  
  
ggplot(data=madrid_origin, aes(x=destination, y=price, color=train_class))+geom_boxplot()

madrid_origin %>%
  filter(destination=="BARCELONA") %>%
  ggplot(aes(x=train_type, y=price, color=train_class))+geom_boxplot()


#바르셀로나 출발
barcelona_origin<-ticket %>%
  filter(origin=="BARCELONA")

summary(barcelona_origin)
barcelona_origin %>%
  group_by(destination) %>%
  summary(mean_price=mean(price))




