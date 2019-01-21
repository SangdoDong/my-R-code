setwd("c:/practice")
library(ggplot2)
library(plotly)
insurance<-read.csv("insurance.csv")

head(insurance)
dim(insurance)
str(insurance)
#성별로 데이터 분류
insurance_m<-subset(insurance, sex=='male')
insurance_f<-subset(insurance, sex=='female')

#male case
attach(insurance_m)

#multiple regression 
m1<-lm(charges~age+bmi+children+smoker+region,data=insurance_m)
summary(m1)
m1_var<-c("charges","age","bmi","children","smoker","region")
pairs(insurance_m[m1_var])
par(mfrow=c(1,1))

#흡연여부와 charges과의 관계
ggplot(data=insurance_m, aes(x=age,y=charges))+
  geom_point(alpha=0.5, aes(color=smoker))

#2nd modeling 
step(m1, direction="both")
step(lm(charges~age+bmi+children+smoker+region,data=insurance_m), direction="backward") 
step(m1, direction="backward") #same_meaning_code with upper one

m2<-step(lm(charges~age+bmi+children+smoker,data=insurance_m))
summary(m2)
#female case
attach(insurance_f)

#multiple regression 
f1<-lm(charges~age+bmi+children+smoker+region,data=insurance_f)
summary(f1)
f1_var<-c("charges","age","bmi","children","smoker","region")
pairs(insurance_m[f1_var])

ggplot(data=insurance_f, aes(x=age,y=charges))+
  geom_point(alpha=0.5, aes(color=smoker))

#2nd modeling 
step(f1, direction="both")
step(lm(charges~age+bmi+children+smoker+region,data=insurance_f), direction="backward") 
step(f1, direction="backward") #same_meaning_code with upper one

f2<-step(lm(charges~age+bmi+smoker,data=insurance_f))
summary(f2)

#drawing plot
#charges and age_male

head(insurance_m)
m3<-insurance_m %>%
  filter(smoker=="yes")
#regression of smoker male 

m3plot<- ggplot(data=m3, aes(x=age, y=charges))+
  geom_point(shape=19, size=2, color="green2")+
  stat_smooth(method=lm, level=0.95)+
  ggtitle("Male smoker's expected charges")

ggplotly(m3plot)

#
m4<-insurance_m %>%
  filter(smoker=="no")
#regression of smoker male 

m4plot<- ggplot(data=m4, aes(x=age, y=charges))+
  geom_point(shape=19, size=1, color="green3")+
  stat_smooth(method=lm, level=0.95)+
  ggtitle("Male non-smoker's expected charges")
  
ggplotly(m4plot)


f3<-insurance_f %>%
  filter(smoker=="yes")

f3plot<- ggplot(data=f3, aes(x=age, y=charges))+
  geom_point(shape=19, size=2, color="green2")+
  stat_smooth(method=lm, level=0.95)+
  ggtitle("Female smoker's expected charges")

ggplotly(f3plot)


f4<-insurance_f %>%
  filter(smoker=="no")

f4plot<- ggplot(data=f4, aes(x=age, y=charges))+
  geom_point(shape=19, size=2, color="green2")+
  stat_smooth(method=lm, level=0.95)+
  ggtitle("Female non smoker's expected charges")

ggplotly(f4plot)

