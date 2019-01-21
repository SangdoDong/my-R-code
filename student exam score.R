setwd("c:/practice")

student<-read.csv("StudentsPerformance.csv")

str(student)

dim(student)

summary(student)


library(ggplot2)
library(dplyr)
library(agricolae)
#성별에 따른 성적차이가 있는가?
#수학점수 박스플롯
ggplot(data=student, aes(x=gender, y=math.score))+geom_boxplot()+ggtitle("math score")
#읽기점수 박스플롯
ggplot(data=student, aes(x=gender, y=reading.score))+geom_boxplot()+ggtitle("reading score")

#쓰기점수 박스플롯
ggplot(data=student, aes(x=gender, y=writing.score))+geom_boxplot()+ggtitle("writing score")

#수학점수 t.test
student_math<-student %>%
  select(gender,math.score)

t.test(data=student_math, math.score~gender, var.equal=T) #분산은 같다고 가정

#읽기 점수 t.test
student_reading<-student %>%
  select(gender,reading.score)

t.test(data=student_reading, reading.score~gender, var.equal=T) #분산은 같다고 가정
# 쓰기 점수 t.test
student_writing<-student %>%
  select(gender,writing.score)

t.test(data=student_writing, writing.score~gender, var.equal=T) #분산은 같다고 가정
#인종에 따른 성적 차이가 있는가?
  #인종에 따른 수학점수 비교
aggregate(math.score~race.ethnicity, data=student, mean) 
aggregate(math.score~race.ethnicity, data=student, sd)
summary(aov(math.score~race.ethnicity,data=student))
  #인종에 따른 읽기 점수 비교 
aggregate(reading.score~race.ethnicity, data=student, mean) 
aggregate(reading.score~race.ethnicity, data=student, sd)
summary(aov(reading.score~race.ethnicity,data=student))
  #인종에 따른 쓰기점수 비교
aggregate(writing.score~race.ethnicity, data=student, mean) 
aggregate(writing.score~race.ethnicity, data=student, sd)
summary(aov(writing.score~race.ethnicity,data=student))  
#부모학력에 따른 성적 차이가 있는가?
  #수학점수 비교
aggregate(math.score~parental.level.of.education, data=student, mean) 
aggregate(math.score~parental.level.of.education, data=student, sd)
summary(aov(math.score~parental.level.of.education,data=student))
  #읽기점수 비교
aggregate(reading.score~parental.level.of.education, data=student, mean) 
aggregate(reading.score~parental.level.of.education, data=student, sd)
summary(aov(reading.score~parental.level.of.education,data=student))
  #쓰기점수 비교
aggregate(writing.score~parental.level.of.education, data=student, mean) 
aggregate(writing.score~parental.level.of.education, data=student, sd)
summary(aov(writing.score~parental.level.of.education,data=student))

#점심식사 여부에 따라 성적차이가 있는가?
  #수학점수
t.test(data=student, math.score~lunch, var.equal=T)
  #읽기점수
t.test(data=student, reading.score~lunch, var.equal=T)
  #쓰기 점수
t.test(data=student, writing.score~lunch, var.equal=T)

#test.preparation course 수강여부에 따라 성적 차이가 있는가?

t.test(data=student, math.score~test.preparation.course, var.equal=T)
#읽기점수
t.test(data=student, reading.score~test.preparation.course, var.equal=T)
#쓰기 점수
t.test(data=student, writing.score~test.preparation.course, var.equal=T)

#위의변수 중에서 어떤 것이 가장 큰 영향을 끼치는가?
  #수학점수
math.regression<-lm(math.score~gender+race.ethnicity+parental.level.of.education+lunch+test.preparation.course, data=student)
summary(math.regression)
  #읽기점수
reading.regression<-lm(reading.score~gender+race.ethnicity+parental.level.of.education+lunch+test.preparation.course, data=student)
summary(reading.regression)
  #쓰기점수 
writing.regression<-lm(writing.score~gender+race.ethnicity+parental.level.of.education+lunch+test.preparation.course, data=student)
summary(writing.regression)

