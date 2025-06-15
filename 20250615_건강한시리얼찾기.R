
#건강한 시리얼 찾기(저칼로리, 저설탕)

#1. 데이터 수집
#DB저장(RDB, NoSQL) -> CSV파일로 변환 -> 파이썬 or R 시작
#RDB : 관계형 DB(MySQL, Oracle, MSSQK ...)
#NoSQL(데이터를 쌓고 조회하는 용도): DynamoDB, MongoDB
scereal = read.csv('UScereal.csv') #수집했다고 가정


#2. 데이터 구조와 변수 확인
str(scereal)
#num : 실수, int: 정수, chr: 문자

#데이터 확인
print(head(scereal,3)) #위에서 3행까지 출력
print(colnames(scereal)) #전체 컬럼명만 조회
print(colSums(is.na(scereal))) #결측값 개수 확인
# 결측값이 너무 많으면 다시 처음부터 수집 (수집 소스 디버깅(파악))
# 결측값 특정 값으로 대체(v), 해당 행을 삭제합니다.




#3. 데이터 탐색 및 시각화 (EDA (Exploratory Data Analysis))
#건강한 시리얼 -> 저칼로리, 저설탕
#칼로리와 설탕에 대한 기본적인 통계를 출력합니다(평균, 중앙값, 최댓값....)

칼로리_평균=mean(scereal$calories, na.rm=TRUE)
칼로리_중앙값=median(scereal$calories, na.rm=TRUE)
칼로리_최솟값=min(scereal$calories, na.rm=TRUE)
칼로리_최댓값=max(scereal$calories, na.rm=TRUE)

cat('칼로리 평균 :', 칼로리_평균, '\n')
cat('칼로리 중앙값 :', 칼로리_중앙값, '\n')
cat('칼로리 최솟값 :', 칼로리_최솟값, '\n')
cat('칼로리 최댓값 :', 칼로리_최댓값, '\n')


설탕_평균값=mean(scereal$sugars, na.rm=TRUE)
설탕_표준편차=sd(scereal$sugars, na.rm=TRUE)

cat('설탕 평균값 :', 설탕_평균값, '\n')
cat('설탕 표준편차 :', 설탕_표준편차, '\n')

#표준편차: 데이터가 평균으로부터 얼마나 퍼져있는지를 나타내는 통계적 지표
#표준편차가 작으면 대부분의 값이 평균 근처에 있음.


#이게 작은 수치인가?
#변동계수로 데이터가 평균에 비해 넓게 분포되어 있는지 아닌지 확인하자
#변동계수(CV) = (표준편차 / 평균) *100

CV = (설탕_표준편차/설탕_평균값)*100
cat('변동계수 :', CV, '\n')

#CV가 50%이상이면 데이터가 평균에 비해 상당히 넓게 분포되어 있음
#10~20% 이하면 고르게 분포라고 해석합니다

if(CV>=50){
  print('설탕의 데이터는 상당히 넓게 분포되어 있습니다.')
}

#더 쉽게 요약하는 법  
#사분위수포함 평균 한번에 알려줌
print(summary(scereal$calories))
print(summary(scereal$sugars))


#상관관계 파악
설탕_칼로리_상관관계 = cor(scereal$sugars, scereal$calories, use='complete.obs')
cat('설탕과 칼로리 상관관계수:', 설탕_칼로리_상관관계, '\n') #0.49보통관계


#전체적인 패턴을 시각화로 표현
library(ggplot2)
#install.packages('patchwork')
library(patchwork)

# 칼로리 평균, 설탕 평균

p1=ggplot(scereal, aes(x=calories)) + 
  geom_histogram(binwidth=10, fill='skyblue', color='white') + #막대설정
  geom_vline(xintercept= 칼로리_평균, color='red', linetype='dashed', size=1) + #평균선
  labs(title= '시리얼 칼로리 분포', x='칼로리', y='제품수')
#print(p1)


p2=ggplot(scereal, aes(x=sugars)) + 
  geom_histogram(binwidth=10, fill='orange', color='white') + #막대설정
  geom_vline(xintercept= 설탕_평균값, color='red', linetype='dashed', size=1) + #평균선
  labs(title= '시리얼 설탕 분포', x='설탕', y='제품수')

#제조사별 칼로리 박스플롯

p3 = ggplot(scereal,aes(x=mfr, y=calories, fill=mfr))+
  geom_boxplot() +
  labs(title='제조사별 칼로리 분포', x= '제조사', y= '칼로리')


#patchwork 그래프 3개를 한번에 출력하기
# | 가로 배치, / 세로 배치
print( (p1|p2)/p3)





#4. 데이터 전처리 (min-max, 이상치제거, 결측값 처리)

#이상치제거는 생략

#저칼로리와 저설탕 기준 설정(전체 25%이하를 '저'로 간주)
저칼로리 = quantile(scereal$calories, 0.25, na.rm=TRUE) #하위 25%
저설탕 = quantile(scereal$sugars, 0.25, na.rm=TRUE) #하위 25%

scereal$low_cal=ifelse(scereal$calories <= 저칼로리,TRUE,FALSE)
scereal$low_sugar=ifelse(scereal$sugars <= 저설탕,TRUE,FALSE)


print(head(scereal,3))

#5. 실용적 분석
library(dplyr)

#저칼로리 저설탕 추출
건강한시리얼 = scereal %>% filter(low_cal & low_sugar) #true&true
print(head(건강한시리얼,3))


#제조사 별 건강한 시리얼 개수 집계
제조사별_건강한시리얼 = 건강한시리얼 %>% group_by(mfr) %>% 
  summarise(count=n()) %>%   #개수 세기
  arrange(desc(count))    #정렬하기 큰수부터 작은수 순서
print(제조사별_건강한시리얼)




#시각화

#stat='identity' 데이터의 y축 값을 그래프에 반영하겠다
최종건강한_시리얼 = ggplot(제조사별_건강한시리얼, aes(x=mfr, y=count, fill= mfr))+
                     geom_bar(stat='identity')+
                     labs(title='제조사별 건강한 시리얼', x='제조사', y='개수')+
                     theme_minimal()
print(최종건강한_시리얼)
  
  
                   



















