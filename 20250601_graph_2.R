#산점도
#변수 간 관계를 시각화
#예)키와 몸무게 관계, 온도와 에너지 소비량 관계, 흡연과 건강결과 관계

x=c(1,4,2,6,10,15)
y=c(2,3,6,6,10,2)

plot(x,y,main='산점도 예시', xlab='x값', ylab='y값', col='blue',
     pch=20)

#회귀선 추가
#linear model: 선형 모델
model = lm(y~x)
abline(model,col='red', lwd=2)


#데이터 스케일링
#데이터 스케일링은 전처리 방법 중 하나
#분석과 머신러닝에서 중요한 과정입니다. 

#데이터 프레임 생성
data=data.frame(
  height_cm=c(150.160,170,180,190),
  weight_kg=c(50,60,70,80,90)
)

#수치를 통일
#min-max라는 기법을 통해서 분석할 데이터를 0과 1사이로 변환
#즉 모든 데이터는 0과 1사이에 존재함. 

#암기
#스케일링= 기존값-최솟값 / 최댓값-최솟값

height_min = min(data$height_cm)
height_max = max(data$height_cm)

# 스케일링 결과 컬럼 추가
data$height_scaled = (data$height_cm - height_min) / 
  (height_max - height_min)
#View(data)


data = data.frame(
  height_cm = c(150, 160, 170, 180, 190),
  weight_kg = c(50, 60, 70, 80, 90)
)

# height_cm 스케일링 (min-max normalization)
height_min = min(data$height_cm)
height_max = max(data$height_cm)

# 스케일링 결과 컬럼 추가
data$height_scaled = (data$height_cm - height_min) / (height_max - height_min)

# 결과 확인
#View(data)










