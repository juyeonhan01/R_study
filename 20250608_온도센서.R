library(dplyr)

# csv파일 불러오기
센서_데이터 = read.csv('sensor_data.csv', fileEncoding = 'CP949', 
                  encoding = 'UTF-8', check.names = FALSE)

#View(센서_데이터)
str(센서_데이터) #데이터 타입확인 숫자인지 문자인지 확인!

# 1. 이상치 제거
# 평균, 표준편차 조회
mu = mean(센서_데이터$온도)
sigma = sd(센서_데이터$온도)
# Z-score 이상치 제거 기법
센서_데이터$z_score = abs((센서_데이터$온도 - mu) / sigma)
print(센서_데이터)

# 필터링 제거
# 임계값 3
# 임계값 2는 비교적 넓은 범위의 이상치를 탐지할 때 사용됩니다.
센서_데이터 = 센서_데이터 %>% filter(z_score < 3)

# 정규분포 시각화
# x,y 축 그리기
온도_최솟값 = min(센서_데이터$온도) 
온도_최댓값 = max(센서_데이터$온도)
x = seq(온도_최솟값 - 10, 온도_최댓값 + 10, length = 50)
y = dnorm(x, mu, sigma) #x값, 평균, 표준편차 (순서 주의!)

# PDF 저장하기
pdf('sensor_data.pdf', family ='Korea1deb') #PDF 선언

# 정규분포 그리기
plot(x, y, type='l',main = '온도센서 정규분포도', xlab='온도', ylab='확률')

# 중앙값, 하위 20%, 상위 20% 조회
med = median(센서_데이터$온도) #중앙값 조회
q20 = quantile(센서_데이터$온도, 0.2) #하위 20
q80 = quantile(센서_데이터$온도, 0.8) #상위 20

# 선표시
abline(v=med, col='purple') #중앙값은 보라
abline(v=q20, col='forestgreen') #초록 하위
abline(v=q80, col='orange') # 주황 상위

# 신뢰구간 표시
n = nrow(센서_데이터) #데이터 행의 수 조회
se = sigma / sqrt(n) #표준편차 조회
# 신뢰구간(confidence interval) 계산
ci_low = mu - 1.96 * se
ci_high = mu + 1.96 * se
# 신뢰구간 표기
abline(v=ci_low, col = 'red')
abline(v=ci_high, col = 'red')

dev.off()# PDF 닫기




