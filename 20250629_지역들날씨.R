#STEP 2 : 데이터 구조와 변수 확인
#CSV 파일 불러오기

print(getwd()) #폴더 조회
print(list.files()) #폴더에 저장된 파일 조회


weather = read.csv('지역들날씨데이터.csv', na.strings = c(""), fileEncoding = 'CP949', encoding = 'UTF-8', check.names = FALSE)
str(weather)

#head, tail
print(head(weather))
print(tail(weather))

#데이터 요약 summary, table
print(summary(weather))
print(table(weather$지점명))

#데이터 행 수 조회 nrow
print(nrow(weather))

#컬럼명 한글 -> 영어로 수정 colnames
colnames(weather) = c("station_id", "station_name", "datetime", "temp", "precip", "windspeed", "winddir", "humidity","CA")
str(weather)

#데이터 형변환
#hint 문자로 입력된 날짜를 문자형으로 변환합니다.
#변환 후 변경된 데이터타입을 다시 확인합니다.
weather$datetime = as.POSIXct(weather$datetime, format = '%Y-%m-%d %H:%M')
str(weather)


#결측값 조회 및 0으로 대체
#hint colSums, is.na
#각 컬럼들 결측값 조회
print(colSums(is.na(weather)))
#ifelse를 이용해서 결측값 0으로 대체
weather$precip = ifelse(is.na(weather$precip),0,weather$precip)
print(colSums((is.na(weather)))) #최종 확인




# STEP 3 : 기초 통계량 확인 및 시각적 탐색(EDA)
# 풍속, 풍향, 습도의 통계량을 확인합니다.
# hint summary
print(summary(weather$windspeed))
print(summary(weather$winddir))
print(summary(weather$humidity))


# 습도의 표준편차와 변동계수 구하기
# hint sd, mean
hum_sd = sd(weather$humidity, na.rm=TRUE)
print(round(hum_sd,2)) #21.69  #소수점 반올림

#변동계수 (변동계수 구하려면 평균 알아야함)
hum_avg = mean(weather$humidity, na.rm = TRUE)
CV = (hum_sd / hum_avg) *100
cat('습도 변동 계수 ; ', CV, '\n')
#보통 CV가 10~20 이하면 고르게 분포
#50이상이면 데이터가 평균에 비해 상당히 넓게 분포되어 있음

# 풍속과 습도의 상관관계 조회
# hint : cor
#상관계수(cor) 행렬(mat)
#상관계수 값은 -1 ~ 1 사이로 0에 가까울 수록 상관관계가 약함을 의미
cor_mat = cor(weather[, c('windspeed', 'humidity')], use='complete.obs')
print(cor_mat)


# 기온, 풍속, 풍향, 습도, 전운량 상관관계 확인 및 시각화
library(corrgram) #상관관계 그래프
#시각화
corrgram(weather[, c('temp', 'windspeed', 'winddir', 'humidity', 'CA')],
         main = '기온, 풍속, 풍향, 습도, 전운량 상관관계',
         lower.panel = panel.shade,
         upper.panel = panel.cor)


# 온도와 습도 데이터 분포를 히스토그램으로 표현
# # hint : hist
# 온도 히스토그램
hist(weather$temp, main = '기온 분포', xlab = '기온')

# 습도 히스토그램
hist(weather$humidity, main = '습도 분포', xlab = '습도')


# 풍속의 이상치를 탐색하기 위해 박스플롯 표현
# hint : boxplot
boxplot(weather$windspeed, main='풍속 박스플롯', ylab = '풍속(m/s)')


# 온도와 습도의 상관관계 표현, 도시 별 색깔 다르게 지정. 단, 범례도 추가할 것
# hint : plot
plot(weather$temp, weather$humidity,col = c('red', 'blue'),
     xlab='온도', ylab='습도', main='온도와 습도의 관계')


# 범례추가 : legend
legend('topright', legend=unique(weather$station_name), col=c('black'), pch=19)


# 풍속과 풍향 시각화
# hint : windRose
library(openair) #풍속과 풍향 데이터 시각화
windRose(weather, ws='windspeed', wd='winddir')



# 풍속과 습도 K 평균 군집화 시각화
# hint : kmeans

# Kmeans(K 평균 군집화)
# 풍속하고 습도 열만 선택 []
weather_var = weather[,c('windspeed', 'humidity')]

set.seed(123) #항상 똑같은 결과가 나오게 해줘

# 데이터를 3개의 그룹으로 나눠줘
clusters = kmeans(weather_var, centers = 3)

#군집 결과 시각화
plot(weather$windspeed, weather$humidity, col=clusters$cluster,
     xlab='풍속', ylab='습도', main='풍석과 습도 클러스터링')


# 온도 변화 시계열 그래프 그리기
# hint : plot, type="l"
#시계열, 시간의 흐름에 따라 연속적으로 수집된 데이터
#시계열 DB 따로  있음
#lab (라벨)
plot(weather$datetime, weather$temp, type='l', col='blue', 
     xlab='날짜', ylab='온도', main='날자별 온도 변화')