weather = read.csv('날씨데이터2.csv', #csv파일이름
                   na.strings = c(""),
                   fileEncoding = 'CP949', #한글깨짐 방지
                   encoding = 'UTF-8', #한글깨짐 방지
                   check.names = FALSE)

colnames(weather) = c('station_id', 'station_name', 'datetime', 'temp', 'precip','windspeed', 'winddir', 'humidity', 'CA')

library(dplyr)

str(weather)

#head로 상위 데이터 확인
print(head(weather)) #상위 6개 행 데이터 확인 / 갯수(n) 정하고 싶으면 weather, n
print(nrow(weather)) #수집한 데이터 전체 개수



print(summary(weather)) #전체 컬럼 요약(사분위수, 평균)


print(table(weather$지점명)) #각 지점명 개수 확인


#일시(datatime) 문자에서 date 형태로 형 변환
#as.Date(연,월,일) -> 시간 정보를 무시
#as.POSIXct(포직스) -> 날짜와 시간을 모두 형 변환
weather$datetime = as.POSIXct(weather$datetime, format = '%Y-%m-%d %H:%M')
str(weather) #형변환 컬럼 확인


#누락된 데이터(결측치 Missing Value) 처리
#col+Sums
print(colSums(is.na(weather))) #전체 컬럼 NA 개수 통계

#결측값 0으로 대체
weather$precip[is.na(weather$precip)] = 0
weather$temp[is.na(weather$temp)] = 0
weather$windspeed[is.na(weather$windspeed)] = 0
weather$winddir[is.na(weather$winddir)] = 0
weather$humidity[is.na(weather$humidity)] = 0
weather$CA[is.na(weather$CA)] = 0
weather$feels_like[is.na(weather$feels_like)] = 0


#ifelse
weather$precip = ifelse(is.na(weather$precip),0, weather$precip)
print(colSums(is.na(weather))) #결측값 개수 확인

#체감온도 만들기
weather$feels_like = weather$temp - ((100-weather$humidity)/5)
print(head(weather))



#3. 기초 통계량 확인 및 시각적 탐색(EDA)
#3-1. 분석할 컬럼 통계량 산출

print(summary(weather$temp))
print(summary(weather$humidity))
print(summary(weather$precip))
print(summary(weather$windspeed))


#온도 표준편차
temp_sd = sd(weather$temp, na.rm=TRUE)
print(round(temp_sd, 2)) #10.34


#변동계수 (변동계수 구하려면 평균을 알아야함)
temp_avg = mean(weather$temp, na.rm=TRUE)
CV = (temp_sd / temp_avg) *100
cat('온도 변동 계수: ', CV, '\n') #온도 변동 계수:  122.8989 
#보통 CV가 10~20 이하면 고르게 분포


#상관계수 행렬(cor_mat)
cor_mat = cor(weather[, c('temp', 'precip',  'windspeed', 'humidity')], use='complete.obs')
print(cor_mat)



#그래프로 해당 상관관계 표현

#시각적 탐색(EDA)
#단일변수 시각화
#두 변수 간의 관계 시각화

library(corrgram) #상관관계 그래프

#시각화
corrgram(cor_mat, main = '온도, 강수량, 풍속, 습도 상관계수',
         lower.panel = panel.shade,
         upper.panel = panel.cor)


par(mfrow=c(1,2)) #1행 2열 그래프 배치

#온도 히스토그램
hist(weather$temp, main='온도 데이터 분포', xlab='온도(c)')

#습도 히스토그램
hist(weather$humidity, main='습도 데이터 분포', xlab='습도')

#박스플롯
boxplot(weather$temp, main='온도 박스플롯', xlab='온도(c)')


#두 변수간의 관계 시각적 표현
#1. 시간별 기온 변화
par(mfrow=c(1,2))
plot(weather$datetime, weather$temp, type='l',
     main='시간별 기온 변화', xlab='시간', ylab='온도(c)')

#회귀선 추가
model = lm(weather$temp ~ weather$datetime)
abline(model, col='red', lwd=2)



#1-1. 풍속과 기온변화 , 회귀선
plot(weather$windspeed, weather$temp, type='l',
     main='풍속과 기온 변화', xlab='풍속', ylab='온도(c)')
#회귀선
model=lm(weather$temp ~ weather$windspeed)
abline(model, col='red', lwd=2)


#4. 데이터 전처리
# 이상치 제거
# 디플리알
library(dplyr) #디플리알 불러오기

#IQR을 이용해서 이상치 판단
Q1 = quantile(weather$temp, 0.25, na.rm=TRUE) 
Q3 = quantile(weather$temp, 0.75, na.rm=TRUE)
IQR = Q3 - Q1


#1.5 수치는 너무 좁지도, 너무 넓지도 않은 적절한 범위를 설정하기 위해 표준으로 사용하고 있음.

lower_bound = Q1 - 1.5 * IQR
upper_bound = Q3 + 1.5 * IQR


weather = weather %>% 
  mutate(temp=ifelse(temp < lower_bound | temp > upper_bound, NA, temp))


#이상치 데이터 개수 확인
#기온 데이터가 NA라는 건 이상치라는 뜻


print(sum(is.na(weather$temp))) #이상치 데이터 0개
cat('이상치 데이터 개수:', sum(is.na(weather$temp)), '\n')




#5월 1일 부터 ~ 5월 31일 서울 지점 데이터 필터링
weather_filter = weather %>% filter(datetime >= '2025-05-01 00:00' &
                                      datetime <= '2025-05-31 23:59' &
                                      station_name == '서울')

#View(weather_filter)


#시간별 데이터 수집 -> 날짜별 평균을 내어 하루에 하나의 값만 남기는 전처리
#디플리알 as.Date(): 시간 정보를 무시
#date라는 컬럼 만들기

weather_daily = weather_filter %>% mutate(date = as.Date(datetime)) %>%
  group_by(date) %>% summarise((temp_avg=mean(temp, na.rm=TRUE)))
print(head(weather_daily))


#실용적 분석
#시간마다 기록된 숫자 -> '시계열 데이터(time series)'

temp_ts = ts(weather_daily$temp_avg, frequency = 30)
print(temp_ts)


#install.packages('forecast')
library(forecast) #시계열 데이터를 바탕으로 미래를 예측하는 통계모델

auto_model = auto.arima(temp_ts)

#향후 30시간 기온 예측
#미래 패턴을 예측하고 신뢰구간까지 산출함.

forecasted = forecast(auto_model, h=30)


# 예측 결과 데이터프레임 생성
predict_data = data.frame(
  time=as.numeric(time(forecasted$mean)),
  forecast = as.numeric(forecasted$mean),
  lower = as.numeric(forecasted$lower[,2]), #95% 신뢰구간 하한
  upper = as.numeric(forecasted$upper[,2]) #95% 신뢰구간 상한
)

print(predict_data)



# 실제 값 데이터 프레임 생성
actual_data = data.frame(
  time = as.numeric(time(temp_ts)),
  temp = as.numeric(temp_ts)
)


library(ggplot2) #고급 시각화


line_plot = ggplot() +
  # 실제값 선그래프
  geom_line(data = actual_data, aes(x = time, y = temp), color = "steelblue", size = 1) +
  # 예측 신뢰구간 리본
  geom_ribbon(data = predict_data, aes(x = time, ymin = lower, ymax = upper), fill = "orange", alpha = 0.3) +
  # 예측값 선그래프
  geom_line(data = predict_data, aes(x = time, y = forecast), color = "red", size = 1.2, linetype = "dashed") +
  labs(
    title = "Temperature Forecast",
    x = "Time",
    y = "Temperature"
  ) +
  theme_minimal()


print(line_plot)



#서울, 경기도 1월부터 6월 일별 온도 평균
weather_daily = weather %>% mutate(date=as.Date(datetime)) %>%
  group_by(date) %>% summarise(temp_avg = mean(temp, na.rm=TRUE))

print(head(weather_daily))


#시계열 타입으로 변환
#frequency = 30 한달에 30일 있다고 가정
temp_ts = ts(weather_daily$temp_avg, frequency = 30)
print(head(temp_ts))

library(forecast)


#arima(): 시계열 데이터를 분석하고 미래 값 예측하는데 사용
auto_model = auto.arima(temp_ts)
#향후 30일 예측 h=30
forecasted = forecast(auto_model, h=30)

print(forecasted)


#예측 데이터와 실제 데이터를 데이터 프레임으로 만들어서 시각화
predict_data = data.frame(
  time = as.numeric(time(forecasted$mean)),
  forecast = as.numeric(forecasted$mean),
  lower = as.numeric(forecasted$lower[,2]),  # 95% 신뢰구간 하한
  upper = as.numeric(forecasted$upper[,2])   # 95% 신뢰구간 상한
)

# 실제 값 데이터 프레임 생성
actual_data = data.frame(
  time = as.numeric(time(temp_ts)),
  temp = as.numeric(temp_ts)
)


library(ggplot2) #고급 시각화


line_plot = ggplot() +
  # 실제값 선그래프
  geom_line(data = actual_data, aes(x = time, y = temp), color = "steelblue", size = 1) +
  # 예측 신뢰구간 리본
  geom_ribbon(data = predict_data, aes(x = time, ymin = lower, ymax = upper), fill = "orange", alpha = 0.3) +
  # 예측값 선그래프
  geom_line(data = predict_data, aes(x = time, y = forecast), color = "red", size = 1.2, linetype = "dashed") +
  labs(
    title = "Temperature Forecast",
    x = "Time",
    y = "Temperature"
  ) +
  theme_minimal()


print(line_plot)




#MAE(모델성능평가)

autual = weather_daily$temp_avg
actual_lenghth = length(actual) #actual 데이터 길이
cat('actual_length: ', actual_length, '\n')


predicted = as.numeric(forecasted$mean) #예측 데이터
predicted_length = length((predicted))
cat('pridicted_length:', predicted_length, '\n') #30



#MAE : 예측이 얼마나 잘 맞았는지 쉽게 알려주는 '점수'

#actual[1 : predicted_lenghth] : 실제값 첫번째부터 predicted_lenghth 길이 만큼만

MAE = mean(abs(actual[1: predicted_length]-predicted))
cat('MAE ;', MAE, '\n') #23.38489 

#예측값과 실제값의 차이가 평균적으로 23도 라는 뜻









