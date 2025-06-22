#1. 데이터 수집
#1-1. 수집한 데이터를 불러오기
weather = read.csv('날씨데이터.csv',
                   na.strings = c(""),
                   fileEncoding = 'CP949', # 한글 데이터 파일은 인코딩이 맞지 않으면 깨질 수 있음.
                   encoding = 'UTF-8', 
                   check.names = FALSE)



#2. 데이터 구조와 변수 확인
#int : 정수 / num : 실수 / chr : 문자
str(weather) #각 컬럼(벡터)들 데이터 타입 꼭 확인



#head로 상위 데이터 확인
print(head(weather)) #상위 6개 행 데이터 확인 / 갯수(n) 정하고 싶으면 weather, n
print(nrow(weather)) #수집한 데이터 전체 개수



print(summary(weather)) #전체 컬럼 요약(사분위수, 평균)


print(table(weather$지점명)) #각 지점명 개수 확인

#컬럼이 한글입니다. 실무에서는 한글쓰지말기
#컬럼명을 한글에서 영어로 변경
#col(컬럼) + names
colnames(weather) = c('station_id', 'station_name', 'datatime', 'temp', 'precip',
                      'windspeed', 'winddir', 'humidity', 'CA')



#한글은 2바이트, 영어는 1바이트
#한글 변수명은 일부 함수(기능)에서 오류로 인식
print(head(weather)) #컬럼 명어로 변경 되었는지 확인



#일시(datatime) 문자에서 date 형태로 형 변환
#as.Date(연,월,일) -> 시간 정보를 무시
#as.POSIXct(포직스) -> 날짜와 시간을 모두 형 변환
weather$datatime = as.POSIXct(weather$datatime, format = '%Y-%m-%d %H:%M')
str(weather) #형변환 컬럼 확인



#누락된 데이터(결측치 Missing Value) 처리
#is.na() 결측값이니?
#col+Sums
print(colSums(is.na(weather))) #전체 컬럼 NA 개수 통계



#결측값을 0으로 대체하거나 해당 컬럼의 평균으로 대체(precip 결측값이 큼)
#결측값 0으로 대체
weather$precip[is.na(weather$precip)] = 0

#ifelse
weather$precip = ifelse(is.na(weather$precip),0, weather$precip)
print(colSums(is.na(weather))) #결측값 개수 확인


#누락된 컬럼이 있는지?
#체감온도는 없네? 만들어보자
weather$feels_like = weather$temp - ((100-weather$humidity)/5)

print(head(weather))



#3. 기초 통계량 확인 및 시각적 탐색(EDA)
#3-1. 분석할 컬럼 통계량 산출
print(summary(weather$temp)) #온도 통계량 요약
print(summary(weather$humidity)) #습도 통계량 요약
print(summary(weather$precip)) #강수량 통계량 요약
print(summary(weather$windspeed)) #풍속 통계량 요약



#온도 표준편차 
#na.rm=TRUE 결측값을 제외하고 표준편차 구하기
# 표준편차가 작다는 것은 데이터가 평균 주변에 잘 모여있음
# 표준편차가 크게 나왔음 -> 아하, 이상치 제거를 해야겠구나

temp_sd = sd(weather$temp, na.rm=TRUE)
print(round(temp_sd,2)) #10.00  #소수점 반올림



#변동계수 (변동계수 구하려면 평균 알아야함)
temp_avg = mean(weather$temp, na.rm = TRUE)
CV = (temp_sd / temp_avg) *100
cat('온도 변동 계수 ; ', CV, '\n')
#보통 CV가 10~20 이하면 고르게 분포
#50이상이면 데이터가 평균에 비해 상당히 넓게 분포되어 있음



#상관계수(cor) 행렬(mat)
#상관계수 값은 -1 ~ 1 사이로 0에 가까울 수록 상관관계가 약함을 의미
# $ : 컬럼 접근, [] : 데이터 접근
#온도, 강수량, 풍속, 습도 상관관계 확인하기
cor_mat = cor(weather[, c('temp', 'precip',  'windspeed', 'humidity')], use='complete.obs')
print(cor_mat)



#그래프로 해당 상관관계 표현

#시각적 탐색(EDA)
#단일변수 시각화
#두 변수 간의 관계 시각화

library(corrgram) #상관관계 그래프


#시각화
corrgram(cor_mat, main='온도,강수량,풍속,습도 상관계수', 
         lower.panel = panel.shade,
         upper.panel = panel.cor)



#기본 plot으로 시각화하기
#ggplot2은 복잡한 시각화에 적함


#히스토그램(단일변수 시각화)
par(mfrow=c(2,3)) #2행 3열로 그래프 배치(단, 기본 그래픽 가능)

hist(weather$temp, main='온도 데이터 분포', xlab='온도(c)')


#습도 히스토그램
hist(weather$humidity, main='습도 데이터 분포', xlab='습도')

#박스플롯
boxplot(weather$temp, main='온도 박스플롯', ylab = '온도(c)')




#두 변수간의 관계를 시각적으로 표현

#1. 시간별 기온 변화
#type='l' : line을 의미
#par(mfrow=c(1,1)) #1행 1열로 다시 나오게 복구
plot(weather$datatime, weather$temp, type='l',
     main = '시간에 따른 온도변화', xlab = '시간', ylab='온도')



#기온과 습도 관계 
#산점도로 회귀선까지 추가
#기온과 습도는 음의 관계

plot(weather$temp, weather$humidity,
     main = '기온과 습도 관계', xlab = '온도', ylab='습도')


# 회귀선 추가
#lm : linear model 선형모델
model = lm(weather$humidity ~ weather$temp) #선형 회귀 모델 생성
abline(model, col = 'red', lwd = 2)
#습도가 높을 수록 기온이 낮아지는 경향이 있다.




#풍속과 기온 관계를 산점도로 표현
plot(weather$windspeed, weather$temp,
     main = '풍속과 기온 관계', xlab ='온도', ylab='풍속')
#회귀선 추가
model = lm(weather$windspeed ~ weather$temp)
abline(model, col = 'red', lwd = 2)




#4. 데이터 전처리
# 이상치 제거
# 디플리알
library(dplyr) #디플리알 불러오기


#기온 이상치 판단

#1. z-score, IQR
# z-score(평균, 표준편차 기준): 키, 몸무게, 성적 등 정규분포 
# IQR(사분위수): 근로소득, 강수량, 기온(비대칭 데이터)


# IQR을 이용해서 이상치 판단
# 데이터를 크기 순서대로 줄 세웠을 때 가운데 50%가 얼마나 퍼져있는 지를 알려줌

Q1 = quantile(weather$temp, 0.25, na.rm = TRUE)
Q3 = quantile(weather$temp, 0.75, na.rm = TRUE)
IQR = Q3 - Q1



#1.5 수치는 너무 좁지도, 너무 넓지도 않은 적절한 범위를 설정하기 위해 표준으로 사용하고 있음.

lower_bound = Q1 - 1.5 * IQR
upper_bound = Q3 + 1.5 * IQR


#mutate:컬럼 수정하거나 추가할 때
weather = weather %>% 
  mutate(temp=ifelse(temp < lower_bound | temp > upper_bound, NA, temp))


#이상치 데이터 개수 확인
#기온 데이터가 NA라는 건 이상치라는 뜻


print(sum(is.na(weather$temp))) #이상치 데이터 0개
cat('이상치 데이터 개수:', sum(is.na(weather$temp)), '\n')


#5월 1일 부터 ~ 5월 31일 대전 지점 데이터 필터링
weather_filter = weather %>% filter(datatime >= '2025-05-01 00:00' &
                                      datatime <= '2025-05-31 23:59' &
                                      station_name == '대전')

#View(weather_filter)




#시간별 데이터 수집 -> 날짜별 평균을 내어 하루에 하나의 값만 남기는 전처리
#디플리알 as.Date(): 시간 정보를 무시
#date라는 컬럼 만들기

weather_daily = weather_filter %>% mutate(date = as.Date(datatime)) %>%
  group_by(date) %>% summarise(temp_avg = mean(temp, na.rm=TRUE))

print(head(weather_daily))




#5. 실용적 분석

#시간마다 기록된 숫자 -> '시계열 데이터'
#시계열 분석은 이 숫자들이 어떻게 바뀌는지 어떤 규칙이 있는지 알아볼 수 있음.

#시계열 데이터(time series)
#frequency = 30 : 일별 평균 기온데이터를 한달 주기의 시계열 데이터로 반환

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
  time = as.numeric(time(forecasted$mean)),
  forecast = as.numeric(forecasted$mean),
  lower = as.numeric(forecasted$lower[,2]),  # 95% 신뢰구간 하한
  upper = as.numeric(forecasted$upper[,2])   # 95% 신뢰구간 상한
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













