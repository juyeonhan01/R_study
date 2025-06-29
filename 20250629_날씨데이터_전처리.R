#CSV 파일 불러오기
weather = read.csv('지역들날씨데이터.csv',
                   na.strings = c(""),
                   fileEncoding = 'CP949', # 한글 데이터 파일은 인코딩이 맞지 않으면 깨질 수 있음.
                   encoding = 'UTF-8', 
                   check.names = FALSE)
str(weather)

#컬럼명 한글 -> 영어로 수정 colnames
colnames(weather) = c("station_id", "station_name", "datetime", "temp", "precip", "windspeed", "winddir", "humidity","CA")
str(weather)

# 풍속 IQR 이상치 제거
# # hint : quantile
Q1 = quantile(weather$windspeed, 0.25) #하위 25%
cat('Q1 :', Q1, '\n')
Q3 = quantile(weather$windspeed, 0.75) #하위 75%
cat('Q3: ', Q3, '\n')

#IQR(사분위 범위) 구하기
#프로그래밍에서 변수이름이 대문자면 -> 수정x
IQR_VALUE = Q3 - Q1 
cat('IQR_VALUE :', IQR_VALUE, '\n')

#3. 이상치 기준선 만들기
#프로그래밍에서 변수이름이 소문자면 -> 수정o
lower_bound = Q1 - 1.5 * IQR_VALUE

upper_bound = Q3 + 1.5 * IQR_VALUE

cat('lower_bound :', lower_bound, '\n')
cat('upper_bound :', upper_bound, '\n')

#4. 이상치 확인
library(dplyr)
outliers = weather %>% filter(windspeed < lower_bound | windspeed > upper_bound)
print(outliers)

# 각 지점별로 평균 기온을 구하시오. (dplyr)

# # hint: group_by()와 summarise() 
library(dplyr)
station_temp = weather %>% group_by(station_id) %>% summarise(temp_avg=mean(temp, na.rm=TRUE))


# 풍속이 3 m/s 이상인 데이터만 골라서, 해당 데이터의 평균 습도를 구하시오. (dplyr)
# # hint: filter()와 summarise()
hum_avg = weather %>% filter(windspeed>3) %>% summarise(hum_avg=mean(humidity, na.rm=TRUE))
print(head(hum_avg))

# 3월부터 5월까지 서울의 평균 강수량, 최대 강수량, 최소 강수량 구하시오. (dplyr)
# # hint: filter()와 summarise()
precip_seoul = weather %>%
  filter(station_name == '서울' & 
           datetime >= '2025-03-01 00:00' & 
           datetime <= '2025-05-31 23:59') %>%
  summarise(
    avg_precip = mean(precip, na.rm = TRUE),
    max_precip = max(precip, na.rm = TRUE),
    min_precip = min(precip, na.rm = TRUE)
  )

print(precip_seoul)


# 각 지점별로 기온이 가장 높았던 시간대와 그 값을 구하시오. (dplyr) 단, 값을 기준으로 내림차순 할 것
# # hint: group_by()와 summarise(), arrange()
max_temp_by_station = weather %>% group_by(station_name) %>% 
  filter(temp==max(temp,na.rm=TRUE)) %>% 
  select(station_id, station_name, datetime, temp) %>%
  arrange(desc(temp))



# 날짜별 습도 평균 구하기 (dplyr)
# # hint: mutate()와 group_by(), summarise()
library(dplyr)
hum_daily = weather %>% mutate(date = as.Date(datetime)) %>%
  group_by(date) %>% summarise(hum_avg = mean(humidity, na.rm=TRUE))
print(head(hum_daily))



# STEP 5 : 실용적 분석

# 일별 평균 습도데이터를 한달 주기의 시계열 데이터로 반환
# # hint: ts, frequency = 30
humidity_ts = ts(hum_daily$hum_avg, frequency = 30 )
print(humidity_ts)



# 향후 10일 습도 예측
library(forecast)
auto_model = auto.arima(humidity_ts)

#  h = 에 올바른 값 넣기
forecasted = forecast(auto_model, h = 30)


# 예측 결과,실제 결과를 데이터프레임으로 생성
predict_data = data.frame(
  time=as.numeric(time(forecasted$mean)),
  forecast = as.numeric(forecasted$mean),
  lower = as.numeric(forecasted$lower[,2]), #95% 신뢰구간 하한
  upper = as.numeric(forecasted$upper[,2]) #95% 신뢰구간 상한
)

actual_data = data.frame(
   time = as.numeric(time(humidity_ts)),
   temp = as.numeric(humidity_ts)
 ) 


# 고급 시각화 
library(ggplot2) #고급 시각화

line_plot = ggplot() +
# 실제값 선그래프
   geom_line(data = actual_data, aes(x = time, y = temp), color = "steelblue", size = 1) +
   # 예측 신뢰구간 리본
   geom_ribbon(data = predict_data, aes(x = time, ymin = lower, ymax = upper), fill = "orange", alpha = 0.3) +
   # 예측값 선그래프
   geom_line(data = predict_data, aes(x = time, y = forecast), color = "red", size = 1.2, linetype = "dashed") +
   labs(
     title = "습도 예측",
     x = "Time",
     y = "습도"
   ) +
   theme_minimal()
 
 print(line_plot)
 



