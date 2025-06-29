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


#데이터 요약
#summary: 내가 분석하고자 하는 컬럼 통계확인
print(summary(weather$humidity))
print(table(weather$station_name)) #지점별 데이터 수 확인
print(nrow(weather)) #데이터프레임 총 행 수


#데이터 형 변환
weather$datetime = as.POSIXct(weather$datetime, format='%Y-%m-%d %H:%M')
weather$station_name = as.factor(weather$station_name)

weather$precip = ifelse(is.na(weather$precip),0,weather$precip)
weather$CA = ifelse(is.na(weather$CA), 0, weather$CA)
print(colSums(is.na(weather)))



#온도와 습도의 상관관계 표현
colors = rainbow(6) #지점 총 6개

#시각화
plot(weather$temp, weather$humidity, col=colors, xlab='온도', ylab='습도',
     main='온도와 습도의 관계')

#pch: 점크기
#cex=0.8 : 크기를 80%로 줄이다 (1이면 기본크기)
legend('topright', legend=unique(weather$station_name),
       col = colors, pch=19, cex=0.8)



#풍속과 습도 K 평균 군집화 시각화
weather_var = weather[,c('windspeed', 'humidity')]
set.seed(123) #항상 같은 결과가 나오게 설정
clusters = kmeans(weather_var, centers=3)
plot(weather$windspeed, weather$humidity, col=clusters$cluster)


#온도 변화 시계열 그래프
#as.Date 시간생략되고 년월일만 표기
weather$date = as.Date(weather$datetime)
plot(weather$date, weather$temp, type='l', col='blue',
     main='날짜별 온도변화', xaxt='n')


#월별로 x축 레이블 표기
month = seq(as.Date('2025-01-01'), as.Date('2025-07-01'), by='month')
axis.Date(1, at=month[month<=max(weather$date)], 
          format='%Y-%m', las=1, cex.axis=0.7)




#데이터 전처리

# 각 지점별로 평균 기온을 구하시오. (dplyr)
# # hint: group_by()와 summarise() 
mean_temp_by_station = weather %>% group_by(station_name) %>%
  summarise(평균기온 = mean(temp, na.rm=TRUE))


# 풍속이 3 m/s 이상인 데이터만 골라서, 해당 데이터의 평균 습도를 구하시오. (dplyr)
# # hint: filter()와 summarise()
mean_hum_high_wind = weather %>% filter(windspeed>=3) %>%
  summarise(평균습도=mean(humidity, na.rm=TRUE))



# 3월부터 5월까지 서울의 평균 강수량, 최대 강수량, 최소 강수량 구하시오. (dplyr)
# # hint: filter()와 summarise()

#방법1
seoul_data = weather %>% filter(datetime >= '2025-03-01 01:00' & 
                                  datetime <= '2025-05-31 23:59' &
                                  station_name=='서울') %>%
  summarise(
    평균강수량 = mean(precip, na.rm = TRUE),
    최대강수량 = max(precip, na.rm = TRUE),
    최소강수량 = min(precip, na.rm = TRUE))

#방법2
seoul_data2 = weather %>% mutate(month=format(datetime, '%m')) %>%
  filter(month %in% c('03','04','05') & station_name =='서울') %>%
  summarise(
    평균강수량 = mean(precip, na.rm = TRUE),
    최대강수량 = max(precip, na.rm = TRUE),
    최소강수량 = min(precip, na.rm = TRUE))

  


# 각 지점별로 기온이 가장 높았던 시간대와 그 값을 구하시오. (dplyr) 단, 값을 기준으로 내림차순 할 것
# # hint: group_by()와 filter(), select(), arrange()

#방법1
max_temp_by_station = weather %>% group_by(station_name) %>% 
  filter(temp==max(temp,na.rm=TRUE)) %>% 
  select(station_id, station_name, datetime, temp) %>%
  arrange(desc(temp))
print(max_temp_by_station)


#방법2
max_temp_by_station2 = weather %>% group_by(station_name) %>%
  select(station_id, station_name, datetime, temp) %>%
  slice_max(temp, n=1)  %>%  arrange(desc(temp))
print(max_temp_by_station2)




# 일짜별 습도 평균 구하기 (dplyr)
mean_hum_by_date = weather %>% mutate(data = as.Date(datetime)) %>%
  group_by(date) %>% 
  summarise(평균습도=mean(humidity, na.rm=TRUE))
print(mean_hum_by_date)


# ts : 시계열 타임으로 형변환
humidity_ts = ts(mean_hum_by_date$평균습도, frequency = 30)
str(humidity_ts)


# 향후 10일 습도 예측
library(forecast)


# ts로 데이터를 변환해야 arima 시계열 모델에 넣을 수  있다.
# arima : 시계열 분석 대표 미래 예측 모델
auto_model = auto.arima(humidity_ts)
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






















