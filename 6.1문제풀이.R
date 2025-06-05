# 한국어 파일의 경우, CP949 또는 EUC-KR 인코딩을 사용하는 것이 일반적
deta = read.csv('서울특별시_공공자전거_이용정보.csv',
                na.strings = c(""), # ""를 NA로 표현한다.
                fileEncoding = 'CP949', 
                encoding = 'UTF-8', 
                check.names = FALSE)
#View(deta)


# 데이터전처리만 진행해주세요.
str(deta)

# 문제 1: 이동거리(M)가 2000 이상인 데이터 중 해당 대여소명과 이동거리(M), 이용시간(분)만 조회.
문제1 = deta %>% filter(`이동거리(M)`>=2000) %>% select(대여소명,`이동거리(M)`,`이용시간(분)`)
print(문제1)


# 문제 2: 대여소 별 이용건 수 조회.
문제2 = deta %>% group_by(대여소명) %>% summarise(이용건수 = n())
print(문제2)


# 문제 3: 일일회원과 정기회원 이용 건 수, 평균 이용시간 조회. 단, 일일회원권 중 비회원은 제외

문제3 = deta %>%
  filter(
    ('대여구분코드' == '일일권' & '대여구분코드' == '정기권')
  ) %>%
  group_by(대여구분코드) %>%
  summarise(
    이용건수 = n(),
    평균_이용시간 = mean(`이용시간(분)`, na.rm = TRUE)
  )

print(문제3)



# 문제 4: 탄소량이 0.8 이상인 이용 건수는 몇 건인지 조회.


colnames(deta)

문제4 = data %>%
  filter('탄소량' >= 0.8) %>%
  summarise(이용건수 = n())

print(문제4)

# 문제 5: 연령대별로 평균 이동거리(M) 조회.

문제5= deta %>%
  group_by(연령대코드) %>% 
  summarise(평균_이동거리 = mean(`이동거리(M)`))
print(문제5)


# 문제 6: 연령대별로 이용건수의 합과 평균 운동량을 구한 뒤, 운동량 평균이 가장 높은 연령대 조회.
str(deta)
문제6 = deta %>% group_by(연령대코드) %>% 
  summarise(이용건수 = n(), 평균_운동량= maen(운동량))  %>%
  arrange(desc(평균_운동량)) %>%
  slice(1)
print(문제6)



# 문제 8: 10대 여성 회원의 평균 운동량, 평균 이동거리 조회. 단, 평균 운동량으로 내림차순 할 것

문제8 = deta %>%
  filter(연령대코드 == "10대", 성별코드 == "F") %>%
  summarise(
    평균_운동량 = mean(운동량, na.rm = TRUE),
    평균_이동거리 = mean(이동거리(M), na.rm = TRUE)
  ) %>%
  arrange(desc(평균_운동량))

print(문제8)


# 문제 9: 운동량을 데이터 스케일링 min-max로 변환한 scaled_운동량 컬럼을 추가 0.8 이하인 회원 이동거리 사분위수 출력


문제9 = deta %>%
  mutate(
    scaled_운동량 = (운동량 - min(운동량, na.rm = TRUE)) / 
      (max(운동량, na.rm = TRUE) - min(운동량, na.rm = TRUE))
  ) %>%
  filter(scaled_운동량 <= 0.8)












