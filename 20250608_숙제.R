library(dplyr)  # 로드
# 한국어 파일의 경우, CP949 또는 EUC-KR 인코딩을 사용하는 것이 일반적
data = read.csv('국민건강보험공단_건강검진정보.csv',
                na.strings = c(""), # ""를 NA로 표현한다.
                fileEncoding = 'CP949', 
                encoding = 'UTF-8', 
                check.names = FALSE)
#View(data)
str(data)



# 문제 1. : 남성의 허리둘레 사분위수 조회.
남성 = data %>% filter(성별 == 1)
print(quantile(남성$허리둘레))


# 문제 2. : 성별 허리둘레와 체중 상관관계 조회.

문제2= data %>% group_by(성별)%>% summarise(
  상관계수 = cor(허리둘레, `체중(5kg단위)`, use='complete.obs')
)
print(문제2)


# 문제 3. : 혈색소 수치는 남성은 13~17, 여성은 12~16이 정상입니다. 정상과 의심을 구별할 수 있는 컬럼 혈색소결과를 만드시오.

data$혈색소_수 = ifelse(data$성별 == 1 & data$혈색소 >= 13 & data$혈색소 <= 17, 
                    '정상',
                    ifelse(data$성별 == 2 & data$혈색소 >= 12 & data$혈색소 <= 16, 
                           '정상', 
                           '의심'))
names(data)




# 문제 4. : 식전혈당이 126이상은 위험, 100미만은 정상 그외는 주의를 나타내는 컬럼 당뇨병위험을 추가하시오.

data$당뇨병위험 = ifelse(data$`식전혈당(공복혈당)`>=126,'위험', ifelse(
  data$`식전혈당(공복혈당)`<100, '정상', '주의'))

print(data)
names(data)

# 추가 후 당뇨병위험 별 인원 수 조회.
인원수 = data %>% group_by(당뇨병위험) %>% summarise(인원수 = n())
print(인원수)



# 문제 5. : 연령대 코드가 5~8인 사람 중 혈색소의 중앙값, 하위 30%, 상위 10%, 표준편차 조회.

문제5 = data %>% filter(`연령대코드(5세단위)` %in% c(5,6,7,8))

med = median(문제5$혈색소, na.rm=TRUE)      # 중앙값
q30 = quantile(문제5$혈색소, 0.3, na.rm=TRUE)  # 하위 30%
q90 = quantile(문제5$혈색소, 0.9, na.rm=TRUE)  # 상위 10%
sigma = sd(문제5$혈색소, na.rm=TRUE)         # 표준편차

print(med)
print(q30)
print(q90)
print(sigma)


# 문제 6. : 음주와 흡연을하는 남성의 혈색소 이상치를 제거한 데이터 수 조회. 임계치는 2로 필터링
남자_음주흡연 = data %>% filter(성별==1, 음주여부==1, 흡연상태==1)
print(남자_음주흡연)

mu = mean(남자_음주흡연$혈색소,na.rm=TRUE) #평균
sigma = sd(남자_음주흡연$혈색소,na.rm=TRUE) #표준편차

#이상치제거

남자_음주흡연$z_score=abs((남자_음주흡연$혈색소-mu)/sigma)
print(남자_음주흡연)

남자_음주흡연_이상치제거 = 남자_음주흡연 %>% filter(z_score < 2)
print(남자_음주흡연_이상치제거)



# 문제 7. : 연령대 코드별로 허리둘레의 분포를 박스플롯으로 나타내세요.

문제7 = data %>% group_by(`연령대코드(5세단위)`)

#박스플롯
boxplot(`연령대코드(5세단위)`~허리둘레, 
        data = 문제7, 
        main = '연령대별 허리둘레',
        xlab='연령대',
        ylab='허리둘레',
        col=c('grey', 'pink', 'skyblue'))


# 문제 8. : 연령대 코드가 5~8인 사람의 신장과 체중의 관계를 산점도로 나타내시오. 회귀선도 추가하시오.

문제8 = data %>% filter(`연령대코드(5세단위)` %in% c(5,6,7,8))

#산점도
plot(문제8$`신장(5cm단위)`, 문제8$`체중(5kg단위)`,
     main='신장과 체중의 관계',
     xlab='신장',
     ylab='체중',
     col=c('grey', 'pink', 'skyblue'))


# 회귀선 추가
model = lm(`신장(5cm단위)` ~ `체중(5kg단위)`, data = 문제8)
abline(model, col = 'red', lwd = 2)





# 문제 9. : 감마지티피의 분포를 정규분포그래프으로 나타내세요. 중앙값, 하위 20%, 상위 20%, 신뢰구간도 표현해주세요.
# 문제 10. : 혈청지오티와 혈청지피티의 관계를 산점도로 나타내시오. 단, 혈청지오티와 혈청지피티를 min-max로 스케일링 후 비교하시오. 회귀선도 추가하시오.























