scereal = read.csv('UScereal.csv')
#View(scereal)

#상관관계 시각화

#cor(correlation)의 약자로 상관계수라는 뜻

#설탕과 칼로리의 상관관계 파악하기

#상관관계 구하기**
#complete.obs 결측값 처리

설탕과칼로리_상관관계=cor(scereal$sugars,scereal$calories, use='complete.obs')

# 결과 출력 (소수 둘째 자리까지)
cat('설탕과 칼로리 상관관계: ', round(설탕과칼로리_상관관계, 2),'\n')


#0.5:  보통 상관관계

#-1 ~ 1 : -1(음의 관계), 0(관계 없음), 1(양의 관계)

#상관계수 해석: 0.3(이하) => 약한 관계, 0.3~0.7 => 중간, 0.7(강한 관계) 

#-----------------------------------------------------

#install.packages('corrgram') #최초 한번 설치
library(corrgram) #설치한 프로그램 불러오기
library(dplyr)

테스트 = scereal %>% select(calories, protein, fat)
print(head(테스트)) #추출 확인

corrgram(테스트, #그래프에 들어갈 데이터
         main = '칼로리, 단백질, 지방 상관관계 행렬', #그래프 제목
         lower.panel = panel.shade, #아래쪽 색상으로 상관관계 표현
         upper.panel = panel.cor, #위쪽 상관관계 수치 표현
         diag.panel = panel.minmax #대각선은 최솟, 최댓값 표현
         )



#문제: 제조사별 평균 칼로리, 평균단백질, 평균식이섬유 상관관계

제조사별_데이터= scereal%>% group_by(mfr) %>%
  summarise(
    cal_avg = mean(calories, na.rm=TRUE),
    pro_avg = mean(protein, na.rm=TRUE),
    fi_avg = mean(fibre, na.rm=TRUE),
  )

corrgram(제조사별_데이터,
         main='제조사별 평균 칼로리, 단백질, 식이섬유 상관관계',
         lower.panel = panel.shade, #아래쪽 색상으로 상관관계 표현
         upper.panel = panel.cor, #위쪽 상관관계 수치 표현, panel.pie도 가능
         diag.panel = panel.minmax #대각선은 최솟, 최댓값 표현
)



#문제: 데이터에서 나트륨(sodium), 식이섬유(fibre), 복합탄수화물(carbo),
#칼륨(potassium) 컬럼을 선택하고, 결측치가 아닌 데이터만
#corrgram으로 상관관계 시각화하시오.


# !is.na :  결측값(누락된 데이터)가 아니라면 **********
결측치가아닌데이터 = scereal %>% 
  filter(!is.na(sodium) & !is.na(fibre) & !is.na(carbo) & !is.na(potassium)) %>%
  select(sodium, fibre, carbo, potassium)

corrgram(결측치가아닌데이터,
         main= '나트륨, 식이섬유, 복합탄수화물, 칼륨 상관관계',
         lower.panel = panel.shade, #아래쪽 색상으로 상관관계 표현
         upper.panel = panel.cor, #위쪽 상관관계 수치 표현, panel.pie도 가능
         diag.panel = panel.minmax #대각선은 최솟, 최댓값 표현
)



상관관계 = cor(scereal$fibre, scereal$potassium, use='complete.obs')
print(상관관계)


































