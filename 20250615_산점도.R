#install.packages('ggplot2') #R에서 자주사용하는 시각화 프로그램
 #설치한 ggplot2 불러오기

scereal = read.csv('UScereal.csv')

#기본 산점도 표현: 칼로리 vs 단백질
#aes: aesthetics (미적 속성)
#labs: labels
#lm: linear model(선형모델)
#se: 회귀선에서 신뢰구간 표시

plot = ggplot(scereal, aes(x=calories, y=protein)) + #산점도 데이터
  geom_point() + #산점도에 점
  labs(title='칼로리vs단백질',
       x='칼로리', y='단백질') +
  geom_smooth(method='lm', se=TRUE) + #회귀선 추가, 신뢰구간 추가(회색)
  theme_minimal() #그래프 배경 깔끔하게 처리

#print(plot) 

#산점도 그래프에 텍스트 추가
#check_overlap: 점이 겹칠 경우 일부 라벨을 표기하지 않도록 합니다

scereal$label = rownames(scereal) #행의 번호를 label 이라는 컬럼에 추가

plot = ggplot(scereal, aes(x=calories, y=protein)) + #산점도 데이터
  geom_point() + #산점도에 점
  labs(title='칼로리vs단백질',
       x='칼로리', y='단백질') +
  geom_smooth(method='lm', se=TRUE) + #회귀선 추가, 신뢰구간 추가(회색)
  geom_text(aes(label = label), hjust=0, vjust=1, size=3,check_overlap = TRUE) +
  theme_minimal() #그래프 배경 깔끔하게 처리
#print(plot)
#View(scereal) #시퀀스 확인


#----------------------------------------------------------------------
#칼로리가 낮은 제품은 good, 높은 제품은 bad를 구분하는 새로운 열 추가
#칼로리가 평균보다 낮으면 good, 아니면 bad
#새로운 열 'grade ' 추가

칼로리_평균=mean(scereal$calories, na.rm=TRUE)
cat('칼로리_평균:', 칼로리_평균, '\n')

scereal$grade=ifelse(scereal$calories <= 칼로리_평균, 'good', 'bad')
print(head(scereal))


#good 이면 동그라미, bad면 세모표시

plot = ggplot(scereal, aes(x=calories, y=protein, shape=grade, color=grade)) + 
  geom_point() + 
  labs(title='칼로리vs단백질', x='칼로리', y='단백질') +
  scale_shape_manual(values=c('good'=16, 'bad'=17)) + 
  scale_color_manual(values=c('good'='blue', 'bad'='red')) + 
  geom_smooth(method='lm', se=TRUE) + 
  theme_minimal()


#print(plot)

#ggplot 에서는 ggsave 라는 명령어로 쉽게 저장할 수 있습니다.
# dpi 해상도까지 조절가능
ggsave('myplot.png', width = 10, height=6, dpi=300)





















