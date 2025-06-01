#시각화
emp=read.csv('emp.csv')
library(dplyr)

#1. 박스플롯
# 박스플롯은 여러 그룹의 데이터 분포를 비교하여 중앙값과 퍼짐 정도의 차이를 분석
#예) 남vs여 영어 점수 비교, AvsB 영업성과 비교

pdf('부서별급여_박스플롯.pdf', family = 'Korea1deb')
#부서별 급여 박스플롯으로 시각화
boxplot(emp$SAL~emp$DEPTNO,main = '부서별 급여 현황', xlab = '부서', 
        ylab = '급여', col = c('orange','green','blue'))

dev.off() #pdf다운로드 종료


#막대그래프
#범주형 데이터 빈도나 크기를 비교할 때 사용
#예) 제품별 판매량 비교, 직업에 따른 평균 소득

#부서별 평균 급여를 막대그래프로 표현
#전처리로 평균 급여 조회
dept_avg_sal=emp %>% group_by(DEPTNO) %>% summarize(AVG_SAL=mean(SAL))

#names.arg: x축 데이터 표시
barplot(dept_avg_sal$AVG_SAL, names.arg=dept_avg_sal$DEPTNO,
        main='부서별 평균 급여', ylab = '급여')

#히스토그램
#데이터를 일정한 구간으로 나누고, 각 구간에 속하는 데이터의 빈도를 
#막대의 높이로 표현

#문제: mutate 사용해서 직원 COMM이 NA인 직원만 100지급
emp =emp %>% mutate(COMM = ifelse(is.na(COMM),100,COMM)) #수정



#문제: mutate 사용해서 직원 급여와 직원 커미션을 더한 새로운 컬럼
#SUM_SAL_COMM 만들기
emp=emp %>% mutate(SUM_SAL_COMM=SAL+COMM) #추가
print(emp)




hist(emp$SUM_SAL_COMM, main = '직원급여+커미션 분포', xlab='급여+커미션',
     ylab='빈도')


pdf("salart_hist.pdf.pdf", family = "Korea1deb")

#평균선 추가 
#abline: add a line 직선을 추가하다
abline(v=mean(emp$SUM_SAL_COMM),col='red',lwd=2)

dev.off()














































