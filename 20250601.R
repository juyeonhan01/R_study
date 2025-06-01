#경로 확인
print(getwd())
print(list.files()) #해당 경로에 있는 파일 조회


#csv파일 불러오기
emp = read.csv('emp.csv')
#View(emp)#데이터확인

#1. 행과 열 개수파악
print(dim(emp)) #dimension: 차원

#2. 전체 컬럼만 조회
print(colnames(emp)) #col: 컬럼

#3.데이터 상위 1~2행 출력
print(head(emp,2))

#4. 데이터 마지막 3개행 출력
print(tail(emp,3))

#5. 데이터 타입 확인 **********암기하기
str(emp) #structure: 구조 '


#dplyr(디플리알): 데이터 가공 (전처리)
#install.packages("dplyr")
library(dplyr) #설치한 프로그램 가져오기(임포트 import 가져오다)

#급여(SAL)가 3000 이상인 직원들의 이름(ENAME)과 직업을 출력하세요
결과 = emp %>% filter(SAL >= 3000) %>% select(ENAME, JOB)
print(결과)

#직업별 평균 급여를 계산하고 출력하세요
결과 = emp %>% group_by(JOB) %>% 
  summarise(AVC_SAL = mean(SAL), EMP_COUNT=n(), SUM_SAL = sum(SAL))
print(결과)


#급여 2000이상인 직원만 필터링후 부서번호 별 직원수를 계산하세요
결과 = emp %>% filter(SAL>=2000) %>% group_by(DEPTNO) %>% summarize(COUNT = n())
print(결과)


dept = read.csv('dept.csv')
str(dept)
View(dept)

#디플리알 병합(JOIN)
#두 테이터 프레임을 특정 컬럼을 기준으로 병합합니다 
조인결과 = emp %>% inner_join(dept,by= "DEPTNO")
View(조인결과)

#근무지가 DALLAS인 직원들의 이름 출력하기
조인결과 = emp %>% inner_join(dept,by= "DEPTNO") %>% filter(LOC=='DALLAS')%>%
  select(ENAME,JOB)
print(조인결과)

#slice 자르다(DBMS = limit)
결과 = emp %>% slice(2,4)
print(결과) #2,4번째 행 추출

#1~3번째 직원 행 추출
결과 = emp %>% slice(1:3)
print(결과)
#slice 맨 마지막에 작성(대부분)


#문제 2: "RESEARCH" 부서에 근무하는 직원들의 이름(ENAME)과 급여(SAL)를 출력하세요.

조인결과 = emp %>% inner_join(dept,by= "DEPTNO") %>% filter(LOC=='RESEARCH')%>%
  select(ENAME,JOB)
print(조인결과)


#문제 4: 각 부서(DNAME)별 직원 수를 계산하고 출력하세요.
# 힌트 : group_by(), summarize()

결과 =  emp %>% inner_join(dept,by= "DEPTNO") %>% group_by(DNAME) %>%
  summarise(EMP_COUNT=n()) %>% slice(1)
print(결과)

#문제 10: 직업(JOB)이 "MANAGER"인 직원들의 이름(ENAME), 부서명(DNAME), 급여(SAL)을 출력하세요.
# 힌트 :  inner_join(), filter(), select()

결과= emp%>% inner_join(dept,by= "DEPTNO") %>% filter(JOB=='MANAGER') %>% select(ENAME,DNAME,SAL)


#문제 4: 각 부서(DNAME)별 직원 수를 계산하고 출력하세요. 
#arrange 정렬 desc 내림차순
결과 =  emp %>% inner_join(dept,by= "DEPTNO") %>% group_by(DNAME) %>%
  summarise(EMP_COUNT=n()) %>% arrange(desc(EMP_COUNT)) %>% slice(1:2)
print(결과)































