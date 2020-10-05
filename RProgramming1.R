#install.packages("data.table")
library(data.table)

ks_project<-fread("ks_project_catch.csv")

#install.packages("DataExplorer")
library(DataExplorer)

# 데이터의 정보
plot_intro(ks_project)

# Missing Value Check
plot_missing(ks_project)

# Continuous Distribution
options(scipen=999)
plot_histogram(ks_project)

# Category Variables
plot_bar(ks_project, maxcat=1000)

#install.packages("dplyr")
library(dplyr)

# Category, Goal와 관련된 변수만 뽑기
colnames(ks_project)
select(ks_project,category, main_category, goal )
select(ks_project, contains("category")) 
select(ks_project, -category, -main_category, -goal)
?select

# main_category가 10명 이상이면서
# Backer가 10명 이상인 브랜드만 보기
# 논리연산자 : 속해있다 A %in% B
# 결측치이다 in.na(A)
filter(ks_project, main_category=="Fashion"&backers>=10)
filter(ks_project, main_category %in% c("Fashion", "Dance"))

ks_filter<-filter(ks_project, main_category=="Fashion"&backers>=10)
select(ks_filter, contains("category"))

# Chain operator
ks_project %>%
  filter(main_category=="Fashion"&backers>=10) %>%
  select(category, main_category, goal)

# Group_by, Summarize 그룹별 요약 통계량 산출
# 평균적인 목표금액과 목표금액의 표준편차 살펴보기
# na.rm : NA를 제거하고 평균내기
ks_project %>% 
  summarise(Mean_Goal=mean(goal, na.rm = T), 
            SD_Goal=sd(goal, na.rm=T))

ks_project %>% 
  group_by(main_category) %>%
  summarise(Mean_Goal=mean(goal, na.rm = T), 
            SD_Goal=sd(goal, na.rm=T))
  