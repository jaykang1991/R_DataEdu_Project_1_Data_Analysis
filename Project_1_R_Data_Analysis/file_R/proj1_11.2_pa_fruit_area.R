# 과수 소득 추세 분석
summary(pa_fruit)
library(ggplot2)

# 필요 컬럼 추출
pa_fruit <- pa_fruit[,c('년도', '품목', '총수입 (원)', '생산비 (원)', '중간재비', '농기계·시설 임차료', 
                        '토지임차료', '위탁영농비', '고용노동비', '소득 (원)', '소득률 (%)')]

# 컬럼 이름 변경
colnames(pa_fruit) <- c('년도', '품목', '총수입', '생산비', '중간재비', '농기계_시설_임차료', '토지임차료',
                        '위탁영농비', '고용노동비', '소득', '소득률')

# 품목별 평균 소득 추출
rank_fruit <- pa_fruit %>% 
  group_by(품목) %>% 
  summarise_at(vars(소득), list(평균_소득=mean))

# 10아르 = 0.1헥타르당 소득이기에 평균 소득을 구해 연평균 소득 구하기
# 평균재배면적 및 농가수 import
setwd("C:/Data Analyst Edu/mini-project-1")
area_fruit_1 <- read.csv("AREA_FRUIT_1.csv")
area_fruit_2 <- read.csv("AREA_FRUIT_2.csv")
area_fruit_3 <- read.csv("AREA_FRUIT_3.csv")
area_fruit_4 <- read.csv("AREA_FRUIT_4.csv")
area_fruit_5 <- read.csv("AREA_FRUIT_5.csv")

area_fruit_1 <- area_fruit_1[1,]
area_fruit_2 <- area_fruit_2[1,]
area_fruit_3 <- area_fruit_3[6,]
area_fruit_4 <- area_fruit_4[4,]
area_fruit_5 <- area_fruit_5[3,]

for (j in 2:(ncol(area_fruit_1))){area_fruit_1[,j] <- as.numeric(unlist(area_fruit_1[,j]))}
for (j in 2:(ncol(area_fruit_2))){area_fruit_2[,j] <- as.numeric(unlist(area_fruit_2[,j]))}
for (j in 2:(ncol(area_fruit_3))){area_fruit_3[,j] <- as.numeric(unlist(area_fruit_3[,j]))}
for (j in 3:(ncol(area_fruit_4))){area_fruit_4[,j] <- as.numeric(unlist(area_fruit_4[,j]))}
for (j in 3:(ncol(area_fruit_5))){area_fruit_5[,j] <- as.numeric(unlist(area_fruit_5[,j]))}

# 과수 구분
unique(pa_fruit$품목)
area_fruit <- data.frame(matrix(ncol=15, nrow=1))
colnames(area_fruit) <- unlist(unique(pa_fruit$품목))

area_fruit$사과 <- area_fruit_1$재배면적..ha./area_fruit_1$사과.농가..가구.
area_fruit$배 <- area_fruit_1$재배면적..ha..1/area_fruit_1$배.농가..가구.
area_fruit$복숭아 <- area_fruit_1$재배면적..ha..2/area_fruit_1$복숭아.농가..가구.
area_fruit$노지포도 <- area_fruit_1$재배면적..ha..4/area_fruit_1$노지포도.농가..가구.
area_fruit$노지감귤 <- area_fruit_1$재배면적..ha..6/area_fruit_1$시설감귤.농가..가구.
area_fruit$단감 <- area_fruit_1$재배면적..ha..3/area_fruit_1$단감.농가..가구.
area_fruit$유자 <- area_fruit_4$재배면적..ha./area_fruit_4$재배면적규모별농가.계..가구.
area_fruit$시설포도 <- area_fruit_1$재배면적..ha..5/area_fruit_1$시설포도.농가..가구.
area_fruit$시설감귤 <- area_fruit_1$재배면적..ha..7/area_fruit_1$시설감귤.농가..가구.
area_fruit$녹차 <- area_fruit_5$재배면적..ha./area_fruit_5$재배면적규모별농가.계..가구.
area_fruit$오미자 <- area_fruit_2$재배면적..ha./area_fruit_2$임가..가구.
area_fruit$블루베리 <- area_fruit_1$재배면적..ha..10/area_fruit_1$블루베리.농가..가구.
area_fruit$매실 <- area_fruit_1$재배면적..ha..9/area_fruit_1$매실.농가..가구.
area_fruit$자두 <- area_fruit_1$재배면적..ha..8/area_fruit_1$자두.농가..가구.
area_fruit$`참다래(키위)` <- area_fruit_3$재배면적..ha./area_fruit_3$재배면적규모별농가.계..가구.

# 소득 * 평균재배면적(ha) * 10 
for (j in 1:(ncol(area_fruit))){rank_fruit[rank_fruit$품목 == names(area_fruit[j]), '평균_소득'] <- 
  rank_fruit[rank_fruit$품목 == names(area_fruit[j]),'평균_소득']*
  10*
  area_fruit[,names(area_fruit[j])]}

# 시각화 위해 10,000으로 나누기
# rank_fruit$평균_소득 <- rank_fruit$평균_소득/10000
rank_fruit$평균_소득 <- rank_fruit$평균_소득/10

# 평균 소득 히스토그램 비교
ggplot(data=rank_fruit, aes_string(x='품목', y='평균_소득'))+
  geom_bar(stat="identity", position = position_dodge(width=0.5))+
  ylab('평균 소득 (만원)') +
  ggtitle('과수 평균 연소득')
