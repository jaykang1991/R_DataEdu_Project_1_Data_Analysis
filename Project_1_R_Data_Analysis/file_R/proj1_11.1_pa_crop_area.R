# 과수 소득 추세 분석
summary(pa_crop)
library(ggplot2)
library(dplyr)

# 필요 컬럼 추출
pa_crop <- pa_crop[,c('년도', '품목', '총수입 (원)', '생산비 (원)', '중간재비', '농기계·시설 임차료', 
                        '토지임차료', '위탁영농비', '고용노동비', '소득 (원)', '소득률 (%)')]

# 컬럼 이름 변경
colnames(pa_crop) <- c('년도', '품목', '총수입', '생산비', '중간재비', '농기계_시설_임차료', '토지임차료',
                        '위탁영농비', '고용노동비', '소득', '소득률')

# 품목별 평균 소득 추출
rank_crop <- pa_crop %>% 
  group_by(품목) %>% 
  summarise_at(vars(소득), list(평균_소득=mean))

# 10아르 = 0.1헥타르당 소득이기에 평균 소득을 구해 연평균 소득 구하기
# 평균재배면적 및 농가수 import
setwd("C:/Data Analyst Edu/mini-project-1")
area_crop_1 <- read.csv("AREA_CROP_1.csv")
area_crop_2 <- read.csv("AREA_CROP_2.csv")
area_crop_3 <- read.csv("AREA_CROP_3.csv")
area_crop_4 <- read.csv("AREA_CROP_4.csv")
area_crop_5 <- read.csv("AREA_CROP_5.csv")

for (j in 2:(ncol(area_crop_1))){area_crop_1[,j] <- as.numeric(unlist(area_crop_1[,j]))}
for (j in 2:(ncol(area_crop_2))){area_crop_2[,j] <- as.numeric(unlist(area_crop_2[,j]))}
for (j in 2:(ncol(area_crop_3))){area_crop_3[,j] <- as.numeric(unlist(area_crop_3[,j]))}
for (j in 2:(ncol(area_crop_4))){area_crop_4[,j] <- as.numeric(unlist(area_crop_4[,j]))}
for (j in 2:(ncol(area_crop_5))){area_crop_5[,j] <- as.numeric(unlist(area_crop_5[,j]))}

# 구분
unique(pa_crop$품목)
area_crop <- data.frame(matrix(ncol=12, nrow=1))
colnames(area_crop) <- unlist(unique(pa_crop$품목))

area_crop$'겉보리' <- area_crop_4$면적..ha./area_crop_4$겉보리.쌀보리.농가..가구.
area_crop$'쌀보리' <- area_crop_4$면적..ha./area_crop_4$겉보리.쌀보리.농가..가구.
area_crop$'맥주보리' <- area_crop_4$면적..ha./area_crop_4$겉보리.쌀보리.농가..가구.
area_crop$'노지풋옥수수' <- area_crop_4$면적..ha..1/area_crop_4$옥수수.농가..가구.
area_crop$'고구마' <- area_crop_4$면적..ha..3/area_crop_4$고구마.농가..가구.
area_crop$'봄감자' <- area_crop_4$면적..ha..2/area_crop_4$감자.농가..가구.
area_crop$'가을감자' <- area_crop_4$면적..ha..2/area_crop_4$감자.농가..가구.
area_crop$'참깨' <- area_crop_1$재배면적..ha./area_crop_1$참깨.재배면적.규모별농가.계..가구.
area_crop$'엽연초' <- area_crop_3$엽연초.최소.재배면적.ha.
area_crop$'인삼(4년근)' <- area_crop_2$재배면적..ha./area_crop_2$인삼재배면적규모별농가.계..가구.
area_crop$'밀' <- area_crop_4$면적..ha./area_crop_4$겉보리.쌀보리.농가..가구.
area_crop$'들깨' <- area_crop_5$재배면적..ha./area_crop_5$들깨.재배면적.규모별농가.계..가구.

# 소득 * 평균재배면적(ha) * 10 
for (j in 1:(ncol(area_crop))){rank_crop[rank_crop$품목 == names(area_crop[j]), '평균_소득'] <- 
  rank_crop[rank_crop$품목 == names(area_crop[j]),'평균_소득']*
  10*
  area_crop[,names(area_crop[j])]}

# 시각화 위해 10,000으로 나누기
# rank_crop$평균_소득 <- rank_crop$평균_소득/10000
rank_crop$평균_소득 <- rank_crop$평균_소득/10

# 평균 소득 히스토그램 비교
ggplot(data=rank_crop, aes_string(x='품목', y='평균_소득'))+
  geom_bar(stat="identity", position = position_dodge(width=0.5))+
  ylab('평균 소득 (만원)') +
  ggtitle('농작물 평균 연소득 (인삼 포함)')

# nrow(rank_crop)
rank_crop_1 <- rbind(rank_crop[1:10,],rank_crop[12,])

# 평균 소득 히스토그램 비교
ggplot(data=rank_crop_1, aes_string(x='품목', y='평균_소득'))+
  geom_bar(stat="identity", position = position_dodge(width=0.5))+
  ylab('평균 소득 (만원)') +
  ggtitle('농작물 평균 연소득 (인삼 제외)')




