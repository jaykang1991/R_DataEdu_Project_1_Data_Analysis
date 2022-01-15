# 시설 채소 소득 추세 분석
summary(pa_vegi_fac)
library(ggplot2)

# 필요 컬럼 추출
pa_vegi_fac <- pa_vegi_fac[,c('년도', '품목', '총수입 (원)', '생산비 (원)', '중간재비', '농기계·시설 임차료', 
                              '토지임차료', '위탁영농비', '고용노동비', '소득 (원)', '소득률 (%)')]

# 컬럼 이름 변경
colnames(pa_vegi_fac) <- c('년도', '품목', '총수입', '생산비', '중간재비', '농기계_시설_임차료', '토지임차료',
                           '위탁영농비', '고용노동비', '소득', '소득률')

# 품목별 평균 소득 추출
rank_vegi_fac <- pa_vegi_fac %>% 
  group_by(품목) %>% 
  summarise_at(vars(소득), list(평균_소득=mean))

# 10아르 = 0.1헥타르당 소득이기에 평균 소득을 구해 연평균 소득 구하기

# 평균재배면적 및 농가수 import
setwd("C:/Data Analyst Edu/mini-project-1")
area_vegi_1 <- read.csv("AREA_VEGI_1.csv")
area_vegi_2 <- read.csv("AREA_VEGI_2.csv")
area_vegi_3 <- read.csv("AREA_VEGI_3.csv")
area_vegi_4 <- read.csv("AREA_VEGI_4.csv")

# 필요 항목 및 컬럼 추출
# View(area_vegi_1)
area_vegi_1 <- area_vegi_1[1,]
area_vegi_2 <- area_vegi_2[1,]
area_vegi_3 <- area_vegi_3[1,]
area_vegi_4 <- area_vegi_4[1,]
summary(area_vegi_5)
for (j in 2:(ncol(area_vegi_1))){area_vegi_1[,j] <- as.numeric(unlist(area_vegi_1[,j]))}
for (j in 2:(ncol(area_vegi_2))){area_vegi_2[,j] <- as.numeric(unlist(area_vegi_2[,j]))}
for (j in 2:(ncol(area_vegi_3))){area_vegi_3[,j] <- as.numeric(unlist(area_vegi_3[,j]))}
for (j in 2:(ncol(area_vegi_4))){area_vegi_4[,j] <- as.numeric(unlist(area_vegi_4[,j]))}

# 시설채소 구분
unique(pa_vegi_fac$품목)
area_vegi_fac <- data.frame(matrix(ncol=19, nrow=1))
colnames(area_vegi_fac) <- unlist(unique(pa_vegi_fac$품목))

area_vegi_fac$`수박(반촉성)` <- area_vegi_1$시설..ha..7/area_vegi_1$시설..가구..7
area_vegi_fac$시설참외 <- area_vegi_2$면적..ha..3/area_vegi_2$참외.농가..가구.
area_vegi_fac$`딸기(촉성)` <- area_vegi_2$면적..ha..2/area_vegi_2$딸기.농가..가구.
area_vegi_fac$`딸기(반촉성)` <- area_vegi_2$면적..ha..2/area_vegi_2$딸기.농가..가구.
area_vegi_fac$`오이(촉성)` <- area_vegi_1$시설..ha..6/area_vegi_1$시설..가구..6
area_vegi_fac$`오이(반촉성)` <- area_vegi_1$시설..ha..6/area_vegi_1$시설..가구..6
area_vegi_fac$`오이(억제)` <- area_vegi_1$시설..ha..6/area_vegi_1$시설..가구..6
area_vegi_fac$시설호박 <- area_vegi_1$시설..ha..5/area_vegi_1$시설..가구..5
area_vegi_fac$`토마토(촉성)` <- area_vegi_2$면적..ha./area_vegi_2$토마토.일반..농가..가구.
area_vegi_fac$`토마토(반촉성)` <- area_vegi_2$면적..ha./area_vegi_2$토마토.일반..농가..가구.
area_vegi_fac$방울토마토 <- area_vegi_2$면적..ha..1/area_vegi_2$토마토.방울..농가..가구.
area_vegi_fac$시설가지 <- area_vegi_4$면적..ha..1/area_vegi_4$가지.농가..가구.
area_vegi_fac$`착색단고추(파프리카)` <- area_vegi_2$면적..ha..4/area_vegi_2$파프리카.농가..가구.
area_vegi_fac$시설무 <- area_vegi_1$시설..ha..1/area_vegi_1$시설..가구..1
area_vegi_fac$시설배추 <- area_vegi_1$시설..ha./area_vegi_1$시설..가구.
area_vegi_fac$시설시금치 <- area_vegi_1$시설..ha..3/area_vegi_1$시설..가구..3
area_vegi_fac$시설상추 <- area_vegi_1$시설..ha..4/area_vegi_1$시설..가구..4
area_vegi_fac$시설고추 <- area_vegi_1$시설..ha..2/area_vegi_1$시설..가구..2

area_vegi_fac$시설부추 <- apply(area_vegi_fac[,1:16], 1, mean)

# View(area_vegi_fac)

# 노지채소 구분
unique(pa_vegi_wild$품목)
area_vegi_wild <- data.frame(matrix(ncol=14, nrow=1))
colnames(area_vegi_wild) <- unlist(unique(pa_vegi_wild$품목))
area_vegi_wild$노지수박 <- area_vegi_1$노지..ha..7/area_vegi_1$노지..가구..7
area_vegi_wild$봄무 <- area_vegi_1$노지..ha..1/area_vegi_1$노지..가구..1 
area_vegi_wild$가을무 <- area_vegi_1$노지..ha..1/area_vegi_1$노지..가구..1
area_vegi_wild$고랭지무 <- area_vegi_1$노지..ha..1/area_vegi_1$노지..가구..1
area_vegi_wild$당근 <- area_vegi_4$면적..ha..2/area_vegi_4$당근.농가..가구.
area_vegi_wild$봄배추 <- area_vegi_1$노지..ha./area_vegi_1$노지..가구.
area_vegi_wild$가을배추 <- area_vegi_1$노지..ha./area_vegi_1$노지..가구.
area_vegi_wild$고랭지배추 <- area_vegi_1$노지..ha./area_vegi_1$노지..가구.
area_vegi_wild$노지시금치 <- area_vegi_1$노지..ha..3/area_vegi_1$노지..가구..3
area_vegi_wild$양배추 <- area_vegi_4$면적..ha./area_vegi_4$양배추.농가..가구.
area_vegi_wild$대파 <- area_vegi_3$재배면적..ha./area_vegi_3$대파.재배면적.규모별농가.계..가구.

area_vegi_wild$쪽파 <- apply(area_vegi_wild[,1:9], 1, mean)
area_vegi_wild$생강 <- apply(area_vegi_wild[,1:9], 1, mean)
area_vegi_wild$노지부추 <- apply(area_vegi_wild[,1:9], 1, mean)

# View(area_vegi_wild)
# names(area_vegi_fac[2])
# 소득 * 평균재배면적(ha) * 10 
for (j in 1:(ncol(area_vegi_fac))){rank_vegi_fac[rank_vegi_fac$품목 == names(area_vegi_fac[j]), '평균_소득'] <- 
  rank_vegi_fac[rank_vegi_fac$품목 == names(area_vegi_fac[j]),'평균_소득']*
  10*
  area_vegi_fac[,names(area_vegi_fac[j])]}

#rank_vegi_fac[rank_vegi_fac$품목 == '수박(반촉성)','평균_소득'] <- 
#  rank_vegi_fac[rank_vegi_fac$품목 == '수박(반촉성)','평균_소득']*10*area_vegi_fac[,'수박(반촉성)']

# 시각화 위해 10,000으로 나누기
# rank_vegi_fac$평균_소득 <- rank_vegi_fac$평균_소득/10000
rank_vegi_fac$평균_소득 <- rank_vegi_fac$평균_소득/10

# 평균 소득 히스토그램 비교
ggplot(data=rank_vegi_fac, aes_string(x='품목', y='평균_소득'))+
  geom_bar(stat="identity", position = position_dodge(width=0.5))

# 품목 명 복잡해 두 그룹으로 분류
nrow(rank_vegi_fac)
rank_vegi_fac_1<- rank_vegi_fac[1:10,]
rank_vegi_fac_2<- rank_vegi_fac[11:19,]

ggplot(data=rank_vegi_fac_1, aes_string(x='품목', y='평균_소득'))+
  geom_bar(stat="identity", position = position_dodge(width=0.5))+
  ylab('평균 소득 (만원)') +
  ggtitle('시설채소 평균 연소득_1')

ggplot(data=rank_vegi_fac_2, aes_string(x='품목', y='평균_소득'))+
  geom_bar(stat="identity", position = position_dodge(width=0.5))+
  ylab('평균 소득 (만원)') +
  ggtitle('시설채소 평균 연소득_2')
