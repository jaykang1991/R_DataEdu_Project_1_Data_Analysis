# 키워드 트렌드 분석
summary(trend_vegi_fac)
# View(trend_vegi_fac)

# 분석을 위한 형태 변경
library(dplyr)
trend_vegi_fac <- select(trend_vegi_fac, -년도)
trend_vegi_fac$날짜 <- as.Date(trend_vegi_fac$날짜)
trend_vegi_fac$토마토 <- format(round(trend_vegi_fac$토마토, 1), nsmall = 2)
trend_vegi_fac$방울토마토 <- format(round(trend_vegi_fac$방울토마토, 1), nsmall = 2)
trend_vegi_fac$가지 <- format(round(trend_vegi_fac$가지, 1), nsmall = 2)
trend_vegi_fac$파프리카 <- format(round(trend_vegi_fac$파프리카, 1), nsmall = 2)
trend_vegi_fac$상추 <- format(round(trend_vegi_fac$상추, 1), nsmall = 2)
trend_vegi_fac$수박 <- format(round(trend_vegi_fac$수박, 1), nsmall = 2)
trend_vegi_fac$참외 <- format(round(trend_vegi_fac$참외, 1), nsmall = 2)
trend_vegi_fac$딸기 <- format(round(trend_vegi_fac$딸기, 1), nsmall = 2)
trend_vegi_fac$오이 <- format(round(trend_vegi_fac$오이, 1), nsmall = 2)
trend_vegi_fac$호박 <- format(round(trend_vegi_fac$호박, 1), nsmall = 2)
trend_vegi_fac$고추 <- format(round(trend_vegi_fac$고추, 1), nsmall = 2)

# 보기 쉬운 비교 위해 2개 그룹으로 분할
trend_vegi_fac_1 <- trend_vegi_fac[,1:6]
trend_vegi_fac_2 <- trend_vegi_fac[,7:12]
trend_vegi_fac_2$날짜 <- trend_vegi_fac$날짜
# View(trend_vegi_fac_2)

# 동시 비교 위해 melt
library(ggplot2)
library(reshape2)
trend_vegi_fac_1 <- melt(trend_vegi_fac_1, id.vars="날짜", variable.name='품목')
trend_vegi_fac_2 <- melt(trend_vegi_fac_2, id.vars="날짜", variable.name='품목')
names(trend_vegi_fac_1)[names(trend_vegi_fac_1) == '날짜'] <- '년도'
names(trend_vegi_fac_2)[names(trend_vegi_fac_2) == '날짜'] <- '년도'

trend_vegi_fac_1$value <- as.numeric(as.character(trend_vegi_fac_1$value))
trend_vegi_fac_2$value <- as.numeric(as.character(trend_vegi_fac_2$value))

max(trend_vegi_fac_1$value)
trend_vegi_fac_1 %>%
  ggplot(aes(x=년도, y=value, group=품목, fill=품목)) +
  geom_smooth(method = "lm") + scale_y_continuous(limits = c(0, 15)) +
  ggtitle("시설 채소 키워드 트렌드 분석_1") +
  ylab("키워드 검색지표") +
  facet_wrap(~품목, scale="free_y")

trend_vegi_fac_2 %>%
  ggplot(aes(x=년도, y=value, group=품목, fill=품목)) +
  geom_smooth(method = "lm") + scale_y_continuous(limits = c(0, 15)) +
  ggtitle("시설 채소 키워드 트렌드 분석_2") +
  ylab("키워드 검색지표") +
  facet_wrap(~품목, scale="free_y")

# 평균 검색지표 비교
rank_trend_vegi_fac_1 <- trend_vegi_fac_1 %>% 
  group_by(품목) %>% 
  summarise_at(vars(value), list(평균_검색지표=mean))

rank_trend_vegi_fac_2 <- trend_vegi_fac_2 %>% 
  group_by(품목) %>% 
  summarise_at(vars(value), list(평균_검색지표=mean))

# 구분 된 두개 품목 결합
rank_trend_vegi_fac <- rbind(rank_trend_vegi_fac_1, rank_trend_vegi_fac_2)

ggplot(data=rank_trend_vegi_fac, aes_string(x='품목', y='평균_검색지표'))+
  geom_bar(stat="identity", position = position_dodge(width=0.5))+
  ylab('평균 검색지표 (토마토 기준)') +
  ggtitle('시설채소 평균 검색지표')

