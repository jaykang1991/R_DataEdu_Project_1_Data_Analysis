# 키워드 트렌드 분석
summary(trend_vegi_wild)
# View(trend_vegi_wild)

# 분석을 위한 형태 변경
library(dplyr)
trend_vegi_wild <- select(trend_vegi_wild, -년도)
trend_vegi_wild$날짜 <- as.Date(trend_vegi_wild$날짜)
trend_vegi_wild$수박 <- format(round(trend_vegi_wild$수박, 1), nsmall = 2)
trend_vegi_wild$무 <- format(round(trend_vegi_wild$무, 1), nsmall = 2)
trend_vegi_wild$당근 <- format(round(trend_vegi_wild$당근, 1), nsmall = 2)
trend_vegi_wild$배추 <- format(round(trend_vegi_wild$배추, 1), nsmall = 2)
trend_vegi_wild$시금치 <- format(round(trend_vegi_wild$시금치, 1), nsmall = 2)
trend_vegi_wild$양배추 <- format(round(trend_vegi_wild$양배추, 1), nsmall = 2)
trend_vegi_wild$부추 <- format(round(trend_vegi_wild$부추, 1), nsmall = 2)
trend_vegi_wild$대파 <- format(round(trend_vegi_wild$대파, 1), nsmall = 2)
trend_vegi_wild$쪽파 <- format(round(trend_vegi_wild$쪽파, 1), nsmall = 2)
trend_vegi_wild$생강 <- format(round(trend_vegi_wild$생강, 1), nsmall = 2)


# 보기 쉬운 비교 위해 2개 그룹으로 분할
trend_vegi_wild_1 <- trend_vegi_wild[,1:6]
trend_vegi_wild_2 <- trend_vegi_wild[,7:11]
trend_vegi_wild_2$날짜 <- trend_vegi_wild$날짜
# View(trend_vegi_fac_2)

# 동시 비교 위해 melt
library(ggplot2)
library(reshape2)
trend_vegi_wild_1 <- melt(trend_vegi_wild_1, id.vars="날짜", variable.name='품목')
trend_vegi_wild_2 <- melt(trend_vegi_wild_2, id.vars="날짜", variable.name='품목')
names(trend_vegi_wild_1)[names(trend_vegi_wild_1) == '날짜'] <- '년도'
names(trend_vegi_wild_2)[names(trend_vegi_wild_2) == '날짜'] <- '년도'

trend_vegi_wild_1$value <- as.numeric(as.character(trend_vegi_wild_1$value))
trend_vegi_wild_2$value <- as.numeric(as.character(trend_vegi_wild_2$value))

max(trend_vegi_wild_1$value)
trend_vegi_wild_1 %>%
  ggplot(aes(x=년도, y=value, group=품목, fill=품목)) +
  geom_smooth(method = "lm") + scale_y_continuous(limits = c(0, 15)) +
  ggtitle("노지채소 키워드 트렌드 분석_1") +
  ylab("키워드 검색지표") +
  facet_wrap(~품목, scale="free_y")

trend_vegi_wild_2 %>%
  ggplot(aes(x=년도, y=value, group=품목, fill=품목)) +
  geom_smooth(method = "lm") + scale_y_continuous(limits = c(0, 10)) +
  ggtitle("노지채소 키워드 트렌드 분석_2") +
  ylab("키워드 검색지표") +
  facet_wrap(~품목, scale="free_y")

# 평균 검색지표 비교
rank_trend_vegi_wild_1 <- trend_vegi_wild_1 %>% 
  group_by(품목) %>% 
  summarise_at(vars(value), list(평균_검색지표=mean))

rank_trend_vegi_wild_2 <- trend_vegi_wild_2 %>% 
  group_by(품목) %>% 
  summarise_at(vars(value), list(평균_검색지표=mean))

# 구분 된 두개 품목 결합
rank_trend_vegi_wild <- rbind(rank_trend_vegi_wild_1, rank_trend_vegi_wild_2)

ggplot(data=rank_trend_vegi_wild, aes_string(x='품목', y='평균_검색지표'))+
  geom_bar(stat="identity", position = position_dodge(width=0.5))+
  ylab('평균 검색지표 (시금치 기준)') +
  ggtitle('노지채소 평균 검색지표')




