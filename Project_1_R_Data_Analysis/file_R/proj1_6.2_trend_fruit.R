# 키워드 트렌드 분석
summary(trend_fruit)
# View(trend_fruit)

# 분석을 위한 형태 변경
library(dplyr)
trend_fruit <- select(trend_fruit, -년도)
trend_fruit$날짜 <- as.Date(trend_fruit$날짜)
trend_fruit$사과 <- format(round(trend_fruit$보리, 1), nsmall = 2)
trend_fruit$배 <- format(round(trend_fruit$옥수수, 1), nsmall = 2)
trend_fruit$복숭아 <- format(round(trend_fruit$고구마, 1), nsmall = 2)
trend_fruit$포도 <- format(round(trend_fruit$감자, 1), nsmall = 2)
trend_fruit$귤 <- format(round(trend_fruit$참깨, 1), nsmall = 2)
trend_fruit$오미자 <- format(round(trend_fruit$오미자, 1), nsmall = 2)
trend_fruit$블루베리 <- format(round(trend_fruit$블루베리, 1), nsmall = 2)
trend_fruit$단감 <- format(round(trend_fruit$단감, 1), nsmall = 2)
trend_fruit$키위 <- format(round(trend_fruit$키위, 1), nsmall = 2)
trend_fruit$자두 <- format(round(trend_fruit$자두, 1), nsmall = 2)

# 보기 쉬운 비교 위해 2개 그룹으로 분할
trend_fruit_1 <- trend_fruit[,1:6]
trend_fruit_2 <- trend_fruit[,7:11]
trend_fruit_2$날짜 <- trend_fruit$날짜
# View(trend_fruit_2)

# 동시 비교 위해 melt
library(ggplot2)
library(reshape2)
trend_fruit_1 <- melt(trend_fruit_1, id.vars="날짜", variable.name='품목')
trend_fruit_2 <- melt(trend_fruit_2, id.vars="날짜", variable.name='품목')
names(trend_fruit_1)[names(trend_fruit_1) == '날짜'] <- '년도'
names(trend_fruit_2)[names(trend_fruit_2) == '날짜'] <- '년도'

trend_fruit_1$value <- as.numeric(as.character(trend_fruit_1$value))
trend_fruit_2$value <- as.numeric(as.character(trend_fruit_2$value))

max(trend_fruit_1$value)
trend_fruit_1 %>%
  ggplot(aes(x=년도, y=value, group=품목, fill=품목)) +
  geom_smooth(method = "lm") + scale_y_continuous(limits = c(0, 15)) +
  ggtitle("과수 키워드 트렌드 분석_1") +
  ylab("키워드 검색지표") +
  facet_wrap(~품목, scale="free_y")

trend_fruit_2 %>%
  ggplot(aes(x=년도, y=value, group=품목, fill=품목)) +
  geom_smooth(method = "lm") + scale_y_continuous(limits = c(0, 15)) +
  ggtitle("과수 키워드 트렌드 분석_2") +
  ylab("키워드 검색지표") +
  facet_wrap(~품목, scale="free_y")

# 평균 검색지표 비교
rank_trend_fruit_1 <- trend_fruit_1 %>% 
  group_by(품목) %>% 
  summarise_at(vars(value), list(평균_검색지표=mean))

rank_trend_fruit_2 <- trend_fruit_2 %>% 
  group_by(품목) %>% 
  summarise_at(vars(value), list(평균_검색지표=mean))

# 구분 된 두개 품목 결합
rank_trend_fruit <- rbind(rank_trend_fruit_1, rank_trend_fruit_2)

ggplot(data=rank_trend_fruit, aes_string(x='품목', y='평균_검색지표'))+
  geom_bar(stat="identity", position = position_dodge(width=0.5))+
  ylab('평균 검색지표 (복숭아 기준)') +
  ggtitle('과수 평균 검색지표')



