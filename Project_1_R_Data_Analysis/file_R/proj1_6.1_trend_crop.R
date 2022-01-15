# 키워드 트렌드 분석
summary(trend_crop)
# View(trend_crop)

# 분석을 위한 형태 변경
library(dplyr)
trend_crop <- select(trend_crop, -년도)
trend_crop$날짜 <- as.Date(trend_crop$날짜)
trend_crop$보리 <- format(round(trend_crop$보리, 1), nsmall = 2)
trend_crop$옥수수 <- format(round(trend_crop$옥수수, 1), nsmall = 2)
trend_crop$고구마 <- format(round(trend_crop$고구마, 1), nsmall = 2)
trend_crop$감자 <- format(round(trend_crop$감자, 1), nsmall = 2)
trend_crop$참깨 <- format(round(trend_crop$참깨, 1), nsmall = 2)
trend_crop$인삼 <- format(round(trend_crop$인삼, 1), nsmall = 2)

# 동시 비교 위해 melt
library(ggplot2)
library(reshape2)
trend_crop <- melt(trend_crop, id.vars="날짜", variable.name='품목')
names(trend_crop)[names(trend_crop) == '날짜'] <- '년도'

trend_crop$value <- as.numeric(as.character(trend_crop$value))
max(trend_crop$value)
trend_crop %>%
  ggplot(aes(x=년도, y=value, group=품목, fill=품목)) +
  geom_smooth(method = "lm") + scale_y_continuous(limits = c(0, 15)) +
  ggtitle("식량특용 키워드 트렌드 분석") +
  ylab("키워드 검색지표") +
  facet_wrap(~품목, scale="free_y")

# 평균 검색지표 비교
rank_trend_crop <- trend_crop %>% 
  group_by(품목) %>% 
  summarise_at(vars(value), list(평균_검색지표=mean))

ggplot(data=rank_trend_crop, aes_string(x='품목', y='평균_검색지표'))+
  geom_bar(stat="identity", position = position_dodge(width=0.5))+
  ylab('평균 검색지표 (감자 기준)') +
  ggtitle('식량특용 평균 검색지표')







