# 노지 채소 소득 추세 분석
summary(pa_vegi_wild)
library(ggplot2)

# 필요 컬럼 추출
pa_vegi_wild <- pa_vegi_wild[,c('년도', '품목', '총수입 (원)', '생산비 (원)', '중간재비', '농기계·시설 임차료', 
                              '토지임차료', '위탁영농비', '고용노동비', '소득 (원)', '소득률 (%)')]

# 컬럼 이름 변경
colnames(pa_vegi_wild) <- c('년도', '품목', '총수입', '생산비', '중간재비', '농기계_시설_임차료', '토지임차료',
                           '위탁영농비', '고용노동비', '소득', '소득률')
# View(pa_vegi_wild)

# 시각화 위해 소득률 제외 1,000으로 나누기
pa_vegi_wild[,3:10] <- pa_vegi_wild[,3:10]/1000

# 품목별 소득 추세
pa_vegi_wild %>%
  ggplot( aes(x=년도, y=소득, group=품목, fill=품목)) +
  geom_smooth(method = "lm") +
  theme(legend.position="none") +
  ggtitle("노지채소 소득 추세") +
  ylab('소득 (천원)')+
  theme(
    legend.position="none",
    panel.spacing = unit(0, "lines"),
    strip.text.x = element_text(size = 8),
    plot.title = element_text(size=13)
  ) +
  facet_wrap(~품목, scale="free_y")

# 품목별 평균 소득 추출
rank_vegi_wild <- pa_vegi_wild %>% 
  group_by(품목) %>% 
  summarise_at(vars(소득), list(평균_소득=mean))

# 평균 소득 히스토그램 비교
ggplot(data=rank_vegi_wild, aes_string(x='품목', y='평균_소득'))+
  geom_bar(stat="identity", position = position_dodge(width=0.5))+
  ylab('평균 소득 (천원)') +
  ggtitle('노지채소 평균 소득')



