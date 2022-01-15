# 시설 채소 소득 추세 분석
summary(pa_vegi_fac)
library(ggplot2)

# 필요 컬럼 추출
pa_vegi_fac <- pa_vegi_fac[,c('년도', '품목', '총수입 (원)', '생산비 (원)', '중간재비', '농기계·시설 임차료', 
                        '토지임차료', '위탁영농비', '고용노동비', '소득 (원)', '소득률 (%)')]

# 컬럼 이름 변경
colnames(pa_vegi_fac) <- c('년도', '품목', '총수입', '생산비', '중간재비', '농기계_시설_임차료', '토지임차료',
                        '위탁영농비', '고용노동비', '소득', '소득률')
# View(pa_vegi_fac)

# 시각화 위해 소득률 제외 1,000으로 나누기
pa_vegi_fac[,3:10] <- pa_vegi_fac[,3:10]/1000

# 소득 추세 확인
pa_vegi_fac %>%
  ggplot( aes(x=년도, y=소득, group=품목, fill=품목)) +
  geom_smooth(method = "lm") +
  theme(legend.position="none") +
  ggtitle("시설채소 소득 추세") +
  ylab('소득 (천원)')+
  theme(
    legend.position="none",
    panel.spacing = unit(0, "lines"),
    strip.text.x = element_text(size = 8),
    plot.title = element_text(size=13)
  ) +
  facet_wrap(~품목, scale="free_y")

# 품목별 평균 소득 추출
rank_vegi_fac <- pa_vegi_fac %>% 
  group_by(품목) %>% 
  summarise_at(vars(소득), list(평균_소득=mean))

# 평균 소득 히스토그램 비교
ggplot(data=rank_vegi_fac, aes_string(x='품목', y='평균_소득'))+
  geom_bar(stat="identity", position = position_dodge(width=0.5))

# 품목 명 복잡해 두 그룹으로 분류
nrow(rank_vegi_fac)
rank_vegi_fac_1<- rank_vegi_fac[1:10,]
rank_vegi_fac_2<- rank_vegi_fac[11:19,]

ggplot(data=rank_vegi_fac_1, aes_string(x='품목', y='평균_소득'))+
  geom_bar(stat="identity", position = position_dodge(width=0.5))+
  ylab('평균 소득 (천원)') +
  ggtitle('시설채소 평균 소득_1')

ggplot(data=rank_vegi_fac_2, aes_string(x='품목', y='평균_소득'))+
  geom_bar(stat="identity", position = position_dodge(width=0.5))+
  ylab('평균 소득 (천원)') +
  ggtitle('시설채소 평균 소득_2')

# 그룹 나눠서 추세 확인

pa_vegi_fac_1 <- subset(pa_vegi_fac, pa_vegi_fac$품목 %in% rank_vegi_fac_1$품목)
pa_vegi_fac_2 <- subset(pa_vegi_fac, pa_vegi_fac$품목 %in% rank_vegi_fac_2$품목)

pa_vegi_fac_1 %>%
  ggplot( aes(x=년도, y=소득, group=품목, fill=품목)) +
  geom_smooth(method = "lm") +
  theme(legend.position="none") +
  ggtitle("시설채소 소득 추세_1") +
  ylab('소득 (천원)')+
  theme(
    legend.position="none",
    panel.spacing = unit(0, "lines"),
    strip.text.x = element_text(size = 8),
    plot.title = element_text(size=13)
  ) +
  facet_wrap(~품목, scale="free_y")

pa_vegi_fac_2 %>%
  ggplot( aes(x=년도, y=소득, group=품목, fill=품목)) +
  geom_smooth(method = "lm") +
  theme(legend.position="none") +
  ggtitle("시설채소 소득 추세_2") +
  ylab('소득 (천원)')+
  theme(
    legend.position="none",
    panel.spacing = unit(0, "lines"),
    strip.text.x = element_text(size = 8),
    plot.title = element_text(size=13)
  ) +
  facet_wrap(~품목, scale="free_y")
