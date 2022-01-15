# 식량특용 소득 추세 분석
summary(pa_crop)
library(ggplot2)

# 필요 컬럼 추출
pa_crop <- pa_crop[,c('년도', '품목', '총수입 (원)', '생산비 (원)', '중간재비', '농기계·시설 임차료', 
                      '토지임차료', '위탁영농비', '고용노동비', '소득 (원)', '소득률 (%)')]

# 컬럼 이름 변경
colnames(pa_crop) <- c('년도', '품목', '총수입', '생산비', '중간재비', '농기계_시설_임차료', '토지임차료',
                       '위탁영농비', '고용노동비', '소득', '소득률')
# View(pa_crop)

# 시각화 위해 소득률 제외 1,000으로 나누기
pa_crop[,3:10] <- pa_crop[,3:10]/1000

# 인삼 4년이기에 4로 나누기
pa_crop[pa_crop$품목 == '인삼(4년근)','소득'] <- pa_crop[pa_crop$품목 == '인삼(4년근)','소득']/4

# 품목별 추세 확인
pa_crop %>%
  ggplot( aes(x=년도, y=소득, group=품목, fill=품목)) +
  geom_smooth(method = "lm") +
  theme(legend.position="none") +
  ggtitle("식량특용 소득 추세") +
  ylab('소득 (천원)')+
  theme(
    legend.position="none",
    panel.spacing = unit(0, "lines"),
    strip.text.x = element_text(size = 8),
    plot.title = element_text(size=13)
  ) +
  facet_wrap(~품목, scale="free_y")

# 평균 소득 히스토그램 비교
# 품목별 평균 소득 추출
rank_crop <- pa_crop %>% 
  group_by(품목) %>% 
  summarise_at(vars(소득), list(평균_소득=mean))

ggplot(data=rank_crop, aes_string(x='품목', y='평균_소득'))+
  geom_bar(stat="identity", position = position_dodge(width=0.5))+
  ylab('평균 소득(천원)') +
  ggtitle('식량특용 평균 소득')

# 인삼만 너무 많기에 제거 후 비교
new_rank_crop <- rank_crop[-11,]
ggplot(data=new_rank_crop, aes_string(x='품목', y='평균_소득'))+
  geom_bar(stat="identity", position = position_dodge(width=0.5))+
  ylab('평균 소득(천원)') +
  ggtitle('식량특용 평균 소득 (인삼 제외)')

# 소득분석표 기준 확인하기
# 당해 성과? 일년 소득/총수입이라고 하기엔 말이 안되는 숫자 (063-238-1201 전화해보기)
# https://meta.narastat.kr/metasvc/index.do?orgId=143&confmNo=143002&kosisYn=Y
