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

# 품목별 평균 소득 추출
rank_vegi_wild <- pa_vegi_wild %>% 
  group_by(품목) %>% 
  summarise_at(vars(소득), list(평균_소득=mean))

# View(area_vegi_wild)
# 소득 * 평균재배면적(ha) * 10 
for (j in 1:(ncol(area_vegi_wild))){rank_vegi_wild[rank_vegi_wild$품목 == names(area_vegi_wild[j]), '평균_소득'] <- 
  rank_vegi_wild[rank_vegi_wild$품목 == names(area_vegi_wild[j]),'평균_소득']*
  10*
  area_vegi_wild[,names(area_vegi_wild[j])]}

# 시각화 위해 10,000으로 나누기 *** 재확인
# rank_vegi_wild$평균_소득 <- rank_vegi_wild$평균_소득/10000
rank_vegi_wild$평균_소득 <- rank_vegi_wild$평균_소득/10

# 평균 소득 히스토그램 비교
ggplot(data=rank_vegi_wild, aes_string(x='품목', y='평균_소득'))+
  geom_bar(stat="identity", position = position_dodge(width=0.5))+
  ylab('평균 소득 (만원)') +
  ggtitle('노지채소 평균 연소득')



