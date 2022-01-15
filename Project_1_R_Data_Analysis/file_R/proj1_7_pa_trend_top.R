# 소득 및 검색지표 활용한 품목 추천

# 작물 소득 상위권 (1-4위)의 소득, 소득추세, 평균 검색 트렌드 추세 제공
library(dplyr)

rank_crop_1 <- rank_crop %>% 
  arrange(desc(평균_소득)) %>% 
  slice(1:4)
pa_crop_1 <-subset(pa_crop, pa_crop$품목 %in% rank_crop_1$품목)

# 평균 소득
ggplot(data=rank_crop_1, aes_string(x='품목', y='평균_소득', fill='품목'))+
  geom_bar(stat="identity", position = position_dodge(width=0.5))+
  ylab('평균 소득 (천원)') +
  ggtitle('식량특용 상위권 평균 소득')

# 소득 추세
pa_crop_1 %>%
  ggplot( aes(x=년도, y=소득, group=품목, fill=품목)) +
  geom_smooth(method = "lm") +
  theme(legend.position="none") +
  ggtitle("식량특용 상위권 소득 추세") +
  ylab('소득 (천원)')+
  theme(
    legend.position="none",
    panel.spacing = unit(0, "lines"),
    strip.text.x = element_text(size = 8),
    plot.title = element_text(size=13)
  ) +
  facet_wrap(~품목, scale="free_y")

# 트렌드 데이터에서 소득 상위권 품목 추출
rank_crop_1$품목
unique(trend_crop$품목)
trend_crop_1 <- subset(trend_crop, trend_crop$품목 %in% c('인삼', '고구마', '옥수수'))

# 작물 소득 상위권 트렌드
trend_crop_1 %>%
  ggplot(aes(x=년도, y=value, group=품목, fill=품목)) +
  geom_smooth(method = "lm") + scale_y_continuous(limits = c(0, 15)) +
  ggtitle("식량특용 상위권 키워드 트렌드 분석") +
  ylab("키워드 검색지표") +
  facet_wrap(~품목, scale="free_y")

# 작물 소득 추세 및 트렌드 추세 상관분석
# 품목별 평균 트렌드 추출
trend_crop$년도_ <- as.character(format(as.Date(trend_crop$년도), format = "%Y"))

rank_crop_trend <- aggregate(x=trend_crop$value,
          by=list(trend_crop$품목,trend_crop$년도_),
          FUN=mean)
colnames(rank_crop_trend) <- c('품목', '년도', '검색지표')

# 트렌드와 동일한 이름으로 변경 및 추출
pa_crop_1$품목[pa_crop_1$품목=='노지풋옥수수'] <- '옥수수'
pa_crop_1$품목[pa_crop_1$품목=='인삼(4년근)'] <- '인삼'
pa_crop_1 <- subset(pa_crop_1, pa_crop_1$품목 %in% rank_crop_trend$품목)
pa_crop_1 <- subset(pa_crop_1, pa_crop_1$년도 %in% rank_crop_trend$년도)

# 평균 트렌드에서 필요한 데이터만 추출
rank_crop_trend <- subset(rank_crop_trend, rank_crop_trend$품목 %in% pa_crop_1$품목)
rank_crop_trend <- subset(rank_crop_trend, rank_crop_trend$년도 %in% pa_crop_1$년도)

# View(pa_crop_1)
crop_pa_trend_corr <- data.frame(matrix(nrow=15, ncol=4))
colnames(crop_pa_trend_corr) <- c('년도', '품목', '검색지표', '소득')
crop_pa_trend_corr$년도 <- rank_crop_trend$년도
crop_pa_trend_corr$품목 <- rank_crop_trend$품목
crop_pa_trend_corr$검색지표 <- rank_crop_trend$검색지표
crop_pa_trend_corr$소득 <- pa_crop_1$소득

View(crop_pa_trend_corr)

library(ggpubr)
ggscatter(crop_pa_trend_corr, x = "검색지표", y = "소득", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", title = "식용작물 트렌드-소득 상관관계 (2016-2020)", 
          xlab = "검색지표", ylab = "소득") +
  facet_wrap(~품목, scale="free")


# 과수 소득 상위권 (1-4위)의 소득, 소득추세, 평균 검색 트렌드 추세 제공
library(dplyr)
rank_fruit_1 <- rank_fruit %>% 
  arrange(desc(평균_소득)) %>% 
  slice(1:4)
pa_fruit_1 <-subset(pa_fruit, pa_fruit$품목 %in% rank_fruit_1$품목)

# 평균 소득
rank_fruit_1$평균_소득 <- rank_fruit_1$평균_소득/1000
ggplot(data=rank_fruit_1, aes_string(x='품목', y='평균_소득', fill='품목'))+
  geom_bar(stat="identity", position = position_dodge(width=0.5))+
  ylab('평균 소득 (천원)') +
  ggtitle('과수 상위권 평균 소득')

# 소득 추세
# pa_fruit_1$소득 <- pa_fruit_1$소득/1000
pa_fruit_1 %>%
  ggplot( aes(x=년도, y=소득, group=품목, fill=품목)) +
  geom_smooth(method = "lm") +
  theme(legend.position="none") +
  ggtitle("과수 상위권 소득 추세") +
  ylab('소득 (천원)')+
  theme(
    legend.position="none",
    panel.spacing = unit(0, "lines"),
    strip.text.x = element_text(size = 8),
    plot.title = element_text(size=13)
  ) +
  facet_wrap(~품목, scale="free_y")

# 트렌드 데이터에서 소득 상위권 품목 추출
rank_fruit_1$품목
names(trend_fruit)[names(trend_fruit) == '날짜'] <- '년도'
trend_fruit_1 <- trend_fruit[,c('년도','귤', '포도', '블루베리')]
trend_fruit_1 <- melt(trend_fruit_1, id.vars="년도", variable.name='품목')
trend_fruit_1$value <- as.numeric(as.character(trend_fruit_1$value))

# 과수 소득 상위권 트렌드
trend_fruit_1 %>%
  ggplot(aes(x=년도, y=value, group=품목, fill=품목)) +
  geom_smooth(method = "lm") + scale_y_continuous(limits = c(0, 7)) +
  ggtitle("과수 상위권 키워드 트렌드 분석") +
  ylab("키워드 검색지표") +
  facet_wrap(~품목, scale="free_y")

# 과수 소득 추세 및 트렌드 추세 상관분석
# 품목별 평균 트렌드 추출
# View(pa_fruit_1)
trend_fruit_1$년도_ <- as.character(format(as.Date(trend_fruit_1$년도), format = "%Y"))

rank_fruit_trend <- aggregate(x=trend_fruit_1$value,
                             by=list(trend_fruit_1$품목,trend_fruit_1$년도_),
                             FUN=mean)
colnames(rank_fruit_trend) <- c('품목', '년도', '검색지표')

# 트렌드와 동일한 이름으로 변경 및 추출
pa_fruit_1$품목[pa_fruit_1$품목=='시설감귤'] <- '귤'
pa_fruit_1$품목[pa_fruit_1$품목=='노지포도'] <- '포도'
pa_fruit_1$품목[pa_fruit_1$품목=='시설포도'] <- '포도'
pa_fruit_1 <- subset(pa_fruit_1, pa_fruit_1$품목 %in% rank_fruit_trend$품목)
pa_fruit_1 <- subset(pa_fruit_1, pa_fruit_1$년도 %in% rank_fruit_trend$년도)
pa_fruit_1 <- pa_fruit_1[c(1,4,6,7,9,10,12,13),]

# 평균 트렌드에서 필요한 데이터만 추출
rank_fruit_trend <- subset(rank_fruit_trend, rank_fruit_trend$품목 %in% pa_fruit_1$품목)
rank_fruit_trend <- subset(rank_fruit_trend, rank_fruit_trend$년도 %in% pa_fruit_1$년도)
rank_fruit_trend <- rank_fruit_trend[-c(2,4),]
# View(rank_fruit_trend)

fruit_pa_trend_corr <- data.frame(matrix(nrow=8, ncol=4))
colnames(fruit_pa_trend_corr) <- c('년도', '품목', '검색지표', '소득')
fruit_pa_trend_corr$년도 <- rank_fruit_trend$년도
fruit_pa_trend_corr$품목 <- rank_fruit_trend$품목
fruit_pa_trend_corr$검색지표 <- rank_fruit_trend$검색지표
fruit_pa_trend_corr$소득 <- pa_fruit_1$소득

# View(fruit_pa_trend_corr)

library(ggpubr)
ggscatter(fruit_pa_trend_corr, x = "검색지표", y = "소득", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", title = "과수 트렌드-소득 상관관계 (2016-2020)", 
          xlab = "검색지표", ylab = "소득") +
  facet_wrap(~품목, scale="free")


# 시설채소 소득 상위권 (1-4위)의 소득, 소득추세, 평균 검색 트렌드 추세 제공
library(dplyr)
rank_vegi_fac_1 <- rank_vegi_fac %>% 
  arrange(desc(평균_소득)) %>% 
  slice(1:4)
pa_vegi_fac_1 <- subset(pa_vegi_fac, pa_vegi_fac$품목 %in% rank_vegi_fac_1$품목)

# 평균 소득
ggplot(data=rank_vegi_fac_1, aes_string(x='품목', y='평균_소득', fill='품목'))+
  geom_bar(stat="identity", position = position_dodge(width=0.5))+
  ylab('평균 소득 (천원)') +
  ggtitle('시설채소 상위권 평균 소득')

# 소득 추세
pa_vegi_fac_1 %>%
  ggplot( aes(x=년도, y=소득, group=품목, fill=품목)) +
  geom_smooth(method = "lm") +
  theme(legend.position="none") +
  ggtitle("시설채소 상위권 소득 추세") +
  ylab('소득 (천원)')+
  theme(
    legend.position="none",
    panel.spacing = unit(0, "lines"),
    strip.text.x = element_text(size = 8),
    plot.title = element_text(size=13)
  ) +
  facet_wrap(~품목, scale="free_y")

# 트렌드 데이터에서 소득 상위권 품목 추출
rank_vegi_fac_1$품목
names(trend_vegi_fac)[names(trend_vegi_fac) == '날짜'] <- '년도'
trend_vegi_fac_1 <- trend_vegi_fac[,c('년도','오이', '파프리카', '딸기')]
trend_vegi_fac_1 <- melt(trend_vegi_fac_1, id.vars="년도", variable.name='품목')
trend_vegi_fac_1$value <- as.numeric(as.character(trend_vegi_fac_1$value))

# 시설채소 소득 상위권 트렌드
trend_vegi_fac_1 %>%
  ggplot(aes(x=년도, y=value, group=품목, fill=품목)) +
  geom_smooth(method = "lm") + scale_y_continuous(limits = c(0, 15)) +
  ggtitle("시설채소 상위권 키워드 트렌드 분석") +
  ylab("키워드 검색지표") +
  facet_wrap(~품목, scale="free_y")

# 시설채소 소득 추세 및 트렌드 추세 상관분석
# 품목별 평균 트렌드 추출
# View(pa_vegi_fac_1)
trend_vegi_fac_1$년도_ <- as.character(format(as.Date(trend_vegi_fac_1$년도), format = "%Y"))

rank_vegi_f_trend <- aggregate(x=trend_vegi_fac_1$value,
                              by=list(trend_vegi_fac_1$품목,trend_vegi_fac_1$년도_),
                              FUN=mean)
colnames(rank_vegi_f_trend) <- c('품목', '년도', '검색지표')

# 트렌드와 동일한 이름으로 변경 및 추출
pa_vegi_fac_1$품목[pa_vegi_fac_1$품목=='딸기(촉성)'] <- '딸기'
pa_vegi_fac_1$품목[pa_vegi_fac_1$품목=='딸기(반촉성)'] <- '딸기'
pa_vegi_fac_1$품목[pa_vegi_fac_1$품목=='오이(촉성)'] <- '오이'
pa_vegi_fac_1$품목[pa_vegi_fac_1$품목=='착색단고추(파프리카)'] <- '파프리카'

pa_vegi_fac_1 <- subset(pa_vegi_fac_1, pa_vegi_fac_1$품목 %in% rank_vegi_f_trend$품목)
pa_vegi_fac_1 <- subset(pa_vegi_fac_1, pa_vegi_fac_1$년도 %in% rank_vegi_f_trend$년도)
pa_vegi_fac_1 <- aggregate(x=pa_vegi_fac_1$소득,
                               by=list(pa_vegi_fac_1$품목,pa_vegi_fac_1$년도),
                               FUN=mean)
colnames(pa_vegi_fac_1) <- c('품목', '년도', '소득')

# 평균 트렌드에서 필요한 데이터만 추출
rank_vegi_f_trend <- subset(rank_vegi_f_trend, rank_vegi_f_trend$품목 %in% pa_vegi_fac_1$품목)
rank_vegi_f_trend <- subset(rank_vegi_f_trend, rank_vegi_f_trend$년도 %in% pa_vegi_fac_1$년도)

# View(rank_vegi_f_trend)

vegi_f_pa_trend_corr <- data.frame(matrix(nrow=15, ncol=4))
colnames(vegi_f_pa_trend_corr) <- c('년도', '품목', '검색지표', '소득')
vegi_f_pa_trend_corr$년도 <- rank_vegi_f_trend$년도
vegi_f_pa_trend_corr$품목 <- rank_vegi_f_trend$품목
vegi_f_pa_trend_corr$검색지표 <- rank_vegi_f_trend$검색지표
vegi_f_pa_trend_corr$소득 <- pa_vegi_fac_1$소득

# View(vegi_f_pa_trend_corr)

library(ggpubr)
ggscatter(vegi_f_pa_trend_corr, x = "검색지표", y = "소득", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", title = "시설채소 트렌드-소득 상관관계 (2016-2020)", 
          xlab = "검색지표", ylab = "소득") +
  facet_wrap(~품목, scale="free")


# 노지채소 소득 상위권 (1-4위)의 소득, 소득추세, 평균 검색 트렌드 추세 제공
library(dplyr)
rank_vegi_wild_1 <- rank_vegi_wild %>% 
  arrange(desc(평균_소득)) %>% 
  slice(1:4)
pa_vegi_wild_1 <- subset(pa_vegi_wild, pa_vegi_wild$품목 %in% rank_vegi_wild_1$품목)

# 평균 소득
ggplot(data=rank_vegi_wild_1, aes_string(x='품목', y='평균_소득', fill='품목'))+
  geom_bar(stat="identity", position = position_dodge(width=0.5))+
  ylab('평균 소득 (천원)') +
  ggtitle('노지채소 상위권 평균 소득')

# 소득 추세
pa_vegi_wild_1 %>%
  ggplot( aes(x=년도, y=소득, group=품목, fill=품목)) +
  geom_smooth(method = "lm") +
  theme(legend.position="none") +
  ggtitle("노지채소 상위권 소득 추세") +
  ylab('소득 (천원)')+
  theme(
    legend.position="none",
    panel.spacing = unit(0, "lines"),
    strip.text.x = element_text(size = 8),
    plot.title = element_text(size=13)
  ) +
  facet_wrap(~품목, scale="free_y")

# 트렌드 데이터에서 소득 상위권 품목 추출
rank_vegi_wild_1$품목
names(trend_vegi_wild)[names(trend_vegi_wild) == '날짜'] <- '년도'
trend_vegi_wild_1 <- trend_vegi_wild[,c('년도','부추', '생강', '쪽파', '수박')]
trend_vegi_wild_1 <- melt(trend_vegi_wild_1, id.vars="년도", variable.name='품목')
trend_vegi_wild_1$value <- as.numeric(as.character(trend_vegi_wild_1$value))

# 노지채소 소득 상위권 트렌드
trend_vegi_wild_1 %>%
  ggplot(aes(x=년도, y=value, group=품목, fill=품목)) +
  geom_smooth(method = "lm") + scale_y_continuous(limits = c(0, 7)) +
  ggtitle("노지채소 상위권 키워드 트렌드 분석") +
  ylab("키워드 검색지표") +
  facet_wrap(~품목, scale="free_y")





# 노지채소 소득 추세 및 트렌드 추세 상관분석
# 품목별 평균 트렌드 추출
# View(pa_vegi_fac_1)
trend_vegi_wild_1$년도_ <- as.character(format(as.Date(trend_vegi_wild_1$년도), format = "%Y"))

rank_vegi_w_trend <- aggregate(x=trend_vegi_wild_1$value,
                               by=list(trend_vegi_wild_1$품목,trend_vegi_wild_1$년도_),
                               FUN=mean)
colnames(rank_vegi_w_trend) <- c('품목', '년도', '검색지표')

# 트렌드와 동일한 이름으로 변경 및 추출
pa_vegi_wild_1$품목[pa_vegi_wild_1$품목=='노지수박'] <- '수박'
pa_vegi_wild_1$품목[pa_vegi_wild_1$품목=='노지부추'] <- '부추'

pa_vegi_wild_1 <- subset(pa_vegi_wild_1, pa_vegi_wild_1$품목 %in% rank_vegi_w_trend$품목)
pa_vegi_wild_1 <- subset(pa_vegi_wild_1, pa_vegi_wild_1$년도 %in% rank_vegi_w_trend$년도)
pa_vegi_wild_1 <- pa_vegi_wild_1[-2,]

# 평균 트렌드에서 필요한 데이터만 추출
rank_vegi_w_trend <- subset(rank_vegi_w_trend, rank_vegi_w_trend$품목 %in% pa_vegi_wild_1$품목)
rank_vegi_w_trend <- subset(rank_vegi_w_trend, rank_vegi_w_trend$년도 %in% pa_vegi_wild_1$년도)

# View(rank_vegi_w_trend)

vegi_w_pa_trend_corr <- data.frame(matrix(nrow=15, ncol=4))
colnames(vegi_w_pa_trend_corr) <- c('년도', '품목', '검색지표', '소득')
vegi_w_pa_trend_corr$년도 <- rank_vegi_w_trend$년도
vegi_w_pa_trend_corr$품목 <- rank_vegi_w_trend$품목
vegi_w_pa_trend_corr$검색지표 <- rank_vegi_w_trend$검색지표
vegi_w_pa_trend_corr$소득 <- pa_vegi_wild_1$소득

# View(vegi_f_pa_trend_corr)

library(ggpubr)
ggscatter(vegi_w_pa_trend_corr, x = "검색지표", y = "소득", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", title = "노지채소 트렌드-소득 상관관계 (2016-2020)", 
          xlab = "검색지표", ylab = "소득") +
  facet_wrap(~품목, scale="free")


