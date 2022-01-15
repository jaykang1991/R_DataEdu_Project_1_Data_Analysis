# 귀농 초기 자금 정보

colnames(f_ini_cost) <- c('정착자금 사용처', '구분', '비율')

# 귀농 초기 비용 추출
f_ini_cost_avg <- f_ini_cost[2,2:3]

# 정착자금 마련방법 추출
f_ini_cost_method <- f_ini_cost[4:9,2:3]

# 정착자금 사용처 추출
f_ini_cost_useage <- f_ini_cost[14:19,2:3]

# View(f_ini_cost_method)

library(dplyr)
library(ggplot2)

ggplot(f_ini_cost_method, aes(x = "", y = 비율, fill = 구분)) +
  geom_col() +
  geom_text(aes(label = 비율),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  ggtitle("정착자금 마련방법") +
  xlab("") + ylab("2020")

# 2020년 평균 정착자금 
f_ini_cost_avg$비율

f_ini_cost_useage$비율<- round((f_ini_cost_useage$비율/100*f_ini_cost_avg$비율), 0)
f_ini_cost_useage[,1] <- c('농지 구입 및 임대', '주택마련', '영농시설 자재구입', '농작물 재배 및 가축사육', '생활비', '기타')

ggplot(data=f_ini_cost_useage, aes_string(x='구분', y='비율', fill='구분'))+
  geom_bar(stat="identity", position = position_dodge(width=0.5))+
  ggtitle("정착자금 사용처 비교 2020")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 18, color = "darkblue")) +
  labs(x = "구분", y = '정착자금 (만원)')
