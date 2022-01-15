# 농가지가지수 추세 분석

property_value <- read.csv("F_PROPERTY_VALUE.csv")

library(dplyr)
property_value <- 
  property_value %>% mutate(전국 = (property_value$경기
                                  +property_value$강원
                                  +property_value$충북
                                  +property_value$충남
                                  +property_value$전북
                                  +property_value$전남
                                  +property_value$경북
                                  +property_value$경남)/8)

# View(property_value)
summary(property_value)

# 날짜 데이터화 (발생 수습 - 지속적으로 문제 발생)
# install.packages('zoo')

loc <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "C") 

require(zoo)
property_value$날짜 <-as.yearmon(as.character(property_value$날짜), format='%b-%y')
Sys.setlocale("LC_TIME", loc) 


# 그룹화 분석 위해 melt
library(ggplot2)
library(reshape2)
property_value <- melt(property_value, id.vars = "날짜", variable.name='지역')
summary(property_value)

property_value %>%
  ggplot(aes(x=날짜, y=value, group=지역, color=지역)) +
  geom_line(size=1)  +
  ggtitle("농가 지가지수 추세분석 (2014-2020)") +
  scale_x_continuous() +
  ylab("농가 지가지수")

library(ggpubr)
ggscatter(property_value, x = "날짜", y = "value", group='지역', color='지역',
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",title = "농가 지가지수 추세분석 (2014-2020)", 
          xlab = "날짜", ylab = "농가 지가지수") +
  facet_wrap(~지역, scale="free_y")








