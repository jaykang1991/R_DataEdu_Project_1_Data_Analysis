# ggplot 라이브러리 소환
library(ggplot2)
trend_fruit$년도 <- as.character(format(as.Date(trend_fruit$날짜), format = "%Y"))
for (j in 2:(ncol(trend_fruit))){trend_fruit[,j] <- as.numeric(unlist(trend_fruit[,j]))}

# 년도 문자화
trend_fruit$년도 <- as.character(format(as.Date(trend_fruit$날짜), format = "%Y"))
trend_fruit_2016 <- subset(trend_fruit, trend_fruit$년도 == '2016')
trend_fruit_2017 <- subset(trend_fruit, trend_fruit$년도 == '2017')
trend_fruit_2018 <- subset(trend_fruit, trend_fruit$년도 == '2018')
trend_fruit_2019 <- subset(trend_fruit, trend_fruit$년도 == '2019')
trend_fruit_2020 <- subset(trend_fruit, trend_fruit$년도 == '2020')
trend_fruit_2021 <- subset(trend_fruit, trend_fruit$년도 == '2021')

################################################################### 사과
# 평균 10회 반복시 이상치 처리
count <- 1:10

for(i in count){
  # 2016년도 이상치 처리
  trend_fruit_2016$사과 <- ifelse(trend_fruit_2016$사과 < boxplot(trend_fruit_2016$사과)$stats[1]
                                | trend_fruit_2016$사과 > boxplot(trend_fruit_2016$사과)$stats[5]
                                , mean(trend_fruit_2016$사과), trend_fruit_2016$사과)
  
  # 2020년도 이상치 처리
  trend_fruit_2020$사과 <- ifelse(trend_fruit_2020$사과 < boxplot(trend_fruit_2020$사과)$stats[1]
                                | trend_fruit_2020$사과 > boxplot(trend_fruit_2020$사과)$stats[5]
                                , mean(trend_fruit_2020$사과), trend_fruit_2020$사과)
  
  # 2021년도 이상치 처리
  trend_fruit_2021$사과 <- ifelse(trend_fruit_2021$사과 < boxplot(trend_fruit_2021$사과)$stats[1]
                                | trend_fruit_2021$사과 > boxplot(trend_fruit_2021$사과)$stats[5]
                                , mean(trend_fruit_2021$사과), trend_fruit_2021$사과)
}

# 처리한 2016, 2018, 2019, 2020, 2021년 데이터 원래 데이터에 삽입 및 이상치 처리 확인
trend_fruit[trend_fruit$년도 == '2016','사과'] <- trend_fruit_2016$사과
trend_fruit[trend_fruit$년도 == '2020','사과'] <- trend_fruit_2020$사과
trend_fruit[trend_fruit$년도 == '2021','사과'] <- trend_fruit_2021$사과
########################################################################배
for(i in count){
  # 2016년도 이상치 처리
  trend_fruit_2016$배 <- ifelse(trend_fruit_2016$배 < boxplot(trend_fruit_2016$배)$stats[1]
                               | trend_fruit_2016$배 > boxplot(trend_fruit_2016$배)$stats[5]
                               , mean(trend_fruit_2016$배), trend_fruit_2016$배)
  
  # 2017년도 이상치 처리 ---- 처리 안됨
  trend_fruit_2017$배 <- ifelse(trend_fruit_2017$배 < boxplot(trend_fruit_2017$배)$stats[1]
                               | trend_fruit_2017$배 > boxplot(trend_fruit_2017$배)$stats[5]
                               , mean(trend_fruit_2017$배), trend_fruit_2017$배)
  
  # 2018년도 이상치 처리
  trend_fruit_2018$배 <- ifelse(trend_fruit_2018$배 < boxplot(trend_fruit_2018$배)$stats[1]
                               | trend_fruit_2018$배 > boxplot(trend_fruit_2018$배)$stats[5]
                               , mean(trend_fruit_2018$배), trend_fruit_2018$배)
  
  # 2019년도 이상치 처리
  trend_fruit_2019$배 <- ifelse(trend_fruit_2019$배 < boxplot(trend_fruit_2019$배)$stats[1]
                               | trend_fruit_2019$배 > boxplot(trend_fruit_2019$배)$stats[5]
                               , mean(trend_fruit_2019$배), trend_fruit_2019$배)
  
  # 2020년도 이상치 처리
  trend_fruit_2020$배 <- ifelse(trend_fruit_2020$배 < boxplot(trend_fruit_2020$배)$stats[1]
                               | trend_fruit_2020$배 > boxplot(trend_fruit_2020$배)$stats[5]
                               , mean(trend_fruit_2020$배), trend_fruit_2020$배)
  
  # 2021년도 이상치 처리
  trend_fruit_2021$배 <- ifelse(trend_fruit_2021$배 < boxplot(trend_fruit_2021$배)$stats[1]
                               | trend_fruit_2021$배 > boxplot(trend_fruit_2021$배)$stats[5]
                               , mean(trend_fruit_2021$배), trend_fruit_2021$배)
}

# 처리한 2017, 2018, 2019, 2020, 2021년 데이터 원래 데이터에 삽입 및 이상치 처리 확인
trend_fruit[trend_fruit$년도 == '2016','배'] <- trend_fruit_2016$배
trend_fruit[trend_fruit$년도 == '2017','배'] <- trend_fruit_2017$배
trend_fruit[trend_fruit$년도 == '2018','배'] <- trend_fruit_2018$배
trend_fruit[trend_fruit$년도 == '2019','배'] <- trend_fruit_2019$배
trend_fruit[trend_fruit$년도 == '2020','배'] <- trend_fruit_2020$배
trend_fruit[trend_fruit$년도 == '2021','배'] <- trend_fruit_2021$배

##################################################################################복숭아

for(i in count){
  # 2016년도 이상치 처리
  trend_fruit_2016$복숭아 <- ifelse(trend_fruit_2016$복숭아 < boxplot(trend_fruit_2016$복숭아)$stats[1]
                                 | trend_fruit_2016$복숭아 > boxplot(trend_fruit_2016$복숭아)$stats[5]
                                 , mean(trend_fruit_2016$복숭아), trend_fruit_2016$복숭아)
  
  # 2017년도 이상치 처리
  trend_fruit_2017$복숭아 <- ifelse(trend_fruit_2017$복숭아 < boxplot(trend_fruit_2017$복숭아)$stats[1]
                                 | trend_fruit_2017$복숭아 > boxplot(trend_fruit_2017$복숭아)$stats[5]
                                 , mean(trend_fruit_2017$복숭아), trend_fruit_2017$복숭아)
  
  # 2018년도 이상치 처리
  trend_fruit_2018$복숭아 <- ifelse(trend_fruit_2018$복숭아 < boxplot(trend_fruit_2018$복숭아)$stats[1]
                                 | trend_fruit_2018$복숭아 > boxplot(trend_fruit_2018$복숭아)$stats[5]
                                 , mean(trend_fruit_2018$복숭아), trend_fruit_2018$복숭아)
  
  # 2019년도 이상치 처리
  trend_fruit_2019$복숭아 <- ifelse(trend_fruit_2019$복숭아 < boxplot(trend_fruit_2019$복숭아)$stats[1]
                                 | trend_fruit_2019$복숭아 > boxplot(trend_fruit_2019$복숭아)$stats[5]
                                 , mean(trend_fruit_2019$복숭아), trend_fruit_2019$복숭아)
  
  # 2020년도 이상치 처리
  trend_fruit_2020$복숭아 <- as.numeric(trend_fruit_2020$복숭아)
  boxplot(trend_fruit_2020$복숭아)$stats
  trend_fruit_2020$복숭아 <- ifelse(trend_fruit_2020$복숭아 < boxplot(trend_fruit_2020$복숭아)$stats[1]
                                 | trend_fruit_2020$복숭아 > boxplot(trend_fruit_2020$복숭아)$stats[5]
                                 , mean(trend_fruit_2020$복숭아), trend_fruit_2020$복숭아)
  
  # 2021년도 이상치 처리
  trend_fruit_2021$복숭아 <- ifelse(trend_fruit_2021$복숭아 < boxplot(trend_fruit_2021$복숭아)$stats[1]
                                 | trend_fruit_2021$복숭아 > boxplot(trend_fruit_2021$복숭아)$stats[5]
                                 , mean(trend_fruit_2021$복숭아), trend_fruit_2021$복숭아)
}

# 처리한 2017, 2018, 2019, 2020, 2021년 데이터 원래 데이터에 삽입 및 이상치 처리 확인
trend_fruit[trend_fruit$년도 == '2016','복숭아'] <- trend_fruit_2016$복숭아
trend_fruit[trend_fruit$년도 == '2017','복숭아'] <- trend_fruit_2017$복숭아
trend_fruit[trend_fruit$년도 == '2018','복숭아'] <- trend_fruit_2018$복숭아
trend_fruit[trend_fruit$년도 == '2019','복숭아'] <- trend_fruit_2019$복숭아
trend_fruit[trend_fruit$년도 == '2020','복숭아'] <- trend_fruit_2020$복숭아
trend_fruit[trend_fruit$년도 == '2021','복숭아'] <- trend_fruit_2021$복숭아
##################################################################################포도
for(i in count){
  # 2016년도 이상치 처리
  trend_fruit_2016$포도 <- ifelse(trend_fruit_2016$포도 < boxplot(trend_fruit_2016$포도)$stats[1]
                                | trend_fruit_2016$포도 > boxplot(trend_fruit_2016$포도)$stats[5]
                                , mean(trend_fruit_2016$포도), trend_fruit_2016$포도)
  
  # 2017년도 이상치 처리
  trend_fruit_2017$포도 <- ifelse(trend_fruit_2017$포도 < boxplot(trend_fruit_2017$포도)$stats[1]
                                | trend_fruit_2017$포도 > boxplot(trend_fruit_2017$포도)$stats[5]
                                , mean(trend_fruit_2017$포도), trend_fruit_2017$포도)
  
  # 2018년도 이상치 처리
  trend_fruit_2018$포도 <- ifelse(trend_fruit_2018$포도 < boxplot(trend_fruit_2018$포도)$stats[1]
                                | trend_fruit_2018$포도 > boxplot(trend_fruit_2018$포도)$stats[5]
                                , mean(trend_fruit_2018$포도), trend_fruit_2018$포도)
  
  # 2019년도 이상치 처리
  trend_fruit_2019$포도 <- ifelse(trend_fruit_2019$포도 < boxplot(trend_fruit_2019$포도)$stats[1]
                                | trend_fruit_2019$포도 > boxplot(trend_fruit_2019$포도)$stats[5]
                                , mean(trend_fruit_2019$포도), trend_fruit_2019$포도)
  
  # 2020년도 이상치 처리
  trend_fruit_2020$포도 <- ifelse(trend_fruit_2020$포도 < boxplot(trend_fruit_2020$포도)$stats[1]
                                | trend_fruit_2020$포도 > boxplot(trend_fruit_2020$포도)$stats[5]
                                , mean(trend_fruit_2020$포도), trend_fruit_2020$포도)
  
  # 2021년도 이상치 처리
  trend_fruit_2021$포도 <- ifelse(trend_fruit_2021$포도 < boxplot(trend_fruit_2021$포도)$stats[1]
                                | trend_fruit_2021$포도 > boxplot(trend_fruit_2021$포도)$stats[5]
                                , mean(trend_fruit_2021$포도), trend_fruit_2021$포도)
}

# 처리한 2017, 2018, 2019, 2020, 2021년 데이터 원래 데이터에 삽입 및 이상치 처리 확인
trend_fruit[trend_fruit$년도 == '2016','포도'] <- trend_fruit_2016$포도
trend_fruit[trend_fruit$년도 == '2017','포도'] <- trend_fruit_2017$포도
trend_fruit[trend_fruit$년도 == '2018','포도'] <- trend_fruit_2018$포도
trend_fruit[trend_fruit$년도 == '2019','포도'] <- trend_fruit_2019$포도
trend_fruit[trend_fruit$년도 == '2020','포도'] <- trend_fruit_2020$포도
trend_fruit[trend_fruit$년도 == '2021','포도'] <- trend_fruit_2021$포도
################################################################################## 귤
for(i in count){
  # 2016년도 이상치 처리
  trend_fruit_2016$귤 <- ifelse(trend_fruit_2016$귤 < boxplot(trend_fruit_2016$귤)$stats[1]
                               | trend_fruit_2016$귤 > boxplot(trend_fruit_2016$귤)$stats[5]
                               , mean(trend_fruit_2016$귤), trend_fruit_2016$귤)
  
  # 2017년도 이상치 처리
  trend_fruit_2017$귤 <- ifelse(trend_fruit_2017$귤 < boxplot(trend_fruit_2017$귤)$stats[1]
                               | trend_fruit_2017$귤 > boxplot(trend_fruit_2017$귤)$stats[5]
                               , mean(trend_fruit_2017$귤), trend_fruit_2017$귤)
  
  # 2018년도 이상치 처리
  trend_fruit_2018$귤 <- ifelse(trend_fruit_2018$귤 < boxplot(trend_fruit_2018$귤)$stats[1]
                               | trend_fruit_2018$귤 > boxplot(trend_fruit_2018$귤)$stats[5]
                               , mean(trend_fruit_2018$귤), trend_fruit_2018$귤)
  
  # 2019년도 이상치 처리
  trend_fruit_2019$귤 <- ifelse(trend_fruit_2019$귤 < boxplot(trend_fruit_2019$귤)$stats[1]
                               | trend_fruit_2019$귤 > boxplot(trend_fruit_2019$귤)$stats[5]
                               , mean(trend_fruit_2019$귤), trend_fruit_2019$귤)
  
  # 2020년도 이상치 처리
  trend_fruit_2020$귤 <- ifelse(trend_fruit_2020$귤 < boxplot(trend_fruit_2020$귤)$stats[1]
                               | trend_fruit_2020$귤 > boxplot(trend_fruit_2020$귤)$stats[5]
                               , mean(trend_fruit_2020$귤), trend_fruit_2020$귤)
  
  # 2021년도 이상치 처리
  trend_fruit_2021$귤 <- ifelse(trend_fruit_2021$귤 < boxplot(trend_fruit_2021$귤)$stats[1]
                               | trend_fruit_2021$귤 > boxplot(trend_fruit_2021$귤)$stats[5]
                               , mean(trend_fruit_2021$귤), trend_fruit_2021$귤)
}

# 처리한 2017, 2018, 2019, 2020, 2021년 데이터 원래 데이터에 삽입 및 이상치 처리 확인
trend_fruit[trend_fruit$년도 == '2016','귤'] <- trend_fruit_2016$귤
trend_fruit[trend_fruit$년도 == '2017','귤'] <- trend_fruit_2017$귤
trend_fruit[trend_fruit$년도 == '2018','귤'] <- trend_fruit_2018$귤
trend_fruit[trend_fruit$년도 == '2019','귤'] <- trend_fruit_2019$귤
trend_fruit[trend_fruit$년도 == '2020','귤'] <- trend_fruit_2020$귤
trend_fruit[trend_fruit$년도 == '2021','귤'] <- trend_fruit_2021$귤
##################################################################################오미자
for(i in count){
  # 2016년도 이상치 처리
  trend_fruit_2016$오미자 <- ifelse(trend_fruit_2016$오미자 < boxplot(trend_fruit_2016$오미자)$stats[1]
                                 | trend_fruit_2016$오미자 > boxplot(trend_fruit_2016$오미자)$stats[5]
                                 , mean(trend_fruit_2016$오미자), trend_fruit_2016$오미자)
  
  # 2017년도 이상치 처리
  trend_fruit_2017$오미자 <- ifelse(trend_fruit_2017$오미자 < boxplot(trend_fruit_2017$오미자)$stats[1]
                                 | trend_fruit_2017$오미자 > boxplot(trend_fruit_2017$오미자)$stats[5]
                                 , mean(trend_fruit_2017$오미자), trend_fruit_2017$오미자)
  
  # 2018년도 이상치 처리
  trend_fruit_2018$오미자 <- ifelse(trend_fruit_2018$오미자 < boxplot(trend_fruit_2018$오미자)$stats[1]
                                 | trend_fruit_2018$오미자 > boxplot(trend_fruit_2018$오미자)$stats[5]
                                 , mean(trend_fruit_2018$오미자), trend_fruit_2018$오미자)
  
  # 2019년도 이상치 처리
  trend_fruit_2019$오미자 <- ifelse(trend_fruit_2019$오미자 < boxplot(trend_fruit_2019$오미자)$stats[1]
                                 | trend_fruit_2019$오미자 > boxplot(trend_fruit_2019$오미자)$stats[5]
                                 , mean(trend_fruit_2019$오미자), trend_fruit_2019$오미자)
  
  # 2020년도 이상치 처리
  trend_fruit_2020$오미자 <- ifelse(trend_fruit_2020$오미자 < boxplot(trend_fruit_2020$오미자)$stats[1]
                                 | trend_fruit_2020$오미자 > boxplot(trend_fruit_2020$오미자)$stats[5]
                                 , mean(trend_fruit_2020$오미자), trend_fruit_2020$오미자)
  
  # 2021년도 이상치 처리
  trend_fruit_2021$오미자 <- ifelse(trend_fruit_2021$오미자 < boxplot(trend_fruit_2021$오미자)$stats[1]
                                 | trend_fruit_2021$오미자 > boxplot(trend_fruit_2021$오미자)$stats[5]
                                 , mean(trend_fruit_2021$오미자), trend_fruit_2021$오미자)
}

# 처리한 2017, 2018, 2019, 2020, 2021년 데이터 원래 데이터에 삽입 및 이상치 처리 확인
trend_fruit[trend_fruit$년도 == '2016','오미자'] <- trend_fruit_2016$오미자
trend_fruit[trend_fruit$년도 == '2017','오미자'] <- trend_fruit_2017$오미자
trend_fruit[trend_fruit$년도 == '2018','오미자'] <- trend_fruit_2018$오미자
trend_fruit[trend_fruit$년도 == '2019','오미자'] <- trend_fruit_2019$오미자
trend_fruit[trend_fruit$년도 == '2020','오미자'] <- trend_fruit_2020$오미자
trend_fruit[trend_fruit$년도 == '2021','오미자'] <- trend_fruit_2021$오미자

##################################################################################블루베리
for(i in count){
  # 2016년도 이상치 처리
  trend_fruit_2016$블루베리 <- ifelse(trend_fruit_2016$블루베리 < boxplot(trend_fruit_2016$블루베리)$stats[1]
                                  | trend_fruit_2016$블루베리 > boxplot(trend_fruit_2016$블루베리)$stats[5]
                                  , mean(trend_fruit_2016$블루베리), trend_fruit_2016$블루베리)
  
  # 2017년도 이상치 처리
  trend_fruit_2017$블루베리 <- ifelse(trend_fruit_2017$블루베리 < boxplot(trend_fruit_2017$블루베리)$stats[1]
                                  | trend_fruit_2017$블루베리 > boxplot(trend_fruit_2017$블루베리)$stats[5]
                                  , mean(trend_fruit_2017$블루베리), trend_fruit_2017$블루베리)
  
  # 2018년도 이상치 처리
  trend_fruit_2018$블루베리 <- ifelse(trend_fruit_2018$블루베리 < boxplot(trend_fruit_2018$블루베리)$stats[1]
                                  | trend_fruit_2018$블루베리 > boxplot(trend_fruit_2018$블루베리)$stats[5]
                                  , mean(trend_fruit_2018$블루베리), trend_fruit_2018$블루베리)
  
  # 2019년도 이상치 처리
  trend_fruit_2019$블루베리 <- ifelse(trend_fruit_2019$블루베리 < boxplot(trend_fruit_2019$블루베리)$stats[1]
                                  | trend_fruit_2019$블루베리 > boxplot(trend_fruit_2019$블루베리)$stats[5]
                                  , mean(trend_fruit_2019$블루베리), trend_fruit_2019$블루베리)
  
  # 2020년도 이상치 처리
  trend_fruit_2020$블루베리 <- ifelse(trend_fruit_2020$블루베리 < boxplot(trend_fruit_2020$블루베리)$stats[1]
                                  | trend_fruit_2020$블루베리 > boxplot(trend_fruit_2020$블루베리)$stats[5]
                                  , mean(trend_fruit_2020$블루베리), trend_fruit_2020$블루베리)
  
  # 2021년도 이상치 처리
  trend_fruit_2021$블루베리 <- ifelse(trend_fruit_2021$블루베리 < boxplot(trend_fruit_2021$블루베리)$stats[1]
                                  | trend_fruit_2021$블루베리 > boxplot(trend_fruit_2021$블루베리)$stats[5]
                                  , mean(trend_fruit_2021$블루베리), trend_fruit_2021$블루베리)
}

# 처리한 2017, 2018, 2019, 2020, 2021년 데이터 원래 데이터에 삽입 및 이상치 처리 확인
trend_fruit[trend_fruit$년도 == '2016','블루베리'] <- trend_fruit_2016$블루베리
trend_fruit[trend_fruit$년도 == '2017','블루베리'] <- trend_fruit_2017$블루베리
trend_fruit[trend_fruit$년도 == '2018','블루베리'] <- trend_fruit_2018$블루베리
trend_fruit[trend_fruit$년도 == '2019','블루베리'] <- trend_fruit_2019$블루베리
trend_fruit[trend_fruit$년도 == '2020','블루베리'] <- trend_fruit_2020$블루베리
trend_fruit[trend_fruit$년도 == '2021','블루베리'] <- trend_fruit_2021$블루베리

##################################################################################단감
for(i in count){
  # 2016년도 이상치 처리
  trend_fruit_2016$단감 <- ifelse(trend_fruit_2016$단감 < boxplot(trend_fruit_2016$단감)$stats[1]
                                | trend_fruit_2016$단감 > boxplot(trend_fruit_2016$단감)$stats[5]
                                , mean(trend_fruit_2016$단감), trend_fruit_2016$단감)
  
  # 2017년도 이상치 처리
  trend_fruit_2017$단감 <- ifelse(trend_fruit_2017$단감 < boxplot(trend_fruit_2017$단감)$stats[1]
                                | trend_fruit_2017$단감 > boxplot(trend_fruit_2017$단감)$stats[5]
                                , mean(trend_fruit_2017$단감), trend_fruit_2017$단감)
  
  # 2018년도 이상치 처리
  trend_fruit_2018$단감 <- ifelse(trend_fruit_2018$단감 < boxplot(trend_fruit_2018$단감)$stats[1]
                                | trend_fruit_2018$단감 > boxplot(trend_fruit_2018$단감)$stats[5]
                                , mean(trend_fruit_2018$단감), trend_fruit_2018$단감)
  
  # 2019년도 이상치 처리 ------ 처리 안됨
  trend_fruit_2019$단감 <- ifelse(trend_fruit_2019$단감 < boxplot(trend_fruit_2019$단감)$stats[1]
                                | trend_fruit_2019$단감 > boxplot(trend_fruit_2019$단감)$stats[5]
                                , mean(trend_fruit_2019$단감), trend_fruit_2019$단감)
  
  # 2020년도 이상치 처리 ------ 처리 안됨
  trend_fruit_2020$단감 <- ifelse(trend_fruit_2020$단감 < boxplot(trend_fruit_2020$단감)$stats[1]
                                | trend_fruit_2020$단감 > boxplot(trend_fruit_2020$단감)$stats[5]
                                , mean(trend_fruit_2020$단감), trend_fruit_2020$단감)
  
  # 2021년도 이상치 처리
  trend_fruit_2021$단감 <- ifelse(trend_fruit_2021$단감 < boxplot(trend_fruit_2021$단감)$stats[1]
                                | trend_fruit_2021$단감 > boxplot(trend_fruit_2021$단감)$stats[5]
                                , mean(trend_fruit_2021$단감), trend_fruit_2021$단감)
}

# 처리한 2017, 2018, 2019, 2020, 2021년 데이터 원래 데이터에 삽입 및 이상치 처리 확인
trend_fruit[trend_fruit$년도 == '2016','단감'] <- trend_fruit_2016$단감
trend_fruit[trend_fruit$년도 == '2017','단감'] <- trend_fruit_2017$단감
trend_fruit[trend_fruit$년도 == '2018','단감'] <- trend_fruit_2018$단감
trend_fruit[trend_fruit$년도 == '2019','단감'] <- trend_fruit_2019$단감
trend_fruit[trend_fruit$년도 == '2020','단감'] <- trend_fruit_2020$단감
trend_fruit[trend_fruit$년도 == '2021','단감'] <- trend_fruit_2021$단감

##################################################################################키위
for(i in count){
  # 2016년도 이상치 처리
  trend_fruit_2016$키위 <- ifelse(trend_fruit_2016$키위 < boxplot(trend_fruit_2016$키위)$stats[1]
                                | trend_fruit_2016$키위 > boxplot(trend_fruit_2016$키위)$stats[5]
                                , mean(trend_fruit_2016$키위), trend_fruit_2016$키위)
  
  # 2017년도 이상치 처리
  trend_fruit_2017$키위 <- ifelse(trend_fruit_2017$키위 < boxplot(trend_fruit_2017$키위)$stats[1]
                                | trend_fruit_2017$키위 > boxplot(trend_fruit_2017$키위)$stats[5]
                                , mean(trend_fruit_2017$키위), trend_fruit_2017$키위)
  
  # 2018년도 이상치 처리
  trend_fruit_2018$키위 <- ifelse(trend_fruit_2018$키위 < boxplot(trend_fruit_2018$키위)$stats[1]
                                | trend_fruit_2018$키위 > boxplot(trend_fruit_2018$키위)$stats[5]
                                , mean(trend_fruit_2018$키위), trend_fruit_2018$키위)
  
  # 2020년도 이상치 처리 ------ 처리 안됨
  trend_fruit_2020$키위 <- ifelse(trend_fruit_2020$키위 < boxplot(trend_fruit_2020$키위)$stats[1]
                                | trend_fruit_2020$키위 > boxplot(trend_fruit_2020$키위)$stats[5]
                                , mean(trend_fruit_2020$키위), trend_fruit_2020$키위)
  
  # 2021년도 이상치 처리
  trend_fruit_2021$키위 <- ifelse(trend_fruit_2021$키위 < boxplot(trend_fruit_2021$키위)$stats[1]
                                | trend_fruit_2021$키위 > boxplot(trend_fruit_2021$키위)$stats[5]
                                , mean(trend_fruit_2021$키위), trend_fruit_2021$키위)
}

# 처리한 2017, 2018, 2019, 2020, 2021년 데이터 원래 데이터에 삽입 및 이상치 처리 확인
trend_fruit[trend_fruit$년도 == '2016','키위'] <- trend_fruit_2016$키위
trend_fruit[trend_fruit$년도 == '2017','키위'] <- trend_fruit_2017$키위
trend_fruit[trend_fruit$년도 == '2018','키위'] <- trend_fruit_2018$키위
trend_fruit[trend_fruit$년도 == '2020','키위'] <- trend_fruit_2020$키위
trend_fruit[trend_fruit$년도 == '2021','키위'] <- trend_fruit_2021$키위

##################################################################################자두
for(i in count){
  # 2016년도 이상치 처리
  trend_fruit_2016$자두 <- ifelse(trend_fruit_2016$자두 < boxplot(trend_fruit_2016$자두)$stats[1]
                                | trend_fruit_2016$자두 > boxplot(trend_fruit_2016$자두)$stats[5]
                                , mean(trend_fruit_2016$자두), trend_fruit_2016$자두)
  
  # 2017년도 이상치 처리
  trend_fruit_2017$자두 <- ifelse(trend_fruit_2017$자두 < boxplot(trend_fruit_2017$자두)$stats[1]
                                | trend_fruit_2017$자두 > boxplot(trend_fruit_2017$자두)$stats[5]
                                , mean(trend_fruit_2017$자두), trend_fruit_2017$자두)
  
  # 2018년도 이상치 처리
  trend_fruit_2018$자두 <- ifelse(trend_fruit_2018$자두 < boxplot(trend_fruit_2018$자두)$stats[1]
                                | trend_fruit_2018$자두 > boxplot(trend_fruit_2018$자두)$stats[5]
                                , mean(trend_fruit_2018$자두), trend_fruit_2018$자두)
  
  # 2019년도 이상치 처리
  trend_fruit_2019$자두 <- ifelse(trend_fruit_2019$자두 < boxplot(trend_fruit_2019$자두)$stats[1]
                                | trend_fruit_2019$자두 > boxplot(trend_fruit_2019$자두)$stats[5]
                                , mean(trend_fruit_2019$자두), trend_fruit_2019$자두)
  
  # 2020년도 이상치 처리
  trend_fruit_2020$자두 <- ifelse(trend_fruit_2020$자두 < boxplot(trend_fruit_2020$자두)$stats[1]
                                | trend_fruit_2020$자두 > boxplot(trend_fruit_2020$자두)$stats[5]
                                , mean(trend_fruit_2020$자두), trend_fruit_2020$자두)
  
  # 2021년도 이상치 처리
  trend_fruit_2021$자두 <- ifelse(trend_fruit_2021$자두 < boxplot(trend_fruit_2021$자두)$stats[1]
                                | trend_fruit_2021$자두 > boxplot(trend_fruit_2021$자두)$stats[5]
                                , mean(trend_fruit_2021$자두), trend_fruit_2021$자두)
}

# 처리한 2017, 2018, 2019, 2020, 2021년 데이터 원래 데이터에 삽입 및 이상치 처리 확인
trend_fruit[trend_fruit$년도 == '2016','자두'] <- trend_fruit_2016$자두
trend_fruit[trend_fruit$년도 == '2017','자두'] <- trend_fruit_2017$자두
trend_fruit[trend_fruit$년도 == '2018','자두'] <- trend_fruit_2018$자두
trend_fruit[trend_fruit$년도 == '2019','자두'] <- trend_fruit_2019$자두
trend_fruit[trend_fruit$년도 == '2020','자두'] <- trend_fruit_2020$자두
trend_fruit[trend_fruit$년도 == '2021','자두'] <- trend_fruit_2021$자두

tr_fruit_cols <- colnames(trend_fruit)
# 모든 boxplot 프린트
for(j in 1:length(tr_fruit_cols)) {
  if (tr_fruit_cols[j] == "날짜" || tr_fruit_cols[j] == "년도"){
    next
  }
  print(ggplot(data=trend_fruit, aes_string(x="년도", y=tr_fruit_cols[j]))+
          geom_boxplot( outlier.color = "red"))
}





















