# ggplot 라이브러리 소환
library(ggplot2)
trend_vegi_fac$년도 <- as.character(format(as.Date(trend_vegi_fac$날짜), format = "%Y"))
for (j in 2:(ncol(trend_vegi_fac))){trend_vegi_fac[,j] <- as.numeric(unlist(trend_vegi_fac[,j]))}

# 년도 문자화
trend_vegi_fac$년도 <- as.character(format(as.Date(trend_vegi_fac$날짜), format = "%Y"))
trend_vegi_fac_2016 <- subset(trend_vegi_fac, trend_vegi_fac$년도 == '2016')
trend_vegi_fac_2017 <- subset(trend_vegi_fac, trend_vegi_fac$년도 == '2017')
trend_vegi_fac_2018 <- subset(trend_vegi_fac, trend_vegi_fac$년도 == '2018')
trend_vegi_fac_2019 <- subset(trend_vegi_fac, trend_vegi_fac$년도 == '2019')
trend_vegi_fac_2020 <- subset(trend_vegi_fac, trend_vegi_fac$년도 == '2020')
trend_vegi_fac_2021 <- subset(trend_vegi_fac, trend_vegi_fac$년도 == '2021')

################################# 토마토 ################################
# 평균 10회 반복시 이상치 처리
count <- 1:10

for(i in count){
  # 2019년도 이상치 평균치로 변경
  trend_vegi_fac_2019$토마토 <- ifelse(trend_vegi_fac_2019$토마토 < 5.87 | trend_vegi_fac_2019$토마토 > 15.01, 
                                    mean(trend_vegi_fac_2019$토마토), trend_vegi_fac_2019$토마토)
}

# 처리한 데이터 원래 데이터에 삽입 및 이상치 처리 확인
trend_vegi_fac[trend_vegi_fac$년도 == '2019','토마토'] <- trend_vegi_fac_2019$토마토
ggplot(data=trend_vegi_fac, aes(x=년도, y=토마토)) + geom_boxplot( outlier.color = "red")


################################# 방울토마토 ################################
# ggplot 그리기 (이상값 찾기) 방울토마토
# ggplot(data=trend_vegi_fac, aes(x=년도, y=방울토마토)) + geom_boxplot( outlier.color = "red")

# 이상치 없음


################################# 가지 ################################
for(i in count){
  # 2016, 2019년도 이상치 평균치로 변경
  trend_vegi_fac_2016$가지 <- ifelse(trend_vegi_fac_2016$가지 < boxplot(trend_vegi_fac_2016$가지)$stats[1]
                                   | trend_vegi_fac_2016$가지 > boxplot(trend_vegi_fac_2016$가지)$stats[5]
                                   , mean(trend_vegi_fac_2016$가지), trend_vegi_fac_2016$가지)
  
  trend_vegi_fac_2019$가지 <- ifelse(trend_vegi_fac_2019$가지 < boxplot(trend_vegi_fac_2019$가지)$stats[1]
                                   | trend_vegi_fac_2019$가지 > boxplot(trend_vegi_fac_2019$가지)$stats[5]
                                   , mean(trend_vegi_fac_2019$가지), trend_vegi_fac_2019$가지)
}

# 처리한 데이터 원래 데이터에 삽입 및 이상치 처리 확인
trend_vegi_fac[trend_vegi_fac$년도 == '2016','가지'] <- trend_vegi_fac_2016$가지
trend_vegi_fac[trend_vegi_fac$년도 == '2019','가지'] <- trend_vegi_fac_2019$가지

################################# 파프리카 ################################
for(i in count){
  # 2016, 2018, 2019, 2020년도 이상치 평균치로 변경
  trend_vegi_fac_2016$파프리카 <- ifelse(trend_vegi_fac_2016$파프리카 < boxplot(trend_vegi_fac_2016$파프리카)$stats[1]
                                     | trend_vegi_fac_2016$파프리카 > boxplot(trend_vegi_fac_2016$파프리카)$stats[5]
                                     , mean(trend_vegi_fac_2016$파프리카), trend_vegi_fac_2016$파프리카)
  
  trend_vegi_fac_2018$파프리카 <- ifelse(trend_vegi_fac_2018$파프리카 < boxplot(trend_vegi_fac_2018$파프리카)$stats[1]
                                     | trend_vegi_fac_2018$파프리카 > boxplot(trend_vegi_fac_2018$파프리카)$stats[5]
                                     , mean(trend_vegi_fac_2018$파프리카), trend_vegi_fac_2018$파프리카)
  
  trend_vegi_fac_2019$파프리카 <- ifelse(trend_vegi_fac_2019$파프리카 < boxplot(trend_vegi_fac_2019$파프리카)$stats[1]
                                     | trend_vegi_fac_2019$파프리카 > boxplot(trend_vegi_fac_2019$파프리카)$stats[5]
                                     , mean(trend_vegi_fac_2019$파프리카), trend_vegi_fac_2019$파프리카)
  
  trend_vegi_fac_2020$파프리카 <- ifelse(trend_vegi_fac_2020$파프리카 < boxplot(trend_vegi_fac_2020$파프리카)$stats[1]
                                     | trend_vegi_fac_2020$파프리카 > boxplot(trend_vegi_fac_2020$파프리카)$stats[5]
                                     , mean(trend_vegi_fac_2020$파프리카), trend_vegi_fac_2020$파프리카)
}

# 처리한 데이터 원래 데이터에 삽입 및 이상치 처리 확인
trend_vegi_fac[trend_vegi_fac$년도 == '2016','파프리카'] <- trend_vegi_fac_2016$파프리카
trend_vegi_fac[trend_vegi_fac$년도 == '2018','파프리카'] <- trend_vegi_fac_2018$파프리카
trend_vegi_fac[trend_vegi_fac$년도 == '2019','파프리카'] <- trend_vegi_fac_2019$파프리카
trend_vegi_fac[trend_vegi_fac$년도 == '2020','파프리카'] <- trend_vegi_fac_2020$파프리카

################################# 상추 ################################
for(i in count){
  # 2016 제외 이상치 평균치로 변경 ----- 2017, 2019 처리 안됨
  trend_vegi_fac_2017$상추 <- ifelse(trend_vegi_fac_2017$상추 < boxplot(trend_vegi_fac_2017$상추)$stats[1]
                                   | trend_vegi_fac_2017$상추 > boxplot(trend_vegi_fac_2017$상추)$stats[5]
                                   , mean(trend_vegi_fac_2017$상추), trend_vegi_fac_2017$상추)
  
  trend_vegi_fac_2018$상추 <- ifelse(trend_vegi_fac_2018$상추 < boxplot(trend_vegi_fac_2018$상추)$stats[1]
                                   | trend_vegi_fac_2018$상추 > boxplot(trend_vegi_fac_2018$상추)$stats[5]
                                   , mean(trend_vegi_fac_2018$상추), trend_vegi_fac_2018$상추)
  
  trend_vegi_fac_2019$상추 <- ifelse(trend_vegi_fac_2019$상추 < boxplot(trend_vegi_fac_2019$상추)$stats[1]
                                   | trend_vegi_fac_2019$상추 > boxplot(trend_vegi_fac_2019$상추)$stats[5]
                                   , mean(trend_vegi_fac_2019$상추), trend_vegi_fac_2019$상추)
  
  trend_vegi_fac_2020$상추 <- ifelse(trend_vegi_fac_2020$상추 < boxplot(trend_vegi_fac_2020$상추)$stats[1]
                                   | trend_vegi_fac_2020$상추 > boxplot(trend_vegi_fac_2020$상추)$stats[5]
                                   , mean(trend_vegi_fac_2020$상추), trend_vegi_fac_2020$상추)
  
  trend_vegi_fac_2021$상추 <- ifelse(trend_vegi_fac_2021$상추 < boxplot(trend_vegi_fac_2021$상추)$stats[1]
                                   | trend_vegi_fac_2021$상추 > boxplot(trend_vegi_fac_2021$상추)$stats[5]
                                   , mean(trend_vegi_fac_2021$상추), trend_vegi_fac_2021$상추)
}

# 처리한 데이터 원래 데이터에 삽입 및 이상치 처리 확인
trend_vegi_fac[trend_vegi_fac$년도 == '2017','상추'] <- trend_vegi_fac_2017$상추
trend_vegi_fac[trend_vegi_fac$년도 == '2018','상추'] <- trend_vegi_fac_2018$상추
trend_vegi_fac[trend_vegi_fac$년도 == '2019','상추'] <- trend_vegi_fac_2019$상추
trend_vegi_fac[trend_vegi_fac$년도 == '2020','상추'] <- trend_vegi_fac_2020$상추
trend_vegi_fac[trend_vegi_fac$년도 == '2021','상추'] <- trend_vegi_fac_2021$상추

################################# 수박 ################################
for(i in count){
  # 2017, 2018 이상치 평균치로 변경 --- 2017 처리 안됨
  trend_vegi_fac_2017$수박 <- ifelse(trend_vegi_fac_2017$수박 < boxplot(trend_vegi_fac_2017$수박)$stats[1]
                                   | trend_vegi_fac_2017$수박 > boxplot(trend_vegi_fac_2017$수박)$stats[5]
                                   , mean(trend_vegi_fac_2017$수박), trend_vegi_fac_2017$수박)
  
  trend_vegi_fac_2018$수박 <- ifelse(trend_vegi_fac_2018$수박 < boxplot(trend_vegi_fac_2018$수박)$stats[1]
                                   | trend_vegi_fac_2018$수박 > boxplot(trend_vegi_fac_2018$수박)$stats[5]
                                   , mean(trend_vegi_fac_2018$수박), trend_vegi_fac_2018$수박)
}

# 처리한 데이터 원래 데이터에 삽입 및 이상치 처리 확인
trend_vegi_fac[trend_vegi_fac$년도 == '2017','수박'] <- trend_vegi_fac_2017$수박
trend_vegi_fac[trend_vegi_fac$년도 == '2018','수박'] <- trend_vegi_fac_2018$수박

################################# 참외 ################################
# ggplot 그리기 (이상값 찾기) 참외
# ggplot(data=trend_vegi_fac, aes(x=년도, y=참외)) + geom_boxplot( outlier.color = "red")

# 이상치 없음


################################# 딸기 ################################
# ggplot 그리기 (이상값 찾기) 딸기
for(i in count){
  # 2020 이상치 평균치로 변경
  trend_vegi_fac_2020$딸기 <- ifelse(trend_vegi_fac_2020$딸기 < boxplot(trend_vegi_fac_2020$딸기)$stats[1]
                                   | trend_vegi_fac_2020$딸기 > boxplot(trend_vegi_fac_2020$딸기)$stats[5]
                                   , mean(trend_vegi_fac_2020$딸기), trend_vegi_fac_2020$딸기)
}

# 처리한 데이터 원래 데이터에 삽입 및 이상치 처리 확인
trend_vegi_fac[trend_vegi_fac$년도 == '2020','딸기'] <- trend_vegi_fac_2020$딸기

################################# 오이 ################################
# 이상치 없음

################################# 호박 ################################
# ggplot 그리기 (이상값 찾기) 호박

for(i in count){
  # 모든 년도 이상치 평균치로 변경 ----- 2020년 처리 안됨
  trend_vegi_fac_2016$호박 <- ifelse(trend_vegi_fac_2016$호박 < boxplot(trend_vegi_fac_2016$호박)$stats[1]
                                   | trend_vegi_fac_2016$호박 > boxplot(trend_vegi_fac_2016$호박)$stats[5]
                                   , mean(trend_vegi_fac_2016$호박), trend_vegi_fac_2016$호박)
  
  trend_vegi_fac_2017$호박 <- ifelse(trend_vegi_fac_2017$호박 < boxplot(trend_vegi_fac_2017$호박)$stats[1]
                                   | trend_vegi_fac_2017$호박 > boxplot(trend_vegi_fac_2017$호박)$stats[5]
                                   , mean(trend_vegi_fac_2017$호박), trend_vegi_fac_2017$호박)
  
  trend_vegi_fac_2018$호박 <- ifelse(trend_vegi_fac_2018$호박 < boxplot(trend_vegi_fac_2018$호박)$stats[1]
                                   | trend_vegi_fac_2018$호박 > boxplot(trend_vegi_fac_2018$호박)$stats[5]
                                   , mean(trend_vegi_fac_2018$호박), trend_vegi_fac_2018$호박)
  
  trend_vegi_fac_2019$호박 <- ifelse(trend_vegi_fac_2019$호박 < boxplot(trend_vegi_fac_2019$호박)$stats[1]
                                   | trend_vegi_fac_2019$호박 > boxplot(trend_vegi_fac_2019$호박)$stats[5]
                                   , mean(trend_vegi_fac_2019$호박), trend_vegi_fac_2019$호박)
  
  trend_vegi_fac_2020$호박 <- ifelse(trend_vegi_fac_2020$호박 < boxplot(trend_vegi_fac_2020$호박)$stats[1]
                                   | trend_vegi_fac_2020$호박 > boxplot(trend_vegi_fac_2020$호박)$stats[5]
                                   , mean(trend_vegi_fac_2020$호박), trend_vegi_fac_2020$호박)
  
  trend_vegi_fac_2021$호박 <- ifelse(trend_vegi_fac_2021$호박 < boxplot(trend_vegi_fac_2021$호박)$stats[1]
                                   | trend_vegi_fac_2021$호박 > boxplot(trend_vegi_fac_2021$호박)$stats[5]
                                   , mean(trend_vegi_fac_2021$호박), trend_vegi_fac_2021$호박)
}

# 처리한 데이터 원래 데이터에 삽입 및 이상치 처리 확인
trend_vegi_fac[trend_vegi_fac$년도 == '2016','호박'] <- trend_vegi_fac_2016$호박
trend_vegi_fac[trend_vegi_fac$년도 == '2017','호박'] <- trend_vegi_fac_2017$호박
trend_vegi_fac[trend_vegi_fac$년도 == '2018','호박'] <- trend_vegi_fac_2018$호박
trend_vegi_fac[trend_vegi_fac$년도 == '2019','호박'] <- trend_vegi_fac_2019$호박
trend_vegi_fac[trend_vegi_fac$년도 == '2020','호박'] <- trend_vegi_fac_2020$호박
trend_vegi_fac[trend_vegi_fac$년도 == '2021','호박'] <- trend_vegi_fac_2021$호박

################################# 고추 ################################
# ggplot 그리기 (이상값 찾기) 고추
for(i in count){
  # 2017, 2018 ,2019 년도 이상치 평균치로 변경 ----  2017년 처리 안됨
  trend_vegi_fac_2017$고추 <- ifelse(trend_vegi_fac_2017$고추 < boxplot(trend_vegi_fac_2017$고추)$stats[1]
                                   | trend_vegi_fac_2017$고추 > boxplot(trend_vegi_fac_2017$고추)$stats[5]
                                   , mean(trend_vegi_fac_2017$고추), trend_vegi_fac_2017$고추)
  
  trend_vegi_fac_2018$고추 <- ifelse(trend_vegi_fac_2018$고추 < boxplot(trend_vegi_fac_2018$고추)$stats[1]
                                   | trend_vegi_fac_2018$고추 > boxplot(trend_vegi_fac_2018$고추)$stats[5]
                                   , mean(trend_vegi_fac_2018$고추), trend_vegi_fac_2018$고추)
  
  trend_vegi_fac_2019$고추 <- ifelse(trend_vegi_fac_2019$고추 < boxplot(trend_vegi_fac_2019$고추)$stats[1]
                                   | trend_vegi_fac_2019$고추 > boxplot(trend_vegi_fac_2019$고추)$stats[5]
                                   , mean(trend_vegi_fac_2019$고추), trend_vegi_fac_2019$고추)
}

# 처리한 데이터 원래 데이터에 삽입 및 이상치 처리 확인
trend_vegi_fac[trend_vegi_fac$년도 == '2017','고추'] <- trend_vegi_fac_2017$고추
trend_vegi_fac[trend_vegi_fac$년도 == '2018','고추'] <- trend_vegi_fac_2018$고추
trend_vegi_fac[trend_vegi_fac$년도 == '2019','고추'] <- trend_vegi_fac_2019$고추

tr_vegi_fac_cols <- colnames(trend_vegi_fac)
# 모든 boxplot 프린트
for(j in 1:length(tr_vegi_fac_cols)) {
  if (tr_vegi_fac_cols[j] == "날짜" || tr_vegi_fac_cols[j] == "년도"){
    next
  }
  print(ggplot(data=trend_vegi_fac, aes_string(x="년도", y=tr_vegi_fac_cols[j]))+
          geom_boxplot( outlier.color = "red"))
}

