# ggplot 라이브러리 소환
library(ggplot2)
trend_crop$년도 <- as.character(format(as.Date(trend_crop$날짜), format = "%Y"))
for (j in 2:(ncol(trend_crop))){trend_crop[,j] <- as.numeric(unlist(trend_crop[,j]))}

# 년도 문자화 및 추출
trend_crop$년도 <- as.character(format(as.Date(trend_crop$날짜), format = "%Y"))
trend_crop_2016 <- subset(trend_crop, trend_crop$년도 == '2016')
trend_crop_2017 <- subset(trend_crop, trend_crop$년도 == '2017')
trend_crop_2018 <- subset(trend_crop, trend_crop$년도 == '2018')
trend_crop_2019 <- subset(trend_crop, trend_crop$년도 == '2019')
trend_crop_2020 <- subset(trend_crop, trend_crop$년도 == '2020')
trend_crop_2021 <- subset(trend_crop, trend_crop$년도 == '2021')

############################################################################################보리
# 평균 10회 반복시 이상치 처리
count <- 1:10

for(i in count){
  # 2016년도 이상치 처리
  trend_crop_2016$보리 <- ifelse(trend_crop_2016$보리 < boxplot(trend_crop_2016$보리)$stats[1]
                               | trend_crop_2016$보리 > boxplot(trend_crop_2016$보리)$stats[5]
                               , mean(trend_crop_2016$보리), trend_crop_2016$보리)
  
  # 2018년도 이상치 처리
  trend_crop_2018$보리 <- ifelse(trend_crop_2018$보리 < boxplot(trend_crop_2018$보리)$stats[1]
                               | trend_crop_2018$보리 > boxplot(trend_crop_2018$보리)$stats[5]
                               , mean(trend_crop_2018$보리), trend_crop_2018$보리)
  
  # 2019년도 이상치 처리
  trend_crop_2019$보리 <- ifelse(trend_crop_2019$보리 < boxplot(trend_crop_2019$보리)$stats[1]
                               | trend_crop_2019$보리 > boxplot(trend_crop_2019$보리)$stats[5]
                               , mean(trend_crop_2019$보리), trend_crop_2019$보리)
  # 2020년도 이상치 처리
  trend_crop_2020$보리 <- ifelse(trend_crop_2020$보리 < boxplot(trend_crop_2020$보리)$stats[1]
                               | trend_crop_2020$보리 > boxplot(trend_crop_2020$보리)$stats[5]
                               , mean(trend_crop_2020$보리), trend_crop_2020$보리)
}
# 처리한 데이터 원래 데이터에 삽입 및 이상치 처리 확인
trend_crop[trend_crop$년도 == '2016','보리'] <- trend_crop_2016$보리
trend_crop[trend_crop$년도 == '2018','보리'] <- trend_crop_2018$보리
trend_crop[trend_crop$년도 == '2019','보리'] <- trend_crop_2019$보리
trend_crop[trend_crop$년도 == '2020','보리'] <- trend_crop_2020$보리

############################################################################################옥수수
for(i in count){
  # 2017년도 이상치 처리--------------------------------------------------------이상치 처리 안됨
  
  trend_crop_2017$옥수수 <- ifelse(trend_crop_2017$옥수수 < boxplot(trend_crop_2017$옥수수)$stats[1]
                                | trend_crop_2017$옥수수 > boxplot(trend_crop_2017$옥수수)$stats[5]
                                , mean(trend_crop_2017$옥수수), trend_crop_2017$옥수수)
  
  # 2018년도 이상치 처리
  trend_crop_2018$옥수수 <- ifelse(trend_crop_2018$옥수수 < boxplot(trend_crop_2018$옥수수)$stats[1]
                                | trend_crop_2018$옥수수 > boxplot(trend_crop_2018$옥수수)$stats[5]
                                , mean(trend_crop_2018$옥수수), trend_crop_2018$옥수수)
  
  # 2019년도 이상치 처리
  trend_crop_2019$옥수수 <- ifelse(trend_crop_2019$옥수수 < boxplot(trend_crop_2019$옥수수)$stats[1]
                                | trend_crop_2019$옥수수 > boxplot(trend_crop_2019$옥수수)$stats[5]
                                , mean(trend_crop_2019$옥수수), trend_crop_2019$옥수수)
  
  # 2020년도 이상치 처리
  trend_crop_2020$옥수수 <- ifelse(trend_crop_2020$옥수수 < boxplot(trend_crop_2020$옥수수)$stats[1]
                                | trend_crop_2020$옥수수 > boxplot(trend_crop_2020$옥수수)$stats[5]
                                , mean(trend_crop_2020$옥수수), trend_crop_2020$옥수수)
  
  # 2021년도 이상치 처리
  trend_crop_2021$옥수수 <- ifelse(trend_crop_2021$옥수수 < boxplot(trend_crop_2021$옥수수)$stats[1]
                                | trend_crop_2021$옥수수 > boxplot(trend_crop_2021$옥수수)$stats[5]
                                , mean(trend_crop_2021$옥수수), trend_crop_2021$옥수수)
}

# 처리한 2017, 2018, 2019, 2020, 2021년 데이터 원래 데이터에 삽입 및 이상치 처리 확인
trend_crop[trend_crop$년도 == '2017','옥수수'] <- trend_crop_2017$옥수수
trend_crop[trend_crop$년도 == '2018','옥수수'] <- trend_crop_2018$옥수수
trend_crop[trend_crop$년도 == '2019','옥수수'] <- trend_crop_2019$옥수수
trend_crop[trend_crop$년도 == '2020','옥수수'] <- trend_crop_2020$옥수수
trend_crop[trend_crop$년도 == '2021','옥수수'] <- trend_crop_2021$옥수수

############################################################################################고구마
for(i in count){
  # 2018년도 이상치 처리
  trend_crop_2018$고구마 <- ifelse(trend_crop_2018$고구마 < boxplot(trend_crop_2018$고구마)$stats[1]
                                | trend_crop_2018$고구마 > boxplot(trend_crop_2018$고구마)$stats[5]
                                , mean(trend_crop_2018$고구마), trend_crop_2018$고구마)
  
  # 2021년도 이상치 처리
  trend_crop_2021$고구마 <- ifelse(trend_crop_2021$고구마 < boxplot(trend_crop_2021$고구마)$stats[1]
                                | trend_crop_2021$고구마 > boxplot(trend_crop_2021$고구마)$stats[5]
                                , mean(trend_crop_2021$고구마), trend_crop_2021$고구마)
}

# 처리한 2017, 2018, 2019, 2020, 2021년 데이터 원래 데이터에 삽입 및 이상치 처리 확인 후 재그림
trend_crop[trend_crop$년도 == '2018','고구마'] <- trend_crop_2018$고구마
trend_crop[trend_crop$년도 == '2021','고구마'] <- trend_crop_2021$고구마


############################################################################################감자
for(i in count){
  # 2016년도 이상치 처리
  trend_crop_2016$감자 <- ifelse(trend_crop_2016$감자 < boxplot(trend_crop_2016$감자)$stats[1]
                               | trend_crop_2016$감자 > boxplot(trend_crop_2016$감자)$stats[5]
                               , mean(trend_crop_2016$감자), trend_crop_2016$감자)
  
  # 2017년도 이상치 처리
  trend_crop_2017$감자 <- ifelse(trend_crop_2017$감자 < boxplot(trend_crop_2017$감자)$stats[1]
                               | trend_crop_2017$감자 > boxplot(trend_crop_2017$감자)$stats[5]
                               , mean(trend_crop_2017$감자), trend_crop_2017$감자)
  
  # 2019년도 이상치 처리----------------------------------------------------------이상치 처리 안됨
  trend_crop_2019$감자 <- ifelse(trend_crop_2019$감자 < boxplot(trend_crop_2019$감자)$stats[1]
                               | trend_crop_2019$감자 > boxplot(trend_crop_2019$감자)$stats[5]
                               , mean(trend_crop_2019$감자), trend_crop_2019$감자)
  
  # 2020년도 이상치 처리
  trend_crop_2020$감자 <- ifelse(trend_crop_2020$감자 < boxplot(trend_crop_2020$감자)$stats[1]
                               | trend_crop_2020$감자 > boxplot(trend_crop_2020$감자)$stats[5]
                               , mean(trend_crop_2020$감자), trend_crop_2020$감자)
  
  # 2021년도 이상치 처리
  trend_crop_2021$감자 <- ifelse(trend_crop_2021$감자 < boxplot(trend_crop_2021$감자)$stats[1]
                               | trend_crop_2021$감자 > boxplot(trend_crop_2021$감자)$stats[5]
                               , mean(trend_crop_2021$감자), trend_crop_2021$감자)
}

# 처리한 2017, 2018, 2019, 2020, 2021년 데이터 원래 데이터에 삽입 및 이상치 처리 확인 및 재그림
trend_crop[trend_crop$년도 == '2016','감자'] <- trend_crop_2016$감자
trend_crop[trend_crop$년도 == '2017','감자'] <- trend_crop_2017$감자
trend_crop[trend_crop$년도 == '2019','감자'] <- trend_crop_2019$감자
trend_crop[trend_crop$년도 == '2020','감자'] <- trend_crop_2020$감자
trend_crop[trend_crop$년도 == '2021','감자'] <- trend_crop_2021$감자

############################################################################################참깨
for(i in count){
  # 2016년도 이상치 처리
  trend_crop_2016$참깨 <- ifelse(trend_crop_2016$참깨 < boxplot(trend_crop_2016$참깨)$stats[1]
                               | trend_crop_2016$참깨 > boxplot(trend_crop_2016$참깨)$stats[5]
                               , mean(trend_crop_2016$참깨), trend_crop_2016$참깨)
  # 2018년도 이상치 처리
  trend_crop_2018$참깨 <- ifelse(trend_crop_2018$참깨 < boxplot(trend_crop_2018$참깨)$stats[1]
                               | trend_crop_2018$참깨 > boxplot(trend_crop_2018$참깨)$stats[5]
                               , mean(trend_crop_2018$참깨), trend_crop_2018$참깨)
  
  # 2019년도 이상치 처리
  trend_crop_2019$참깨 <- ifelse(trend_crop_2019$참깨 < boxplot(trend_crop_2019$참깨)$stats[1]
                               | trend_crop_2019$참깨 > boxplot(trend_crop_2019$참깨)$stats[5]
                               , mean(trend_crop_2019$참깨), trend_crop_2019$참깨)
  
  # 2020년도 이상치 처리
  trend_crop_2020$참깨 <- ifelse(trend_crop_2020$참깨 < boxplot(trend_crop_2020$참깨)$stats[1]
                               | trend_crop_2020$참깨 > boxplot(trend_crop_2020$참깨)$stats[5]
                               , mean(trend_crop_2020$참깨), trend_crop_2020$참깨)
  
  # 2021년도 이상치 처리
  trend_crop_2021$참깨 <- ifelse(trend_crop_2021$참깨 < boxplot(trend_crop_2021$참깨)$stats[1]
                               | trend_crop_2021$참깨 > boxplot(trend_crop_2021$참깨)$stats[5]
                               , mean(trend_crop_2021$참깨), trend_crop_2021$참깨)
}

# 처리한 2019, 2021년 데이터 원래 데이터에 삽입 및 이상치 처리 확인 및 재그림
trend_crop[trend_crop$년도 == '2016','참깨'] <- trend_crop_2016$참깨
trend_crop[trend_crop$년도 == '2018','참깨'] <- trend_crop_2018$참깨
trend_crop[trend_crop$년도 == '2019','참깨'] <- trend_crop_2019$참깨
trend_crop[trend_crop$년도 == '2020','참깨'] <- trend_crop_2020$참깨
trend_crop[trend_crop$년도 == '2021','참깨'] <- trend_crop_2021$참깨
############################################################################################인삼
for(i in count){
  # 2016년도 이상치 처리
  trend_crop_2016$인삼 <- ifelse(trend_crop_2016$인삼 < boxplot(trend_crop_2016$인삼)$stats[1]
                               | trend_crop_2016$인삼 > boxplot(trend_crop_2016$인삼)$stats[5]
                               , mean(trend_crop_2016$인삼), trend_crop_2016$인삼)
  # 2017년도 이상치 처리
  trend_crop_2017$인삼 <- ifelse(trend_crop_2017$인삼 < boxplot(trend_crop_2017$인삼)$stats[1]
                               | trend_crop_2017$인삼 > boxplot(trend_crop_2017$인삼)$stats[5]
                               , mean(trend_crop_2017$인삼), trend_crop_2017$인삼)
  
  # 2018년도 이상치 처리
  trend_crop_2018$인삼 <- ifelse(trend_crop_2018$인삼 < boxplot(trend_crop_2018$인삼)$stats[1]
                               | trend_crop_2018$인삼 > boxplot(trend_crop_2018$인삼)$stats[5]
                               , mean(trend_crop_2018$인삼), trend_crop_2018$인삼)
  
  # 2019년도 이상치 처리
  trend_crop_2019$인삼 <- ifelse(trend_crop_2019$인삼 < boxplot(trend_crop_2019$인삼)$stats[1]
                               | trend_crop_2019$인삼 > boxplot(trend_crop_2019$인삼)$stats[5]
                               , mean(trend_crop_2019$인삼), trend_crop_2019$인삼)
  
  # 2020년도 이상치 처리 ---------------------------------------------------------------------처리 안됨
  trend_crop_2020$인삼 <- ifelse(trend_crop_2020$인삼 < boxplot(trend_crop_2020$인삼)$stats[1]
                               | trend_crop_2020$인삼 > boxplot(trend_crop_2020$인삼)$stats[5]
                               , mean(trend_crop_2020$인삼), trend_crop_2020$인삼)
  
  # 2021년도 이상치 처리
  trend_crop_2021$인삼 <- ifelse(trend_crop_2021$인삼 < boxplot(trend_crop_2021$인삼)$stats[1]
                               | trend_crop_2021$인삼 > boxplot(trend_crop_2021$인삼)$stats[5]
                               , mean(trend_crop_2021$인삼), trend_crop_2021$인삼)
}

# 처리한 2019, 2021년 데이터 원래 데이터에 삽입 및 이상치 처리 확인 및 재그림
trend_crop[trend_crop$년도 == '2016','인삼'] <- trend_crop_2016$인삼
trend_crop[trend_crop$년도 == '2017','인삼'] <- trend_crop_2017$인삼
trend_crop[trend_crop$년도 == '2018','인삼'] <- trend_crop_2018$인삼
trend_crop[trend_crop$년도 == '2019','인삼'] <- trend_crop_2019$인삼
trend_crop[trend_crop$년도 == '2020','인삼'] <- trend_crop_2020$인삼
trend_crop[trend_crop$년도 == '2021','인삼'] <- trend_crop_2021$인삼

tr_crop_cols <- colnames(trend_crop)
# 모든 boxplot 프린트
for(j in 1:length(tr_crop_cols)) {
  if (tr_crop_cols[j] == "날짜" || tr_crop_cols[j] == "년도"){
    next
  }
  print(ggplot(data=trend_crop, aes_string(x="년도", y=tr_crop_cols[j]))+
          geom_boxplot( outlier.color = "red"))
}

