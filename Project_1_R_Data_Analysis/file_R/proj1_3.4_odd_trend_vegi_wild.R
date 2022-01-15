# ggplot 라이브러리 소환
library(ggplot2)
trend_vegi_wild$년도 <- as.character(format(as.Date(trend_vegi_wild$날짜), format = "%Y"))
for (j in 2:(ncol(trend_vegi_wild))){trend_vegi_wild[,j] <- as.numeric(unlist(trend_vegi_wild[,j]))}

# 년도 문자화
trend_vegi_wild$년도 <- as.character(format(as.Date(trend_vegi_wild$날짜), format = "%Y"))
trend_vegi_wild_2016 <- subset(trend_vegi_wild, trend_vegi_wild$년도 == '2016')
trend_vegi_wild_2017 <- subset(trend_vegi_wild, trend_vegi_wild$년도 == '2017')
trend_vegi_wild_2018 <- subset(trend_vegi_wild, trend_vegi_wild$년도 == '2018')
trend_vegi_wild_2019 <- subset(trend_vegi_wild, trend_vegi_wild$년도 == '2019')
trend_vegi_wild_2020 <- subset(trend_vegi_wild, trend_vegi_wild$년도 == '2020')
trend_vegi_wild_2021 <- subset(trend_vegi_wild, trend_vegi_wild$년도 == '2021')


################################# 수박 ################################
# 평균 10회 반복시 이상치 처리
count <- 1:10

for(i in count){
  # 2017, 2018 년도 이상치 평균치로 변경 ---- 2017년 처리 안됨
  trend_vegi_wild_2017$수박 <- ifelse(trend_vegi_wild_2017$수박 < boxplot(trend_vegi_wild_2017$수박)$stats[1]
                                    | trend_vegi_wild_2017$수박 > boxplot(trend_vegi_wild_2017$수박)$stats[5]
                                    , mean(trend_vegi_wild_2017$수박), trend_vegi_wild_2017$수박)
  
  trend_vegi_wild_2018$수박 <- ifelse(trend_vegi_wild_2018$수박 < boxplot(trend_vegi_wild_2018$수박)$stats[1]
                                    | trend_vegi_wild_2018$수박 > boxplot(trend_vegi_wild_2018$수박)$stats[5]
                                    , mean(trend_vegi_wild_2018$수박), trend_vegi_wild_2018$수박)
}

# 처리한 데이터 원래 데이터에 삽입 및 이상치 처리 확인
trend_vegi_wild[trend_vegi_wild$년도 == '2017','수박'] <- trend_vegi_wild_2017$수박
trend_vegi_wild[trend_vegi_wild$년도 == '2018','수박'] <- trend_vegi_wild_2018$수박

################################# 무 ################################
for(i in count){
  # 2020 년도 제외 이상치 평균치로 변경 ---- 2019년 처리 안됨
  trend_vegi_wild_2016$무 <- ifelse(trend_vegi_wild_2016$무 < boxplot(trend_vegi_wild_2016$무)$stats[1]
                                   | trend_vegi_wild_2016$무 > boxplot(trend_vegi_wild_2016$무)$stats[5]
                                   , mean(trend_vegi_wild_2016$무), trend_vegi_wild_2016$무)
  
  trend_vegi_wild_2017$무 <- ifelse(trend_vegi_wild_2017$무 < boxplot(trend_vegi_wild_2017$무)$stats[1]
                                   | trend_vegi_wild_2017$무 > boxplot(trend_vegi_wild_2017$무)$stats[5]
                                   , mean(trend_vegi_wild_2017$무), trend_vegi_wild_2017$무)
  
  trend_vegi_wild_2018$무 <- ifelse(trend_vegi_wild_2018$무 < boxplot(trend_vegi_wild_2018$무)$stats[1]
                                   | trend_vegi_wild_2018$무 > boxplot(trend_vegi_wild_2018$무)$stats[5]
                                   , mean(trend_vegi_wild_2018$무), trend_vegi_wild_2018$무)
  
  trend_vegi_wild_2019$무 <- ifelse(trend_vegi_wild_2019$무 < boxplot(trend_vegi_wild_2019$무)$stats[1]
                                   | trend_vegi_wild_2019$무 > boxplot(trend_vegi_wild_2019$무)$stats[5]
                                   , mean(trend_vegi_wild_2019$무), trend_vegi_wild_2019$무)
  
  trend_vegi_wild_2021$무 <- ifelse(trend_vegi_wild_2021$무 < boxplot(trend_vegi_wild_2021$무)$stats[1]
                                   | trend_vegi_wild_2021$무 > boxplot(trend_vegi_wild_2021$무)$stats[5]
                                   , mean(trend_vegi_wild_2021$무), trend_vegi_wild_2021$무)
}

# 처리한 데이터 원래 데이터에 삽입 및 이상치 처리 확인
trend_vegi_wild[trend_vegi_wild$년도 == '2016','무'] <- trend_vegi_wild_2016$무
trend_vegi_wild[trend_vegi_wild$년도 == '2017','무'] <- trend_vegi_wild_2017$무
trend_vegi_wild[trend_vegi_wild$년도 == '2018','무'] <- trend_vegi_wild_2018$무
trend_vegi_wild[trend_vegi_wild$년도 == '2019','무'] <- trend_vegi_wild_2019$무
trend_vegi_wild[trend_vegi_wild$년도 == '2021','무'] <- trend_vegi_wild_2021$무

################################# 당근 ################################
# ggplot 그리기 (이상값 찾기) 당근
for(i in count){
  # 2016, 2021 년도 제외 이상치 평균치로 변경 ---- 2020년 처리 안됨 
  trend_vegi_wild_2017$당근 <- ifelse(trend_vegi_wild_2017$당근 < boxplot(trend_vegi_wild_2017$당근)$stats[1]
                                    | trend_vegi_wild_2017$당근 > boxplot(trend_vegi_wild_2017$당근)$stats[5]
                                    , mean(trend_vegi_wild_2017$당근), trend_vegi_wild_2017$당근)
  
  trend_vegi_wild_2018$당근 <- ifelse(trend_vegi_wild_2018$당근 < boxplot(trend_vegi_wild_2018$당근)$stats[1]
                                    | trend_vegi_wild_2018$당근 > boxplot(trend_vegi_wild_2018$당근)$stats[5]
                                    , mean(trend_vegi_wild_2018$당근), trend_vegi_wild_2018$당근)
  
  trend_vegi_wild_2019$당근 <- ifelse(trend_vegi_wild_2019$당근 < boxplot(trend_vegi_wild_2019$당근)$stats[1]
                                    | trend_vegi_wild_2019$당근 > boxplot(trend_vegi_wild_2019$당근)$stats[5]
                                    , mean(trend_vegi_wild_2019$당근), trend_vegi_wild_2019$당근)
  
  trend_vegi_wild_2020$당근 <- ifelse(trend_vegi_wild_2020$당근 < boxplot(trend_vegi_wild_2020$당근)$stats[1]
                                    | trend_vegi_wild_2020$당근 > boxplot(trend_vegi_wild_2020$당근)$stats[5]
                                    , mean(trend_vegi_wild_2020$당근), trend_vegi_wild_2020$당근)
}

# 처리한 데이터 원래 데이터에 삽입 및 이상치 처리 확인
trend_vegi_wild[trend_vegi_wild$년도 == '2017','당근'] <- trend_vegi_wild_2017$당근
trend_vegi_wild[trend_vegi_wild$년도 == '2018','당근'] <- trend_vegi_wild_2018$당근
trend_vegi_wild[trend_vegi_wild$년도 == '2019','당근'] <- trend_vegi_wild_2019$당근
trend_vegi_wild[trend_vegi_wild$년도 == '2020','당근'] <- trend_vegi_wild_2020$당근

################################# 배추 ################################
for(i in count){
  # 2016 년도 제외 이상치 평균치로 변경 ----- 2017, 2020년 처리 안됨 
  trend_vegi_wild_2017$배추 <- ifelse(trend_vegi_wild_2017$배추 < boxplot(trend_vegi_wild_2017$배추)$stats[1]
                                    | trend_vegi_wild_2017$배추 > boxplot(trend_vegi_wild_2017$배추)$stats[5]
                                    , mean(trend_vegi_wild_2017$배추), trend_vegi_wild_2017$배추)
  
  trend_vegi_wild_2018$배추 <- ifelse(trend_vegi_wild_2018$배추 < boxplot(trend_vegi_wild_2018$배추)$stats[1]
                                    | trend_vegi_wild_2018$배추 > boxplot(trend_vegi_wild_2018$배추)$stats[5]
                                    , mean(trend_vegi_wild_2018$배추), trend_vegi_wild_2018$배추)
  
  trend_vegi_wild_2019$배추 <- ifelse(trend_vegi_wild_2019$배추 < boxplot(trend_vegi_wild_2019$배추)$stats[1]
                                    | trend_vegi_wild_2019$배추 > boxplot(trend_vegi_wild_2019$배추)$stats[5]
                                    , mean(trend_vegi_wild_2019$배추), trend_vegi_wild_2019$배추)
  
  trend_vegi_wild_2020$배추 <- ifelse(trend_vegi_wild_2020$배추 < boxplot(trend_vegi_wild_2020$배추)$stats[1]
                                    | trend_vegi_wild_2020$배추 > boxplot(trend_vegi_wild_2020$배추)$stats[5]
                                    , mean(trend_vegi_wild_2020$배추), trend_vegi_wild_2020$배추)
  
  trend_vegi_wild_2021$배추 <- ifelse(trend_vegi_wild_2021$배추 < boxplot(trend_vegi_wild_2021$배추)$stats[1]
                                    | trend_vegi_wild_2021$배추 > boxplot(trend_vegi_wild_2021$배추)$stats[5]
                                    , mean(trend_vegi_wild_2021$배추), trend_vegi_wild_2021$배추)
}

# 처리한 데이터 원래 데이터에 삽입 및 이상치 처리 확인
trend_vegi_wild[trend_vegi_wild$년도 == '2017','배추'] <- trend_vegi_wild_2017$배추
trend_vegi_wild[trend_vegi_wild$년도 == '2018','배추'] <- trend_vegi_wild_2018$배추
trend_vegi_wild[trend_vegi_wild$년도 == '2019','배추'] <- trend_vegi_wild_2019$배추
trend_vegi_wild[trend_vegi_wild$년도 == '2020','배추'] <- trend_vegi_wild_2020$배추
trend_vegi_wild[trend_vegi_wild$년도 == '2021','배추'] <- trend_vegi_wild_2021$배추

################################# 시금치 ################################
for(i in count){
  # 2019, 2020 년도 이상치 평균치로 변경
  trend_vegi_wild_2019$시금치 <- ifelse(trend_vegi_wild_2019$시금치 < boxplot(trend_vegi_wild_2019$시금치)$stats[1]
                                     | trend_vegi_wild_2019$시금치 > boxplot(trend_vegi_wild_2019$시금치)$stats[5]
                                     , mean(trend_vegi_wild_2019$시금치), trend_vegi_wild_2019$시금치)
  
  trend_vegi_wild_2020$시금치 <- ifelse(trend_vegi_wild_2020$시금치 < boxplot(trend_vegi_wild_2020$시금치)$stats[1]
                                     | trend_vegi_wild_2020$시금치 > boxplot(trend_vegi_wild_2020$시금치)$stats[5]
                                     , mean(trend_vegi_wild_2020$시금치), trend_vegi_wild_2020$시금치)
}

# 처리한 데이터 원래 데이터에 삽입 및 이상치 처리 확인
trend_vegi_wild[trend_vegi_wild$년도 == '2019','시금치'] <- trend_vegi_wild_2019$시금치
trend_vegi_wild[trend_vegi_wild$년도 == '2020','시금치'] <- trend_vegi_wild_2020$시금치

################################# 양배추 ################################
for(i in count){
  # 2020 년도 제외 이상치 평균치로 변경
  trend_vegi_wild_2016$양배추 <- ifelse(trend_vegi_wild_2016$양배추 < boxplot(trend_vegi_wild_2016$양배추)$stats[1]
                                     | trend_vegi_wild_2016$양배추 > boxplot(trend_vegi_wild_2016$양배추)$stats[5]
                                     , mean(trend_vegi_wild_2016$양배추), trend_vegi_wild_2016$양배추)
  
  trend_vegi_wild_2017$양배추 <- ifelse(trend_vegi_wild_2017$양배추 < boxplot(trend_vegi_wild_2017$양배추)$stats[1]
                                     | trend_vegi_wild_2017$양배추 > boxplot(trend_vegi_wild_2017$양배추)$stats[5]
                                     , mean(trend_vegi_wild_2017$양배추), trend_vegi_wild_2017$양배추)
  
  trend_vegi_wild_2018$양배추 <- ifelse(trend_vegi_wild_2018$양배추 < boxplot(trend_vegi_wild_2018$양배추)$stats[1]
                                     | trend_vegi_wild_2018$양배추 > boxplot(trend_vegi_wild_2018$양배추)$stats[5]
                                     , mean(trend_vegi_wild_2018$양배추), trend_vegi_wild_2018$양배추)
  
  trend_vegi_wild_2019$양배추 <- ifelse(trend_vegi_wild_2019$양배추 < boxplot(trend_vegi_wild_2019$양배추)$stats[1]
                                     | trend_vegi_wild_2019$양배추 > boxplot(trend_vegi_wild_2019$양배추)$stats[5]
                                     , mean(trend_vegi_wild_2019$양배추), trend_vegi_wild_2019$양배추)
  
  trend_vegi_wild_2021$양배추 <- ifelse(trend_vegi_wild_2021$양배추 < boxplot(trend_vegi_wild_2021$양배추)$stats[1]
                                     | trend_vegi_wild_2021$양배추 > boxplot(trend_vegi_wild_2021$양배추)$stats[5]
                                     , mean(trend_vegi_wild_2021$양배추), trend_vegi_wild_2021$양배추)
}

# 처리한 데이터 원래 데이터에 삽입 및 이상치 처리 확인
trend_vegi_wild[trend_vegi_wild$년도 == '2016','양배추'] <- trend_vegi_wild_2016$양배추
trend_vegi_wild[trend_vegi_wild$년도 == '2017','양배추'] <- trend_vegi_wild_2017$양배추
trend_vegi_wild[trend_vegi_wild$년도 == '2018','양배추'] <- trend_vegi_wild_2018$양배추
trend_vegi_wild[trend_vegi_wild$년도 == '2019','양배추'] <- trend_vegi_wild_2019$양배추
trend_vegi_wild[trend_vegi_wild$년도 == '2021','양배추'] <- trend_vegi_wild_2021$양배추

################################# 부추 2019년도 확인 ################################

for(i in count){
  # 2019 년도 이상치 평균치로 변경 ---- 2019년 처리 안됨
  trend_vegi_wild_2019$부추 <- ifelse(trend_vegi_wild_2019$부추 < boxplot(trend_vegi_wild_2019$부추)$stats[1]
                                    | trend_vegi_wild_2019$부추 > boxplot(trend_vegi_wild_2019$부추)$stats[5]
                                    , mean(trend_vegi_wild_2019$부추), trend_vegi_wild_2019$부추)
}

# 처리한 데이터 원래 데이터에 삽입 및 이상치 처리 확인
trend_vegi_wild[trend_vegi_wild$년도 == '2019','부추'] <- trend_vegi_wild_2019$부추

################################# 대파 ################################
for(i in count){
  # 2018, 2020, 2021 년도 이상치 평균치로 변경
  trend_vegi_wild_2018$대파 <- ifelse(trend_vegi_wild_2018$대파 < boxplot(trend_vegi_wild_2018$대파)$stats[1]
                                    | trend_vegi_wild_2018$대파 > boxplot(trend_vegi_wild_2018$대파)$stats[5]
                                    , mean(trend_vegi_wild_2018$대파), trend_vegi_wild_2018$대파)
  
  trend_vegi_wild_2020$대파 <- ifelse(trend_vegi_wild_2020$대파 < boxplot(trend_vegi_wild_2020$대파)$stats[1]
                                    | trend_vegi_wild_2020$대파 > boxplot(trend_vegi_wild_2020$대파)$stats[5]
                                    , mean(trend_vegi_wild_2020$대파), trend_vegi_wild_2020$대파)
  
  trend_vegi_wild_2021$대파 <- ifelse(trend_vegi_wild_2021$대파 < boxplot(trend_vegi_wild_2021$대파)$stats[1]
                                    | trend_vegi_wild_2021$대파 > boxplot(trend_vegi_wild_2021$대파)$stats[5]
                                    , mean(trend_vegi_wild_2021$대파), trend_vegi_wild_2021$대파)
}

# 처리한 데이터 원래 데이터에 삽입 및 이상치 처리 확인
trend_vegi_wild[trend_vegi_wild$년도 == '2018','대파'] <- trend_vegi_wild_2018$대파
trend_vegi_wild[trend_vegi_wild$년도 == '2020','대파'] <- trend_vegi_wild_2020$대파
trend_vegi_wild[trend_vegi_wild$년도 == '2021','대파'] <- trend_vegi_wild_2021$대파

################################# 쪽파 ################################
for(i in count){
  # 2016, 2018, 2020 년도 이상치 평균치로 변경
  trend_vegi_wild_2016$쪽파 <- ifelse(trend_vegi_wild_2016$쪽파 < boxplot(trend_vegi_wild_2016$쪽파)$stats[1]
                                    | trend_vegi_wild_2016$쪽파 > boxplot(trend_vegi_wild_2016$쪽파)$stats[5]
                                    , mean(trend_vegi_wild_2016$쪽파), trend_vegi_wild_2016$쪽파)
  
  trend_vegi_wild_2018$쪽파 <- ifelse(trend_vegi_wild_2018$쪽파 < boxplot(trend_vegi_wild_2018$쪽파)$stats[1]
                                    | trend_vegi_wild_2018$쪽파 > boxplot(trend_vegi_wild_2018$쪽파)$stats[5]
                                    , mean(trend_vegi_wild_2018$쪽파), trend_vegi_wild_2018$쪽파)
  
  trend_vegi_wild_2020$쪽파 <- ifelse(trend_vegi_wild_2020$쪽파 < boxplot(trend_vegi_wild_2020$쪽파)$stats[1]
                                    | trend_vegi_wild_2020$쪽파 > boxplot(trend_vegi_wild_2020$쪽파)$stats[5]
                                    , mean(trend_vegi_wild_2020$쪽파), trend_vegi_wild_2020$쪽파)
}

# 처리한 데이터 원래 데이터에 삽입 및 이상치 처리 확인
trend_vegi_wild[trend_vegi_wild$년도 == '2016','쪽파'] <- trend_vegi_wild_2016$쪽파
trend_vegi_wild[trend_vegi_wild$년도 == '2018','쪽파'] <- trend_vegi_wild_2018$쪽파
trend_vegi_wild[trend_vegi_wild$년도 == '2020','쪽파'] <- trend_vegi_wild_2020$쪽파


################################# 생강 ################################
for(i in count){
  # 모든년도 이상치 평균치로 변경
  trend_vegi_wild_2016$생강 <- ifelse(trend_vegi_wild_2016$생강 < boxplot(trend_vegi_wild_2016$생강)$stats[1]
                                    | trend_vegi_wild_2016$생강 > boxplot(trend_vegi_wild_2016$생강)$stats[5]
                                    , mean(trend_vegi_wild_2016$생강), trend_vegi_wild_2016$생강)
  
  trend_vegi_wild_2017$생강 <- ifelse(trend_vegi_wild_2017$생강 < boxplot(trend_vegi_wild_2017$생강)$stats[1]
                                    | trend_vegi_wild_2017$생강 > boxplot(trend_vegi_wild_2017$생강)$stats[5]
                                    , mean(trend_vegi_wild_2017$생강), trend_vegi_wild_2017$생강)
  
  trend_vegi_wild_2018$생강 <- ifelse(trend_vegi_wild_2018$생강 < boxplot(trend_vegi_wild_2018$생강)$stats[1]
                                    | trend_vegi_wild_2018$생강 > boxplot(trend_vegi_wild_2018$생강)$stats[5]
                                    , mean(trend_vegi_wild_2018$생강), trend_vegi_wild_2018$생강)
  
  trend_vegi_wild_2019$생강 <- ifelse(trend_vegi_wild_2019$생강 < boxplot(trend_vegi_wild_2019$생강)$stats[1]
                                    | trend_vegi_wild_2019$생강 > boxplot(trend_vegi_wild_2019$생강)$stats[5]
                                    , mean(trend_vegi_wild_2019$생강), trend_vegi_wild_2019$생강)
  
  trend_vegi_wild_2020$생강 <- ifelse(trend_vegi_wild_2020$생강 < boxplot(trend_vegi_wild_2020$생강)$stats[1]
                                    | trend_vegi_wild_2020$생강 > boxplot(trend_vegi_wild_2020$생강)$stats[5]
                                    , mean(trend_vegi_wild_2020$생강), trend_vegi_wild_2020$생강)
  
  trend_vegi_wild_2021$생강 <- ifelse(trend_vegi_wild_2021$생강 < boxplot(trend_vegi_wild_2021$생강)$stats[1]
                                    | trend_vegi_wild_2021$생강 > boxplot(trend_vegi_wild_2021$생강)$stats[5]
                                    , mean(trend_vegi_wild_2021$생강), trend_vegi_wild_2021$생강)
}

# 처리한 데이터 원래 데이터에 삽입 및 이상치 처리 확인
trend_vegi_wild[trend_vegi_wild$년도 == '2016','생강'] <- trend_vegi_wild_2016$생강
trend_vegi_wild[trend_vegi_wild$년도 == '2017','생강'] <- trend_vegi_wild_2017$생강
trend_vegi_wild[trend_vegi_wild$년도 == '2018','생강'] <- trend_vegi_wild_2018$생강
trend_vegi_wild[trend_vegi_wild$년도 == '2019','생강'] <- trend_vegi_wild_2019$생강
trend_vegi_wild[trend_vegi_wild$년도 == '2020','생강'] <- trend_vegi_wild_2020$생강
trend_vegi_wild[trend_vegi_wild$년도 == '2021','생강'] <- trend_vegi_wild_2021$생강

tr_vegi_wild_cols <- colnames(trend_vegi_wild)
# 모든 boxplot 프린트
for(j in 1:length(tr_vegi_wild_cols)) {
  if (tr_vegi_wild_cols[j] == "날짜" || tr_vegi_wild_cols[j] == "년도"){
    next
  }
  print(ggplot(data=trend_vegi_wild, aes_string(x="년도", y=tr_vegi_wild_cols[j]))+
          geom_boxplot( outlier.color = "red"))
}

