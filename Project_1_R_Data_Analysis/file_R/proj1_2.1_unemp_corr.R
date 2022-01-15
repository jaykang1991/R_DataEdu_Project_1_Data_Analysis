# 실업률 및 귀농인구 상관관계 분석

# 테이블 확인
# View(unemp_rate)
# View(area_farm_move)

# 컬럼 이름 변경
colnames(unemp_rate) <- c("년도", "구분", "계", "15_19", "20_29", "30_39", "40_49", "50_59", "60_up")

# 실업률과 무관한 15-19세 열 계에서 차감
unemp_rate$계 <- (unemp_rate$계 - unemp_rate$`15_19`)
unemp_rate <- unemp_rate[,c("년도", "계")]

# 실업자 비교 위한 실업률 행 삭제
Nth.delete<-function(dataframe, n)dataframe[-(seq(n,to=nrow(dataframe),by=n)),]
unemp_number <- Nth.delete(unemp_rate, 2)

# 실업자 년도별 평균내기
new_unemp_number <- unemp_number[-1,] # 2013년 평균 필요 없어 별도, 계만 남기기
new_unemp_number$계 <- as.numeric(new_unemp_number$계) #숫자로 변경
new_unemp_number_mean <- do.call(rbind,
        lapply(seq(1, nrow(new_unemp_number), 2), function(i){
          x <- new_unemp_number[ i:(i + 1),2 , drop = FALSE]
          res <- rbind(x, colSums(x)/2)
          rownames(res)[ nrow(res) ] <- paste(rownames(x), collapse = "_")
          res
        }))

# 평균행인 3번째 행만 추출
Nth.keep<-function(dataframe, n)dataframe[(seq(n,to=nrow(dataframe),by=n)),]
new_unemp_number_mean <- Nth.keep(new_unemp_number_mean,3)

# 연도별 / 평균 실업자수 / 귀농인구 수 넣을 데이터프레임 생성
final_unemp_number <- data.frame(matrix(ncol = 3, nrow = 8))
colnames(final_unemp_number) <- c("년도", "실업자 수", "귀농인구 수")

# 연도별 / 평균 실업자 삽입
final_unemp_number$년도 <- c(2013:2020)
final_unemp_number$`실업자 수` <- (unemp_number[1,"계"]/1000)
final_unemp_number[2:8, "실업자 수"] <- (new_unemp_number_mean[1:7]/1000)
# View(final_unemp_number)

# 귀농인구 년도/인구 수 추출 및 삽입
area_farm_move <- subset(area_farm_move, area_farm_move$항목.1 == "계")
colnames(area_farm_move) <- c("년도", "항목", "귀농가구 수", "귀농인구 수")
final_unemp_number$`귀농인구 수` <- (area_farm_move$`귀농인구 수`/1000)
# View(final_unemp_number)

# 상관관계 분석
# install.packages("ggpubr")
library(ggpubr)

ggscatter(final_unemp_number, x = "실업자 수", y = "귀농인구 수", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", title = "실업자-귀농인구 상관관계 (2013-2020)", 
          xlab = "실업자 수 (천명)", ylab = "귀농인구 수 (천명)")

# 실업자 증가할 수록 귀농인구 증가 추세


# 농가인구 및 실업자 비교 
# 농가인구 데이터 수집
farm_pop <- read_xlsx("FARM_POP.xlsx")
farm_pop$`농가인구 (명)` <- (farm_pop$`농가인구 (명)`/1000)
# View(farm_pop)

# 실업자수 데이터 재수집(농가인구 데이터 년도와 맞는 자료 수집)
unemp_number_new <- read_xlsx("UNEMP_NUMBER.xlsx")
unemp_number_new

final_unemp_number_new <- data.frame(matrix(nrow = 10, ncol = 3))
colnames(final_unemp_number_new) <- c("년도", "농가인구 수", "실업자 수")
final_unemp_number_new$년도 <- farm_pop$년도
final_unemp_number_new$`농가인구 수` <- farm_pop$`농가인구 (명)`
final_unemp_number_new$`실업자 수` <- unemp_number_new$계[1:10]
# View(final_unemp_number_new)

# 상관관계 분석 (http://www.sthda.com/english/wiki/correlation-test-between-two-variables-in-r)
ggscatter(final_unemp_number_new, x = "농가인구 수", y = "실업자 수", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",title = "실업자-농가인구 상관관계 (2010-2019)", 
          xlab = "농가인구 수 (천명)", ylab = "실업자 수 (천명)")

# 농가인구 수가 증가할 수록 실업자 수 감소, 따라서 귀농 진입장벽 해소를 통한 귀농인구 증가가 된다면 실업자 수 하락 예상

