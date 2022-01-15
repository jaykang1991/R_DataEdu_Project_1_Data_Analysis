# 농업대출 및 귀농인구 상관관계 분석

# 대출 파일 전처리
loan_industry[loan_industry == '-'] = '0'
loan_industry$X2013 <- as.numeric(loan_industry$X2013)
loan_industry$X2014 <- as.numeric(loan_industry$X2014)
loan_industry$X2015 <- as.numeric(loan_industry$X2015)
loan_industry$X2016 <- as.numeric(loan_industry$X2016)

summary(loan_industry)
# View(loan_industry)
colnames(loan_industry) <- c('산업', '2013', '2014', '2015', '2016', '2017', '2018', '2019', '2020')

f_loan <- subset(loan_industry, loan_industry$산업 == '농업, 임업 및 어업')

library(reshape2)
f_loan <- melt(f_loan, id.vars="산업", variable.name='년도')
new_f_loan <- f_loan[,2:3]
new_f_loan$년도 <- as.Date(new_f_loan$년도, format='%Y')
# View(new_f_loan)

# 연도별 / 농업대출 / 귀농인구 수 넣을 데이터프레임 생성
final_f_loan <- data.frame(matrix(ncol = 3, nrow = 8))
colnames(final_f_loan) <- c("년도", "대출금", "귀농인구 수")

final_f_loan$`귀농인구 수` <- (area_farm_move$`귀농인구 수`/1000)
final_f_loan$년도 <- 2013:2020
final_f_loan$대출금 <- new_f_loan$value
# View(area_farm_move)

ggscatter(final_f_loan, x = "귀농인구 수", y = "대출금", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", title = "농업대출금-귀농인구 상관관계 (2013-2020)", 
          xlab = "귀농인구 수 (천명)", ylab = "농업 대출금 (억원)")




