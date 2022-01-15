library(ggplot2)
library(reshape2)
########################### agri_income 농업 총수입
# 단위 (천원) / (1년)
# agri_income integer -> character -- "시점" --
agri_income$시점<- as.character(format(agri_income$시점))
# agri_income character-> numeric -- "시점" 외 모든 변수 --
for (j in 2:ncol(agri_income)){agri_income[,j] <- as.numeric(gsub(",", "", agri_income[,j]))}
# 필요한 데이터(시점, 계), (미곡, 맥류, 잡곡, 두류, 서류, 채소, 특작, 과수, 기타) 추출하기
(agri_day_total <- agri_income[,c("시점","계","농작물수입")])
(agri_day_kind <- agri_income[,c("시점", "미곡", "맥류", "잡곡", "두류", "서류", "채소", "특작", "과수", "기타")])

# 시점 > 년도 로 변경
colnames(agri_day_total) <- c("년도","계","농작물수입")
colnames(agri_day_kind) <- c("년도", "미곡", "맥류", "잡곡", "두류", "서류", "채소", "특작", "과수", "기타")

# 값이 큰 (농업총수입, 농작물수입) 의 동시 비교를 위해 melt 사용
agri_day_total <- melt(agri_day_total, id.vars="년도", variable.name="품목별")
# 동시 비교를 위해 melt 사용
agri_day_kind <- melt(agri_day_kind, id.vars="년도", variable.name="품목별")
# 품목별 > 구분 으로 변경
names(agri_day_total)[2] <- c("구분")
names(agri_day_kind)[2] <- c("구분")
# 농업 총 수입(계, 농작물수입) 그래프
ggplot(data=agri_day_total, aes(x=년도,
                                y=value,
                                group=구분,
                                color=구분))+geom_line(size=1.3)+
  ggtitle("농업 총 수입 (1980-2020)")+ylab("총 수입 (만원)")

# (미곡, 맥류, 잡곡, 두류, 서류, 채소, 특작, 과수, 기타) 추세
ggplot(data=agri_day_kind, aes(x=년도,
                               y=value,
                               group=구분,
                               color=구분))+geom_line(size=1.3)+
  geom_smooth(method='lm')+
  ggtitle("품목별 농업 총 수입 (1980-2020)")+ylab("총 수입 (만원)") +
  facet_wrap(~구분, scale="free_y")
############################# F_EARNINGS_BYAGE 연령별 농가소득 
# 단위(천원) / (1년)
# f_earning_age integer -> character -- "시점" --
f_earning_age$시점<- as.character(format(f_earning_age$시점))
#변환된 데이터 구조확인
str(f_earning_age)
# 라인차트 - melt 활용한 연령별 라인 그래프 그리기
# 시각화 위해 소득률 제외 10으로 나누기
f_earning_age[,2:7] <- f_earning_age[,2:7]/10
# 필요한 데이터 전국, 40대이하, 40~49, 50~59, 60,69, 70대이상 추출하기
f_earning_age <- f_earning_age[,1:7]
# 보기쉽게 연령별 이름 변경
colnames(f_earning_age) <- c('년도',"평균","39세 이하","40대","50대","60대","70세 이상")
# 동시 비교를 위해 melt 사용
f_earning_age <- melt(f_earning_age, id.vars="년도", variable.name='연령별')
# value 의 값을 numeric으로 변경
f_earning_age$value <- as.numeric(gsub(",", "", f_earning_age$value))
# 연령별 추세 그래프
ggplot(data=f_earning_age, aes(x=년도,
                               y=value,
                               group=연령별,
                               color=연령별))+geom_line(size=1.3)+
  ggtitle("연령별 농가 소득 (2006-2020)")+ylab("소득 (만원)")+
  annotate('text', x=15.2, y=7070, label = '50', colour = "sky blue", size=5)+
  annotate('text', x=15, y=5650, label = '39이하', colour = "brown", size=5) +
  annotate('text', x=15.2, y=5300, label = '40' ,colour = "dark green", size=5)+
  annotate('text', x=15.2, y=5100, label = '60', colour = "blue", size=5)+
  annotate('text', x=15.3, y=4550, label = '평균', colour = "red", size=5.5) +
  annotate('text', x=15.2, y=3400, label = '70' ,colour = "purple", size=5)

########################### 정부지원금 farm_govloan
# 단위 (백만원) / 1년
# 시각화 위해 소득률 제외 100으로 나누기
farm_govloan[,2:6] <- farm_govloan[,2:6]/100
# 필요한 데이터 (합계, 기타) 추출하기
govloan_total <- farm_govloan[,c("시점","합계")]
# 필요한 데이터 (채소, 과수, 특작) 추출하기
govloan_kind <- farm_govloan[,c("시점","채소","과수","특작")]

# 동시 비교를 위해 melt 사용
govloan_total <- melt(govloan_total, id.vars="시점", variable.name="합계")
govloan_kind <- melt(govloan_kind, id.vars="시점", variable.name="구분")

# 시점 > 년도 로 변경
colnames(govloan_total) <- c("년도","합계","value")
colnames(govloan_kind) <- c("년도","구분","value")



# 정부지원금 추세 그래프 (합계)
ggplot(data=govloan_total, aes(x=년도,
                               y=value,
                               group=합계,
                               color=합계))+geom_line(size=1.3)+
  ggtitle("농업종합자금 정부지원금 (2014-2020)")+ylab("총 금액(억원)")
# 정부지원금 추세 그래프 (채소, 과수, 특작)
ggplot(data=govloan_kind, aes(x=년도,
                              y=value,
                              group=구분,
                              color=구분))+geom_line(size=1.3)+
  ggtitle("농업종합자금 정부지원금 (2014-2020)")+ylab("총 금액 (억원)") +
  annotate('text', x=7.2, y=1080, label = '채소', colour = "red", size=8)+
  annotate('text', x=7.2, y=1350, label = '과수', colour = "dark green", size=8) +
  annotate('text', x=7.2, y=480, label = '특작' ,colour = "blue", size=8)

