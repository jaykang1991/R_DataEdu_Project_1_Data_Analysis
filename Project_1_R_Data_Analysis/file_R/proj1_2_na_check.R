# 결측값이 포함되어 있는지 확인
is.na(trend_crop)
# 결측값이 총 몇개인지 계산
sum(is.na(trend_crop))
sum(is.na(trend_fruit))
sum(is.na(trend_vegi_wild))
sum(is.na(trend_vegi_fac))
sum(is.na(pa_crop))
sum(is.na(pa_fruit))
sum(is.na(pa_vegi_wild))
sum(is.na(pa_vegi_fac))
sum(is.na(property_value))
sum(is.na(month_cost_compare))
sum(is.na(c_house_exp))
sum(is.na(f_house_exp))
sum(is.na(f_earning_age))
sum(is.na(agri_income))
sum(is.na(f_income))
sum(is.na(area_farm_move))
sum(is.na(farm_govloan))
sum(is.na(unemp_rate))
sum(is.na(farm_pop))
sum(is.na(unemp_number_new))
sum(is.na(loan_industry))
# 자료를 확인 해 보니 결측값이 없음.

# - 값이 포함되는 데이터가 확인됨. 
# View(agri_income)
# View(f_earning_age)
# View(pa_crop)
# pa_fruit
# pa_vegi_fac
# pa_vegi_wild
# View(month_cost_compare)
# View(loan_industry)

# 해당위치 확인 후 수정
agri_income[agri_income == '-'] = 0
f_earning_age[f_earning_age == '-'] = 0
# 2개 이외에는 값이 변경이 되지 않아 차이점과 방법 모색중

# summary 통하여 계산 안되는 컬럼 확인
summary(agri_income)
summary(f_earning_age)
summary(pa_crop)
summary(pa_fruit)
summary(pa_vegi_fac)
summary(pa_vegi_wild)
summary(month_cost_compare)

# 계산안되는 컬럼의 원인 '0'으로 대체
pa_crop[pa_crop == '-'] = '0'
pa_fruit[pa_fruit == '-'] = '0'
pa_vegi_fac[pa_vegi_fac == '-'] = '0'
pa_vegi_wild[pa_vegi_wild == '-'] = '0'
unemp_number_new[unemp_number_new == '-'] = '0'
month_cost_compare[month_cost_compare == '-'] = '0'

# character 컬럽 numeric으로 변경
pa_crop$부산물가액 <- as.numeric(as.character(pa_crop$부산물가액))
pa_crop$소농구비 <- as.numeric(as.character(pa_crop$소농구비))
pa_crop$기타비용 <- as.numeric(as.character(pa_crop$기타비용))
pa_crop$년도 <- as.numeric(as.character(pa_crop$년도))

pa_fruit$부산물가액 <- as.numeric(as.character(pa_fruit$부산물가액))
pa_fruit$보통비료비 <- as.numeric(as.character(pa_fruit$보통비료비))
pa_fruit$농약비 <- as.numeric(as.character(pa_fruit$농약비))
pa_fruit$년도 <- as.numeric(as.character(pa_fruit$년도))
pa_fruit$"농기계·시설 임차료" <- as.numeric(as.character(pa_fruit$"농기계·시설 임차료"))
pa_fruit$위탁영농비 <- as.numeric(as.character(pa_fruit$위탁영농비))
pa_fruit$기타비용 <- as.numeric(as.character(pa_fruit$기타비용))
pa_fruit$토지임차료 <- as.numeric(as.character(pa_fruit$토지임차료))
pa_fruit$소농구비 <- as.numeric(as.character(pa_fruit$소농구비))

pa_vegi_fac$부산물가액 <- as.numeric(as.character(pa_vegi_fac$부산물가액))
pa_vegi_fac$기타비용 <- as.numeric(as.character(pa_vegi_fac$기타비용))
pa_vegi_fac$위탁영농비 <- as.numeric(as.character(pa_vegi_fac$위탁영농비))
pa_vegi_fac$년도 <- as.numeric(as.character(pa_vegi_fac$년도))
pa_vegi_fac$"농기계·시설 임차료" <- as.numeric(as.character(pa_vegi_fac$"농기계·시설 임차료"))

pa_vegi_wild$부산물가액 <- as.numeric(as.character(pa_vegi_wild$부산물가액))
pa_vegi_wild$기타비용 <- as.numeric(as.character(pa_vegi_wild$기타비용))
pa_vegi_wild$위탁영농비 <- as.numeric(as.character(pa_vegi_wild$위탁영농비))
pa_vegi_wild$년도 <- as.numeric(as.character(pa_vegi_wild$년도))

unemp_number_new$세종특별자치시 <- as.numeric(as.character(unemp_number_new$세종특별자치시))
unemp_number_new$년도 <- as.numeric(as.character(unemp_number_new$년도))

month_cost_compare$모름.무응답.....1 <- as.numeric(as.character(month_cost_compare$모름.무응답.....1))
month_cost_compare$없음.....1 <- as.numeric(as.character(month_cost_compare$없음.....1))
month_cost_compare$모름.무응답.... <- as.numeric(as.character(month_cost_compare$모름.무응답....))
month_cost_compare$없음.... <- as.numeric(as.character(month_cost_compare$없음....))

# 변경 여부 확인
summary(trend_fruit) # 이상치 처리시 숫자로 변경
summary(pa_crop)
summary(pa_fruit)
summary(pa_vegi_wild)
summary(pa_vegi_fac)
summary(unemp_number_new)

# numeric으로변환 시 기존 0이 아닌 값이 NA로 바뀜..
# 1. agri_income : 분석에 필요한 농작물 수입만 숫자로 변환
agri_income$농작물수입 <- as.numeric(gsub(",", "", agri_income$농작물수입))
# View(agri_income)

# numeric으로변환 시 기존 0이 아닌 값이 NA로 바뀜..
# 2. f_earning_age : 분석 필요한 7번째 컬럼까지 추출
f_earning_age <- f_earning_age[,1:7]
f_earning_age$전국 <- as.numeric(gsub(",", "", f_earning_age$전국))
f_earning_age$X40대.이하 <- as.numeric(gsub(",", "", f_earning_age$X40대.이하))
f_earning_age$X40.49 <- as.numeric(gsub(",", "", f_earning_age$X40.49))
f_earning_age$X50.59 <- as.numeric(gsub(",", "", f_earning_age$X50.59))
f_earning_age$X60.69 <- as.numeric(gsub(",", "", f_earning_age$X60.69))
f_earning_age$X70세이상 <- as.numeric(gsub(",", "", f_earning_age$X70세이상))

# View(f_earning_age)

# View(f_ini_cost)
f_ini_cost[f_ini_cost == '-'] = '0'
f_ini_cost$X2020 <- as.numeric(gsub(",", "", f_ini_cost$X2020))

