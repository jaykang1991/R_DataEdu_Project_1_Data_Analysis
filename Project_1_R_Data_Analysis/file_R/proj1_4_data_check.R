# 데이터 결측치 및 이상치 최종 체크

summary(trend_crop)
# View(trend_crop)
# 이상 없음

summary(trend_fruit)
# View(trend_fruit)
# 이상 없음

summary(trend_vegi_fac)
# View(trend_vegi_fac)
# 이상 없음

summary(trend_vegi_wild)
# View(trend_vegi_wild)
# 이상 없음

summary(pa_crop)
# View(pa_crop)
# 이상 없음

summary(pa_fruit)
# View(pa_fruit)
# 이상 없음

summary(property_value)
summary(pa_vegi_fac)
# View(pa_vegi_fac)
# 이상 없음

summary(pa_vegi_wild)
# View(pa_vegi_wild)
# 이상 없음

# View(property_value)
# property_value(농지지가지수)에 전국 데이터 자료가 없어 확인.
# 지역별, 월별만 있는걸 확인함.


# 전국(평균) 컬럼과 지역별 평균값 추가
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

summary(month_cost_compare)
# View(month_cost_compare)
# 이상 없음

summary(c_house_exp)
# View(c_house_exp)
# 분석시 숫자로 변환 필요

summary(f_house_exp)
# View(f_house_exp)
# 분석시 숫자로 변환 필요

summary(f_earning_age)
# View(f_earning_age)
# 이상 없음

summary(agri_income)
# View(agri_income)
# 이상 없음

summary(f_income)
# View(f_income)
# 이상 없음

summary(area_farm_move)
# View(area_farm_move)
# 이상 없음

summary(farm_govloan)
# View(farm_govloan)
# 이상 없음

summary(unemp_rate)
# View(unemp_rate)
# 이상 없음

summary(farm_pop)
# View(farm_pop)
# 이상 없음

summary(unemp_number_new)
# View(unemp_number_new)
# 이상 없음음



