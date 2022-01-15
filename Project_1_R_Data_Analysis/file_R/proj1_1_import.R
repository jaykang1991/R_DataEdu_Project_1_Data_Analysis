#install.packages("readxl")
library(readxl)
setwd("C:/Data_Edu/Project/Project_1_R_Data_Analysis/file")

trend_crop <- read_xlsx("TREND_CROP.xlsx")
trend_fruit <- read_xlsx("TREND_FRUIT.xlsx")
trend_vegi_wild <- read_xlsx("TREND_VEGI_WILD.xlsx")
trend_vegi_fac <- read_xlsx("TREND_VEGI_FACILITY.xlsx")

pa_crop <- read_xlsx("PA_CROP.xlsx")
pa_fruit <- read_xlsx("PA_FRUIT.xlsx")
pa_vegi_wild <- read_xlsx("PA_VEGI_WILD.xlsx")
pa_vegi_fac <- read_xlsx("PA_VEGI_FACILITY.xlsx")

property_value <- read.csv("F_PROPERTY_VALUE.csv")

month_cost_compare <- read.csv("MONTH_COST_COMPARE.csv")
c_house_exp <- read.csv("C_HOUSE_EXPENSE.csv")
f_house_exp<- read.csv("F_HOUSE_EXPENSE.csv")
f_earning_age<- read.csv("F_EARNING_BYAGE.csv")
agri_income<- read.csv("AGRI_GROSS_INCOME.csv")

f_income <- read.csv("F_INCOME.csv")
area_farm_move <- read.csv("AREA_FARM_MOVE.csv")
farm_govloan <- read_xlsx("FARM_GOVLOAN.xlsx")
unemp_rate <- read.csv("UNEMP_RATE.csv")

farm_pop <- read_xlsx("FARM_POP.xlsx")
unemp_number_new <- read_xlsx("UNEMP_NUMBER.xlsx")
loan_industry <- read.csv("LOAN_INDUSTRY.csv")

f_ini_cost <- read.csv("F_INI_COST.csv")