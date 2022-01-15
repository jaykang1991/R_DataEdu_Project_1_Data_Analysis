
# 도시 농촌 multi-level treemap
#2020년 추출
c_house2020 <- c_house_exp[c_house_exp$시점==2020,]
f_house2020 <- f_house_exp[f_house_exp$시점==2020,]
commonCol <-intersect(colnames(c_house2020), colnames(f_house2020))
commonCol <- commonCol[commonCol!="시점"] #시점 제거
commonCol <- commonCol[commonCol!="소비지출"] #소비지출 제거


#2020년 농촌, 도시
cf_house2020 <- rbind(c_house2020[,commonCol], f_house2020[,commonCol])
cf_house2020$도시구분 <- c("도시", "농촌")

#자료구조변환
library(reshape2)
melt_cf_house2020 <- melt(cf_house2020,  id.vars=c("도시구분"), measure.vars = commonCol)
melt_cf_house2020$value <- as.numeric(gsub(",", "", melt_cf_house2020$value))

library(RColorBrewer)

#treemap
library(treemap)
treemap(melt_cf_house2020, index = c("도시구분", "variable"), 
        vSize = "value", vColor = "value", type = "value",
        title = "도시/농촌 가계지출 비교 2020 (단위 : 천원)",
        fontsize.title=18,
        border.col=c("black","black"),  # Color of borders of groups, of subgroups, of subsubgroups ....
        border.lwds=c(5,2),             # Width of colors
        bg.labels="white", # 레이블의 배경색
        fontsize.labels=18,lowerbound.cex.labels= 0.1)

  

