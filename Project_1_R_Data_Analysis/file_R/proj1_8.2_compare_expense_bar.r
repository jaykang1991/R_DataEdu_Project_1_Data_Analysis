# c_house_exp integer -> character -- "시점" --
c_house_exp$시점<- as.character(format(c_house_exp$시점))

# c_house_exp character-> numeric -- "시점" 외 모든 변수 --
for (j in 2:ncol(c_house_exp)){c_house_exp[,j] <- as.numeric(gsub(",", "", c_house_exp[,j]))}

# f_house_exp integer -> character -- "시점" --
f_house_exp$시점 <- as.character(format(f_house_exp$시점))

# f_house_exp character-> numeric -- "시점" 외 모든 변수 --
for (j in 2:ncol(f_house_exp)){f_house_exp[,j] <- as.numeric(gsub(",", "", f_house_exp[,j]))}

#c_house_exp를 farm의 연도와 맞추기
c_house_exp <- c_house_exp[(c_house_exp$시점 >= 2013), ]

#변환된 데이터 구조확인
str(c_house_exp)
str(f_house_exp)

#컬럼 추가(농촌/도시 구분)
c_house_exp$구분 <- "도시"
f_house_exp$구분 <- "농촌"

#컬럼 이름 저장
(cityColname <- colnames(c_house_exp))
(farmColname <- colnames(f_house_exp))

#그래프 그리기
library(ggplot2)
for(cityCol in cityColname){
  if (cityCol ==  "시점" || cityCol == "구분"){
    next #시점, 구분 컬럼 건너 뛰기
  }
  farmColIndex <- which(farmColname==cityCol)
  if (length(farmColIndex)!=0){ #도시, 농촌 동시에 존재하는 컬럼만
    mergeData <- rbind(c_house_exp[,c(1,which(cityColname==cityCol), which(cityColname=="구분"))],
                       f_house_exp[,c(1,which(farmColname==cityCol), which(farmColname=="구분"))]) #도시, 농촌 데이터 합치기
    
    print(ggplot(data=mergeData, aes_string(x="시점", y=cityCol, fill="구분"))+
            geom_bar(stat="identity", position = position_dodge(width=0.5))+
            ggtitle("도시/농촌 연간 소비 지출 비교 2013~2020 (단위 : 천원)")+
            theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 18, color = "darkblue")) +
            labs(x = "년도", y = cityCol)
            )
  }
}
