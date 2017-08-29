DVR1=read.csv("DVR1Data.csv")
DVR2=read.csv("DVR2Data.csv")
DVR3=read.csv("DVR3Data.csv")


install.packages("car")

datTest <- airquality
head(datTest)
str(datTest)

datTest$Month <- factor(datTest$Month)

car::scatterplotMatrix(~log(Ozone) + Wind, diagonal = "histogram", data = datTest)

plot(log(Ozone) ~ Wind, data = datTest, ylab = "Log Transformed Ozone", xlab = "Wind", pch = 16)
text(labels = rownames(datTest), y = log(datTest$Ozone), x = datTest$Wind)

length(unique(datTest$Month))

install.packages("dplyr")
library(dplyr)
summarise(group_by(datTest, Month), mean = mean(Ozone, na.rm = TRUE))

xtabs(~Month, data=datTest)

#HONOURS
Merged = merge(DVR1, DVR2, all=TRUE)
DVR = merge(Merged, DVR3, all = TRUE)
#for experimental changes
    DVR.Master = DVR


length(unique(DVR$LizID))
str(Merged)
head(DVR1)

#PLOTS
TreatmentVDist=plot(TotalDist ~ Trt, data = DVR)

#REMOVING VOID TRIALS
summary(DVR.Master$Arena_Duration)
DVR.Master[is.na(DVR.Master$Arena_Duration),]
is.na(DVR.Master$Arena_Duration)
DVR.Master <- DVR.Master[!is.na(DVR.Master$Arena_Duration),]

#CLEANING DATA
DVR.Master$Result <- NULL
install.packages("stringr")
library(stringr)
stringr::str_split_fixed
str_split_fixed(DVR.Master$ChDate, "_", 2)
group_by(DVR.Master, LizID)
DVR.Master[ ! DVR.Master$Sex %in% c(M), ]
