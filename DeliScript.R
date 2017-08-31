DVR1=read.csv("DVR1Data.csv")
DVR2=read.csv("DVR2Data.csv")
DVR3=read.csv("DVR3Data.csv")
BodySize=read.csv("BodySize.csv")
install.packages("car")

datTest <- airquality
head(datTest)
str(datTest)

datTest$Month <- factor(datTest$Month)

car::scatterplotMatrix(~log(Ozone) + Wind, diagonal = "histogram", data = datTest)

plot(log(Ozone) ~ Wind, data = datTest, ylab = "Log Transformed Ozone", xlab = "Wind", pch = 16)
text(labels = rownames(datTest), y = log(datTest$Ozone), x = datTest$Wind)

length(unique(datTest$Month))

summarise(group_by(datTest, Month), mean = mean(Ozone, na.rm = TRUE))

xtabs(~Month, data=datTest)


#PACKAGES
install.packages("dplyr")
library(dplyr)

install.packages("tidyverse")
library(tidyverse)

install.packages("ggplot2")
library(ggplot2)

install.packages("gridExtra")
library(gridExtra)

install.packages("stringr") 
library(stringr)


#HONOURS
Merged = merge(DVR1, DVR2, all=TRUE)
DVR = merge(Merged, DVR3, all = TRUE)


#for experimental changes
    DVR.Master = DVR

str(DVR.Master)

#HISTOGRAMS

#Total Distance
hist(DVR.Master$TotalDist)  ;  shapiro.test(DVR.Master$TotalDist)

#Novel Item
hist(log(DVR.Master$NovItem_Freq))  ;  shapiro.test(DVR.Master$NovItem_Freq)
hist(log(DVR.Master$NovItem_Duration)) ;  shapiro.test(DVR.Master$NovItem_Duration)
hist(DVR.Master$NovItem_LatFirst) ;  shapiro.test(DVR.Master$NovItem_LatFirst)

#Novel Zone
hist(log(DVR.Master$NovZone_Freq)) ;  shapiro.test(DVR.Master$NovZone_Freq)
hist((DVR.Master$NovZone_Duration)) ;  shapiro.test(DVR.Master$NovZone_Duration)
hist((DVR.Master$NovZone_LatFirst)) ;  shapiro.test(DVR.Master$NovZone_LatFirst)

#Hide
hist(log(DVR.Master$Hide_Freq)) ;  shapiro.test(DVR.Master$Hide_Freq)
hist((DVR.Master$Hide_Duration)) ;  shapiro.test(DVR.Master$Hide_Duration)
hist(log(DVR.Master$Hide_LatFirst)) ;  shapiro.test(DVR.Master$Hide_LatFirst)

#Social
hist(log(DVR.Master$Social_Freq)) ;  shapiro.test(DVR.Master$Social_Freq)
hist(log(DVR.Master$Social_Duration)) ;  shapiro.test(DVR.Master$Social_Duration)
hist((DVR.Master$Social_LatFirst)) ;  shapiro.test(DVR.Master$Social_LatFirst)
hist(log(DVR.Master$Asocial_Freq)) ;  shapiro.test(DVR.Master$Asocial_Freq)
hist((DVR.Master$Asocial_Duration)) ;  shapiro.test(DVR.Master$Asocial_Duration)
hist(log(DVR.Master$Asocial_LatFirst)) ;  shapiro.test(DVR.Master$Asocial_LatFirst)
##Shapiro test for log transformed data?



#SCATTERPLOT
pairs(DVR.Master[,c(11, 14, 17, 24,30)])  #compares all durations 
pairs(DVR.Master[,c(10, 13, 16, 20, 23, 26, 29)])   #compares all frequencies


#BOXPLOT
p1 <- ggplot(DVR.Master,aes(y = TotalDist, x = factor(Trt))) + geom_boxplot() + labs(x="Treatment",y="Total Distance (cm)") 
p2 <- ggplot(DVR.Master,aes(y = Hide_Duration, x = factor(Trt))) + geom_boxplot() + labs(x="Treatment",y="Time spent in Hide (s)") 
p3 <- ggplot(DVR.Master,aes(y = NovZone_Duration, x = factor(Trt))) + geom_boxplot() + labs(x="Treatment",y="Time in Novel Zone (s)") 
p4 <- ggplot(DVR.Master,aes(y = Social_Duration, x = factor(Trt))) + geom_boxplot() + labs(x="Treatment",y="Time spent in Social Zone (s)") 
p5 <- ggplot(DVR.Master,aes(y = Hide_Freq, x = factor(Trt))) + geom_boxplot()  + labs(x="Treatment",y="Hide Frequency") 
p6 <- ggplot(DVR.Master,aes(y = NovZone_LatFirst, x = factor(Trt))) + geom_boxplot()  + labs(x="Treatment",y="Latency to Novel Zone (s)")


grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 2, ncol = 3)
grid.arrange(p1, nrow = 2, ncol = 2)
##how to create boxplot with High vs Low but only within certain assays???

#OUTLIERS

DVR.Master$obs <- seq(1:nrow(DVR.Master))

#TotalDist
ggplot(DVR.Master, aes(TotalDist, obs, label = obs)) +
  geom_point(colour = "white") +
  geom_text() +
  geom_vline(aes(xintercept = (mean(DVR.Master$TotalDist, na.rm = T) + (sd(DVR.Master$TotalDist, na.rm = T)*3)), color = "red"))
  #geom_text(data = filter(DVR.Master, TotalDist <= mean(DVR.Master$TotalDist) - sd(DVR.Master$TotalDist)*3), color = "red") +
  #theme_bw()

#Novel Item Frequency
ggplot(DVR.Master, aes(NovItem_Freq, obs, label = obs)) +
  geom_point(colour = "white") +
  geom_text() +
  geom_vline(aes(xintercept = (mean(DVR.Master$NovItem_Freq, na.rm = T) + (sd(DVR.Master$NovItem_Freq, na.rm = T)*3)), color = "red")) +
  geom_vline(aes(xintercept = (mean(DVR.Master$NovItem_Freq, na.rm = T) - (sd(DVR.Master$NovItem_Freq, na.rm = T)*3)), color = "red"))

#Novel Item Duration
ggplot(DVR.Master, aes(NovItem_Duration, obs, label = obs)) +
  geom_point(colour = "white") +
  geom_text() +
  geom_vline(aes(xintercept = (mean(DVR.Master$NovItem_Duration, na.rm = T) + (sd(DVR.Master$NovItem_Duration, na.rm = T)*3)), color = "red")) +
  geom_vline(aes(xintercept = (mean(DVR.Master$NovItem_Duration, na.rm = T) - (sd(DVR.Master$NovItem_Duration, na.rm = T)*3)), color = "red"))


#REMOVING VOID TRIALS
summary(DVR.Master$Arena_Duration)
DVR.Master[is.na(DVR.Master$Arena_Duration),]
is.na(DVR.Master$Arena_Duration)
DVR.Master <- DVR.Master[!is.na(DVR.Master$Arena_Duration),]

#CLEANING DATA
DVR.Master$Results <- NULL   #removes Result column
DVR.Master$Arena_Freq <- NULL   #removes Arena Freq column

str_split_fixed(DVR.Master$ChDate, "_", 2)  #attempt to split ChDate Column
group_by(DVR.Master, LizID)       #tried grouping by same ID
DVR.Master[ ! DVR.Master$Sex %in% c(M), ]     #tried removing M rows
