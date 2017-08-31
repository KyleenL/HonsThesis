# install.packages("dplyr")
library(dplyr)

# install.packages("tidyverse")
library(tidyverse)

# install.packages("ggplot2")
library(ggplot2)

# install.packages("gridExtra")
library(gridExtra)

# install.packages("stringr") 
library(stringr)

# install.packages("car")
library(car)


DVR1=read.csv("DVR1Data.csv")
DVR2=read.csv("DVR2Data.csv")
DVR3=read.csv("DVR3Data.csv")
BodySize=read.csv("BodySize.csv")

Merged = merge(DVR1, DVR2, all=TRUE)
DVR.Master = merge(Merged, DVR3, all = TRUE)

#####remove na#####
DVR.Master <- filter(DVR.Master, !is.na(Arena_Duration))
DVR.Female <- filter(DVR.Master, Sex == "F")

Exploration <- filter(DVR.Female, Assay == "Exploration")
nrow(distinct(DVR.Female, LizID))


Novel <- filter(DVR.Female, Assay == "Novel")
Social <- filter(DVR.Female, Assay == "Social") 

#####Outliers#####
Exploration$obs <- seq(1:nrow(Exploration))
ggplot(Exploration, aes(TotalDist, obs, label = obs)) +
  geom_point(colour = "white") +
  geom_text() +
  geom_vline(aes(xintercept = (mean(Exploration$TotalDist, na.rm = T) + (sd(Exploration$TotalDist, na.rm = T)*3)), color = "red"))


#####group and summarise by lizards#####
  
#Exploration
Exploration_by_lizID <- Exploration %>% 
  group_by(LizID, Trt) %>% 
  summarise(
    "mean_TotalDist" = mean(TotalDist),
    "mean_Hide_Duration" = mean(Hide_Duration)
    ) 

#Novel
Novel_by_lizID <- Novel %>% 
  group_by(LizID, Trt) %>% 
  summarise(
    "mean_TotalDist" = mean(TotalDist),
    "mean_Hide_Duration" = mean(Hide_Duration),
    "mean_NovItem_LatFirst" = mean(NovItem_LatFirst),
    "mean_NovItem_Freq" = mean(NovItem_Freq),
    "mean_NovItem_Duration" = mean(NovItem_Duration),
    "mean_NovZone_LatFirst" = mean(NovZone_LatFirst),
    "mean_NovZone_Freq" = mean(NovZone_Freq),
    "mean_NovZone_Duration" = mean(NovZone_Duration)
  ) 

#Social
Social_by_lizID <- Social %>% 
  group_by(LizID, Trt) %>% 
  summarise(
    "mean_TotalDist" = mean(TotalDist),
    "mean_Hide_Duration" = mean(Hide_Duration),
    "mean_Social_LatFirst" = mean(Social_LatFirst),
    "mean_Social_Freq" = mean(Social_Freq),
    "mean_Social_Duration" = mean(Social_Duration),
    "mean_Neutral_LatFirst" = mean(Neutral_LatFirst),
    "mean_Neutral_Freq" = mean(Neutral_Freq),
    "mean_Neutral_Duration" = mean(Neutral_Duration),
    "mean_Asocial_Freq" = mean(Asocial_Freq),
    "mean_Asocial_Duration" = mean(Asocial_Duration)
  )


#####make boxplot#####


##EXPLORATION
#Hide duration
ggplot(Exploration_by_lizID, aes(x = Trt, y= mean_Hide_Duration)) + geom_boxplot() + ggtitle("Exploration Hide Duration")
t.test(mean_Hide_Duration ~ Trt, data = Exploration_by_lizID, var.equal = TRUE)

#Total Distance
ggplot(Exploration_by_lizID, aes(x = Trt, y= mean_TotalDist)) + geom_boxplot() +ggtitle ("Exploration Total Distance")
t.test(mean_TotalDist ~ Trt, data = Exploration_by_lizID, var.equal = TRUE)

##NOVEL
#Total Distance
ggplot(Novel_by_lizID, aes(x = Trt, y= mean_TotalDist)) + geom_boxplot() + ggtitle("Novel Total Distance")
t.test(mean_TotalDist ~ Trt, data = Novel_by_lizID, var.equal = TRUE)




