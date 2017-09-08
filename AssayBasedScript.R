#####PACKAGES#####
setwd("~/Documents/Honours/HonsThesis")

#install.packages("ade4")
library("ade4")

#install.packages("MCMCglmm")
library("MCMCglmm")

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


#####DATA#####
DVR1=read.csv("DVR1Data.csv")
DVR2=read.csv("DVR2Data.csv")
DVR3=read.csv("DVR3Data.csv")
BodySize=read.csv("BodySize.csv")
  
#Merge data 
Merged = merge(DVR1, DVR2, all=TRUE)
DVRAll = merge(Merged, DVR3, all = TRUE)
DVR.Master = left_join(DVRAll, BodySize, by = NULL)


#remove NA
DVR.Master <- filter(DVR.Master, !is.na(Arena_Duration))

#Filter by Female
DVR.Female <- filter(DVR.Master, Sex == "F")
nrow(distinct(DVR.Female, LizID))


  #Filter by assay
EV <- c("LizID", "ChDate", "Trt","TotalDist","EndMass", "NovZone_Duration", "Social_Duration")
Exploration <- filter(DVR.Female, Assay == "Exploration")[EV]
Exploration$LogTotalDist <- log(Exploration$TotalDist + 0.05)
Exploration$CatDist <- ifelse(Exploration$TotalDist == 0, 0, 1)
Exploration$ExNovDuration <- Exploration$NovZone_Duration
Exploration$NovZone_Duration <- NULL
Exploration$ExSocDuration <- Exploration$Social_Duration
Exploration$Social_Duration <- NULL

NV <- c("LizID", "ChDate", "Trt","NovZone_LatFirst", "NovZone_Duration", "EndMass")
Novel <- filter(DVR.Female, Assay == "Novel")[NV]
#Novel$LogNovZone_Duration <- log(Novel$AdjNovDuration+0.05)
Novel$CatNovLat <- ifelse(is.na(Novel$NovZone_LatFirst), 0, 1)
Novel$NovAdjLat <- ifelse(is.na(Novel$NovZone_LatFirst), 1200, Novel$NovZone_LatFirst)
#Novel$CatNovDuration <- ifelse(Novel$AdjNovDuration == 0, 0, 1)

SV <- c("LizID", "ChDate", "Trt","Social_Duration", "EndMass", "Social_LatFirst")
Social <- filter(DVR.Female, Assay == "Social")[SV]
#Social$LogSocial_Duration <- log(Social$Social_Duration + 0.05)
#Social$CatSocial <- ifelse(Social$Social_Duration == 0, 0, 1)
Social$SocAdjLat <- ifelse(is.na(Social$Social_LatFirst), 1200, Social$Social_LatFirst)
Social$CatSocLat <- ifelse(is.na(Social$Social_LatFirst), 0, 1)


EV = left_join(Novel, Exploration, by = NULL)
LizVarib = left_join(EV, Social, by = NULL)

#Novel Duration adjustments
LizVarib$AdjNov_Duration <- LizVarib$NovZone_Duration - LizVarib$ExNovDuration    #removes activity novel duration data from novel assays
LizVarib$AdjNovDuration <- ifelse(LizVarib$AdjNov_Duration <= 0, 0, LizVarib$AdjNov_Duration)    #caps subtractions at 0
LizVarib$LogAdjNovDuration <- log(LizVarib$AdjNovDuration + 0.5)     #Log transforms adjusted Novel duration
LizVarib$AdjNov_Duration <- NULL
LizVarib$CatNovDuration <- ifelse(LizVarib$AdjNovDuration == 0, 0, 1)  #categorises novel zone activity

#Social Duration adjustments
LizVarib$AdjSoc_Duration <- LizVarib$Social_Duration - LizVarib$ExSocDuration    #removes activity social duration data from social assays
LizVarib$AdjSocDuration <- ifelse(LizVarib$AdjSoc_Duration <= 0, 0, LizVarib$AdjSoc_Duration)    #caps subtractions at 0
LizVarib$LogAdjSocDuration <- log(LizVarib$AdjSocDuration + 0.5)     #Log transforms adjusted Social duration
LizVarib$AdjSoc_Duration <- NULL
LizVarib$CatSocDuration <- ifelse(LizVarib$AdjSocDuration == 0, 0, 1)  #categorises Social zone activity
LizVarib$LogSocial_Duration <- log(LizVarib$Social_Duration + 0.5)
  

#View(LizVarib)

#CatV <- c("LizID", "ChDate", "Trt", "EndMass", "CatSocDuration", "CatSocLat", "CatNovLat", "CatNovDuration", "CatDist")
#Categorical <- (LizVarib)[CatV]
#View(Categorical)

subsetData <- subset(LizVarib, LizVarib$int == "1.1.1.1")     #446 obs


LizVarib$int.adj <- interaction(LizVarib$CatNovLat, LizVarib$CatNovDuration, LizVarib$CatDist, LizVarib$CatSocDuration)
subsetData.adj <- subset(LizVarib, LizVarib$int == "1.1.1.1")     #145 obs
Female.HighAdj <- filter(subsetData.adj, Trt == "High") # Contain NAs in cbind(varibs) 
Female.LowAdj <- filter(subsetData.adj, Trt == "Low") # Contain NAs in cbind(varibs) 


#High Female
HighFemale <- filter(LizVarib, Trt == "High")   #Not subsetted 
str(HighFemale)

##### BOXPLOT #####


##EXPLORATION
#Hide duration
ggplot(ExplorationFiltered, aes(x = Trt, y= mean_LogTotalDist)) + geom_boxplot() + ggtitle("Exploration Total Distance")
t.test(mean_LogTotalDist ~ Trt, data = ExplorationFiltered, var.equal = TRUE)

#Total Distance
ggplot(ExplorationFiltered, aes(x = Trt, y= mean_LogTotalDist)) + geom_boxplot() +ggtitle ("Exploration Total Distance")
t.test(mean_TotalDist ~ Trt, data = Exploration_by_lizID, var.equal = TRUE)

##NOVEL
#Novel Zone 
ggplot(NovelFiltered, aes(x = Trt, y= mean_LogNovZone_Duration)) + geom_boxplot() + ggtitle("Novel Zone Duration")
t.test(mean_LogNovZone_Duration ~ Trt, data = NovelFiltered, var.equal = TRUE)

ggplot(NovelFiltered, aes(x = Trt, y= mean_NovZone_Duration)) + geom_boxplot() + ggtitle("Novel Zone Duration")
t.test(mean_NovZone_Duration ~ Trt, data = NovelFiltered, var.equal = TRUE)


ggplot(Novel, aes(x = Trt, y= NovZone_Duration)) + geom_boxplot() + ggtitle("Novel Zone Duration")
t.test(NovZone_Duration ~ Trt, data = Novel, var.equal = FALSE)



##SOCIAL
#Social Zone
ggplot(SocialFiltered, aes(x = Trt, y= mean_Social_Duration)) + geom_boxplot() + ggtitle("Social Zone Duration")
t.test(mean_LogNovZone_Duration ~ Trt, data = NovelFiltered, var.equal = TRUE)

ggplot(SocialFiltered, aes(x = Trt, y= mean_Social_LatFirst)) + geom_boxplot() + ggtitle("Social Latency")
t.test(mean_LogNovZone_Duration ~ Trt, data = NovelFiltered, var.equal = TRUE)

##### Bayesian approaches #####

    #####TotalDist, NovZone_LatFirst, Social_Duration#####

# subset High diet
# varibs.need <- c("LizID", "Trt","TotalDist", "NovZone_LatFirst", "NovZone_Duration", "Social_Duration", "EndMass")

SubFemale.High <- filter(subsetData, Trt == "High") # Contain NAs in cbind(varibs) 
str(Female.High) #255

Female.HighAdj <- filter(subsetData.adj, Trt == "High") # Contain NAs in cbind(varibs) 


#priors
prior <- list(R = list(V = diag(3), nu = 0.01), 
              G = list(G1= list(V = diag(3), nu = 0.01)))

#modelHI <- MCMCglmm(cbind(LogTotalDist, NovZone_LatFirst, LogSocial_Duration) ~ EndMass + trait-1, 
#                   random = ~us(trait):LizID, 
#                    rcov= ~us(trait):units, 
#                    family = rep("gaussian", 3), 
#                    prior = prior, 
#                    nitt = 70000, 
#                    burnin = 10000, 
#                    thin = 100, 
#                    data = HighFemale)


saveRDS(modelHI, "modelHI")
modelHI <- readRDS("modelHI")

#modelHI.Sub <- MCMCglmm(cbind(LogTotalDist, NovZone_LatFirst, LogSocial_Duration) ~ EndMass + trait-1, 
#                   random = ~us(trait):LizID, 
#                    rcov= ~us(trait):units, 
#                    family = rep("gaussian", 3), 
#                    prior = prior, 
#                    nitt = 70000, 
#                    burnin = 10000, 
#                    thin = 100, 
#                    data = Female.High)

#saveRDS(modelHI.Sub, "modelHI.Sub")
modelHI <- readRDS("modelHI")
plot(modelHI.Sub$VCV)
autocorr(modelHI$VCV)

summary(modelHI)


#modelHI.AdjSub <- MCMCglmm(cbind(LogTotalDist, NovZone_LatFirst, LogAdjSocDuration) ~ EndMass + trait-1, 
#                   random = ~us(trait):LizID, 
#                    rcov= ~us(trait):units, 
#                    family = rep("gaussian", 3), 
#                    prior = prior, 
#                    nitt = 70000, 
#                    burnin = 10000, 
#                    thin = 100, 
#                    data = Female.HighAdj)

#saveRDS(modelHI.AdjSub, "modelHI.AdjSub")
modelHI.AdjSub <- readRDS("modelHI.AdjSub")

#  subset Low Diet

SubFemale.Low <- filter(subsetData, Trt == "Low") # Contain NAs in cbind(varibs) 
str(Female.Low) #211 obvs

#modelLOW.Sub <- MCMCglmm(cbind(LogTotalDist, NovZone_LatFirst, LogSocial_Duration) ~ EndMass +  trait-1, 
#                     random = ~us(trait):LizID, 
#                     rcov= ~us(trait):units, 
#                     family = rep("gaussian", 3), 
#                     prior = prior, 
#                     nitt = 70000, 
#                     burnin=10000, 
#                     thin = 100, 
#                     data = Female.Low)

#saveRDS(modelLOW.Sub, "modelLOW.Sub")
modelLOW <- readRDS("modelLOW")
plot(modelLOW$VCV)
autocorr(modelLOW$VCV)
summary(modelLOW)
posterior.mode(modelLOW$VCV)
HPDinterval(modelLOW$VCV)

posterior.mode(modelLOW$Sol)
HPDinterval(modelLOW$Sol)

#modelLOW.AdjSub <- MCMCglmm(cbind(LogTotalDist, NovZone_LatFirst, LogAdjSocDuration) ~ EndMass +  trait-1, 
#                     random = ~us(trait):LizID, 
#                     rcov= ~us(trait):units, 
#                     family = rep("gaussian", 3), 
#                     prior = prior, 
#                     nitt = 70000, 
#                     burnin=10000, 
#                     thin = 100, 
#                     data = Female.LowAdj)

#saveRDS(modelLOW.AdjSub, "modelLOW.AdjSub")
modelLOW.AdjSub <- readRDS("modelLOW.AdjSub")


# All Female Data

Female.All <- filter(DVR.Master, Sex == "F")[varibs.need] # Contain NAs in cbind(varibs) 
str(DVR.Female) #2580 obs
#modelALL <- MCMCglmm(cbind(TotalDist, NovZone_LatFirst, Social_Duration) ~ Trt + EndMass +  trait-1, 
#                     random = ~us(trait):LizID, 
#                     rcov= ~us(trait):units, 
#                     family = rep("gaussian", 3), 
#                     prior = prior, 
#                     nitt = 70000, 
#                     burnin=10000, 
#                     thin = 100, 
#                     data = DVR.Female)


#saveRDS(modelALL, "modelALL")
modelALL <- readRDS("modelALL")
plot(modelALL$VCV)
autocorr(modelALL$VCV)
summary(modelALL)
posterior.mode(modelALL$VCV)
HPDinterval(modelALL$VCV)

posterior.mode(modelALL$Sol)
HPDinterval(modelALL$Sol)


Female.All <- filter(DVR.Master, Sex == "F")[varibs.need] # Contain NAs in cbind(varibs) 
str(Female.NaOmit)
Female.NaOmit <- na.omit(Female.All) #1858 obvs


##### TotalDist, NovZone_Duration, Social_Duration #####

#priors
prior <- list(R = list(V = diag(3), nu = 0.01), 
              G = list(G1= list(V = diag(3), nu = 0.01)))

#modelHI.NovDur <- MCMCglmm(cbind(LogTotalDist, LogNovZone_Duration, LogSocial_Duration) ~ EndMass + trait-1, 
#                   random = ~us(trait):LizID, 
#                    rcov= ~us(trait):units, 
#                    family = rep("gaussian", 3), 
#                    prior = prior, 
#                    nitt = 70000, 
#                    burnin = 10000, 
#                    thin = 100, 
#                    data = HighFemale)

saveRDS(modelHI.NovDur, "modelHI.NovDur")
modelHI.NovDur <- readRDS("modelHI.NovDur")

#modelHI.NovDurSub <- MCMCglmm(cbind(LogTotalDist, LogNovZone_Duration, LogSocial_Duration) ~ EndMass + trait-1, 
#                   random = ~us(trait):LizID, 
#                    rcov= ~us(trait):units, 
#                    family = rep("gaussian", 3), 
#                    prior = prior, 
#                    nitt = 70000, 
#                    burnin = 10000, 
#                    thin = 100, 
#                    data = Female.High)

#saveRDS(modelHI.NovDurSub, "modelHI.NovDurSub")
modelHI.NovDur <- readRDS("modelHI.NovDur")
plot(modelHI.NovDur$VCV)
autocorr(modelHI.NovDur$VCV)

summary(modelHI.NovDur)
posterior.mode(modelHI.NovDur$VCV)
HPDinterval(modelHI.NovDur$VCV)


posterior.mode(modelHI.NovDur$Sol)
HPDinterval(modelHI.NovDur$Sol)

#modelHI.DurSubAdj <- MCMCglmm(cbind(LogTotalDist, LogAdjNovDuration, LogAdjSocDuration) ~ EndMass + trait-1, 
#                   random = ~us(trait):LizID, 
#                    rcov= ~us(trait):units, 
#                    family = rep("gaussian", 3), 
#                    prior = prior, 
#                    nitt = 70000, 
#                    burnin = 10000, 
#                    thin = 100, 
#                    data = Female.HighAdj)

#saveRDS(modelHI.DurSubAdj, "modelHI.DurSubAdj")
modelHI.NovDur <- readRDS("modelHI.NovDur")


#  subset Low Diet

modelLOW.NovDur <- readRDS("modelLOW.NovDur")


#modelLOW.NovDur <- MCMCglmm(cbind(LogTotalDist, LogNovZone_Duration, LogSocial_Duration) ~ EndMass +  trait-1, 
#                     random = ~us(trait):LizID, 
#                     rcov= ~us(trait):units, 
#                     family = rep("gaussian", 3), 
#                     prior = prior, 
#                     nitt = 70000, 
#                     burnin=10000, 
#                     thin = 100, 
#                     data = Female.Low)

#saveRDS(modelLOW.NovDurSub, "modelLOW.NovDurSub")
modelLOW.NovDur <- readRDS("modelLOW.NovDur")

summary(modelLOW.NovDur)

plot(modelLOW.NovDur$VCV)
autocorr(modelLOW.NovDur$VCV)
posterior.mode(modelLOW.NovDur$VCV)
HPDinterval(modelLOW.NovDur$VCV)

posterior.mode(modelLOW.NovDur$Sol)
HPDinterval(modelLOW.NovDur$Sol)


#modelLOW.NovDurSub <- MCMCglmm(cbind(LogTotalDist, LogNovZone_Duration, LogSocial_Duration) ~ EndMass +  trait-1, 
#                     random = ~us(trait):LizID, 
#                     rcov= ~us(trait):units, 
#                     family = rep("gaussian", 3), 
#                     prior = prior, 
#                     nitt = 70000, 
#                     burnin=10000, 
#                     thin = 100, 
#                     data = SubFemale.Low)

#saveRDS(modelLOW.NovDurSub, "modelLOW.NovDurSub")
modelLOW.NovDur <- readRDS("modelLOW.NovDur")

#modelLOW.DurSubAdj <- MCMCglmm(cbind(LogTotalDist, LogAdjNovDuration, LogAdjSocDuration) ~ EndMass +  trait-1, 
#                     random = ~us(trait):LizID, 
#                     rcov= ~us(trait):units, 
#                     family = rep("gaussian", 3), 
#                     prior = prior, 
#                     nitt = 70000, 
#                     burnin=10000, 
#                     thin = 100, 
#                     data = SubFemale.Low)

saveRDS(modelLOW.DurSubAdj, "modelLOW.DurSubAdj")
modelLOW.NovDur <- readRDS("modelLOW.NovDur")
# All Female Data

Female.All <- filter(DVR.Master, Sex == "F")[varibs.need] # Contain NAs in cbind(varibs) 
str(DVR.Female) #2580 obs

#modelALL.NovDur <- MCMCglmm(cbind(TotalDist, NovZone_Duration, Social_Duration) ~ Trt + EndMass +  trait-1, 
#                     random = ~us(trait):LizID, 
#                     rcov= ~us(trait):units, 
#                     family = rep("gaussian", 3), 
#                     prior = prior, 
#                     nitt = 70000, 
#                     burnin=10000, 
#                     thin = 100, 
#                     data = DVR.Female)

#saveRDS(modelALL.NovDur, "modelALL.NovDur")
summary(modelALL.NovDur)


Female.All <- filter(DVR.Master, Sex == "F")[varibs.need] # Contain NAs in cbind(varibs) 
str(Female.NaOmit)
Female.NaOmit <- na.omit(Female.All) #1858 obvs

#modelALL.NaOm.NovDur <- MCMCglmm(cbind(TotalDist, NovZone_Duration, Social_Duration) ~ Trt + EndMass +  trait-1, 
#                     random = ~us(trait):LizID, 
#                     rcov= ~us(trait):units, 
#                     family = rep("gaussian", 3), 
#                     prior = prior, 
#                     nitt = 70000, 
#                     burnin=10000, 
#                     thin = 100, 
#                     data = Female.NaOmit)

#saveRDS(modelALL.NaOm.NovDur, "modelALL.NaOm.NovDur")

summary(modelALL.NaOm.NovDur)

#Scaled
#modelALL.ScNovDur <- MCMCglmm(cbind(scale(TotalDist), scale(NovZone_Duration), scale(Social_Duration)) ~ Trt + EndMass +  trait-1, 
#                            random = ~us(trait):LizID, 
#                            rcov= ~us(trait):units, 
#                            family = rep("gaussian", 3), 
#                            prior = prior, 
#                            nitt = 70000, 
#                            burnin=10000, 
#                            thin = 100, 
#                            data = DVR.Female)


#saveRDS(modelALL.ScNovDur, "modelALL.ScNovDur")
summary(modelALL)


#modelALL.NaOm.ScNovDur <- MCMCglmm(cbind(scale(TotalDist), scale(NovZone_Duration), scale(Social_Duration)) ~ Trt + EndMass +  trait-1, 
#                                   random = ~us(trait):LizID, 
#                                   rcov= ~us(trait):units, 
#                                   family = rep("gaussian", 3), 
#                                   prior = prior, 
#                                   nitt = 70000, 
#                                   burnin=10000, 
#                                   thin = 100, 
#                                   data = Female.NaOmit)

#saveRDS(modelALL.NaOm.ScNovDur, "modelALL.NaOm.ScNovDur")



##### Repeatability #####
    
    #example
loctions <- grep("TotalDist", colnames(modelHI$VCV))
activity <- modelHI$VCV[,loctions]
head(activity)

loctions2 <- grep("TotalDist:TotalDist", colnames(activity))
activity_r <- activity[,loctions2]

repeatability_ttdist <- activity_r[,1] / (activity_r[,1] + activity_r[,2])
posterior.mode(repeatability_ttdist)
HPDinterval(repeatability_ttdist)


    #high activity repeatability
H.Activity_Locations <- grep("TotalDist", colnames(modelHI$VCV))
H.Activity <- modelHI$VCV[,H.Activity_Locations]
head(H.Activity)

HRL.Activity <- grep("TotalDist:TotalDist", colnames(H.Activity))
HR.Activity <- H.Activity[,HRL.Activity]

HRepeatability_Activity <- HR.Activity[,1] / (HR.Activity[,1] + HR.Activity[,2])
posterior.mode(HRepeatability_Activity)
HPDinterval(HRepeatability_Activity)
 
    #high novel LatFirst repeatability
H.NovLatency_Locations <- grep("NovZone_LatFirst", colnames(modelHI$VCV))
H.NovLatency <- modelHI$VCV[,H.NovLatency_Locations]
head(H.NovLatency)

HRL.NovLatency <- grep("NovZone_LatFirst:NovZone_LatFirst", colnames(H.NovLatency))
HR.NovLatency <- H.NovLatency[,HRL.NovLatency]

HRepeatability_NovLatency <- HR.NovLatency[,1] / (HR.NovLatency[,1] + HR.NovLatency[,2])
posterior.mode(HRepeatability_NovLatency)
HPDinterval(HRepeatability_NovLatency)

  #high novel duration repeatability
H.NovDuration_Locations <- grep("NovZone_Duration", colnames(modelHI.NovDur$VCV))
H.NovDuration <- modelHI.NovDur$VCV[,H.NovDuration_Locations]
head(H.NovDuration)

HRL.NovDuration <- grep("NovZone_Duration:NovZone_Duration", colnames(H.NovDuration))
HR.NovDuration <- H.NovDuration[,HRL.NovDuration]

HRepeatability_NovDuration <- HR.NovDuration[,1] / (HR.NovDuration[,1] + HR.NovDuration[,2])
posterior.mode(HRepeatability_NovDuration)
HPDinterval(HRepeatability_NovDuration)

  #high social duration repeatability
H.Social_Locations <- grep("Social_Duration", colnames(modelHI$VCV))
H.Social <- modelHI$VCV[,H.Social_Locations]
head(H.Social)

HRL.Social <- grep("Social_Duration:Social_Duration", colnames(H.Social))
HR.Social <- H.Social[,HRL.Social]

HRepeatability_Social <- HR.Social[,1] / (HR.Social[,1] + HR.Social[,2])
posterior.mode(HRepeatability_Social)
HPDinterval(HRepeatability_Social)



  #low activity repeatability
L.Activity_Locations <- grep("TotalDist", colnames(modelLOW$VCV))
L.Activity <- modelLOW$VCV[,L.Activity_Locations]
head(L.Activity)

LRL.Activity <- grep("TotalDist:TotalDist", colnames(L.Activity))
LR.Activity <- L.Activity[,LRL.Activity]

LRepeatability_Activity <- LR.Activity[,1] / (LR.Activity[,1] + LR.Activity[,2])
posterior.mode(LRepeatability_Activity)
HPDinterval(LRepeatability_Activity)

  #low novel LatFirst repeatability
L.NovLatency_Locations <- grep("NovZone_LatFirst", colnames(modelLOW$VCV))
L.NovLatency <- modelLOW$VCV[,L.NovLatency_Locations]
head(L.NovLatency)

LRL.NovLatency <- grep("NovZone_LatFirst:NovZone_LatFirst", colnames(L.NovLatency))
LR.NovLatency <- L.NovLatency[,LRL.NovLatency]

LRepeatability_NovLatency <- LR.NovLatency[,1] / (LR.NovLatency[,1] + LR.NovLatency[,2])
posterior.mode(LRepeatability_NovLatency)
HPDinterval(LRepeatability_NovLatency)

  #low novel duration repeatability
L.NovDuration_Locations <- grep("NovZone_Duration", colnames(modelLOW.NovDur$VCV))
L.NovDuration <- modelLOW.NovDur$VCV[,L.NovDuration_Locations]
head(L.NovDuration)

LRL.NovDuration <- grep("NovZone_Duration:NovZone_Duration", colnames(L.NovDuration))
LR.NovDuration <- L.NovDuration[,LRL.NovDuration]

LRepeatability_NovDuration <- LR.NovDuration[,1] / (LR.NovDuration[,1] + LR.NovDuration[,2])
posterior.mode(LRepeatability_NovDuration)
HPDinterval(LRepeatability_NovDuration)

  #low social duration repeatability
L.Social_Locations <- grep("Social_Duration", colnames(modelLOW$VCV))
L.Social <- modelLOW$VCV[,L.Social_Locations]
head(L.Social)

LRL.Social <- grep("Social_Duration:Social_Duration", colnames(L.Social))
LR.Social <- L.Social[,HRL.Social]

LRepeatability_Social <- LR.Social[,1] / (LR.Social[,1] + LR.Social[,2])
posterior.mode(LRepeatability_Social)
HPDinterval(LRepeatability_Social)


##### Matrices #####

#example
#mat <-posterior.mode(modelHI$VCV)
#HPDinterval(modelHI$VCV)

#head(modelHI$VCV)
#Locations <- grep("LizID", names(mat))
#B <- mat[Locations]


#B_mat_cov <- matrix(B, nrow = 3, ncol = 3)
#B_mat_cor <- cov2cor(B_mat_cov)

#B.matrix <- matrices.Lat(B)

# B = posterior.mode
#matrices.Lat <- function(B, names = c("Activity", "Latency", "Social")){
  #B_mat_cov <- matrix(B, nrow = 3, ncol = 3)
  #B_mat_cor <- cov2cor(B_mat_cov)
  #colnames(B_mat_cov) <- colnames(B_mat_cor) <- names
  #rownames(B_mat_cov) <- rownames(B_mat_cor) <- names
  
  #return(list(cov = B_mat_cov, cor = B_mat_cor))
#}

LizID.Locations <- grep("LizID", names(mat))
B_VCV <- modelHI.Sub$VCV[,LizID.Locations]
Matrix <- apply(modelHI$VCV, 1, function(x) matrices.Lat(x))

#High
#matHI <-posterior.mode(modelHI$VCV)
#HPDinterval(modelHI$VCV)

#head(modelHI$VCV)
#B.HI <- mat[Locations]

#HI.matrix <- matrices.Lat(B.HI)

LizID.Locations <- grep("LizID", names(mat))
B_VCV <- modelHI.Sub$VCV[,LizID.Locations]
Matrix <- apply(modelHI$VCV, 1, function(x) matrices.Lat(x))


#High subset - Only the active individuals 
matHI.Sub <-posterior.mode(modelHI.Sub$VCV)
HPDinterval(modelHI.Sub$VCV)

head(modelHI.Sub$VCV)
B.HI.Sub <- matHI.Sub[Locations]

HI.Sub.matrix <- matrices.Lat(B.HI.Sub)

LizID.Locations <- grep("LizID", names(mat))
B_VCV <- modelHI.Sub$VCV[,LizID.Locations]
Matrix <- apply(modelHI$VCV, 1, function(x) matrices.Lat(x))

#High subset with adjusted values
matHI.AdjSub <-posterior.mode(modelHI.AdjSub$VCV)
HPDinterval(modelHI.AdjSub$VCV)

head(modelHI.AdjSub$VCV)
B.HI.AdjSub <- matHI.AdjSub[Locations]

HI.AdjSub.matrix <- matrices.Lat(B.HI.AdjSub)

LizID.Locations <- grep("LizID", names(mat))
B_VCV <- modelHI.Sub$VCV[,LizID.Locations]
Matrix <- apply(modelHI$VCV, 1, function(x) matrices,Lat(x))



  # Low
matLOW <-posterior.mode(modelLOW$VCV)
HPDinterval(modelLOW$VCV)

head(modelLOW$VCV)
B.LOW <- matLOW[Locations]

LOW.matrix <- matrices.Lat(B.LOW)

LizID.Locations <- grep("LizID", names(mat))
B_VCV <- modelHI.Sub$VCV[,LizID.Locations]
Matrix <- apply(modelHI$VCV, 1, function(x) matrices.Lat(x))


  # Low subset - Only active individuals
matLOW.Sub <-posterior.mode(modelLOW.Sub$VCV)
HPDinterval(modelLOW.Sub$VCV)

head(modelLOW.Sub$VCV)
B.LOW.Sub <- matLOW.Sub[Locations]

LOW.Sub.matrix <- matrices.Lat(B.LOW.Sub)

LizID.Locations <- grep("LizID", names(mat))
B_VCV <- modelHI.Sub$VCV[,LizID.Locations]
Matrix <- apply(modelHI$VCV, 1, function(x) matrices.Lat(x))

#Low subset with adjusted values
matLOW.AdjSub <-posterior.mode(modelLOW.AdjSub$VCV)
HPDinterval(modelLOW.AdjSub$VCV)

head(modelLOW.AdjSub$VCV)
B.LOW.AdjSub <- matLOW.AdjSub[Locations]

LOW.AdjSub.matrix <- matrices.Lat(B.LOW.AdjSub)

LizID.Locations <- grep("LizID", names(mat))
B_VCV <- modelHI.Sub$VCV[,LizID.Locations]
Matrix <- apply(modelHI$VCV, 1, function(x) matrices.Lat(x))

#Matrices with Novel duration instead  of Novel latency
matD <-posterior.mode(modelHI.NovDur$VCV)
HPDinterval(modelHI.NovDur$VCV)

head(modelHI.NovDur$VCV)
LocationsD <- grep("LizID", names(matD))
BD <- matD[LocationsD]


BD_mat_cov <- matrix(BD, nrow = 3, ncol = 3)
BD_mat_cor <- cov2cor(BD_mat_cov)

BD.matrix <- matrices.Dur(BD)

# B = posterior.mode
matrices.Dur <- function(B, names = c("Activity", "Novel", "Social")){
  B_mat_cov <- matrix(B, nrow = 3, ncol = 3)
  B_mat_cor <- cov2cor(B_mat_cov)
  colnames(B_mat_cov) <- colnames(B_mat_cor) <- names
  rownames(B_mat_cov) <- rownames(B_mat_cor) <- names
  
  return(list(cov = B_mat_cov, cor = B_mat_cor))
}

LizID.Locations <- grep("LizID", names(mat))
B_VCV <- modelHI.Sub$VCV[,LizID.Locations]
Matrix <- apply(modelHI$VCV, 1, function(x) matrices.Lat(x))

#High
matHID <-posterior.mode(modelHI.NovDur$VCV)
HPDinterval(modelHI.NovDur$VCV)

head(modelHI.NovDur$VCV)
B.HID <- matHID[LocationsD]

HID.matrix <- matrices.Dur(B.HID)

LizID.Locations <- grep("LizID", names(mat))
B_VCV <- modelHI.Sub$VCV[,LizID.Locations]
Matrix <- apply(modelHI$VCV, 1, function(x) matrices.Lat(x))


#High subset - Only the active individuals 
matHID.Sub <-posterior.mode(modelHI.NovDurSub$VCV)
HPDinterval(modelHI.NovDurSub$VCV)

head(modelHI.NovDurSub$VCV)
B.HID.Sub <- matHID.Sub[Locations]

HID.Sub.matrix <- matrices.Dur(B.HID.Sub)

LizID.Locations <- grep("LizID", names(mat))
B_VCV <- modelHI.Sub$VCV[,LizID.Locations]
Matrix <- apply(modelHI$VCV, 1, function(x) matrices.Lat(x))

#High subset with adjusted values
matHID.AdjSub <-posterior.mode(modelHI.DurSubAdj$VCV)
HPDinterval(modelHI.DurSubAdj$VCV)

head(modelHI.DurSubAdj$VCV)
B.HI.DurSubAdj <- matHID.AdjSub[Locations]

HID.AdjSub.matrix <- matrices.Lat(B.HI.DurSubAdj)

LizID.Locations <- grep("LizID", names(mat))
B_VCV <- modelHI.Sub$VCV[,LizID.Locations]
Matrix <- apply(modelHI$VCV, 1, function(x) matrices,Lat(x))



# Low
matLOW <-posterior.mode(modelLOW$VCV)
HPDinterval(modelLOW$VCV)

head(modelLOW$VCV)
B.LOW <- matLOW[Locations]

LOW.matrix <- matrices.Lat(B.LOW)

LizID.Locations <- grep("LizID", names(mat))
B_VCV <- modelHI.Sub$VCV[,LizID.Locations]
Matrix <- apply(modelHI$VCV, 1, function(x) matrices.Lat(x))


# Low subset - Only active individuals
matLOW.Sub <-posterior.mode(modelLOW.Sub$VCV)
HPDinterval(modelLOW.Sub$VCV)

head(modelLOW.Sub$VCV)
B.LOW.Sub <- matLOW.Sub[Locations]

LOW.Sub.matrix <- matrices.Lat(B.LOW.Sub)

LizID.Locations <- grep("LizID", names(mat))
B_VCV <- modelHI.Sub$VCV[,LizID.Locations]
Matrix <- apply(modelHI$VCV, 1, function(x) matrices.Lat(x))

#Low subset with adjusted values
matLOW.AdjSub <-posterior.mode(modelLOW.AdjSub$VCV)
HPDinterval(modelLOW.AdjSub$VCV)

head(modelLOW.AdjSub$VCV)
B.LOW.AdjSub <- matLOW.AdjSub[Locations]

LOW.AdjSub.matrix <- matrices.Lat(B.LOW.AdjSub)

LizID.Locations <- grep("LizID", names(mat))
B_VCV <- modelHI.Sub$VCV[,LizID.Locations]
Matrix <- apply(modelHI$VCV, 1, function(x) matrices.Lat(x))

#####NORMALITY#####
#Exploration - Total Distance
hist((Exploration$LogTotalDist + 0.5))  ;  shapiro.test(log(Exploration$TotalDist))

#Novel - Zone Duration
hist((Novel$LogNovZone_Duration)) ;  shapiro.test(log(Novel$NovZone_Duration))

#Novel - Latency to First
hist((Novel$NovZone_LatFirst)) ;  shapiro.test(Novel$NovZone_LatFirst)

#Social
hist((Social$LogSocial_Duration)) ;  shapiro.test(Social$Social_Duration)
hist((SC$Social_LatFirst)) ;  shapiro.test(Social$Social_LatFirst)
hist(log(Social$Asocial_Duration)) ;  shapiro.test(Social$Asocial_Duration)

SC <- filter(DVR.Female, Assay == "Social")

# correlation between Item and Zone duration
cor(Novel$NovItem_Duration, Novel$NovZone_Duration)
plot(log(Novel$NovItem_Duration), log(Novel$NovZone_Duration))



#####Outliers#####
Exploration$obs <- seq(1:nrow(Exploration))
ggplot(Exploration, aes(TotalDist, obs, label = obs)) +
  geom_point(colour = "white") +
  geom_text() +
  geom_vline(aes(xintercept = (mean(Exploration$TotalDist, na.rm = T) + (sd(Exploration$TotalDist, na.rm = T)*3)), color = "red"))+
  geom_vline(aes(xintercept = (mean(Exploration$TotalDist, na.rm = T) - (sd(Exploration$TotalDist, na.rm = T)*3)), color = "red"))


Novel$obs <- seq(1:nrow(Novel))
ggplot(Novel, aes(NovZone_Duration, obs, label = obs)) +
  geom_point(colour = "white") +
  geom_text() +
  geom_vline(aes(xintercept = (mean(Novel$NovZone_Duration, na.rm = T) + (sd(Novel$NovZone_Duration, na.rm = T)*3)), color = "red"))+
  geom_vline(aes(xintercept = (mean(Novel$NovZone_Duration, na.rm = T) - (sd(Novel$NovZone_Duration, na.rm = T)*3)), color = "red"))

ggplot(Novel, aes(NovZone_LatFirst, obs, label = obs)) +
  geom_point(colour = "white") +
  geom_text() +
  geom_vline(aes(xintercept = (mean(Novel$NovZone_LatFirst, na.rm = T) + (sd(Novel$NovZone_LatFirst, na.rm = T)*3)), color = "red")) +
  geom_vline(aes(xintercept = (mean(Novel$NovZone_LatFirst, na.rm = T) - (sd(Novel$NovZone_LatFirst, na.rm = T)*3)), color = "red"))



Social$obs <- seq(1:nrow(Social))
ggplot(Social, aes(Social_Duration, obs, label = obs)) +
  geom_point(colour = "white") +
  geom_text() +
  geom_vline(aes(xintercept = (mean(Social$Social_Duration, na.rm = T) + (sd(Social$Social_Duration, na.rm = T)*3)), color = "red"))+
  geom_vline(aes(xintercept = (mean(Social$Social_Duration, na.rm = T) - (sd(Social$Social_Duration, na.rm = T)*3)), color = "red"))



#####group and summarise by lizards#####

#Exploration
Exploration_by_lizID <- Exploration %>% 
  group_by(LizID, Trt) %>% 
  summarise(
    "mean_TotalDist" = mean(TotalDist),
    "mean_Hide_Duration" = mean(Hide_Duration)
  ) 

ExplorationFiltered <- Exploration %>% 
  group_by(LizID, Trt) %>% 
  summarise(
    "mean_LogTotalDist" = mean(log(TotalDist))
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


NovelFiltered <- Novel %>% 
  group_by(LizID, Trt) %>% 
  summarise(
    "mean_NovZone_LatFirst" = mean(NovZone_LatFirst),
    "mean_NovZone_Duration" = mean((NovZone_Duration)),
    "mean_LogNovZone_Duration" = mean(log((NovZone_Duration) + 1))
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

SocialFiltered <- Social %>% 
  group_by(LizID, Trt) %>% 
  summarise(
    "mean_Social_Duration" = mean(log(Social_Duration + 1)),
    "mean_Social_LatFirst" = mean(Social_LatFirst)
  )

