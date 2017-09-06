#####PACKAGES#####

install.packages("ade4")
library("ade4")

install.packages("MCMCglmm")
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

  #Filter by Female
DVR.Female <- filter(DVR.Master, Sex == "F")
DVR.Female <- read.csv("DVR.Female.csv")

  #remove NA
DVR.Master <- filter(DVR.Master, !is.na(Arena_Duration))

  #Filter by assay
Exploration <- filter(DVR.Female, Assay == "Exploration")
nrow(distinct(DVR.Female, LizID))
Novel <- filter(DVR.Female, Assay == "Novel")
Social <- filter(DVR.Female, Assay == "Social") 

  #Filter by treatment
Female.Low <- filter(DVR.Female, Trt == "Low")[varibs.need] # Contain NAs in cbind(varibs) 
str(Female.Low) 
Female.Low.na.omit <- na.omit(Female.Low) 

#####NORMALITY#####
#Exploration - Total Distance
hist(log(Exploration$TotalDist + 0.5))  ;  shapiro.test(log(Exploration$TotalDist))

#Novel - Zone Duration
hist(log(Novel$NovZone_Duration)) ;  shapiro.test(log(Novel$NovZone_Duration))

#Novel - Latency to First
hist((Novel$NovItem_LatFirst)) ;  shapiro.test(Novel$NovItem_LatFirst)

#Social
hist(log(Social$Social_Duration)) ;  shapiro.test(Social$Social_Duration)
hist((Social$Social_LatFirst)) ;  shapiro.test(Social$Social_LatFirst)
hist(log(Social$Asocial_Duration)) ;  shapiro.test(Social$Asocial_Duration)


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


#High Female
HighFemale <- filter(DVR.Female, Trt == "High")
LogHighFemale <- HighFemale    
LogHighFemale[, 10:49] <- log((LogHighFemale[13:39]) + 1)
rm(datTest)

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

    #####TotalDist, NovZone_LatFirst, NovZone_Duration#####

# subeset High diet
varibs.need <- c("LizID", "Trt","TotalDist", "NovZone_LatFirst", "NovZone_Duration", "Social_Duration", "EndMass")
Female.High <- filter(DVR.Female, Trt == "High")[varibs.need] # Contain NAs in cbind(varibs) 
str(Female.High) #1302 obs
Female.High.na.omit <- na.omit(Female.High) #Excluding NAs 961 obs


#priors
prior <- list(R = list(V = diag(3), nu = 0.01), 
              G = list(G1= list(V = diag(3), nu = 0.01)))

#modelHI <- MCMCglmm(cbind(TotalDist, NovZone_LatFirst, Social_Duration) ~ EndMass + trait-1, 
#                   random = ~us(trait):LizID, 
#                    rcov= ~us(trait):units, 
#                    family = rep("gaussian", 3), 
#                    prior = prior, 
#                    nitt = 70000, 
#                    burnin = 10000, 
#                    thin = 100, 
#                    data = Female.High)

#saveRDS(modelHI, "modelHI")
modelHI <- readRDS("modelHI")
plot(modelHI$VCV)
autocorr(modelHI$VCV)

summary(modelHI)

#modelHI.NaOmit <- MCMCglmm(cbind(TotalDist, NovZone_LatFirst, Social_Duration) ~ EndMass + trait-1, 
#                   random = ~us(trait):LizID, 
#                    rcov= ~us(trait):units, 
#                    family = rep("gaussian", 3), 
#                    prior = prior, 
#                    nitt = 70000, 
#                    burnin = 10000, 
#                    thin = 100, 
#                    data = Female.High.na.omit)

#saveRDS(modelHI.NaOmit, "modelHI.NaOmit")
modelHI.NaOmit <- readRDS("modelHI.NaOmit")
plot(modelHI.NaOmit$VCV)
autocorr(modelHI$VCV)

summary(modelHI)
posterior.mode(modelHI$VCV)
HPDinterval(modelHI$VCV)

posterior.mode(modelHI$Sol)
HPDinterval(modelHI$Sol)

      #scaled
#modelHI.Scaled <- MCMCglmm(cbind(scale(TotalDist), scale(NovZone_LatFirst), scale(Social_Duration)) ~ EndMass + trait-1, 
#                   random = ~us(trait):LizID, 
#                    rcov= ~us(trait):units, 
#                    family = rep("gaussian", 3), 
#                    prior = prior, 
#                    nitt = 70000, 
#                    burnin = 10000, 
#                    thin = 100, 
#                    data = Female.High)

#saveRDS(modelHI.Scaled, "modelHI.Scaled")
modelHI.Scaled <- readRDS("modelHI.Scaled")
summary(modelHI.Scaled)
plot(modelHI.Scaled$VCV)
autocorr(modelHI.Scaled$VCV)
posterior.mode(modelHI.Scaled$VCV)
HPDinterval(modelHI.Scaled$VCV)

posterior.mode(modelHI$Sol)
HPDinterval(modelHI$Sol)

?poster



#modelHI.NaOmit.Scaled <- MCMCglmm(cbind(scale(TotalDist), scale(NovZone_LatFirst), scale(Social_Duration)) ~ EndMass + trait-1, 
#                   random = ~us(trait):LizID, 
#                    rcov= ~us(trait):units, 
#                    family = rep("gaussian", 3), 
#                    prior = prior, 
#                    nitt = 70000, 
#                    burnin = 10000, 
#                    thin = 100, 
#                    data = Female.High.na.omit)

#saveRDS(modelHI.NaOmit.Scaled, "modelHI.NaOmit.Scaled")
modelHI.NaOmit.Scaled <- readRDS("modelHI.NaOmit.Scaled")
plot(modelHI.NaOmit.Scaled$VCV)
autocorr(modelHI.NaOmit.Scaled$VCV)
summary(modelHI.NaOmit.Scaled)
posterior.mode(modelHI.NaOmit.Scaled$VCV)
HPDinterval(modelHI.NaOmit.Scaled$VCV)

posterior.mode(modelHI.NaOmit.Scaled$Sol)
HPDinterval(modelHI.NaOmit.Scaled$Sol)


#  subset Low Diet

Female.Low <- filter(DVR.Female, Trt == "Low")[varibs.need] # Contain NAs in cbind(varibs) 
str(Female.Low) 
Female.Low.na.omit <- na.omit(Female.Low) 

names(DVR.Female)

#modelLOW <- MCMCglmm(cbind(TotalDist, NovZone_LatFirst, Social_Duration) ~ EndMass +  trait-1, 
#                     random = ~us(trait):LizID, 
#                     rcov= ~us(trait):units, 
#                     family = rep("gaussian", 3), 
#                     prior = prior, 
#                     nitt = 70000, 
#                     burnin=10000, 
#                     thin = 100, 
#                     data = Female.Low)

#saveRDS(modelLOW, "modelLOW")
modelLOW <- readRDS("modelLOW")
plot(modelLOW$VCV)
autocorr(modelLOW$VCV)
summary(modelLOW)
posterior.mode(modelLOW$VCV)
HPDinterval(modelLOW$VCV)

posterior.mode(modelLOW$Sol)
HPDinterval(modelLOW$Sol)


#modelLOW.NaOmit <- MCMCglmm(cbind(TotalDist, NovZone_LatFirst, Social_Duration) ~ EndMass +  trait-1, 
#                     random = ~us(trait):LizID, 
#                     rcov= ~us(trait):units, 
#                     family = rep("gaussian", 3), 
#                     prior = prior, 
#                     nitt = 70000, 
#                     burnin=10000, 
#                     thin = 100, 
#                     data = Female.Low.na.omit)

#saveRDS(modelLOW.NaOmit, "modelLOW.Na.omit")
modelLow.NaOmit <- readRDS("modelLOW.Na.omit")
plot(modelLow.NaOmit$VCV)
autocorr(modelLow.NaOmit$VCV)
summary(modelLow.NaOmit)
posterior.mode(modelLow.NaOmit$VCV)
HPDinterval(modelLow.NaOmit$VCV)

posterior.mode(modelLow.NaOmit$Sol)
HPDinterval(modelLow.NaOmit$Sol)


#modelLOW.Scaled <- MCMCglmm(cbind(scale(TotalDist), scale(NovZone_LatFirst), scale(Social_Duration)) ~ EndMass +  trait-1, 
#                     random = ~us(trait):LizID, 
#                     rcov= ~us(trait):units, 
#                     family = rep("gaussian", 3), 
#                     prior = prior, 
#                     nitt = 70000, 
#                     burnin=10000, 
#                     thin = 100, 
#                     data = Female.Low)

#saveRDS(modelLOW.Scaled, "modelLOW.Scaled")
modelLOW.Scaled <- readRDS("modelLOW.Scaled")
plot(modelLOW.Scaled$VCV)
autocorr(modelLOW.Scaled$VCV)
summary(modelLOW.Scaled)
posterior.mode(modelLOW.Scaled$VCV)
HPDinterval(modelLOW.Scaled$VCV)

posterior.mode(modelLOW.Scaled$Sol)
HPDinterval(modelLOW.Scaled$Sol)


#modelLOW.Scaled.NaOmit <- MCMCglmm(cbind(scale(TotalDist), scale(NovZone_LatFirst), scale(Social_Duration)) ~ EndMass +  trait-1, 
#                     random = ~us(trait):LizID, 
#                     rcov= ~us(trait):units, 
#                     family = rep("gaussian", 3), 
#                     prior = prior, 
#                     nitt = 70000, 
#                     burnin=10000, 
#                     thin = 100, 
#                     data = Female.Low.na.omit)

saveRDS(modelLOW.Scaled.NaOmit, "modelLOW.Scaled.NaOmit")
modelLOW.Scaled.NaOmit <- readRDS("modelLOW.Scaled.NaOmit")
plot(modelLOW.Scaled.NaOmit$VCV)
autocorr(modelLOW.Scaled.NaOmit$VCV)
summary(modelLOW.Scaled.NaOmit)
posterior.mode(modelLOW.Scaled.NaOmit$VCV)
HPDinterval(modelLOW.Scaled.NaOmit$VCV)

posterior.mode(modelLOW.Scaled.NaOmit$Sol)
HPDinterval(modelLOW.Scaled.NaOmit$Sol)



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

#modelALL.NaOmit <- MCMCglmm(cbind(TotalDist, NovZone_LatFirst, Social_Duration) ~ Trt + EndMass +  trait-1, 
#                     random = ~us(trait):LizID, 
#                     rcov= ~us(trait):units, 
#                     family = rep("gaussian", 3), 
#                     prior = prior, 
#                     nitt = 70000, 
#                     burnin=10000, 
#                     thin = 100, 
#                     data = Female.NaOmit)

#saveRDS(modelALL.NaOmit, "modelALL.NaOmit")
modelALL.NaOmit <- readRDS("modelALL.NaOmit")
plot(modelALL.NaOmit$VCV)
autocorr(modelALL.NaOmit$VCV)
summary(modelALL.NaOmit)
posterior.mode(modelALL.NaOmit$VCV)
HPDinterval(modelALL.NaOmit$VCV)

posterior.mode(modelALL.NaOmit$Sol)
HPDinterval(modelALL.NaOmit$Sol)


    #Scaled
#modelALL.Scaled <- MCMCglmm(cbind(scale(TotalDist), scale(NovZone_LatFirst), scale(Social_Duration)) ~ Trt + EndMass +  trait-1, 
#                     random = ~us(trait):LizID, 
#                     rcov= ~us(trait):units, 
#                     family = rep("gaussian", 3), 
#                     prior = prior, 
#                     nitt = 70000, 
#                     burnin=10000, 
#                     thin = 100, 
#                     data = DVR.Female)

   
#saveRDS(modelALL.Scaled, "modelALL.Scaled")
modelALL.Scaled <- readRDS("modelALL.Scaled")
plot(modelALL.Scaled$VCV)
autocorr(modelALL.Scaled$VCV)
summary(modelALL.Scaled)
posterior.mode(modelALL.Scaled$VCV)
HPDinterval(modelALL.Scaled$VCV)

posterior.mode(modelALL.Scaled$Sol)
HPDinterval(modelALL.Scaled$Sol)


#modelALL.NaOmit.Scaled <- MCMCglmm(cbind(scale(TotalDist), scale(NovZone_LatFirst), scale(Social_Duration)) ~ Trt + EndMass +  trait-1, 
#                     random = ~us(trait):LizID, 
#                     rcov= ~us(trait):units, 
#                     family = rep("gaussian", 3), 
#                     prior = prior, 
#                     nitt = 70000, 
#                     burnin=10000, 
#                     thin = 100, 
#                     data = Female.NaOmit)

#saveRDS(modelALL.NaOmit.Scaled, "modelALL.NaOmit.Scaled")
modelALL.NaOmit.Scaled <- readRDS("modelALL.NaOmit.Scaled")
plot(modelALL.NaOmit.Scaled$VCV)
autocorr(modelALL.NaOmit.Scaled$VCV)
summary(modelALL.NaOmit.Scaled)
posterior.mode(modelALL.NaOmit.Scaled$VCV)
HPDinterval(modelALL.NaOmit.Scaled$VCV)

posterior.mode(modelALL.NaOmit.Scaled$Sol)
HPDinterval(modelALL.NaOmit.Scaled$Sol)

##### TotalDist, NovZone_Duration, Social_Duration #####

# subeset High diet
varibs.need <- c("LizID", "Trt","TotalDist", "NovZone_LatFirst", "NovZone_Duration", "Social_Duration", "EndMass")
Female.High <- filter(DVR.Female, Trt == "High")[varibs.need] # Contain NAs in cbind(varibs) 
str(Female.High) #1302 obs
Female.High.na.omit <- na.omit(Female.High) #Excluding NAs 961 obs


#priors
prior <- list(R = list(V = diag(3), nu = 0.01), 
              G = list(G1= list(V = diag(3), nu = 0.01)))

#modelHI.NovDur <- MCMCglmm(cbind(TotalDist, NovZone_Duration, Social_Duration) ~ EndMass + trait-1, 
#                   random = ~us(trait):LizID, 
#                    rcov= ~us(trait):units, 
#                    family = rep("gaussian", 3), 
#                    prior = prior, 
#                    nitt = 70000, 
#                    burnin = 10000, 
#                    thin = 100, 
#                    data = Female.High)

#saveRDS(modelHI.NovDur, "modelHI.NovDur")
modelHI.NovDur <- readRDS("modelHI.NovDur")
plot(modelHI.NovDur$VCV)
autocorr(modelHI.NovDur$VCV)

summary(modelHI.NovDur)
posterior.mode(modelHI.NovDur$VCV)
HPDinterval(modelHI.NovDur$VCV)


posterior.mode(modelHI.NovDur$Sol)
HPDinterval(modelHI.NovDur$Sol)


#modelHI.NaOmit.NovDur <- MCMCglmm(cbind(TotalDist, NovZone_Duration, Social_Duration) ~ EndMass + trait-1, 
#                   random = ~us(trait):LizID, 
#                    rcov= ~us(trait):units, 
#                    family = rep("gaussian", 3), 
#                    prior = prior, 
#                    nitt = 70000, 
#                    burnin = 10000, 
#                    thin = 100, 
#                    data = Female.High.na.omit)

#saveRDS(modelHI.NaOmit.NovDur, "modelHI.NaOmit.NovDur")
modelHI.NaOmit.NovDur <- readRDS("modelHI.NaOmit.NovDur")
plot(modelHI.NaOmit.NovDur$VCV)
autocorr(modeHI.NaOmit.NovDur$VCV)
summary(modelHI.NaOmit.NovDur)
posterior.mode(modelHI.NaOmit.NovDur$VCV)
HPDinterval(modelHI.NaOmit.NovDur$VCV)

posterior.mode(modelHI.NaOmit.NovDur$Sol)
HPDinterval(modelHI.NaOmit.NovDur$Sol)

  #scaled
#modelHI.ScNovDur <- MCMCglmm(cbind(scale(TotalDist), scale(NovZone_Duration), scale(Social_Duration)) ~ EndMass + trait-1, 
#                           random = ~us(trait):LizID, 
#                           rcov= ~us(trait):units, 
#                           family = rep("gaussian", 3), 
#                           prior = prior, 
#                           nitt = 70000, 
#                           burnin = 10000, 
#                           thin = 100, 
#                           data = Female.High)

#saveRDS(modelHI.ScNovDur, "modelHI.ScNovDur")
modelHI.ScNovDur <- readRDS("modelHI.ScNovDur")
modelHI.ScNovDur <- readRDS("modelHI.ScNovDur")
plot(modelHI.ScNovDur$VCV)
autocorr(modelHI.ScNovDur$VCV)
summary(modelHI.ScNovDur)
posterior.mode(modelHI.ScNovDur$VCV)
HPDinterval(modelHI.ScNovDur$VCV)

posterior.mode(modelHI.ScNovDur$Sol)
HPDinterval(modelHI.ScNovDur$Sol)



#modelHI.NaOm.ScNovDur <- MCMCglmm(cbind(scale(TotalDist), scale(NovZone_Duration), scale(Social_Duration)) ~ EndMass + trait-1, 
#                   random = ~us(trait):LizID, 
#                    rcov= ~us(trait):units, 
#                    family = rep("gaussian", 3), 
#                    prior = prior, 
#                    nitt = 70000, 
#                    burnin = 10000, 
#                    thin = 100, 
#                    data = Female.High.na.omit)

#saveRDS(modelHI.NaOm.ScNovDur, "modelHI.NaOm.ScNovDur")
modelHI.NaOm.ScNovDur <- readRDS("modelHI.NaOm.ScNovDur")

summary(modelHI.NaOm.ScNovDur)

plot(modelHI.NaOm.ScNovDur$VCV)
autocorr(modelHI.NaOm.ScNovDur$VCV)
posterior.mode(modelHI.NaOm.ScNovDur$VCV)
HPDinterval(modelHI.NaOm.ScNovDur$VCV)

posterior.mode(modelHI.NaOm.ScNovDur$Sol)
HPDinterval(modelHI.NaOm.ScNovDur$Sol)

#  subset Low Diet

Female.Low <- filter(DVR.Female, Trt == "Low")[varibs.need] # Contain NAs in cbind(varibs) 
str(Female.Low) 
Female.Low.na.omit <- na.omit(Female.Low) 

#modelLOW.NovDur <- MCMCglmm(cbind(TotalDist, NovZone_Duration, Social_Duration) ~ EndMass +  trait-1, 
#                     random = ~us(trait):LizID, 
#                     rcov= ~us(trait):units, 
#                     family = rep("gaussian", 3), 
#                     prior = prior, 
#                     nitt = 70000, 
#                     burnin=10000, 
#                     thin = 100, 
#                     data = Female.Low)

#saveRDS(modelLOW.NovDur, "modelLOW.NovDur")
modelLOW.NovDur <- readRDS("modelLOW.NovDur")

summary(modelLOW.NovDur)

plot(modelLOW.NovDur$VCV)
autocorr(modelLOW.NovDur$VCV)
posterior.mode(modelLOW.NovDur$VCV)
HPDinterval(modelLOW.NovDur$VCV)

posterior.mode(modelLOW.NovDur$Sol)
HPDinterval(modelLOW.NovDur$Sol)

#modelLOW.NaOmit.NovDur <- MCMCglmm(cbind(TotalDist, NovZone_Duration, Social_Duration) ~ EndMass +  trait-1, 
#                     random = ~us(trait):LizID, 
#                     rcov= ~us(trait):units, 
#                     family = rep("gaussian", 3), 
#                     prior = prior, 
#                     nitt = 70000, 
#                     burnin=10000, 
#                     thin = 100, 
#                     data = Female.Low.na.omit)

#saveRDS(modelLOW.NaOmit.NovDur, "modelLOW.Na.omit.NovDur")
modelLOW.NaOmit.NovDur <- readRDS("modelLOW.Na.omit.NovDur")

summary(modelLOW.NaOmit.NovDur)

plot(modelLOW.NaOmit.NovDur$VCV)
autocorr(modelLOW.NaOmit.NovDur$VCV)
posterior.mode(modelLOW.NaOmit.NovDur$VCV)
HPDinterval(modelLOW.NaOmit.NovDur$VCV)

posterior.mode(modelLOW.NaOmit.NovDur$Sol)
HPDinterval(modelLOW.NaOmit.NovDur$Sol)

#modelLOW.ScNovDur <- MCMCglmm(cbind(scale(TotalDist), scale(NovZone_Duration), scale(Social_Duration)) ~ EndMass +  trait-1, 
#                     random = ~us(trait):LizID, 
#                     rcov= ~us(trait):units, 
#                     family = rep("gaussian", 3), 
#                     prior = prior, 
#                     nitt = 70000, 
#                     burnin=10000, 
#                     thin = 100, 
#                     data = Female.Low)

#saveRDS(modelLOW.ScNovDur, "modelLOW.ScNovDur")
modelLOW.ScNovDur <- readRDS("modelLOW.ScNovDur")

summary(modelLOW.ScNovDur)

plot(modelLOW.ScNovDur$VCV)
autocorr(modelLOW.ScNovDur$VCV)
posterior.mode(modelLOW.ScNovDur$VCV)
HPDinterval(modelLOW.ScNovDur$VCV)

posterior.mode(modelLOW.ScNovDur$Sol)
HPDinterval(modelLOW.ScNovDur$Sol)


modelLOW.NaOm.ScNovDur <- MCMCglmm(cbind(scale(TotalDist), scale(NovZone_Duration), scale(Social_Duration)) ~ EndMass +  trait-1, 
                     random = ~us(trait):LizID, 
                     rcov= ~us(trait):units, 
                     family = rep("gaussian", 3), 
                     prior = prior, 
                     nitt = 70000, 
                     burnin=10000, 
                     thin = 100, 
                     data = Female.Low.na.omit)

saveRDS(modelLOW.Scaled.NaOmit, "modelLOW.Scaled.NaOmit")
modelLOW.Scaled.NaOmit <- readRDS("modelLOW.Scaled.NaOmit")
plot(modelLOW.Scaled.NaOmit$VCV)
autocorr(modelLOW.Scaled.NaOmit$VCV)
summary(modelLOW.Scaled.NaOmit)
posterior.mode(modelLOW.Scaled.NaOmit$VCV)
HPDinterval(modelLOW.Scaled.NaOmit$VCV)

posterior.mode(modelLOW.Scaled.NaOmit$Sol)
HPDinterval(modelLOW.Scaled.NaOmit$Sol)



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

mat <-posterior.mode(modelHI$VCV)
HPDinterval(modelHI$VCV)

head(modelHI$VCV)
loctions3 <- grep("LizID", names(mat))
B <- mat[loctions3]


B_mat_cov <- matrix(B, nrow = 3, ncol = 3)
B_mat_cor <- cov2cor(B_mat_cov)

matrices(B)

# B = posterior.mode
matrices <- function(B, names = c("activity", "novel", "social")){
  B_mat_cov <- matrix(B, nrow = 3, ncol = 3)
  B_mat_cor <- cov2cor(B_mat_cov)
  colnames(B_mat_cov) <- colnames(B_mat_cor) <- names
  rownames(B_mat_cov) <- rownames(B_mat_cor) <- names
  
  return(list(cov = B_mat_cov, cor = B_mat_cor))
}

LizID.Locations <- grep("LizID", names(mat))
B_VCV <- modelHI$VCV[,LizID.Locations]
test <- apply(modelHI$VCV, 1, function(x) matrices(x))



