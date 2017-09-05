

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
DVRAll = merge(Merged, DVR3, all = TRUE)
DVR.Master = left_join(DVRAll, BodySize, by = NULL)
rm(LogHighFemale)

#####remove na#####
DVR.Master <- filter(DVR.Master, !is.na(Arena_Duration))
DVR.Female <- filter(DVR.Master, Sex == "F")
#DVR.Female <- read.csv("DVR.Female.csv")

Exploration <- filter(DVR.Female, Assay == "Exploration")
nrow(distinct(DVR.Female, LizID))


Novel <- filter(DVR.Female, Assay == "Novel")
Social <- filter(DVR.Female, Assay == "Social") 

#####NORMALITY#####
#Exploration - Total Distance
hist(log(Exploration$TotalDist))  ;  shapiro.test(log(Exploration$TotalDist))

#Novel - Zone Duration
hist(log(Novel$NovZone_Duration)) ;  shapiro.test(log(Novel$NovZone_Duration))

#Novel - Latency to First
hist((Novel$NovItem_LatFirst)) ;  shapiro.test(Novel$NovItem_LatFirst)

#Social
hist(log(Social$Social_Duration)) ;  shapiro.test(Social$Social_Duration)
hist((Social$Social_LatFirst)) ;  shapiro.test(Social$Social_LatFirst)

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

install.packages("MCMCglmm")
library("MCMCglmm")
    
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
posterior.mode(modelHI$VCV)
HPDinterval(modelHI$VCV)

scale(variable)

posterior.mode(modelHI$Sol)
HPDinterval(modelHI$Sol)


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
modelHI <- readRDS("modelHI")

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
modelHI <- readRDS("modelHI")
plot(modelHI$VCV)
autocorr(modelHI$VCV)

summary(modelHI)
posterior.mode(modelHI$VCV)
HPDinterval(modelHI$VCV)

scale(variable)

posterior.mode(modelHI$Sol)
HPDinterval(modelHI$Sol)


#  subset Low Diet

Female.Low <- filter(DVR.Female, Trt == "Low")[varibs.need] # Contain NAs in cbind(varibs) 
str(Female.Low) 
Female.Low.na.omit <- na.omit(Female.Low) 

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
modelLow.NaOmit <- readRDS("modelLOW.NaOmit")


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

names(Female.Low)
summary(modelLOW)
posterior.mode(modelHI$VCV)


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
summary(modelALL)


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

summary(modelALL)
    
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
summary(modelALL)


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
modelHI <- readRDS("modelHI.NovDur")
plot(modelHI$VCV)
autocorr(modelHI$VCV)

summary(modelHI)
posterior.mode(modelHI$VCV)
HPDinterval(modelHI$VCV)

scale(variable)

posterior.mode(modelHI$Sol)
HPDinterval(modelHI$Sol)


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
modelHI.Scaled <- readRDS("modelHI.Scaled")
summary(modelHI.Scaled)




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
plot(modelHI$VCV)
autocorr(modelHI$VCV)

summary(modelHI)
posterior.mode(modelHI$VCV)
HPDinterval(modelHI$VCV)

scale(variable)

posterior.mode(modelHI$Sol)
HPDinterval(modelHI$Sol)


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
modelLOW <- readRDS("modelLOW.NovDur")


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
modelLOW.NaOmit.NovDur <- readRDS("modelLOW.NaOmit.NovDur")


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
modelLOW.Scaled <- readRDS("modelLOW.ScNovDur")

summary(modelLOW.ScNovDur)
posterior.mode(modelHI$VCV)


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
