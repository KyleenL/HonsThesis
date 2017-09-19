setwd("~/Documents/Honours/HonsThesis")

#####DATA#####
DVR1=read.csv("DVR1Data.csv")
DVR2=read.csv("DVR2Data.csv")
DVR3=read.csv("DVR3Data.csv")
BodySize=read.csv("BodySize.csv")

#Merge data 
library(dplyr)
Merged = merge(DVR1, DVR2, all=TRUE)
DVRAll = merge(Merged, DVR3, all = TRUE)
DVR.Master = left_join(DVRAll, BodySize, by = NULL)

#remove NA
DVR.Master <- filter(DVR.Master, !is.na(Arena_Duration))

#Filter by Female
DVR.Female <- filter(DVR.Master, Sex == "F")

#Filter by assay
EV <- c("LizID", "ChDate", "Trt","TotalDist","EndMass", "NovZone_Duration", "Social_Duration")
Exploration <- filter(DVR.Female, Assay == "Exploration")[EV]
Exploration$LogTotalDist <- log(Exploration$TotalDist + 0.5)
Exploration$CatDist <- ifelse(Exploration$TotalDist == 0, 0, 1)
Exploration$ExNovDuration <- Exploration$NovZone_Duration
Exploration$NovZone_Duration <- NULL
Exploration$ExSocDuration <- Exploration$Social_Duration
Exploration$Social_Duration <- NULL


NV <- c("LizID", "ChDate", "Trt","NovZone_LatFirst", "NovZone_Duration", "EndMass")
Novel <- filter(DVR.Female, Assay == "Novel")[NV]
Novel$LogNovZone_Duration <- log(Novel$NovZone_Duration + 0.5)     # log transforms original duration
Novel$CatNovLat <- ifelse(is.na(Novel$NovZone_LatFirst), 0, 1)  # turns latency into categorical data in a new column
Novel$NovZone_LatFirst <- ifelse(is.na(Novel$NovZone_LatFirst), 1200, Novel$NovZone_LatFirst) #turns all NAs in latency to the maximum time (1200sec)
Novel$CatNovDuration <- ifelse(Novel$NovZone_Duration == 0, 0, 1) # turns Novel duration into categorical data


SV <- c("LizID", "ChDate", "Trt","Social_Duration", "EndMass")
Social <- filter(DVR.Female, Assay == "Social")[SV]
Social$LogSocial_Duration <- log(Social$Social_Duration + 0.05) # log transforms original social duration data
Social$CatSocial <- ifelse(Social$Social_Duration == 0, 0, 1) # categorical data for social duration

EN = left_join(Novel, Exploration, by = NULL)
Female = left_join(EN, Social, by = NULL)

Female$int <- interaction(Female$CatNovLat, Female$CatNovDuration, Female$CatDist, Female$CatSocial) #adds new column with all categorical data
FemaleSubset <- subset(Female, Female$int == "1.1.1.1")     #446 obs


##### Bayesian Approach #####
library(MCMCglmm)

prior <- list(R = list(V = diag(3), nu = 0.01), 
              G = list(G1= list(V = diag(3), nu = 0.01)))

## Original Values
HighFemale <- filter(FemaleSubset, Trt == "High")   #subset data, 255 obs

#modelHighSub <- MCMCglmm(cbind(LogTotalDist, NovZone_LatFirst, LogSocial_Duration) ~ EndMass + trait-1, 
#                   random = ~us(trait):LizID, 
#                    rcov= ~us(trait):units, 
#                    family = rep("gaussian", 3), 
#                    prior = prior, 
#                    nitt = 70000, 
#                    burnin = 10000, 
#                    thin = 100, 
#                    data = HighFemale)


#saveRDS(modelHighSub, "modelHighSub") # model of high subset data
modelHighSub <- readRDS("modelHighSub")


LowFemale <- filter(FemaleSubset, Trt == "Low")
#modelLowSub <- MCMCglmm(cbind(LogTotalDist, NovZone_LatFirst, LogSocial_Duration) ~ EndMass + trait-1, 
#                   random = ~us(trait):LizID, 
#                    rcov= ~us(trait):units, 
#                    family = rep("gaussian", 3), 
#                    prior = prior, 
#                    nitt = 70000, 
#                    burnin = 10000, 
#                    thin = 100, 
#                    data = LowFemale)


#saveRDS(modelLowSub, "modelLowSub")
modelLowSub <- readRDS("modelLowSub")

#modelHighSubD <- MCMCglmm(cbind(LogTotalDist, LogNovZone_Duration, LogSocial_Duration) ~ EndMass + trait-1, 
#                   random = ~us(trait):LizID, 
#                    rcov= ~us(trait):units, 
#                    family = rep("gaussian", 3), 
#                    prior = prior, 
#                    nitt = 70000, 
#                    burnin = 10000, 
#                    thin = 100, 
#                    data = HighFemale)


#saveRDS(modelHighSubD, "modelHighSubD")
modelHighSubD <- readRDS("modelHighSubD")

#modelLowSubD <- MCMCglmm(cbind(LogTotalDist, LogNovZone_Duration, LogSocial_Duration) ~ EndMass + trait-1, 
#                          random = ~us(trait):LizID, 
#                          rcov= ~us(trait):units, 
#                          family = rep("gaussian", 3), 
#                          prior = prior, 
#                          nitt = 70000, 
#                          burnin = 10000, 
#                          thin = 100, 
#                          data = LowFemale)

#saveRDS(modelLowSubD, "modelLowSubD")
modelLowSubD <- readRDS("modelLowSubD")

    # All data

prior2 <- list(R = list(V = diag(4), nu = 0.01), 
              G = list(G1= list(V = diag(4), nu = 0.01)))

# uses FemaleSubset to add Treatment to the model

names(FemaleSubset)
#modelAllSub <- MCMCglmm(cbind(LogTotalDist, LogNovZone_Duration, NovZone_LatFirst, LogSocial_Duration) ~ Trt + EndMass +  trait-1, 
#                     random = ~us(trait):LizID, 
#                     rcov= ~us(trait):units, 
#                     family = rep("gaussian", 4), 
#                     prior = prior2, 
#                     nitt = 70000, 
#                     burnin=10000, 
#                     thin = 100, 
#                     data = FemaleSubset)

#saveRDS(modelAllSub, "modelAllSub")
modelAllSub <- readRDS("modelAllSub")

summary(modelAllSub)

# matrix for All data modeled with Treatment added
matAllSub <-posterior.mode(modelAllSub$VCV)
HPDinterval(modelAllSub$VCV)

head(modelAllSub$VCV)
LocationsAll <- grep("LizID", names(matAllSub))
BAllSub <- matAllSub[LocationsAll]


B_mat_covAll <- matrix(BAllSub, nrow = 4, ncol = 4)
B_mat_corAll <- cov2cor(B_mat_covAllT)


#B = posterior.mode
matricesT <- function(B, names = c("Activity", "Duration", "Latency", "Social")){
  B_mat_covAllT <- matrix(BAllSubT, nrow = 4, ncol = 4)
  B_mat_corAllT <- cov2cor(B_mat_covAllT)
  colnames(B_mat_covAllT) <- colnames(B_mat_corAllT) <- names
  rownames(B_mat_covAllT) <- rownames(B_mat_corAllT) <- names
  
  return(list(cov = B_mat_covAllT, cor = B_mat_corAllT))
}

MatrixAllT <- matricesT(BAllSubT)
HPDinterval(modelAllSubT$VCV)


  # matrix for within individual on FemaleSubset
LocationsAllW <- grep("unit", names(matAllSub))
BAllSubW <- matAllSub[LocationsAllW]


matricesW <- function(B, names = c("Activity", "Duration", "Latency", "Social")){
  B_mat_covAllT <- matrix(BAllSubW, nrow = 4, ncol = 4)
  B_mat_corAllT <- cov2cor(B_mat_covAllT)
  colnames(B_mat_covAllT) <- colnames(B_mat_corAllT) <- names
  rownames(B_mat_covAllT) <- rownames(B_mat_corAllT) <- names
  
  return(list(cov = B_mat_covAllT, cor = B_mat_corAllT))
}

matrixAllW <- matricesW(BAllSubW)

  # Uses FemaleSubset to add Treatment:trait to the model 
#modelAllSubT <- MCMCglmm(cbind(LogTotalDist, LogNovZone_Duration, NovZone_LatFirst, LogSocial_Duration) ~ Trt:trait + EndMass +  trait-1, 
#                        random = ~us(trait):LizID, 
#                        rcov= ~us(trait):units, 
#                        family = rep("gaussian", 4), 
#                        prior = prior2, 
#                        nitt = 70000, 
#                        burnin=10000, 
#                        thin = 100, 
#                        data = FemaleSubset)

#saveRDS(modelAllSubT, "modelAllSubT")
modelAllSubT <- readRDS("modelAllSubT")
summary(modelAllSubT)

plot(modelAllSubT$VCV)
autocorr(modelAllSubT$VCV)
posterior.mode(modelAllSubT$VCV)
HPDinterval(modelAllSubT$VCV)


  # matrix for All data modeled with Trt:trait
matAllSubT <-posterior.mode(modelAllSubT$VCV)
HPDinterval(modelAllSubT$VCV)

head(modelAllSubT$VCV)
LocationsAll <- grep("LizID", names(matAllSubT))
BAllSubT <- matAllSubT[LocationsAll]


B_mat_covAllT <- matrix(BAllSubT, nrow = 4, ncol = 4)
B_mat_corAllT <- cov2cor(B_mat_covAllT)


#B = posterior.mode
matricesT <- function(B, names = c("Activity", "Duration", "Latency", "Social")){
  B_mat_covAllT <- matrix(BAllSubT, nrow = 4, ncol = 4)
  B_mat_corAllT <- cov2cor(B_mat_covAllT)
  colnames(B_mat_covAllT) <- colnames(B_mat_corAllT) <- names
  rownames(B_mat_covAllT) <- rownames(B_mat_corAllT) <- names
  
  return(list(cov = B_mat_covAllT, cor = B_mat_corAllT))
}

MatrixAllT <- matricesT(BAllSubT)
HPDinterval(modelAllSubT$VCV)

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


#high activity repeatability in subset
H.Activity_Locations <- grep("LogTotalDist", colnames(modelHighSub$VCV))
H.Activity <- modelHighSub$VCV[,H.Activity_Locations]
head(H.Activity)

HRL.Activity <- grep("LogTotalDist:LogTotalDist", colnames(H.Activity))
HR.Activity <- H.Activity[,HRL.Activity]

HRepeatability_Activity <- HR.Activity[,1] / (HR.Activity[,1] + HR.Activity[,2])
posterior.mode(HRepeatability_Activity)
HPDinterval(HRepeatability_Activity)

#high novel LatFirst repeatability in subset
H.NovLatency_Locations <- grep("NovZone_LatFirst", colnames(modelHighSub$VCV))
H.NovLatency <- modelHighSub$VCV[,H.NovLatency_Locations]
head(H.NovLatency)

HRL.NovLatency <- grep("NovZone_LatFirst:NovZone_LatFirst", colnames(H.NovLatency))
HR.NovLatency <- H.NovLatency[,HRL.NovLatency]

HRepeatability_NovLatency <- HR.NovLatency[,1] / (HR.NovLatency[,1] + HR.NovLatency[,2])
posterior.mode(HRepeatability_NovLatency)
HPDinterval(HRepeatability_NovLatency)

#high novel duration repeatability
H.NovDuration_Locations <- grep("LogNovZone_Duration", colnames(modelHighSubD$VCV))
H.NovDuration <- modelHighSubD$VCV[,H.NovDuration_Locations]
head(H.NovDuration)

HRL.NovDuration <- grep("LogNovZone_Duration:LogNovZone_Duration", colnames(H.NovDuration))
HR.NovDuration <- H.NovDuration[,HRL.NovDuration]

HRepeatability_NovDuration <- HR.NovDuration[,1] / (HR.NovDuration[,1] + HR.NovDuration[,2])
posterior.mode(HRepeatability_NovDuration)
HPDinterval(HRepeatability_NovDuration)

#high social duration repeatability
H.Social_Locations <- grep("LogSocial_Duration", colnames(modelHighSub$VCV))
H.Social <- modelHighSub$VCV[,H.Social_Locations]
head(H.Social)

HRL.Social <- grep("LogSocial_Duration:LogSocial_Duration", colnames(H.Social))
HR.Social <- H.Social[,HRL.Social]

HRepeatability_Social <- HR.Social[,1] / (HR.Social[,1] + HR.Social[,2])
posterior.mode(HRepeatability_Social)
HPDinterval(HRepeatability_Social)



#low activity repeatability
L.Activity_Locations <- grep("LogTotalDist", colnames(modelLowSub$VCV))
L.Activity <- modelLowSub$VCV[,L.Activity_Locations]
head(L.Activity)

LRL.Activity <- grep("LogTotalDist:LogTotalDist", colnames(L.Activity))
LR.Activity <- L.Activity[,LRL.Activity]

LRepeatability_Activity <- LR.Activity[,1] / (LR.Activity[,1] + LR.Activity[,2])
posterior.mode(LRepeatability_Activity)
HPDinterval(LRepeatability_Activity)

#low novel LatFirst repeatability
L.NovLatency_Locations <- grep("NovZone_LatFirst", colnames(modelLowSub$VCV))
L.NovLatency <- modelLowSub$VCV[,L.NovLatency_Locations]
head(L.NovLatency)

LRL.NovLatency <- grep("NovZone_LatFirst:NovZone_LatFirst", colnames(L.NovLatency))
LR.NovLatency <- L.NovLatency[,LRL.NovLatency]

LRepeatability_NovLatency <- LR.NovLatency[,1] / (LR.NovLatency[,1] + LR.NovLatency[,2])
posterior.mode(LRepeatability_NovLatency)
HPDinterval(LRepeatability_NovLatency)

#low novel duration repeatability
L.NovDuration_Locations <- grep("LogNovZone_Duration", colnames(modelLowSubD$VCV))
L.NovDuration <- modelLowSubD$VCV[,L.NovDuration_Locations]
head(L.NovDuration)

LRL.NovDuration <- grep("LogNovZone_Duration:LogNovZone_Duration", colnames(L.NovDuration))
LR.NovDuration <- L.NovDuration[,LRL.NovDuration]

LRepeatability_NovDuration <- LR.NovDuration[,1] / (LR.NovDuration[,1] + LR.NovDuration[,2])
posterior.mode(LRepeatability_NovDuration)
HPDinterval(LRepeatability_NovDuration)

#low social duration repeatability
L.Social_Locations <- grep("LogSocial_Duration", colnames(modelLowSub$VCV))
L.Social <- modelLowSub$VCV[,L.Social_Locations]
head(L.Social)

LRL.Social <- grep("LogSocial_Duration:LogSocial_Duration", colnames(L.Social))
LR.Social <- L.Social[,HRL.Social]

LRepeatability_Social <- LR.Social[,1] / (LR.Social[,1] + LR.Social[,2])
posterior.mode(LRepeatability_Social)
HPDinterval(LRepeatability_Social)


## Repeatability for adjusted values

#high activity repeatability in subset for adjusted values
H.Activity_Locations.Adj <- grep("LogTotalDist", colnames(modelHighSub.Adj$VCV))
H.Activity.Adj <- modelHighSub.Adj$VCV[,H.Activity_Locations.Adj]
head(H.Activity.Adj)

HRL.Activity.Adj <- grep("LogTotalDist:LogTotalDist", colnames(H.Activity.Adj))
HR.Activity.Adj <- H.Activity.Adj[,HRL.Activity.Adj]

HRepeatability_Activity.Adj <- HR.Activity.Adj[,1] / (HR.Activity.Adj[,1] + HR.Activity.Adj[,2])
posterior.mode(HRepeatability_Activity.Adj)
HPDinterval(HRepeatability_Activity.Adj)

#high novel LatFirst repeatability in subset with adjusted values
H.NovLatency_Locations.Adj <- grep("NovZone_LatFirst", colnames(modelHighSub.Adj$VCV))
H.NovLatency.Adj <- modelHighSub.Adj$VCV[,H.NovLatency_Locations.Adj]
head(H.NovLatency.Adj)

HRL.NovLatency.Adj <- grep("NovZone_LatFirst:NovZone_LatFirst", colnames(H.NovLatency.Adj))
HR.NovLatency.Adj <- H.NovLatency.Adj[,HRL.NovLatency.Adj]

HRepeatability_NovLatency.Adj <- HR.NovLatency.Adj[,1] / (HR.NovLatency.Adj[,1] + HR.NovLatency.Adj[,2])
posterior.mode(HRepeatability_NovLatency.Adj)
HPDinterval(HRepeatability_NovLatency.Adj)

#high novel duration repeatability with adjusted values
H.NovDuration_Locations.Adj <- grep("LogNovZone_Duration.Adj", colnames(modelHighSubD.Adj$VCV))
H.NovDuration.Adj <- modelHighSubD.Adj$VCV[,H.NovDuration_Locations.Adj]
head(H.NovDuration.Adj)

HRL.NovDuration.Adj <- grep("LogNovZone_Duration.Adj:LogNovZone_Duration.Adj", colnames(H.NovDuration.Adj))
HR.NovDuration.Adj <- H.NovDuration.Adj[,HRL.NovDuration.Adj]

HRepeatability_NovDuration.Adj <- HR.NovDuration.Adj[,1] / (HR.NovDuration.Adj[,1] + HR.NovDuration.Adj[,2])
posterior.mode(HRepeatability_NovDuration.Adj)
HPDinterval(HRepeatability_NovDuration.Adj)

#high social duration repeatability with adjusted values
H.Social_Locations.Adj <- grep("LogSocial_Duration.Adj", colnames(modelHighSub.Adj$VCV))
H.Social.Adj <- modelHighSub.Adj$VCV[,H.Social_Locations.Adj]
head(H.Social.Adj)

HRL.Social.Adj <- grep("LogSocial_Duration.Adj:LogSocial_Duration.Adj", colnames(H.Social.Adj))
HR.Social.Adj <- H.Social.Adj[,HRL.Social.Adj]

HRepeatability_Social.Adj <- HR.Social.Adj[,1] / (HR.Social.Adj[,1] + HR.Social.Adj[,2])
posterior.mode(HRepeatability_Social.Adj)
HPDinterval(HRepeatability_Social.Adj)



#low activity repeatability with adjusted values
L.Activity_Locations.Adj <- grep("LogTotalDist", colnames(modelLowSub.Adj$VCV))
L.Activity.Adj <- modelLowSub.Adj$VCV[,L.Activity_Locations.Adj]
head(L.Activity.Adj)

LRL.Activity.Adj <- grep("LogTotalDist:LogTotalDist", colnames(L.Activity.Adj))
LR.Activity.Adj <- L.Activity.Adj[,LRL.Activity.Adj]

LRepeatability_Activity.Adj <- LR.Activity.Adj[,1] / (LR.Activity.Adj[,1] + LR.Activity.Adj[,2])
posterior.mode(LRepeatability_Activity.Adj)
HPDinterval(LRepeatability_Activity.Adj)

#low novel LatFirst repeatability with adjusted values
L.NovLatency_Locations.Adj <- grep("NovZone_LatFirst", colnames(modelLowSub.Adj$VCV))
L.NovLatency.Adj <- modelLowSub.Adj$VCV[,L.NovLatency_Locations.Adj]
head(L.NovLatency.Adj)

LRL.NovLatency.Adj <- grep("NovZone_LatFirst:NovZone_LatFirst", colnames(L.NovLatency.Adj))
LR.NovLatency.Adj <- L.NovLatency.Adj[,LRL.NovLatency.Adj]

LRepeatability_NovLatency.Adj <- LR.NovLatency.Adj[,1] / (LR.NovLatency.Adj[,1] + LR.NovLatency.Adj[,2])
posterior.mode(LRepeatability_NovLatency.Adj)
HPDinterval(LRepeatability_NovLatency.Adj)

#low novel duration repeatability with adjusted values
L.NovDuration_Locations.Adj <- grep("LogNovZone_Duration.Adj", colnames(modelLowSubD.Adj$VCV))
L.NovDuration.Adj <- modelLowSubD.Adj$VCV[,L.NovDuration_Locations.Adj]
head(L.NovDuration.Adj)

LRL.NovDuration.Adj <- grep("LogNovZone_Duration.Adj:LogNovZone_Duration.Adj", colnames(L.NovDuration.Adj))
LR.NovDuration.Adj <- L.NovDuration.Adj[,LRL.NovDuration.Adj]

LRepeatability_NovDuration.Adj <- LR.NovDuration.Adj[,1] / (LR.NovDuration.Adj[,1] + LR.NovDuration.Adj[,2])
posterior.mode(LRepeatability_NovDuration.Adj)
HPDinterval(LRepeatability_NovDuration.Adj)

#low social duration repeatability with adjusted values
L.Social_Locations.Adj <- grep("LogSocial_Duration.Adj", colnames(modelLowSub.Adj$VCV))
L.Social.Adj <- modelLowSub.Adj$VCV[,L.Social_Locations.Adj]
head(L.Social.Adj)

LRL.Social.Adj <- grep("LogSocial_Duration.Adj:LogSocial_Duration.Adj", colnames(L.Social.Adj))
LR.Social.Adj <- L.Social.Adj[,HRL.Social.Adj]

LRepeatability_Social.Adj <- LR.Social.Adj[,1] / (LR.Social.Adj[,1] + LR.Social.Adj[,2])
posterior.mode(LRepeatability_Social.Adj)
HPDinterval(LRepeatability_Social.Adj)

##### Between Individual Matrices #####

## Example - also sets up matrix function
modelHI <- readRDS("modelHI")

mat <-posterior.mode(modelHI$VCV)
HPDinterval(modelHI$VCV)

head(modelHI$VCV)
Locations <- grep("LizID", names(mat))
B <- mat[Locations]


B_mat_cov <- matrix(B, nrow = 3, ncol = 3)
B_mat_cor <- cov2cor(B_mat_cov)


 B = posterior.mode
matrices.Lat <- function(B, names = c("Activity", "Latency", "Social")){
B_mat_cov <- matrix(B, nrow = 3, ncol = 3)
B_mat_cor <- cov2cor(B_mat_cov)
colnames(B_mat_cov) <- colnames(B_mat_cor) <- names
rownames(B_mat_cov) <- rownames(B_mat_cor) <- names

return(list(cov = B_mat_cov, cor = B_mat_cor))
}
B.matrix <- matrices.Lat(B)

#High subset - Only the active individuals 
modelHighSub <- readRDS("modelHighSub")

matHighSub <-posterior.mode(modelHighSub$VCV)
HPDinterval(modelHighSub$VCV)


head(modelHighSub$VCV)
B.HighSub <- matHighSub[Locations]

HighSub.matrix <- matrices.Lat(B.HighSub)

LizID.Locations <- grep("LizID", names(matHighSub))
BHighSub_VCV <- modelHighSub$VCV[,LizID.Locations]
MatrixHighSub <- apply(modelHighSub$VCV, 1, function(x) matrices.Lat(x))

#Low subset - Only the active individuals 
modelLowSub <- readRDS("modelLowSub")

matLowSub <-posterior.mode(modelLowSub$VCV)
HPDinterval(modelLowSub$VCV)

head(modelLowSub$VCV)
B.LowSub <- matLowSub[Locations]

LowSub.matrix <- matrices.Lat(B.LowSub)

BLowSub_VCV <- modelLowSub$VCV[,LizID.Locations]
MatrixLowSub <- apply(modelLowSub$VCV, 1, function(x) matrices.Lat(x))

#High subset with adjusted values - Only the active individuals 
modelHighSub.Adj <- readRDS("modelHighSub.Adj")

matHighSub.Adj <-posterior.mode(modelHighSub.Adj$VCV)
HPDinterval(modelHighSub.Adj$VCV)

head(modelHighSub.Adj$VCV)
B.HighSub.Adj <- matHighSub.Adj[Locations]

HighSub.Adjmatrix <- matrices.Lat(B.HighSub.Adj)

BHighSub.Adj_VCV <- modelHighSub.Adj$VCV[,LizID.Locations]
MatrixHighSub.Adj <- apply(modelHighSub.Adj$VCV, 1, function(x) matrices.Lat(x))


#Low subset with adjusted values - Only the active individuals 
modelLowSub.Adj <- readRDS("modelLowSub.Adj")

matLowSub.Adj <-posterior.mode(modelLowSub.Adj$VCV)
HPDinterval(modelLowSub.Adj$VCV)

head(modelLowSub.Adj$VCV)
B.LowSub.Adj <- matLowSub.Adj[Locations]

LowSub.Adjmatrix <- matrices.Lat(B.LowSub.Adj)

BLowSub.Adj_VCV <- modelLowSub.Adj$VCV[,LizID.Locations]
MatrixLowSub.Adj <- apply(modelLowSub.Adj$VCV, 1, function(x) matrices.Lat(x))


## Matrices with Novel Duration

##Set up of new function
matD <-posterior.mode(modelHighSubD$VCV)
HPDinterval(modelHighSubD$VCV)

head(modelHighSubD$VCV)
LocationsD <- grep("LizID", names(matD))
BD <- matD[LocationsD]


BD_mat_cov <- matrix(BD, nrow = 3, ncol = 3)
BD_mat_cor <- cov2cor(BD_mat_cov)

BD.matrix <- matrices.Dur(BD)

# B = posterior.mode
matrices.Dur <- function(B, names = c("Activity", "Novel", "Social")){
  BD_mat_cov <- matrix(B, nrow = 3, ncol = 3)
  BD_mat_cor <- cov2cor(BD_mat_cov)
  colnames(BD_mat_cov) <- colnames(BD_mat_cor) <- names
  rownames(BD_mat_cov) <- rownames(BD_mat_cor) <- names
  
  return(list(cov = BD_mat_cov, cor = BD_mat_cor))
}


#High subset matrix
modelHighSubD <- readRDS("modelHighSubD")

matHighSubD <-posterior.mode(modelHighSubD$VCV)
HPDinterval(modelHighSubD$VCV)

head(modelHighSubD$VCV)
B.HighSubD <- matHighSubD[Locations]

HighSubDmatrix <- matrices.Dur(B.HighSubD)

BHighSubD_VCV <- modelHighSubD$VCV[,LizID.Locations]
MatrixHighSubD <- apply(modelHighSubD$VCV, 1, function(x) matrices.Dur(x))

  #High adjusted subset
modelHighSubD.Adj <- readRDS("modelHighSubD.Adj")

matHighSubD.Adj <-posterior.mode(modelHighSubD.Adj$VCV)
HPDinterval(modelHighSubD.Adj$VCV)

head(modelHighSubD.Adj$VCV)
B.HighSubD.Adj <- matHighSubD.Adj[Locations]

HighSubDmatrix.Adj <- matrices.Dur(B.HighSubD.Adj)

BHighSubD_VCV.Adj <- modelHighSubD.Adj$VCV[,LizID.Locations]
MatrixHighSubD.Adj <- apply(modelHighSubD.Adj$VCV, 1, function(x) matrices.Dur(x))


#Low subset matrix
modelLowSubD <- readRDS("modelLowSubD")

matLowSubD <-posterior.mode(modelLowSubD$VCV)
HPDinterval(modelLowSubD$VCV)

head(modelLowSubD$VCV)
B.LowSubD <- matLowSubD[Locations]

LowSubDmatrix <- matrices.Dur(B.LowSubD)

BLowSubD_VCV <- modelLowSubD$VCV[,LizID.Locations]
MatrixLowSubD <- apply(modelLowSubD$VCV, 1, function(x) matrices.Dur(x))

  #Low adjusted subset
modelLowSubD.Adj <- readRDS("modelLowSubD.Adj")

matLowSubD.Adj <-posterior.mode(modelLowSubD.Adj$VCV)
HPDinterval(modelLowSubD.Adj$VCV)

head(modelLowSubD.Adj$VCV)
B.LowSubD.Adj <- matLowSubD.Adj[Locations]

LowSubDmatrix.Adj <- matrices.Dur(B.LowSubD.Adj)

BLowSubD_VCV.Adj <- modelLowSubD.Adj$VCV[,LizID.Locations]
MatrixLowSubD.Adj <- apply(modelLowSubD.Adj$VCV, 1, function(x) matrices.Dur(x))

##### Within Individual Matrices #####

## Example - also sets up matrix function
modelHI <- readRDS("modelHI")

mat <-posterior.mode(modelHI$VCV)
HPDinterval(modelHI$VCV)

head(modelHI$VCV)
Locations.wit <- grep("unit", names(mat))
B.wit <- mat[Locations.wit]


B_mat_cov.wit <- matrix(B.wit, nrow = 3, ncol = 3)
B_mat_cor.wit <- cov2cor(B_mat_cov.wit)


# B.wit = posterior.mode
matrices.LatW <- function(B.wit, names = c("Activity", "Latency", "Social")){
  B_mat_cov.wit <- matrix(B.wit, nrow = 3, ncol = 3)
  B_mat_cor.wit <- cov2cor(B_mat_cov.wit)
  colnames(B_mat_cov.wit) <- colnames(B_mat_cor.wit) <- names
  rownames(B_mat_cov.wit) <- rownames(B_mat_cor.wit) <- names
  
  return(list(cov = B_mat_cov.wit, cor = B_mat_cor.wit))
}
B.matrix.wit <- matrices.LatW(B.wit)

#High subset - Only the active individuals 
modelHighSub <- readRDS("modelHighSub")

matHighSub <-posterior.mode(modelHighSub$VCV)
HPDinterval(modelHighSub$VCV)


head(modelHighSub$VCV)
B.HighSub.wit <- matHighSub[Locations.wit]

HighSub.matrix.wit <- matrices.LatW(B.HighSub.wit)

LizID.Locations <- grep("LizID", names(matHighSub))
BHighSub_VCV <- modelHighSub$VCV[,LizID.Locations]
MatrixHighSub <- apply(modelHighSub$VCV, 1, function(x) matrices.Lat(x))

#Low subset - Only the active individuals 
modelLowSub <- readRDS("modelLowSub")

matLowSub <-posterior.mode(modelLowSub$VCV)
HPDinterval(modelLowSub$VCV)

head(modelLowSub$VCV)
B.LowSub.wit <- matLowSub[Locations.wit]

LowSub.matrix.wit <- matrices.LatW(B.LowSub.wit)

BLowSub_VCV <- modelLowSub$VCV[,LizID.Locations]
MatrixLowSub <- apply(modelLowSub$VCV, 1, function(x) matrices.Lat(x))

#High subset with adjusted values - Only the active individuals 
modelHighSub.Adj <- readRDS("modelHighSub.Adj")

matHighSub.Adj <-posterior.mode(modelHighSub.Adj$VCV)
HPDinterval(modelHighSub.Adj$VCV)

head(modelHighSub.Adj$VCV)
B.HighSub.Adj.wit <- matHighSub.Adj[Locations.wit]

HighSub.Adjmatrix.wit <- matrices.LatW(B.HighSub.Adj.wit)

BHighSub.Adj_VCV <- modelHighSub.Adj$VCV[,LizID.Locations]
MatrixHighSub.Adj <- apply(modelHighSub.Adj$VCV, 1, function(x) matrices.Lat(x))


#Low subset with adjusted values - Only the active individuals 
modelLowSub.Adj <- readRDS("modelLowSub.Adj")

matLowSub.Adj <-posterior.mode(modelLowSub.Adj$VCV)
HPDinterval(modelLowSub.Adj$VCV)

head(modelLowSub.Adj$VCV)
B.LowSub.Adj.wit <- matLowSub.Adj[Locations.wit]

LowSub.Adjmatrix.wit <- matrices.LatW(B.LowSub.Adj.wit)

BLowSub.Adj_VCV <- modelLowSub.Adj$VCV[,LizID.Locations]
MatrixLowSub.Adj <- apply(modelLowSub.Adj$VCV, 1, function(x) matrices.Lat(x))


## Matrices with Novel Duration

##Set up of new function
matD <-posterior.mode(modelHighSubD$VCV)
HPDinterval(modelHighSubD$VCV)

head(modelHighSubD$VCV)
LocationsD.wit <- grep("unit", names(matD))
BD.wit <- matD[LocationsD.wit]


BD_mat_cov.wit <- matrix(BD.wit, nrow = 3, ncol = 3)
BD_mat_cor.wit <- cov2cor(BD_mat_cov.wit)

BD.matrix <- matrices.Dur(BD)

# B = posterior.mode
matrices.DurW <- function(B.wit, names = c("Activity", "Novel", "Social")){
  BD_mat_cov.wit <- matrix(B.wit, nrow = 3, ncol = 3)
  BD_mat_cor.wit <- cov2cor(BD_mat_cov.wit)
  colnames(BD_mat_cov.wit) <- colnames(BD_mat_cor) <- names
  rownames(BD_mat_cov.wit) <- rownames(BD_mat_cor.wit) <- names
  
  return(list(cov = BD_mat_cov.wit, cor = BD_mat_cor.wit))
}


#High subset matrix
modelHighSubD <- readRDS("modelHighSubD")

matHighSubD <-posterior.mode(modelHighSubD$VCV)
HPDinterval(modelHighSubD$VCV)

head(modelHighSubD$VCV)
B.HighSubD.wit <- matHighSubD[Locations.wit]

HighSubDmatrix.wit <- matrices.DurW(B.HighSubD.wit)

BHighSubD_VCV <- modelHighSubD$VCV[,LizID.Locations]
MatrixHighSubD <- apply(modelHighSubD$VCV, 1, function(x) matrices.Dur(x))

#High adjusted subset
modelHighSubD.Adj <- readRDS("modelHighSubD.Adj")

matHighSubD.Adj <-posterior.mode(modelHighSubD.Adj$VCV)
HPDinterval(modelHighSubD.Adj$VCV)

head(modelHighSubD.Adj$VCV)
B.HighSubD.Adj.wit <- matHighSubD.Adj[Locations.wit]

HighSubDmatrix.Adj.wit <- matrices.DurW(B.HighSubD.Adj.wit)

BHighSubD_VCV.Adj <- modelHighSubD.Adj$VCV[,LizID.Locations]
MatrixHighSubD.Adj <- apply(modelHighSubD.Adj$VCV, 1, function(x) matrices.Dur(x))


#Low subset matrix
modelLowSubD <- readRDS("modelLowSubD")

matLowSubD <-posterior.mode(modelLowSubD$VCV)
HPDinterval(modelLowSubD$VCV)

head(modelLowSubD$VCV)
B.LowSubD.wit <- matLowSubD[Locations.wit]

LowSubDmatrix.wit <- matrices.DurW(B.LowSubD.wit)

BLowSubD_VCV <- modelLowSubD$VCV[,LizID.Locations]
MatrixLowSubD <- apply(modelLowSubD$VCV, 1, function(x) matrices.Dur(x))

#Low adjusted subset
modelLowSubD.Adj <- readRDS("modelLowSubD.Adj")

matLowSubD.Adj <-posterior.mode(modelLowSubD.Adj$VCV)
HPDinterval(modelLowSubD.Adj$VCV)

head(modelLowSubD.Adj$VCV)
B.LowSubD.Adj <- matLowSubD.Adj[Locations]

LowSubDmatrix.Adj <- matrices.Dur(B.LowSubD.Adj)

BLowSubD_VCV.Adj <- modelLowSubD.Adj$VCV[,LizID.Locations]
MatrixLowSubD.Adj <- apply(modelLowSubD.Adj$VCV, 1, function(x) matrices.Dur(x))


##### Box Plots #####
library(ggplot2)

#Exploration
ggplot(Exploration, aes(x = Trt, y= LogTotalDist)) + geom_boxplot() +ggtitle ("Exploration") + xlab("Treatment") + ylab("Log Total Distance (cm")   #ALL data, not grouped by ID
t.test(LogTotalDist ~ Trt, data = Exploration, var.equal = TRUE)

    #filtered by lizard
ExplorationFiltered <- Exploration %>% 
  group_by(LizID, Trt) %>% 
  summarise(
    "mean_LogTotalDist" = mean(log(TotalDist+0.5))
  ) 
ggplot(ExplorationFiltered, aes(x = Trt, y = mean_LogTotalDist)) + geom_boxplot() +ggtitle ("Exploration") + xlab("Treatment") + ylab("Average Total Distance (cm)")
t.test(mean_LogTotalDist ~ Trt, data = ExplorationFiltered, var.equal = TRUE)

#Novel

  # latency
ggplot(Novel, aes(x = Trt, y = NovZone_LatFirst)) + geom_boxplot() + ggtitle("Novel Zone Latency (Ungrouped)") + xlab("Treatment") + ylab("Latency (s)")     #All data not grouped by ID
t.test(NovZone_LatFirst ~ Trt, data = Novel, var.equal = TRUE)

NovelFiltered <- Female %>%     #filters by LizID
  group_by(LizID, Trt) %>% 
  summarise(
    "mean_LogNovZone_Duration" = mean(log(NovZone_Duration+0.5), na.rm = TRUE),
    "mean_LogNovZone_Duration.Adj" = mean(log(NovZone_Duration.Adj+0.5), na.rm = TRUE),
    "mean_Latency" = mean(NovZone_LatFirst)
  ) 

ggplot(NovelFiltered, aes(x = Trt, y = mean_Latency)) + geom_boxplot() + ggtitle ("Novel Zone Latency (grouped)") + xlab("Treatment") + ylab("Latency (s)")     #plots grouped data by aevrage of each individual 
t.test(mean_Latency ~ Trt, data = NovelFiltered, var.equal = TRUE)


  # duration
ggplot(Novel, aes(x = Trt, y = LogNovZone_Duration)) + geom_boxplot() + ggtitle("Novel Duration (Ungrouped)") + xlab("Treatment") + ylab("Cumulative Duration in Novel Zone (s)")   #ungrouped data
t.test(LogNovZone_Duration ~ Trt, data = Novel, var.equal = TRUE)

ggplot(NovelFiltered, aes(x = Trt, y = mean_LogNovZone_Duration)) + geom_boxplot() +ggtitle ("Novel Duration (grouped)") + xlab("Treatment") + ylab("Cumulative Duration in Novel Zone (s)") #grouped data
t.test(mean_LogNovZone_Duration ~ Trt, data = NovelFiltered, var.equal = TRUE)

  # adjusted values
ggplot(Female, aes(x = Trt, y= LogNovZone_Duration.Adj)) + geom_boxplot() +ggtitle ("Adjusted Novel Duration (Ungrouped)") + xlab("Treatment") + ylab("Cumulative Duration in Novel Zone (s)")   #Ungrouped data
t.test(LogNovZone_Duration.Adj ~ Trt, data = Female, var.equal = TRUE)

ggplot(NovelFiltered, aes(x = Trt, y= mean_LogNovZone_Duration.Adj)) + geom_boxplot() +ggtitle ("Adjusted Novel Duration (Grouped)") + xlab("Treatment") + ylab("Cumulative Duration in Novel Zone (s)")  #grouped data
t.test(mean_LogNovZone_Duration.Adj ~ Trt, data = NovelFiltered, var.equal = TRUE)


#Social
ggplot(Social, aes(x = Trt, y = LogSocial_Duration)) + geom_boxplot() + ggtitle("Social Zone Duration (Ungrouped)") + xlab("Treatment") + ylab("Time in Social Zone (s)")   # ungrouped data
t.test(LogSocial_Duration ~ Trt, data = Social, var.equal = TRUE)

  # grouped by LizID
SocialFiltered <- Female %>% 
  group_by(LizID, Trt) %>% 
  summarise(
    "mean_SocialDuration" = mean(log(Social_Duration + 0.5), na.rm = TRUE),
    "mean_SocialDuration.Adj" = mean(log(Social_Duration.Adj + 0.5), na.rm = TRUE)
  )

ggplot(SocialFiltered, aes(x = Trt, y = mean_SocialDuration)) + geom_boxplot() +ggtitle ("Social Zone Duration (Grouped)") + xlab("Treatment") + ylab("Time in Social Zone (s)")  # grouped data
t.test(mean_SocialDuration ~ Trt, data = SocialFiltered, var.equal = TRUE)

# adjusted values
ggplot(Female, aes(x = Trt, y= LogSocial_Duration.Adj)) + geom_boxplot() +ggtitle ("Adjusted Social Duration") + xlab("Treatment") + ylab("Time in Social Zone (s)")  # ungrouped and adjusted data
t.test(LogSocial_Duration.Adj ~ Trt, data = Female, var.equal = TRUE)

ggplot(SocialFiltered, aes(x = Trt, y = mean_SocialDuration.Adj)) + geom_boxplot() +ggtitle ("Adjusted Social Duration") + xlab("Treatment") + ylab("Time in Social Zone (s)")  # grouped and adjusted
t.test(mean_SocialDuration.Adj ~ Trt, data = SocialFiltered, var.equal = TRUE)



