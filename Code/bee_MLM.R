#sandhill bee Multilevel modeling

library(tidyverse)
library(vegan)
library(lme4)
library(lmerTest)
library(glmmTMB)
library(DHARMa)
library(ggplot2)
install.packages("MuMIn")
library(MuMIn)
library(lattice)

#read in data cleaned from "clean_data.R" script
bee_data<-read.csv("data/sandhill_bee.csv")[,-1]%>%
          rename(Sav_prop=Savanna_Prop,Flower_abun=AbundanceAllPl_NoHA,
          Flower_chao=Chao1_NoHA, Fire=NumFires_90,Soil=Average_soil_moisture)%>%
          select(Region,Site,Plot,Flower_chao,Flower_abun,Fire,Soil,Sav_prop,MEM1,Species, Genus, Month,n)%>%
          mutate_at(c("Flower_chao","Flower_abun","Fire","Soil","Sav_prop"), ~(scale(.) %>% as.vector))









dat<-bee_data[,c(4,5,6,7,8)]
cor(dat)
is.na(bee_data)
####Build Models####


#FULL MODEL NO INTERACTIONS
TMB1 <- glmmTMB(n ~ (1|Species) + Flower_chao*Flower_abun+Fire+Soil+Sav_prop +Month+
                  (0 + Flower_chao|Species) + 
                  (0 + Flower_abun|Species) + 
                  (0 + Fire|Species) + 
                  (0 + Soil|Species) +
                  (0 + Sav_prop|Species) + 
                  #(0 + Month|Species) + 
                  (1|Site)+ (1|Site:Plot), 
                  family = "poisson", 
                  data = bee_data)
summary(TMB1)
r.squaredGLMM(TMB1)

simulationOutput <- simulateResiduals(fittedModel = TMB1, plot=F)

hist(residuals(simulationOutput))
plot(simulationOutput)    

plotResiduals(simulationOutput, form = data$BF)
plotResiduals(simulationOutput, form = data$Elevation)
plotResiduals(simulationOutput, form = data$fTime)
plotResiduals(simulationOutput, form = data$WD)

testDispersion(simulationOutput) #looks good 
testZeroInflation(simulationOutput)


#
TMB2 <- glmmTMB(n ~ (1|Species) + Flower_chao*Flower_abun+Month+
                  (0 + Flower_chao|Species) + 
                  (0 + Flower_abun|Species) + 
                  #(0 + Month|Species) + 
                  (1|Site)+ (1|Site:Plot), 
                family = "poisson", 
                data = bee_data)
summary(TMB2)
r.squaredGLMM(TMB2)

