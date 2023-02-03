#sandhill bee Multilevel modeling

library(tidyverse)
library(vegan)
library(lme4)
library(lmerTest)
library(glmmTMB)
library(DHARMa)
library(ggplot2)
library(MuMIn)
library(lattice)

#read in data cleaned from "clean_data.R" script
bee_data<-read.csv("data/sandhill_bee.csv")[,-1]%>%
          rename(Sav_prop=Savanna_Prop,Flower_abun=AbundanceAllPl_NoHA,
          flower_chao=Chao1_NoHA, Fire=NumFires_90,Soil=Average_soil_moisture)%>%
          select(Region,Site,Plot,flower_chao,Flower_abun,Fire,Soil,Sav_prop,MEM1,Species, Genus, Month,n)



####Build Models####
TMB1 <- glmmTMB(N ~ (1|Species) + BF_2000*Elevation*IN_20.5 + I(Elevation^2) +
                  (0 + BF_2000|SPP) + 
                  (0 + Elevation|SPP) + 
                  (0 + IN_20.5|SPP) + 
                  (1|Block)+(1|Block:Plot2)+ (1|Block:Plot2:Submodule2), 
                family = "binomial", 
                data = veg)