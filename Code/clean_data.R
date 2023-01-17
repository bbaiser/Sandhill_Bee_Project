###Sandhill bee data cleaning and prep###
library(dplyr)
library(tidyverse)

#Data
#bee data with Lasioglossum as sp. for bowl traps (bt) and vane traps (VT)
bee_sp<-read.csv("data/dataBee.csv", header = T, row=1)#

#Lasioglossum species designations
lassio_sp<-read.csv("data/Lassioglossum_species.csv", header=T)


bee_drop<-bee_sp %>%
       filter(!(Trap_Type=="NT"& Genus== "Lasioglossum"))
df %>% 
  filter(!(parameter=="param_A" & year == 2003))