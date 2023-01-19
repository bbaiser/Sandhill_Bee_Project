###Sandhill bee data cleaning and prep###
library(dplyr)
library(tidyverse)
library(stringr)
#Data
#bee data with Lasioglossum as sp. for bowl traps (bt) and vane traps (VT)
bee_sp<-read.csv("data/dataBee.csv", header = T, row=1)#

#Lasioglossum species designations
lassio_sp<-read.csv("data/Lassioglossum_species.csv", header=T)

#identification of Lasioglossum to species by Scott Gibb
las_scott<-read.csv("data/Lasioglossums_5202020.csv", header=T)


#Drop Lasioglossum sp. from vane trap and bowl trap
bee_drop<-bee_sp %>%
          filter(!(Genus== "Lasioglossum" & Trap_Type=="BT"|Genus=="Lasioglossum" & Trap_Type=="VT"))


#make a new month column from sample data
dd<-las_scott%>%
    mutate(Month=str_extract( Date,"[A-z]+"))

#replace motn abbreviations with full month name
vec<-unique(dd$Month)

str_replace_all(dd$Month, c("Mar"="March","May"="May", "Apr"="April", "Jul"="July", "Jun"="June", "Sep"="September",  "Aug"= "August", "Oct"="October" ,"Nov"="November"))

JobDF %>% 
  mutate(inNameDF = ifelse(str_detect(occupation, paste0(NameDF$names, collapse = "|")),"yes","no"))