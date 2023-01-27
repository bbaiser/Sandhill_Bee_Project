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
          filter(!(Genus== "Lasioglossum" & Trap_Type=="BT"|Genus=="Lasioglossum" & Trap_Type=="VT"))%>%
          select(-Sex)


#make a new month column from sample data
las_scott_mon<-las_scott%>%
               mutate(Mon=str_extract( Date,"[A-z]+"))%>%
               mutate(Month=str_replace_all(Mon, c("Mar"="March","May"="May", "Apr"="April", "Jul"="July", "Jun"="June", "Sep"="September",  "Aug"= "August", "Oct"="October" ,"Nov"="November")))%>%
               select(-Mon)

#take all singletons out
las_sing<-las_scott_mon%>%
          filter(extra.count==0)

#make replicates for multiples and bind with singletons
las_ind<-las_scott_mon%>%
         filter(!extra.count==0)%>% 
         uncount(extra.count)%>%
         bind_rows(las_sing)%>%
         rename (Trap_Type=Trap,Genus=Genera,Species=species,Plotnum=Plot)%>%
         select(-Date,-sex..ref.bee.only.,-Family,-extra.count,-Order)%>%
         mutate(Plot= paste(Site, Plotnum, sep = ''))%>%
         select(-Plotnum)%>%
         mutate(Plot_Month = paste(Plot, Month, sep = '_'))%>%
         mutate(Species2 = paste(Genus, Species, sep = ' '))%>%
         select(-Species)%>%
         rename(Species=Species2)


#add Lasioglossum sp. from vane trap and bowl trap back 
full_bee<-las_ind%>%
          bind_rows(bee_drop)%>%
          mutate(across("Species", str_replace, 'Lasioglossum alachuense|Lasioglossum apopkense', 'Lasioglossum sp'))%>%
          mutate(across("Site", str_replace, 'WCN|WIN', 'WN'))%>%#fix site names
          mutate(across("Site", str_replace, 'WCS|WIS', 'WS'))%>%#fix site names
          mutate(across("Trap_Type", str_replace, 'BR', 'BT'))%>%#fix site names
          filter(!Species == "Perdita sp")# remove unknown perdita sp 
         
#get species specific counts for each plot/month combo
bee_count<-full_bee%>%
          group_by(Plot_Month, Species,Genus,Month, Site, Plot) %>% 
          tally()
                 
#look for errors
unique(bee_count$Site)
unique(bee_count$Trap_Type)
unique(bee_count$Genus)
sort(unlist(unique(bee_count$Genus)))
unique(bee_count$Month)
unique(bee_count$Plot_Month)
unique(bee_count$Species)
length(sort(unlist(unique(bee_count$Species))))
unique(bee_count$Plot)
