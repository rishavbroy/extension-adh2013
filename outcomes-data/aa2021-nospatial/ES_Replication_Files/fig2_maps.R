######################## Code Summary ##################
#Replication for Partisanship Nationalization in American Elections: Evidence from Presidential, Senatorial, Gubernatorial Elections in the U.S. Counties, 1872-2018
#Electoral Studies
#Algara & Amlani (2021)
#R Version: R 4.0.3
#August 27 2021

#This code creates the maps in Figure 2 

########################## Prelude #####################

rm(list=ls(all=TRUE))
options(stringsAsFactors = FALSE)
options(scipen = 3)
set.seed(1993)

######################### Functions ###################
'%!in%' <- function(x,y)!('%in%'(x,y))

######################### Library #####################
library(ggplot2)
library("rgdal") # requires sp, will use proj.4 if installed
library("maptools")
library("plyr")
library(sf)
library(ggthemes)

######################## Upload Data ##################

#****************** Presidential Vote Share ******************
#Set Working Directory - CHANGE ME
setwd("/Users/caalgara/Desktop/es replication files/ES_Replication_Files")

#Upload Data
load("electoral_studies_replication_presidential_analysis_data.rData")

#******************** Shape Files *************************
#Set Working Directory  - CHANGE ME
setwd("/Users/caalgara/Desktop/es replication files/ES_Replication_Files")

#Upload Data
load("county_geo_spatial_data_1870_2020.rData")

########################## Examine Data ##################

###################### Data Management: Vote Share #####################
#Create Two-Party Vote Share
pres_elections_release$two_party_dem_vote_share <- (pres_elections_release$democratic_raw_votes /(pres_elections_release$democratic_raw_votes + pres_elections_release$republican_raw_votes)) *100
pres_elections_release$two_party_rep_vote_share <- (pres_elections_release$republican_raw_votes /(pres_elections_release$democratic_raw_votes + pres_elections_release$republican_raw_votes)) *100

#Create Two-Party Vote Share - Factor
pres_elections_release$two_party_dem_vote_share_factor <- NA
pres_elections_release$two_party_dem_vote_share_factor[pres_elections_release$two_party_dem_vote_share >= 0 & pres_elections_release$two_party_dem_vote_share < 10] <- "0-10%"
pres_elections_release$two_party_dem_vote_share_factor[pres_elections_release$two_party_dem_vote_share >= 10 & pres_elections_release$two_party_dem_vote_share < 20] <- "10-20%"
pres_elections_release$two_party_dem_vote_share_factor[pres_elections_release$two_party_dem_vote_share >= 20 & pres_elections_release$two_party_dem_vote_share < 30] <- "20-30%"
pres_elections_release$two_party_dem_vote_share_factor[pres_elections_release$two_party_dem_vote_share >= 30 & pres_elections_release$two_party_dem_vote_share < 40] <- "30-40%"
pres_elections_release$two_party_dem_vote_share_factor[pres_elections_release$two_party_dem_vote_share >= 40 & pres_elections_release$two_party_dem_vote_share < 50] <- "40-50%"
pres_elections_release$two_party_dem_vote_share_factor[pres_elections_release$two_party_dem_vote_share >= 50 & pres_elections_release$two_party_dem_vote_share < 60] <- "50-60%"
pres_elections_release$two_party_dem_vote_share_factor[pres_elections_release$two_party_dem_vote_share >= 60 & pres_elections_release$two_party_dem_vote_share < 70] <- "60-70%"
pres_elections_release$two_party_dem_vote_share_factor[pres_elections_release$two_party_dem_vote_share >= 70 & pres_elections_release$two_party_dem_vote_share < 80] <- "70-80%"
pres_elections_release$two_party_dem_vote_share_factor[pres_elections_release$two_party_dem_vote_share >= 80 & pres_elections_release$two_party_dem_vote_share < 90] <- "80-90%"
pres_elections_release$two_party_dem_vote_share_factor[pres_elections_release$two_party_dem_vote_share >= 90 & pres_elections_release$two_party_dem_vote_share <=100] <- "90-100%"

#Create Margin of Victory
pres_elections_release$MOV <- pres_elections_release$two_party_dem_vote_share - pres_elections_release$two_party_rep_vote_share

#Create Margin of Victory - Factor
pres_elections_release$MOV_factor <- NA
pres_elections_release$MOV_factor[pres_elections_release$MOV >= -100 & pres_elections_release$MOV < -80] <- "Rep: 100 to 80%"
pres_elections_release$MOV_factor[pres_elections_release$MOV >= -80 & pres_elections_release$MOV < -60] <-  "Rep: 80 to 60%"
pres_elections_release$MOV_factor[pres_elections_release$MOV >= -60 & pres_elections_release$MOV < -40] <-  "Rep: 60 to 40%"
pres_elections_release$MOV_factor[pres_elections_release$MOV >= -40 & pres_elections_release$MOV < -20] <-  "Rep: 40 to 20%"
pres_elections_release$MOV_factor[pres_elections_release$MOV >= -20 & pres_elections_release$MOV < 0] <-    "Rep: 20 to 0%"
pres_elections_release$MOV_factor[pres_elections_release$MOV >= 0 & pres_elections_release$MOV < 20] <-     "Dem: 0 to 20%"
pres_elections_release$MOV_factor[pres_elections_release$MOV >= 20 & pres_elections_release$MOV < 70] <-    "Dem: 20 to 40%"
pres_elections_release$MOV_factor[pres_elections_release$MOV >= 40 & pres_elections_release$MOV < 80] <-    "Dem: 40 to 60%"
pres_elections_release$MOV_factor[pres_elections_release$MOV >= 60 & pres_elections_release$MOV < 90] <-    "Dem: 60 to 80%"
pres_elections_release$MOV_factor[pres_elections_release$MOV >= 80 & pres_elections_release$MOV <=100] <-   "Dem: 80 to 100%"

#Set Levels
pres_elections_release$MOV_factor <- factor(pres_elections_release$MOV_factor, levels = c("Rep: 100 to 80%", "Rep: 80 to 60%", "Rep: 60 to 40%", "Rep: 40 to 20%", "Rep: 20 to 0%", 
                                                                                          "Dem: 0 to 20%",   "Dem: 20 to 40%", "Dem: 40 to 60%", "Dem: 60 to 80%", "Dem: 80 to 100%"))
###################### Subset Data ####################

#******************** Vote Share *************************
#Drop HI and Subset useful years

#1924
pres_elections_1924 <- subset(pres_elections_release, state != "HI" & election_year == 1924)

#1988
pres_elections_1988 <- subset(pres_elections_release, state != "HI" & election_year == 1988)

#******************** Shapes *************************
#Use the decade ahead because some counties that voted in 1924  were established in early 1920s are not  included in the 1920/1980 shape file

#1924
County_Shape_1924 <- county_shapes[[1930]] 

#1988
County_Shape_1988 <- county_shapes[[1990]]

###################### Data Management: Shapes ####################
#************************ Make into an SF Object  ************************
County_Shape_1924.SF <- st_as_sf(County_Shape_1924)
County_Shape_1988.SF <- st_as_sf(County_Shape_1988)

#******************* Drop AK and HI ************************
County_Shape_1924.SF.1 <- subset(County_Shape_1924.SF,STATENAM %!in% c("Alaska Territory", "Hawaii Territory"))

County_Shape_1988.SF.1 <- subset(County_Shape_1988.SF,STATENAM %!in% c("Alaska", "Hawaii"))

###################### Manage Fips Codes ####################
#****************** Create FIPS Codes in Shapefile ************************
County_Shape_1924.SF.1$fips <- paste(substr(County_Shape_1924.SF.1$STATE,1,2), substr(County_Shape_1924.SF.1$COUNTY,1,3), sep = "")
County_Shape_1988.SF.1$fips <- paste(substr(County_Shape_1988.SF.1$STATE,1,2), substr(County_Shape_1988.SF.1$COUNTY,1,3), sep = "")

County_Shape_1924.SF.1$fips[County_Shape_1924.SF.1$fips == "NANA"] <- NA
County_Shape_1988.SF.1$fips[County_Shape_1988.SF.1$fips == "NANA"] <- NA

#********************* Check Fips Codes Overlap
Missing_Shape_1924 <- setdiff(pres_elections_1924$fips, County_Shape_1924.SF.1$fips)
Missing_Shape_1988 <- setdiff(pres_elections_1988$fips, County_Shape_1988.SF.1$fips)

Missing_Pres_1924 <- setdiff(County_Shape_1924.SF.1$fips, pres_elections_1924$fips)
Missing_Pres_1988 <- setdiff(County_Shape_1988.SF.1$fips, pres_elections_1988$fips)

#***************** County Names of Missing - Pres Perspective *********************
#****************** 1924
data.frame(county_name = subset(pres_elections_1924, fips %in% Missing_Shape_1924)$county_name, 
           state = subset(pres_elections_1924, fips %in% Missing_Shape_1924)$state,
           fips = subset(pres_elections_1924, fips %in% Missing_Shape_1924)$fips)
# "CAMPBELL GA" #Disbanded in 1931
# "MILTON GA"   #Disbanded in 1931 
# "SOUTH NORFOLK CITY VA" #This one is just missing -- Recovered

#****************** 1988
data.frame(county_name = subset(pres_elections_1988, fips %in% Missing_Shape_1988)$county_name, 
           state = subset(pres_elections_1988, fips %in% Missing_Shape_1988)$state,
           fips = subset(pres_elections_1988, fips %in% Missing_Shape_1988)$fips)

#********************** County Names of Missing - Shape Perspective *********************
#Note: These are the cases that I lose in the merge
#****************** 1924
data.frame(NHGISNAM = subset(County_Shape_1924.SF.1, fips %in% Missing_Pres_1924)$NHGISNAM, 
           STATENAM = subset(County_Shape_1924.SF.1, fips %in% Missing_Pres_1924)$STATENAM,
           fips = subset(County_Shape_1924.SF.1, fips %in% Missing_Pres_1924)$fips)

#Gulf Florida 12045 --  Founded 1925 - Check
#Gilchrist Florida 12041 -- Founded 1925 - Check
#Indian River Florida 12061 -- Founded 1925 - Check
#Martin Florida 12085 -- Founded 1925 - Check
#Peach Georgia 13225 -- Founded July 8, 1924 - Missing
#Petroleum Montana -- Founded February 25, 1925 -- Check

#            Yellowstone National Park Montana 30113 - DONT NEED
# Yellowstone National Park in Wyoming Wyoming 56047 - DONT NEED
# District Of Columbia District of Columbia 11001    - DONT NEED

#****************** 1988
data.frame(NHGISNAM = subset(County_Shape_1988.SF.1, fips %in% Missing_Pres_1988)$NHGISNAM, 
           STATENAM = subset(County_Shape_1988.SF.1, fips %in% Missing_Pres_1988)$STATENAM,
           fips = subset(County_Shape_1988.SF.1, fips %in% Missing_Pres_1988)$fips)
#Only AK and HI are Missing (and Yellowstone National Park Montana). This is OK becasue AK and HI are getting dropped anyway.

#Yellowstone National Park  Montana 30113 - DONT NEED

#*********************Recover FIPS Code - South Norfolk City Virginia *********************
County_Shape_1924.SF.1$fips[County_Shape_1924.SF.1$NHGISNAM == "South Norfolk City"  & County_Shape_1924.SF.1$STATENAM == "Virginia"] <- pres_elections_1924$fips[pres_elections_1924$county_name == "SOUTH NORFOLK CITY" &  pres_elections_1924$state == "VA"]

#Check
Missing_Shape_1924 <- setdiff(pres_elections_1924$fips, County_Shape_1924.SF.1$fips)

data.frame(county_name = subset(pres_elections_1924, fips %in% Missing_Shape_1924)$county_name, 
           state = subset(pres_elections_1924, fips %in% Missing_Shape_1924)$state,
           fips = subset(pres_elections_1924, fips %in% Missing_Shape_1924)$fips)


#*********************Missing Report *********************
#Some missing counties where not founded at the time
#HI and AK are going to get dropped from the map anyway. 
#Only potential Missing county is Peach Georgia and it is missing from the shape file
#Good to move forward

###################### Merge ####################
#******************** 1924 ***********************
#Merge
County_Shape_1924.SF.2 <- merge( x= County_Shape_1924.SF.1, y = pres_elections_1924, by = "fips", all.x = T)

#******************** 1988 ***********************
#Merge
County_Shape_1988.SF.2 <- merge( x= County_Shape_1988.SF.1, y = pres_elections_1988, by = "fips", all.x = T)

##################### Plot: Margin of Victory ####################
#Use if you do not have a legend or NAs
fill <- c(
  "Rep: 100 to 80%" = "#B71C1C",
  "Rep: 80 to 60%"  = "#D32F2F",
  "Rep: 60 to 40%"  = "#F44336",
  "Rep: 40 to 20%"  = "#E57373",
  "Rep: 20 to 0%"    = "#EF9A9A",  
  "Dem: 0 to 20%"     = "#90CAF9",
  "Dem: 20 to 40%"    = "#42A5F5",
  "Dem: 40 to 60%"    = "#2196F3",
  "Dem: 60 to 80%"    = "#1976D2",
  "Dem: 80 to 100%"   = "#0D47A1")

#******************** 1924 ********************
#Use if you have NAs
Plot_24 <- ggplot(data = County_Shape_1924.SF.2) +
  geom_sf(aes(fill = MOV_factor)) +
  scale_fill_manual(breaks = c("Rep: 100 to 80%", "Rep: 80 to 60%", "Rep: 60 to 40%", "Rep: 40 to 20%", "Rep: 20 to 0%", 
                               "Dem: 0 to 20%",   "Dem: 20 to 40%", "Dem: 40 to 60%", "Dem: 60 to 80%", "Dem: 80 to 100%"),
                    values= c("#B71C1C", "#D32F2F", "#F44336", "#E57373", "#EF9A9A", 
                              "#90CAF9", "#42A5F5", "#2196F3", "#1976D2", "#0D47A1"),
                    na.value="grey") +
  labs(title = "1924 Presidential Election",
       subtitle = "Margin of Victory") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

#******************** 1988 ********************
Plot_88 <- ggplot(data = County_Shape_1988.SF.2) +
  geom_sf(aes(fill = MOV_factor)) +
  scale_fill_manual(values = fill, na.value="grey", na.translate=FALSE) +
  labs(title = "1988 Presidential Election",
       subtitle = "Margin of Victory") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

#################### Figure 2 #######################
#1924
Plot_24

#1988
Plot_88

#################### Save #################### 

#If you want to save the plot. Change the working directory and save. 
# setwd("C:/Users/Shari/OneDrive/University of California, Davis/Third Year/Incumbency Project/1924 and 1988 Maps/Figures")
# #******************** 1924 ********************
# ggsave(Plot_24, 
#        file = "1924 - MOV - Presidential Election.png",
#        width=7, height=6,  dpi = 300)
# 
# #******************** 1988 ********************
# ggsave(Plot_88, 
#        file = "1988 - MOV - Presidential Election.png",
#        width=7, height=6,  dpi = 300)
