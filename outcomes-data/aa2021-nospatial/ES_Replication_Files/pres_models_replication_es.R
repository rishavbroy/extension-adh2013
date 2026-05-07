######################## Code Summary ##################
#Replication for Partisanship Nationalization in American Elections: Evidence from Presidential, Senatorial, Gubernatorial Elections in the U.S. Counties, 1872-2018
#Electoral Studies
#Algara & Amlani (2021)
#R Version: R 4.0.3
#August 27 2021

#This R script runs the spatial regression models for Presidential elections. This script produces the estimates necessary to create figures 3-6

#NOTE: Please update the working directory/file pathways in the sink function in the Prelude section.

#NOTE: Please update the working directory/file pathways in the Library and Data Upload section.
########################## Prelude #####################
rm(list=ls(all=TRUE))
options(scipen = 3)
set.seed(1993)

#This code creates .txt of model outputs
sink("/Users/caalgara/Desktop/es replication files/ES_Replication_Files/geo_spatial_models_presidential.txt",type="output")

########################## Library and Data Upload #####################
library(readstata13)
library(ggplot2)
library(reshape2)
library(plyr)
library(descr)
library(data.table)
library(readxl)
library(sandwich)
library(lmtest)
library(multiwayvcov)
library(ggmap)
library(data.table)
library(descr)
library(spdep)

set.ZeroPolicyOption(T)

setwd("/Users/caalgara/Desktop/es replication files/ES_Replication_Files/")

load("/Users/caalgara/Desktop/es replication files/ES_Replication_Files/electoral_studies_replication_presidential_analysis_data.Rdata")

# Spatial Lag models--Estimate the parameters taking into account the spatial dependency

library(tmap) 
library(sf)
library(rgdal)
library(raster)
library(tidyverse)

# Load Geoshape Data

load("/Users/caalgara/Desktop/es replication files/ES_Replication_Files/county_geo_spatial_data_1870_2020.Rdata")

########################## Run the Models #####################
## Presidential Elections

electoral_studies_presidential$decade <-paste(substr(electoral_studies_presidential$election_year,1,3),0,sep="")

ols_results_pres <- list()
ols_results_pres_cluster_se <- list()
lm_results_pres <- list()
moran_lm_results_pres <- list()
sar_chi_pres_results <- list()
sar_chi_pres_results_impacts <- list()
errorsalm_chi_pres_results <- list()

for(i in c(2020)){
  
  x <- subset(electoral_studies_presidential,electoral_studies_presidential$election_year == i)
  x <- subset(x,!is.na(x$fips))
  print(paste(nrow(x),"Original Used Counties"))
  x$fips[x$county_name %in% "OGALA LAKOTA"] <- 46102
  
  y <- county_shapes[[as.numeric(unique(x$decade))]]
  
  y@data$STATEFP <- as.character(y@data$STATEFP)
  
  #ggplot(data = y, mapping = aes(x = long, y = lat, group = group)) + coord_fixed(1.3) + theme_nothing() + geom_polygon(data = y, fill = NA, color = "white") + geom_polygon(color = "black", fill = NA)  # get the state border back on top #https://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html
  #plot(y) 
  #tm_shape(y) + tm_fill("white") + tm_borders("grey40", lwd = 0.5) + tm_layout(frame = F) + tm_legend(title=paste(i,"Presidential Election"))
  
  x <- data.table(x)
  x <- x[,duplicates := 1:.N, by=c("fips")]
  x <- subset(x,x$duplicates == 1)
  x$duplicates <- NULL
  x <- data.frame(x)
  
  x$GEOID <- x$fips
  
  y <- sp::merge(y,x,by=c("GEOID"))
  
  y <- y[y@data$fips %in% unique(x$fips),]
  rownames(y@data) <- y@data$fips
  paste(nrow(y@data),"Number Used N Counties")
  
  #print(tm_shape(y) + tm_fill("white") + tm_borders("grey40", lwd = 0.5) + tm_layout(frame = F) + tm_legend(title=paste(i,"Presidential Election")))
  
  print(tm_shape(y) + tm_borders("grey40", lwd = 0.5) + tm_layout(frame = F) + tm_fill(col = "pres_dem_two_party_vote_percent", title = paste(i,"Democratic %"),palette = "RdBu",style="cont")) # DV
  
  # Merge with data frame
  list.queen <- poly2nb(y, queen=T)
  W <-  spdep::nb2listw(list.queen,glist=NULL, style="W", zero.policy=T)
  W
  plot(W,coordinates(y))
  
  chi.ols <- lm(pres_dem_two_party_vote_percent~lagged_pres_dem_two_party_vote_percent +blackpct+forgnpct+people_per_mi2 + factor(state), data=y@data)
  print(summary(chi.ols))
  ols_results_pres[[i]] <- chi.ols
  
  chi.ols2 <- coeftest(chi.ols, vcov = cluster.vcov(chi.ols, cluster=y@data$state))
  ols_results_pres_cluster_se [[i]] <- chi.ols2
  
  LM <- lm.LMtests(chi.ols, W, test="all")
  print(LM)
  lm_results_pres[[i]] <- LM
  
  moran.lm<-lm.morantest(chi.ols, W, alternative="two.sided")
  print(moran.lm)
  moran_lm_results_pres[[i]] <- moran.lm
  
  sar.chi<-lagsarlm(pres_dem_two_party_vote_percent~lagged_pres_dem_two_party_vote_percent +blackpct+forgnpct+people_per_mi2, data=y@data, W,zero.policy = TRUE)
  print(summary(sar.chi))
  sar_chi_pres_results[[i]] <- sar.chi
  
  impacts_sar_chi <- impacts(sar.chi, listw=W)
  print(impacts_sar_chi)
  sar_chi_pres_results_impacts[[i]] <- impacts_sar_chi
  
  errorsalm.chi<-errorsarlm(pres_dem_two_party_vote_percent~lagged_pres_dem_two_party_vote_percent +blackpct+forgnpct+people_per_mi2, data=y@data, W,zero.policy = TRUE)
  print(summary(errorsalm.chi))
  
  errorsalm_chi_pres_results[[i]] <- errorsalm.chi
  print(paste("Done with",i,"Presidential Election"))
}

#save.image("presidential_models_es_replication_1872_2020.Rdata")

for(i in c(2012,2016)){
  
  x <- subset(electoral_studies_presidential,electoral_studies_presidential$election_year == i)
  x <- subset(x,!is.na(x$fips))
  print(paste(nrow(x),"Number Used Counties"))
  x$fips[x$county_name %in% "OGALA LAKOTA"] <- 46102
  
  y <- county_shapes[[as.numeric(unique(x$decade))]]
  
  y@data$STATEFP <- as.character(y@data$STATEFP)
  
  #ggplot(data = y, mapping = aes(x = long, y = lat, group = group)) + coord_fixed(1.3) + theme_nothing() + geom_polygon(data = y, fill = NA, color = "white") + geom_polygon(color = "black", fill = NA)  # get the state border back on top #https://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html
  #plot(y) 
  #tm_shape(y) + tm_fill("white") + tm_borders("grey40", lwd = 0.5) + tm_layout(frame = F) + tm_legend(title=paste(i,"Presidential Election"))
  
  x <- data.table(x)
  x <- x[,duplicates := 1:.N, by=c("fips")]
  x <- subset(x,x$duplicates == 1)
  x$duplicates <- NULL
  x <- data.frame(x)
  
  y@data$fips <- paste(y@data$STATEFP,y@data$COUNTYFP,sep="")
  
  y <- sp::merge(y,x,by=c("fips"))
  
  y <- y[y@data$fips %in% unique(x$fips),]
  
  cat<- data.table(y@data)
  cat <- cat[,duplicates := 1:.N, by=c("fips")]
  descr::freq(cat$duplicates)
  cat <- data.frame(cat)
  y@data <- cat
  
  y <- subset(y,y@data$duplicates == 1)
  
  rownames(y@data) <- y@data$fips
  paste(nrow(y@data),"Number Used N Counties")
  
  #print(tm_shape(y) + tm_fill("white") + tm_borders("grey40", lwd = 0.5) + tm_layout(frame = F) + tm_legend(title=paste(i,"Presidential Election")))
  
  print(tm_shape(y) + tm_borders("grey40", lwd = 0.5) + tm_layout(frame = F) + tm_fill(col = "pres_dem_two_party_vote_percent", title = paste(i,"Democratic %"),palette = "RdBu",style="cont")) # DV
  
  # Merge with data frame
  list.queen <- poly2nb(y, queen=T)
  W <-  spdep::nb2listw(list.queen,glist=NULL, style="W", zero.policy=T)
  W
  plot(W,coordinates(y))
  
  chi.ols <- lm(pres_dem_two_party_vote_percent~lagged_pres_dem_two_party_vote_percent +blackpct+forgnpct+people_per_mi2 + factor(state), data=y@data)
  print(summary(chi.ols))
  ols_results_pres[[i]] <- chi.ols
  
  chi.ols2 <- coeftest(chi.ols, vcov = cluster.vcov(chi.ols, cluster=y@data$state))
  ols_results_pres_cluster_se [[i]] <- chi.ols2
  
  LM <- lm.LMtests(chi.ols, W, test="all")
  print(LM)
  lm_results_pres[[i]] <- LM
  
  moran.lm<-lm.morantest(chi.ols, W, alternative="two.sided")
  print(moran.lm)
  moran_lm_results_pres[[i]] <- moran.lm
  
  sar.chi<-lagsarlm(pres_dem_two_party_vote_percent~lagged_pres_dem_two_party_vote_percent +blackpct+forgnpct+people_per_mi2, data=y@data, W,zero.policy = TRUE)
  print(summary(sar.chi))
  sar_chi_pres_results[[i]] <- sar.chi
  
  impacts_sar_chi <- impacts(sar.chi, listw=W)
  print(impacts_sar_chi)
  sar_chi_pres_results_impacts[[i]] <- impacts_sar_chi
  
  errorsalm.chi<-errorsarlm(pres_dem_two_party_vote_percent~lagged_pres_dem_two_party_vote_percent +blackpct+forgnpct+people_per_mi2, data=y@data, W,zero.policy = TRUE)
  print(summary(errorsalm.chi))
  
  errorsalm_chi_pres_results[[i]] <- errorsalm.chi
  print(paste("Done with",i,"Presidential Election"))
}

#save.image("presidential_models_es_replication_1872_2020.Rdata")

for(i in seq(1872,2008,4)){
  
  x <- subset(electoral_studies_presidential,electoral_studies_presidential$election_year == i)
  x <- subset(x,!is.na(x$fips))
  print(paste(nrow(x),"Original Used Counties"))
  x$fips[x$county_name %in% "OGALA LAKOTA"] <- 46102
  
  y <- county_shapes[[as.numeric(unique(x$decade))]]
  
  y@data$STATENAM <- as.character(y@data$STATENAM)
  
  #ggplot(data = y, mapping = aes(x = long, y = lat, group = group)) + coord_fixed(1.3) + theme_nothing() + geom_polygon(data = y, fill = NA, color = "white") + geom_polygon(color = "black", fill = NA)  # get the state border back on top #https://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html
  #plot(y) 
  #tm_shape(y) + tm_fill("white") + tm_borders("grey40", lwd = 0.5) + tm_layout(frame = F) + tm_legend(title=paste(i,"Presidential Election"))
  
  x <- data.table(x)
  x <- x[,duplicates := 1:.N, by=c("fips")]
  paste(nrow(x),"Number Used N Counties")
  x <- subset(x,x$duplicates == 1)
  x$duplicates <- NULL
  x <- data.frame(x)
  
  y@data$fips <- paste(substr(y@data$GISJOIN2,start=1,stop=2),substr(y@data$GISJOIN2,start=4,stop=6),sep="")
  
  y <- sp::merge(y,x,by=c("fips"))
  
  y <- y[y@data$fips %in% unique(x$fips),]
  
  cat<- data.table(y@data)
  cat <- cat[,duplicates := 1:.N, by=c("fips")]
  descr::freq(cat$duplicates)
  cat <- data.frame(cat)
  y@data <- cat
  
  y <- subset(y,y@data$duplicates == 1)
  
  rownames(y@data) <- y@data$fips
  
  paste(nrow(y@data),"Number Used N Counties")
  
  #print(tm_shape(y) + tm_fill("white") + tm_borders("grey40", lwd = 0.5) + tm_layout(frame = F) + tm_legend(title=paste(i,"Presidential Election")))
  
  print(tm_shape(y) + tm_borders("grey40", lwd = 0.5) + tm_layout(frame = F) + tm_fill(col = "pres_dem_two_party_vote_percent", title = paste(i,"Democratic %"),palette = "RdBu",style="cont")) # DV
  
  # Merge with data frame
  
  list.queen <- poly2nb(y, queen=T)
  W <-  spdep::nb2listw(list.queen,glist=NULL, style="W", zero.policy=T)
  plot(W,coordinates(y))
  
  chi.ols <- lm(pres_dem_two_party_vote_percent~lagged_pres_dem_two_party_vote_percent +blackpct+forgnpct+people_per_mi2 + factor(state), data=y@data)
  print(summary(chi.ols))
  ols_results_pres[[i]] <- chi.ols
  
  chi.ols2 <- coeftest(chi.ols, vcov = cluster.vcov(chi.ols, cluster=y@data$state))
  ols_results_pres_cluster_se [[i]] <- chi.ols2
  
  LM <- lm.LMtests(chi.ols, W, test="all")
  print(LM)
  lm_results_pres[[i]] <- LM
  
  moran.lm<-lm.morantest(chi.ols, W, alternative="two.sided")
  print(moran.lm)
  moran_lm_results_pres[[i]] <- moran.lm
  
  sar.chi<-spatialreg::lagsarlm(pres_dem_two_party_vote_percent~lagged_pres_dem_two_party_vote_percent +blackpct+forgnpct+people_per_mi2, data=y@data, W,zero.policy = TRUE)
  print(summary(sar.chi))
  sar_chi_pres_results[[i]] <- sar.chi
  
  impacts_sar_chi <- impacts(sar.chi, listw=W)
  print(impacts_sar_chi)
  sar_chi_pres_results_impacts[[i]] <- impacts_sar_chi
  
  errorsalm.chi<-errorsarlm(pres_dem_two_party_vote_percent~lagged_pres_dem_two_party_vote_percent +blackpct+forgnpct+people_per_mi2, data=y@data, W,zero.policy = TRUE)
  print(summary(errorsalm.chi))
  
  errorsalm_chi_pres_results[[i]] <- errorsalm.chi
  print(paste("Done with",i,"Presidential Election"))
}

rm(county_shapes,i,chi.ols2,y,x,W,sar.chi,moran.lm,missing_county_pop,LM,list.queen,j,impacts_sar_chi,errorsalm.chi,chi.ols,cat)

save.image("presidential_models_es_replication_1872_2020.Rdata")
