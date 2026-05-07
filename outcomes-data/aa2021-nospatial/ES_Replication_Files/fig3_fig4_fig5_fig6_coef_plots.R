######################## Code Summary ##################
#Replication for Partisanship Nationalization in American Elections: Evidence from Presidential, Senatorial, Gubernatorial Elections in the U.S. Counties, 1872-2018
#Electoral Studies
#Algara & Amlani (2021)
#R Version: R 4.0.3
#August 27 2021

#This R script unpacks and plots spatial regression models in figures 3-6

#!NOTE!: Reset the working directory of the Model Outputs in the "Upload Data" section

#!NOTE!: Define the working directory (as a pathway) where the Figures should output to in the "PLOT WORKING DIRECTORY" section on line 201

######################### Notes ################
#Models of Interest:
#errorsalm_chi_gov_results
#sar_chi_gov_results
########################## Prelude #####################

rm(list=ls(all=TRUE))
options(stringsAsFactors = FALSE)
options(scipen = 3)
set.seed(1993)

######################### Functions ###################
'%!in%' <- function(x,y)!('%in%'(x,y))

#Turns a Regression into a data frame
Model.DF <- function(Model, Robust.SE = NULL) {
  
  #Extract Coefficients
  Model.Output <- as.data.frame(coef(summary(Model)))
  Model.Output$Label <- rownames(Model.Output)
  rownames(Model.Output) <- NULL
  
  #Generate Confidence Intervals
  CI <- as.data.frame(confint(Model, variable.names(Model), level=0.95))
  CI$Label <- rownames(CI)
  rownames(CI) <- NULL
  
  #Merge Model and CIs together 
  Model.Output.Final <- merge(x = Model.Output, y = CI, by =c("Label"))
  
  #Name the columns numeric
  colnames(Model.Output.Final) <- c("Label", "Coeff", "SE", "t.value", "P.Value", "lower", "upper")
  
  Model.Output.Final$Sig.05 <- ifelse(Model.Output.Final$P.Value <= .05, 1,0)
  Model.Output.Final$Sig.10 <- ifelse(Model.Output.Final$P.Value <= .10, 1,0)
  
  #Adjusted R Squared
  Model.Output.Final$AdJ.R2 <- summary(Model)$adj.r.squared
  
  #Dependent Variable
  Model.Output.Final$DV <- all.vars(formula(Model))[1]
  
  #Check for NA's in Model
  for(n in names(coef(Model))){
    if(is.na(Model$coefficients[[n]]) == T){
      newRow <- data.frame(Label=n, 
                           Coeff = NA, 
                           SE = NA, 
                           t.value = NA,
                           P.Value = NA,
                           lower = NA,
                           upper = NA,
                           AdJ.R2 = NA, 
                           Sig.05 = NA,
                           Sig.10 = NA,
                           DV=all.vars(formula(Model))[1])
      
      Model.Output.Final <- rbind(Model.Output.Final, newRow)
      
    }
  }
  
  #Option for Robust Standard Errors
  if(is.null(Robust.SE) == F){
    library(sandwich)
    x<- coeftest(Model, vcov = sandwich::vcovHC(Model, type=Robust.SE))
    xr<- setNames(data.frame(x[1:dim(x)[1], 2]), c("Robust Standard Errors"))
    xr$Label<- rownames(xr); rownames(xr) <- NULL
    
    Model.Output.Final <- merge(Model.Output.Final, xr, by = "Label")
    
  }
  
  return(Model.Output.Final)
  
}


# Plot Function
Plot_Vote <- function(Data, IV, o, Model){
  if(o != "President"){
    library(ggplot2)
    plot_final <- ggplot(data = subset(Data, Label_Plot == IV & Office_Plot == o),
                         aes(x = year, y = Coeff, ymin=lower,ymax=upper)) +
      geom_smooth(method="loess", se = T, color = "darkgrey", linetype = "solid")  + 
      geom_pointrange() + 
      theme_minimal() + 
      labs(title = paste("County-Level: ", IV, " on ", o,  " Vote", sep = ""),
           subtitle = paste("Model: ", Model, sep = ""),
           x = "Year",
           y = "Model Estmate")  + 
      geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
      scale_x_continuous("Year",breaks=seq(min(Data$year),max(Data$year),20)) 
    return(plot_final)
    
  }
  
  if(o == "President"){
    library(ggplot2)
    plot_final <- ggplot(data = subset(Data, Label_Plot == IV & Office_Plot == o),
                         aes(x = year, y = Coeff, ymin=lower,ymax=upper)) +
      geom_smooth(method="loess", se = T, color = "darkgrey", linetype = "solid")  + 
      geom_pointrange() + 
      theme_minimal() + 
      labs(title = paste("County-Level: ", IV, " on ", o,  " Vote", sep = ""),
           subtitle = paste("Model: ", Model, sep = ""),
           x = "Year",
           y = "Model Estmate") +
      geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
      scale_x_continuous("Year",breaks=seq(min(Data$year),max(Data$year),20)) 
    return(plot_final)
    
  }
}


#************** Function
Model.DF.SpatialModel <- function(Model){
  
  Model_DF <- data.frame(Coeff = Model$coefficients,
                         Asy_SE = Model$rest.se)
  Model_DF$Label <- rownames(Model_DF)
  rownames(Model_DF) <- NULL
  
  #Confidence Intervals
  Model_DF$lower <- Model_DF$Coeff - 1.96*Model_DF$Asy_SE
  
  Model_DF$upper <- Model_DF$Coeff + 1.96*Model_DF$Asy_SE
  Model_DF
  
  #Dependent Varaible
  Model_DF$DV <- all.vars(Model$call)[1]
  
  #Signifigance 
  Model_DF$Sig<- ifelse((Model_DF$lower <= 0) & (Model_DF$upper >= 0), 0, 1)
  
  return(Model_DF)
  
  
}

######################### Library #####################
library(broom)
library(spdep)
library(spatialreg)

######################## Upload Data ##################
#*********************** Governor Data *****************
#Set Working Directory
setwd("/Users/caalgara/Desktop/es replication files/ES_Replication_Files/")

#Upload Data
load("gov_models_es_replication_1872_2020.rData")

#************************ Rename Gov Objects ********************
lm_results_gov <- lm_results_pres; rm(lm_results_pres) #Lagrange multiplier
moran_lm_results_gov <- moran_lm_results_pres;rm(moran_lm_results_pres)
ols_results_gov <- ols_results_pres; rm(ols_results_pres)
ols_results_gov_cluster_se <- ols_results_pres_cluster_se; rm(ols_results_pres_cluster_se)
errorsalm_chi_gov_results <- errorsalm_chi_pres_results; rm(errorsalm_chi_pres_results)
sar_chi_gov_results <- sar_chi_pres_results; rm(sar_chi_pres_results)
sar_chi_gov_results_impacts <- sar_chi_pres_results_impacts; rm(sar_chi_pres_results_impacts)

#*********************** Senate Data *****************
#Set Working Directory
setwd("/Users/caalgara/Desktop/es replication files/ES_Replication_Files/")

load("senate_models_es_replication_1914_2020.rData")

#************************ Rename Senate Objects ********************
lm_results_senate <- lm_results_pres; rm(lm_results_pres) #Lagrange multiplier
moran_lm_results_senate <- moran_lm_results_pres;rm(moran_lm_results_pres)
ols_results_senate <- ols_results_pres; rm(ols_results_pres)
ols_results_senate_cluster_se <- ols_results_pres_cluster_se; rm(ols_results_pres_cluster_se)
errorsalm_chi_senate_results <- errorsalm_chi_pres_results; rm(errorsalm_chi_pres_results)
sar_chi_senate_results <- sar_chi_pres_results; rm(sar_chi_pres_results)
sar_chi_senate_results_impacts <- sar_chi_pres_results_impacts; rm(sar_chi_pres_results_impacts)

#*********************** Presidency Data *****************
#Set Working Directory
setwd("/Users/caalgara/Desktop/es replication files/ES_Replication_Files/")

load("presidential_models_es_replication_1872_2020.rData")

######################### PLOT WORKING DIRECTORY ############################
#PLEASE DEFINE THE PLOT WORKING DIRECTORY AS A FILE PATHWAY HERE
Plot_Working_Directory <- "/Users/caalgara/Desktop/es replication files/ES_Replication_Files/"

######################### Models of Interest ################
#Models of Interest:
#errorsalm_chi_gov_results
#sar_chi_gov_results

###################### Extract Spatial Models ##########################
#Test
broom::tidy(errorsalm_chi_senate_results[[2018]]) 
errorsalm_chi_senate_results[[2018]]$coefficients
errorsalm_chi_senate_results[[2018]]$rest.se
Model.DF.SpatialModel(errorsalm_chi_senate_results[[2018]])


Model_type <- c("sar_chi_", "errorsalm_chi_")
office <- c("gov", "senate", "pres")
DF_Model_Final_Spatial <- NULL

for(t in Model_type){
  for(o in office){
    
    if(o == "gov"){
      for(i in seq(1872, 2020, by =2)){ #Min Gov Year to Max Gov Year
        Model_Loop<- get(paste(t, o, "_results", sep = ""))
        DF_Model <- broom::tidy(Model_Loop[[i]])
        DF_Model$year <- i
        DF_Model$Office <- o
        DF_Model$Model_Type <- t
        DF_Model_Final_Spatial <- rbind(DF_Model_Final_Spatial, DF_Model)
      }
    }
    
    if(o == "senate"){
      for(i in seq(1914, 2020, by =2)){ #Min Senate Year to Max Senate Year
        Model_Loop<- get(paste(t, o, "_results", sep = ""))
        DF_Model <- broom::tidy(Model_Loop[[i]])
        DF_Model$year <- i
        DF_Model$Office <- o
        DF_Model$Model_Type <- t
        DF_Model_Final_Spatial <- rbind(DF_Model_Final_Spatial, DF_Model)
      }
    }
    
    if(o == "pres"){
      for(i in seq(1872, 2020, by =4)){ #Min Pres Year to Max Pres Year
        Model_Loop<- get(paste(t, o, "_results", sep = ""))
        DF_Model <- broom::tidy(Model_Loop[[i]])
        DF_Model$year <- i
        DF_Model$Office <- o
        DF_Model$Model_Type <- t
        DF_Model_Final_Spatial <- rbind(DF_Model_Final_Spatial, DF_Model)
      }
    }
    
  }
}


#************** Change Column Names  ********************
colnames(DF_Model_Final_Spatial)
colnames(DF_Model_Final_Spatial)[1:2] <- c("Label", "Coeff")

#************** Key Stats  ********************
#Confidence Intervals
DF_Model_Final_Spatial$lower <- DF_Model_Final_Spatial$Coeff - (1.96*DF_Model_Final_Spatial$std.error)

DF_Model_Final_Spatial$upper <- DF_Model_Final_Spatial$Coeff + (1.96*DF_Model_Final_Spatial$std.error)

#Significance 
DF_Model_Final_Spatial$Sig<- ifelse(DF_Model_Final_Spatial$p.value < 0.05, 1, 0)

#*************** Coefficient Checks *******************
data.frame(subset(DF_Model_Final_Spatial, Office == "senate" & year == 2016 & Model_Type == "errorsalm_chi_"))
errorsalm_chi_senate_results[[2016]]$coefficients
errorsalm_chi_senate_results[[2016]]$rest.se

#************** Separate Spatial Models ********************
DF_Model_Final_Spatial_Sar_Chi <- data.frame(subset(DF_Model_Final_Spatial, Model_Type == "sar_chi_"))
DF_Model_Final_Spatial_ErrorSalm <- data.frame(subset(DF_Model_Final_Spatial, Model_Type == "errorsalm_chi_"))

######################## Data Management #######################

#********************* Subset Coefficients in F3-F6 *********************

DF_Model_Final_Spatial_Sar_Chi<- subset(DF_Model_Final_Spatial_Sar_Chi, Label %in% c("pres_dem_two_party_vote_percent", "lagged_pres_dem_two_party_vote_percent",
                                       "blackpct", "forgnpct", "people_per_mi2"))

DF_Model_Final_Spatial_ErrorSalm<- subset(DF_Model_Final_Spatial_ErrorSalm, Label %in% c("pres_dem_two_party_vote_percent", "lagged_pres_dem_two_party_vote_percent",
                                                                                     "blackpct", "forgnpct", "people_per_mi2"))

#****************** Data Management: Office 
DF_Model_Final_Spatial_Sar_Chi$Office_Plot[DF_Model_Final_Spatial_Sar_Chi$Office == "gov"] <- "Governor"
DF_Model_Final_Spatial_Sar_Chi$Office_Plot[DF_Model_Final_Spatial_Sar_Chi$Office == "pres"] <- "President"
DF_Model_Final_Spatial_Sar_Chi$Office_Plot[DF_Model_Final_Spatial_Sar_Chi$Office == "senate"] <- "Senate"

DF_Model_Final_Spatial_ErrorSalm$Office_Plot[DF_Model_Final_Spatial_ErrorSalm$Office == "gov"] <- "Governor"
DF_Model_Final_Spatial_ErrorSalm$Office_Plot[DF_Model_Final_Spatial_ErrorSalm$Office == "pres"] <- "President"
DF_Model_Final_Spatial_ErrorSalm$Office_Plot[DF_Model_Final_Spatial_ErrorSalm$Office == "senate"] <- "Senate"

#****************** Data Management: Labels
DF_Model_Final_Spatial_Sar_Chi$Label_Plot[DF_Model_Final_Spatial_Sar_Chi$Label == "pres_dem_two_party_vote_percent"] <- "Presidental Vote Share"
DF_Model_Final_Spatial_Sar_Chi$Label_Plot[DF_Model_Final_Spatial_Sar_Chi$Label == "forgnpct"] <- "Percent Foreign Born"
DF_Model_Final_Spatial_Sar_Chi$Label_Plot[DF_Model_Final_Spatial_Sar_Chi$Label == "blackpct"] <- "Percent Black"
DF_Model_Final_Spatial_Sar_Chi$Label_Plot[DF_Model_Final_Spatial_Sar_Chi$Label == "people_per_mi2"] <- "Population Density"
DF_Model_Final_Spatial_Sar_Chi$Label_Plot[DF_Model_Final_Spatial_Sar_Chi$Label == "lagged_pres_dem_two_party_vote_percent"] <- "Lagged Presidental Vote Share"
DF_Model_Final_Spatial_Sar_Chi$Label_Plot[DF_Model_Final_Spatial_Sar_Chi$Label == "dem_county_pres_voteshare" ] <- "County Presidental Vote Share"

DF_Model_Final_Spatial_ErrorSalm$Label_Plot[DF_Model_Final_Spatial_ErrorSalm$Label == "pres_dem_two_party_vote_percent"] <- "Presidental Vote Share"
DF_Model_Final_Spatial_ErrorSalm$Label_Plot[DF_Model_Final_Spatial_ErrorSalm$Label == "forgnpct"] <- "Percent Foreign Born"
DF_Model_Final_Spatial_ErrorSalm$Label_Plot[DF_Model_Final_Spatial_ErrorSalm$Label == "blackpct"] <- "Percent Black"
DF_Model_Final_Spatial_ErrorSalm$Label_Plot[DF_Model_Final_Spatial_ErrorSalm$Label == "people_per_mi2"] <- "Population Density"
DF_Model_Final_Spatial_ErrorSalm$Label_Plot[DF_Model_Final_Spatial_ErrorSalm$Label == "lagged_pres_dem_two_party_vote_percent"] <- "Lagged Presidental Vote Share"
DF_Model_Final_Spatial_ErrorSalm$Label_Plot[DF_Model_Final_Spatial_ErrorSalm$Label == "dem_county_pres_voteshare" ] <- "County Presidental Vote Share"

################# Plot Coefficients ##################
library(ggplot2)
for(o in unique(DF_Model_Final_Spatial_Sar_Chi$Office_Plot)){
  
  #Grab unique Labels in data
  Labels_Model <- unique(subset(DF_Model_Final_Spatial_Sar_Chi, Office_Plot == o)$Label_Plot)
  
  #Drop fixed effects coeff and the intercept coefficient
  Drop <- append(grep("factor(state)", Labels_Model, value = TRUE, fixed = TRUE), "(Intercept)")
  
  #Values includes the left over coefficients. The ones we want to plot
  Values <- na.omit(Labels_Model[Labels_Model %!in% Drop ])
  
  #**************** Plot Coefficients ************
  #Here, I Iterate over the coeffs and plot them
  n <- 1
  plot_list <- list()
  for(v in Values){
    
    #Running the plot function
    plot_list[[n]] <-Plot_Vote(DF_Model_Final_Spatial_Sar_Chi, v, o, "Spatial Simultaneous Autoregressive Lag Model Estimation (lagsarlm)")
    plot_list[[n + 1]] <-Plot_Vote(DF_Model_Final_Spatial_ErrorSalm, v, o, "Spatial Simultaneous Autoregressive Error Model Estimation (errorsarlm)")
    
    n <- n + 2 #Here, I keep track the number of plots
  }
  
  #****************** Save Plots ************
  #Set Plot Working Directory
  setwd(Plot_Working_Directory)

  #Save Individual Plots
  for(i in seq(1, length(plot_list), by = 1)){
    
    ggplot2::ggsave(plot_list[[i]], 
                    file = paste("Plot Results ", i, " - ", o, ".png", sep = ""),
                    width=7, height=6)
    
  }
  
}


