######################## Code Summary ##################
#Replication for Partisanship Nationalization in American Elections: Evidence from Presidential, Senatorial, Gubernatorial Elections in the U.S. Counties, 1872-2018
#Electoral Studies
#Algara & Amlani (2021)
#R Version: R 4.0.3
#August 27 2021

#This R script plots Figures 1

########################## Notes ########################
#Set working directory on line 23 for county level data

#Set working directory and upload state level DTA file on line 76

########################## Prelude #####################
rm(list=ls(all=TRUE))
options(scipen=999)
set.seed(1993)

######################## Upload Data ##################

#Set the working directory here:


#Upload Data
load("/Users/caalgara/Desktop/es replication files/ES_Replication_Files/dataverse_shareable_presidential_county_returns_1868_2020.Rdata")

######################## ANOVAs and Plots ##################

# ANOVAs at the county-level
pres_elections_release$dem_two_party <- (pres_elections_release$democratic_raw_votes/(pres_elections_release$democratic_raw_votes+pres_elections_release$republican_raw_votes))
pres_elections_release$gop_two_party <- (pres_elections_release$republican_raw_votes/(pres_elections_release$democratic_raw_votes+pres_elections_release$republican_raw_votes))

pres_elections_release$abs_margin <- (pres_elections_release$dem_two_party-pres_elections_release$gop_two_party)

# The Democratic Party did not have a presidential ticket on the ballot in the states of Colorado, Idaho, Kansas, North Dakota, or Wyoming, and Populist Weaver won the first four of these states. Florida Republicans decided not to put up presidential electors for Harrison, urging their supported to back Populist James B. Weaver, thus he was not on the ballot.

pres_elections_release$abs_margin[pres_elections_release$state %in% "CO" &  pres_elections_release$election_year %in% "1892"] <- NA
pres_elections_release$abs_margin[pres_elections_release$state %in% "ID" &  pres_elections_release$election_year %in% "1892"] <- NA
pres_elections_release$abs_margin[pres_elections_release$state %in% "KS" &  pres_elections_release$election_year %in% "1892"] <- NA
pres_elections_release$abs_margin[pres_elections_release$state %in% "WY" &  pres_elections_release$election_year %in% "1892"] <- NA
pres_elections_release$abs_margin[pres_elections_release$state %in% "ND" &  pres_elections_release$election_year %in% "1892"] <- NA
pres_elections_release$abs_margin[pres_elections_release$state %in% "FL" &  pres_elections_release$election_year %in% "1892"] <- NA

library(car)

anovas <- list()
for(i in seq(1868,2020,4)){
  x <- subset(pres_elections_release,pres_elections_release$election_year %in% i,select=c(state,county_name,abs_margin,democratic_raw_votes,republican_raw_votes,dem_two_party,gop_two_party))
  x$state <- factor(x$state)
  cat <- aov(abs_margin~state,data=x)
  paste(i)
  summary(cat)
  df <- data.frame(election_year=i,within_state_sum_squares=summary(cat)[[1]]["Residuals", "Sum Sq"],between_state_sum_squares=summary(cat)[[1]]["state", "Sum Sq"])
  df$total_sum_squares <- df$between_state_sum_squares + df$within_state_sum_squares
  anovas[[i]] <- df
}

library(plyr)
anovas <- ldply(anovas,data.frame)

anovas$between_variance <- anovas$between_state_sum_squares/anovas$total_sum_squares
anovas$within_variance <- anovas$within_state_sum_squares/anovas$total_sum_squares

anovas$label <- ifelse(anovas$election_year %in% 1988,"1988",ifelse(anovas$election_year %in% 1924,"1924",NA))

library(ggrepel)

plot <- ggplot(anovas,aes(x=election_year,y=within_variance,label=label)) + geom_point(shape=21) + theme_minimal() + stat_smooth(method="loess") + scale_x_continuous("",breaks=seq(1868,2020,8)) + scale_y_continuous("Proportion of County-Level Variation in Two-Party Margin",breaks=seq(0,1,0.10)) + labs(title="Proportion of County-Level Variation in Two-Party Presidential Margin Due to \nWithin-State Variation, 1868-2020",caption="Statistic derived from ANOVA assessing variation in county-level margin due to within & between state voting differences in a given election.\n Proportion of variation due to within-state differences = sum of the squared residuals/total sum of squares.") + geom_text_repel(min.segment.length = 0, seed = 42, box.padding= 0.5,point.padding = 0.5,arrow = arrow(length = unit(0.015, "npc"))) + geom_hline(yintercept = 0.5, colour = gray(1/2), lty = 2)
ggsave(file="within_county_variation_presidential_margin.png",plot,width = 8, height = 6, units = "in")


# ANOVAs at the state-level

library(readstata13)
pres <- read.dta13("/Users/caalgara/Desktop/es replication files/ES_Replication_Files/1868_2020_presvote.dta")
pres <- subset(pres,pres$year >= 1868)
pres$dem_two_party <- (pres$dvote/(pres$dvote+pres$rvote))
pres$gop_two_party <- (pres$rvote/(pres$dvote+pres$rvote))
pres$abs_margin <- (pres$dem_two_party-pres$gop_two_party)

pres <- subset(pres,pres$state != "US")

pres$abs_margin[pres$state %in% "CO" &  pres$year %in% "1892"] <- NA
pres$abs_margin[pres$state %in% "ID" &  pres$year %in% "1892"] <- NA
pres$abs_margin[pres$state %in% "KS" &  pres$year %in% "1892"] <- NA
pres$abs_margin[pres$state %in% "WY" &  pres$year %in% "1892"] <- NA
pres$abs_margin[pres$state %in% "ND" &  pres$year %in% "1892"] <- NA
pres$abs_margin[pres$state %in% "FL" &  pres$year %in% "1892"] <- NA

pres <- subset(pres,select=c(year,state,abs_margin,dem_two_party,gop_two_party))

pres <- na.omit(pres)

anovas2 <- list()
for(i in seq(1868,2020,4)){
  x <- subset(pres,pres$year %in% i,select=c(state,abs_margin,dem_two_party,gop_two_party))
  x <- na.omit(x)
  x$state <- factor(x$state)
  cat <- aov(abs_margin~state,data=x)
  paste(i)
  summary(cat)
  df <- data.frame(election_year=i,within_state_sum_squares=summary(cat)[[1]]["Residuals", "Sum Sq"],between_state_sum_squares=summary(cat)[[1]]["state", "Sum Sq"])
  df$total_sum_squares <- df$between_state_sum_squares + df$within_state_sum_squares
  anovas2[[i]] <- df
}

anovas2 <- ldply(anovas2,data.frame)

anovas2$between_variance <- anovas2$between_state_sum_squares/anovas2$total_sum_squares
anovas2$within_variance <- anovas2$within_state_sum_squares/anovas2$total_sum_squares

# Comparing variances

variances <- list()
for(i in seq(1868,2020,4)){
  x <- subset(pres,pres$year %in% i,select=c(state,abs_margin,dem_two_party,gop_two_party))
  x1 <- subset(pres_elections_release,pres_elections_release$election_year %in% i,select=c(state,county_name,abs_margin,democratic_raw_votes,republican_raw_votes,dem_two_party,gop_two_party))
  x <- na.omit(x)
  x1 <- na.omit(x1)
  cat <- var.test(x$abs_margin,x1$abs_margin,alternative = "two.sided")
  df <- data.frame(election_year=i,variance_state=var(x$abs_margin),variance_county=var(x1$abs_margin),mean_margin_state=mean(x$abs_margin),mean_margin_county=mean(x1$abs_margin),sd_margin_state=sd(x$abs_margin),sd_margin_county=sd(x1$abs_margin),state_n=nrow(x),county_n=nrow(x1),f_statistic=cat$statistic,f_test_p_value=cat$p.value)
  variances[[i]] <- df
}
variances <- ldply(variances,data.frame)
variances$population_variance_county <- variances$variance_county*((variances$county_n-1)/variances$county_n)
variances$population_variance_state <- variances$variance_state*((variances$state_n-1)/variances$state_n)

x <- variances[,c(1,13)]
x1 <- variances[,c(1,12)]

colnames(x)[2] <- "variance"
colnames(x1)[2] <- "variance"

x$level <- "State-Level Variance"
x1$level <- "County-Level Variance"

y <- rbind(x,x1)

plot <- ggplot(y,aes(x=election_year,y=variance,color=level,shape=level)) + geom_point() + theme_minimal() + scale_x_continuous("",breaks=seq(1868,2020,8)) + scale_y_continuous("Variance in Aggregate Election Outcomes",breaks=seq(0,1,0.05)) + labs(title="Comparing County & State Level Variance in Two-Party Presidential Margin, 1868-2020",caption="Population variance static articulated.") + theme(legend.position = "none") + geom_line() + scale_color_manual("",values=c("coral3","dodgerblue3")) + scale_shape_manual("",values=c(1,1))  + annotate("label", x = 2011, y = 0.12, label = "County-Level",color="coral3") + annotate("label", x = 2012, y = 0.03, label = "State-Level",color="dodgerblue3") 
ggsave(file="population_variance_presidential.png",plot,width = 8, height = 6, units = "in")