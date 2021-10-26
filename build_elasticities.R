####LIBRARIES####
library(dplyr)
library(tidyr)
library(factoextra)
library(readr)
library(RStata)

#For Bayesian IV:
library(rstan)
suppressPackageStartupMessages(library(mlbench))
suppressPackageStartupMessages(library(rstanarm))
suppressPackageStartupMessages(library(bayestestR))
suppressPackageStartupMessages(library(bayesplot))
suppressPackageStartupMessages(library(insight))
suppressPackageStartupMessages(library(broom))
library(EstimationTools)
options(mc.cores = parallel::detectCores())
#####DATA IMPORT####
setwd("C:/Users/Ansel/Box Sync/Documents/Structural Change/Job market paper/Markdown project/Industry elasticites/Use tables_Detailed")

#The use table shows the uses of commodities by intermediate and final
# users. In contrast to the make table, the rows in the use table present the commodities
# or products, and the columns display the industries and final users that utilize
# them. The sum of the entries in a row is the output of that commodity. The columns show
# the products consumed by each industry and the three components of
# "value added"-compensation of employees, taxes on production and imports less
# subsidies, and gross operating surplus. Value added is the difference between an
# industry's output and the cost of its intermediate inputs, and total value added is
# equal to GDP. The sum of the entries in a column is that industry's output.
# The use table is sometimes referred to as a "recipe" matrix because it
# shows the components that are necessary for producing the output of each industry.


#import use tables:
temp = list.files(pattern="*.csv")
use_tables = lapply(temp, read_csv, skip=4)
use_tables=lapply(use_tables, function(x){x<-x[-c(1),]})
commodity_code_key<-use_tables[[1]][,c(1,2)]
use_tables=lapply(use_tables, function(x){apply(x[,-c(1,2)], 2, function(y){y<-as.numeric(y)})})
use_tables=lapply(use_tables, cbind, commodity_code_key)


#####DATA WRANGLING#####

#Select 3-digit manufacturing industries:
use_tables=lapply(use_tables, function(x){
  x %>% dplyr::select(starts_with("3") | starts_with("X1") | starts_with("Commodities/Industries"))
})

#combine the two 336 industries:
use_tables=lapply(use_tables, function(x){
  
  #NA->0
  x[is.na(x)]<-0
  
  #combine 336 rows:
  temp<-subset(x, X1=="3361MV" | X1=="3364OT")
  temp$X1<-"336"
  temp$`Commodities/Industries`<-"Transportation Equipment Manufacturing"
  temp<-temp %>% group_by(X1, `Commodities/Industries`) %>% summarize_all(sum, na.rm=TRUE)
  x<-bind_rows(subset(x, X1!="3361MV" & X1!="3364OT"),temp)
  rm(temp)
  
  #combine 336 columns:
  x<-x %>% 
    mutate(`336`=`3361MV`+`3364OT`) %>%
    dplyr::select(!ends_with("MV") & !ends_with("OT"))
})

#update commodity_code_key:
commodity_code_key$X1[commodity_code_key$X1=="3361MV"]<-"336"
commodity_code_key$`Commodities/Industries`[commodity_code_key$X1=="336"]<-"Transportation Equipment Manufacturing"
commodity_code_key<-subset(commodity_code_key, commodity_code_key$X1!="3364OT")


#establish global manufacturing industry list:
ind_list<-substr(colnames(use_tables[[1]][,-c(which(colnames(use_tables[[1]])=="X1" | colnames(use_tables[[1]])=="Commodities/Industries"))]),1,3)


#rename columns with years:
for(i in 1:length(temp)){
  use_tables[[i]] <- use_tables[[i]] %>%
    rename_at(vars(-X1, -`Commodities/Industries`), funs(paste(.,i+1996, sep="_")))
}



#Now create panel dataframes for each industry...
#To do this, for each industry I need to go through each use-table and select the corresponding 
#column. Make sure to keep track of what year that column belongs to.


#initialize industry panels:
df<-use_tables[[1]][,c(which(colnames(use_tables[[1]])=="X1" | colnames(use_tables[[1]])=="Commodities/Industries"))]
industry_panels<-list(df)[rep(1,length(ind_list))]
rm(df)

#create industry panels:
for(i in 1:length(ind_list)){
  for(j in 1:length(temp)){
    industry_panels[[i]]<-cbind(dplyr::select(use_tables[[j]], starts_with(ind_list[i])), industry_panels[[i]])
  }
}


#reshape for regression:
industry_panels_long<-lapply(industry_panels, gather, year, value, 1:23)
industry_panels_long<-lapply(industry_panels_long, dplyr::select, !starts_with("X1") )
industry_panels_wide<-lapply(industry_panels_long, spread, `Commodities/Industries`, value )

rm(industry_panels_long, industry_panels)


#ADD LABOR INPUTS:

#Should I use FTPT or FT equivalent here?
#Benefit of FTPT: is consistent with employment definition used in other parts of the paper
#Benefits of FT equivalent:  more accurate measure of input...


#import:
ind_empl <- read_csv("C:/Users/Ansel/Box Sync/Documents/Structural Change/Job market paper/Markdown project/Industry elasticites/Industry employment/ind_empl.csv", 
                     skip = 7)

#combine 366 rows in employment:
temp<-subset(ind_empl, Industry=="Motor vehicles, bodies and trailers, and parts" | Industry=="Other transportation equipment")
temp$Industry<-"Transportation Equipment Manufacturing"
temp$Line<-"22"
temp$X3<-"N4322C"
temp<-temp %>% group_by(Industry,Line,X3) %>% summarize_all(sum, na.rm=TRUE)
ind_empl<-bind_rows(subset(ind_empl, Industry!="Motor vehicles, bodies and trailers, and parts" & Industry!="Other transportation equipment"),temp)
rm(temp)

#clean:
ind_empl<-ind_empl %>%
  dplyr::select(!starts_with("Line") & !starts_with("X3")) %>%
  filter(!is.na("Industry"))

#reshape
ind_empl_long<-gather(ind_empl,year,Labor, `1998`:`2019`)
ind_empl_long<-merge(ind_empl_long, commodity_code_key, by.x = "Industry", by.y="Commodities/Industries")

rm(ind_empl)

ind_empl_long$year<-paste(ind_empl_long$X1, ind_empl_long$year, sep = "_")
ind_empl_long<- ind_empl_long%>%
  dplyr::select(starts_with("year") | starts_with("Labor"))

#merge with industry panels to create complete input panel:
industry_panels_wide_complete<-lapply(industry_panels_wide, merge, ind_empl_long, by="year")
rm(industry_panels_wide, ind_empl_long)

#split year and industry into seperate variables:
industry_panels_wide_complete<-lapply(industry_panels_wide_complete, function(x){
  x %>%
    mutate(Ind=substr(year, 1,nchar(year)-5),
           year=as.numeric(substr(year,nchar(year)-3,nchar(year))))
  })


#####ESTABLISH FUNCTIONAL FORM OF PRODUCTION FUNCTION(S)#####

#From IOManual: "Homogeneity principle: One of the three fundamental principles underlying the I-O accounts.
#Under this principle, each industry's output is produced with a unique
#set of inputs or a unique production function."

#define aggregate production function inputs:
l<-"Labor"
k<-c("333", "334", "3361MV", "3364OT", "337", "339")
m<-c("111CA", "113FF",
     "211", "212", "23",
     "321", "327", "331", "332", "311FT", "313TT", "315AL", "322", "323", "324", "325", "326",
     "42", "441", "445", "452", "481", "482", "483", "484", "485", "486", "487OS", "493",
     "511","512","513","514","521CI","523","524","525","HS","ORE","532RL","5411","5415","5412OP","55","561","562","61",
     "621","622","623","624","711AS","713","721","722","81")
e<-c("22")   
y<-"T018" #total output

#Aggregate inputs and outputs and transform to log:
industry_panels_wide_complete=lapply(industry_panels_wide_complete,function(x){
  x %>% 
    mutate(
      l = log(rowSums(dplyr::select(., all_of(l)), na.rm = TRUE)),
      k = log(rowSums(dplyr::select(., all_of(commodity_code_key$`Commodities/Industries`[commodity_code_key$X1 %in% k])), na.rm = TRUE)),
      m = log(rowSums(dplyr::select(., all_of(commodity_code_key$`Commodities/Industries`[commodity_code_key$X1 %in% m])), na.rm = TRUE)),
      e = log(rowSums(dplyr::select(., all_of(commodity_code_key$`Commodities/Industries`[commodity_code_key$X1 %in% e])), na.rm = TRUE)),
      y = log(rowSums(dplyr::select(., all_of(commodity_code_key$`Commodities/Industries`[commodity_code_key$X1 %in% y])), na.rm = TRUE)) ) %>%
    dplyr::select(., all_of(c("l","k","m","e","y", "year", "Ind")))
})


#establish complete panel of all industries:
complete_panel<-industry_panels_wide_complete[[1]]

for(i in 2:length(industry_panels_wide_complete)){
  complete_panel<-rbind(complete_panel, industry_panels_wide_complete[[i]])
}

####BAYESIAN IV ESTIMATION (commented out for quick running)####

#Example from: https://www.r-bloggers.com/2020/04/bayesian-linear-regression/ 
#see Insdustry elasticities/Estimation/Bayesian_IV_esimation.R for source code
#
#construct lag variables:
complete_panel<-complete_panel[order(complete_panel$year),]
complete_panel<- complete_panel %>%
  group_by(Ind) %>%
  mutate(l_lag=dplyr::lag(l),
         m_lag=dplyr::lag(m),
         e_lag=dplyr::lag(e)) %>%
  ungroup()



#define list of models:
models<-list()

#estimate:
for(i in unique(complete_panel$Ind)){
  x<-subset(complete_panel, Ind==i)

  #FREQUENTIST MODEL:
  #model_freq<-lm(y~0+l+k+e+m, data=x)
  #broom::tidy(model_freq)


  #BAYESIAN LINEAR MODEL:
  # kim_prior<-normal(location=c(0.731,0.25,0.307,0.991), scale=c(0.1,0.1,0.1,0.1)) #Kim (1998) prior
  # model_bayes<- stan_glm(y~l+k+e+m, data=x, prior=kim_prior)
  # print(model_bayes)


  #BAYESIAN IV MODEL:

  #define instrument
  x$residual<-residuals(stan_glm(y~0+l+m+e+k,data=x))
  x$omega_hat<-NA
  x$omega_hat[-1]<-predict(stan_glm(residual~l_lag+k+e_lag+m_lag, data=x))

  #define prior
  #kim_prior<-normal(location=c(0.731,0.25,0.307,0.991), scale=c(0.1,0.1,0.1,0.1)) #Kim (1998) prior
  labor_only_prior<-normal(location=c(0.9,0,0.03,0), scale=c(0.1,1,0.1,1))
    
  #estimate production function
  model_bayes_IV<- stan_glm((y-omega_hat)~0+l+k+e+m, data=x, prior = labor_only_prior)
  #model_bayes_IV<- stan_glm((y-omega_hat)~0+l+k+e+m, data=x)
  models[[match(i, unique(complete_panel$Ind))]]<-model_bayes_IV

  #attach industry
  models[[match(i, unique(complete_panel$Ind))]]$Ind<-i
}



#create list of constrained models
models_constrained<-list()
for(i in 1:length(models)){
  models_constrained[[i]]<-models[[i]]
}

#IMPORTANCE SAMPLING CONSTRAINT (THETAs>0):
# for (i in 1:length(models_constrained)){
#   for(j in 1:4){
#     sim<-rnorm(10000, mean=models_constrained[[i]]$coefficients[j], sd=models_constrained[[i]]$ses[j]*sqrt(22)) #simulate data from posterior...
#     sim_trunc<-sim[sim>0] #remove illegal values...
# 
# 
#     #plot(density(sim), ylim=c(0,3)) #plot differences in posterior (black) and importance sample correction (red)
#     #lines(density(sim_trunc), col="red")
# 
#     theta<-maxlogL(sim_trunc, dist="dnorm", fixed=list(sd=models_constrained[[i]]$ses[j]*sqrt(22))) #compute maximum likelihood
# 
#     models_constrained[[i]]$coefficients[j]<-theta$fit$par #reassign coeff
#   }
# }

rm(sim, sim_trunc, theta)

#CREATE ELASTICITY PANEL (constrained >0 IV):
elasticity_panel_BAYESIAN_IV_constrianed<-data.frame(l=numeric(), k=numeric(), e=numeric(), m=numeric(), Ind=character())

for(i in 1:length(unique(complete_panel$Ind))){
  temp<-models_constrained[[i]]$coefficients
  temp$Ind<-models_constrained[[i]]$Ind
  elasticity_panel_BAYESIAN_IV_constrianed<-bind_rows(elasticity_panel_BAYESIAN_IV_constrianed, temp)
}

rm(temp, models, models_constrained)

#formatting - rename 311FT, 313TT, 315AL, rename variables:
elasticity_panel_BAYESIAN_IV_constrianed$Ind[elasticity_panel_BAYESIAN_IV_constrianed$Ind=="311FT"]<-"311"
elasticity_panel_BAYESIAN_IV_constrianed$Ind[elasticity_panel_BAYESIAN_IV_constrianed$Ind=="313TT"]<-"313"
elasticity_panel_BAYESIAN_IV_constrianed$Ind[elasticity_panel_BAYESIAN_IV_constrianed$Ind=="315AL"]<-"316"
elasticity_panel_BAYESIAN_IV_constrianed <- elasticity_panel_BAYESIAN_IV_constrianed %>%
  dplyr::rename(ind=Ind,
         theta_l=l,
         theta_k=k,
         theta_e=e,
         theta_m=m)

 #attach industry names:
 elasticity_panel_BAYESIAN_IV_constrianed=merge(elasticity_panel_BAYESIAN_IV_constrianed, commodity_code_key, by.x="ind", by.y="X1")


#import (instead of re-running)
write.csv(elasticity_panel_BAYESIAN_IV_constrianed, "C:/Users/Ansel/Box Sync/Documents/Structural Change/Job market paper/Markdown project/Industry elasticites/Estimation/elasticity_panel_BAYESIAN_IV_constrianed.csv")
#elasticity_panel_BAYESIAN_IV_constrianed <- read_csv("C:/Users/Ansel/Box Sync/Documents/Structural Change/Job market paper/Industry elasticites/Estimation/elasticity_panel_BAYESIAN_IV_constrianed.csv")


####GMM ESTIMATION#####
#Estimate the production function parameters and subsequent output elasticities following Loecker & Warzinski (2012) and Hershbein (2019):

#export:
# write.csv(complete_panel,"C:/Users/Ansel/Box Sync/Documents/Structural Change/Job market paper/Industry elasticites/Estimation/DLW_AER0747/complete_panel.csv" )
# 
# #run estimation (Stata)
# setwd("C:/Users/Ansel/Box Sync/Documents/Structural Change/Job market paper/Industry elasticites/Estimation/DLW_AER0747")
# RStata::stata("estimation.do",
#       stata.path = "\"C:\\Program Files (x86)\\Stata15\\Stata-64\"",
#       stata.version = 15,
#       stata.echo = FALSE)
# 
# #import elasticity estimates:
# setwd("C:/Users/Ansel/Box Sync/Documents/Structural Change/Job market paper/Markdown project/Industry elasticites/Estimation/DLW_AER0747/Results")
# temp = list.files(pattern="*.csv")
# elasticity_list = lapply(temp, read.csv)
# 
# elasticity_list = lapply(elasticity_list, function(x){
#   x<-x %>%
#     mutate(ind=as.character(ind))
# })
# 
# elasticity_panel_GMM=bind_rows(elasticity_list)
# elasticity_panel_GMM=distinct_all(elasticity_panel_GMM)
# 
# #attach industry definitions:
# elasticity_panel_GMM=merge(elasticity_panel_GMM, commodity_code_key, by.x="ind", by.y="X1")
# 
# #rename 311FT, 313TT, 315AL:
# elasticity_panel_GMM$ind[elasticity_panel_GMM$ind=="311FT"]<-"311"
# elasticity_panel_GMM$ind[elasticity_panel_GMM$ind=="313TT"]<-"313"
# elasticity_panel_GMM$ind[elasticity_panel_GMM$ind=="315AL"]<-"316"
# 

#remove estimation input data:
rm(use_tables, complete_panel, industry_panels_wide, industry_panels_wide_complete, elasticity_list, temp, test)

####SELECT MODEL AND BUILD COUNTY-LEVEL ELASTICITIES (call build_county_empl.R and build_energy_output_panels.R)####

#call script to build county employment data panel:
source(file="C:/Users/Ansel/Box Sync/Documents/Structural Change/Job market paper/Markdown project/County employment/build_county_empl.R")



#call script to build industry energy use panel:
#NOTE: ENERGY OUTPUT RATIOS FOR 2018 WILL BECOME AVAILABLE IN APRIL 2021!!!
source(file="C:/Users/Ansel/Box Sync/Documents/Structural Change/Job market paper/Markdown project/Energy data/Energy-output ratios/build_energy_output_panels.R")

#merge county data with industry elasticities and industry energy consumption, calculate energy consumption:
#(!) NOTE: results will vary depending on which elasticity_panel_XX is used (GMM, BAYESIAN_IV, BAYESIAN_IV_CONSTRAINED)
county_data=lapply(county_data, function(x){
  x<-merge(x, elasticity_panel_BAYESIAN_IV_constrianed, by.x="naics", by.y="ind")
  x<-merge(x, us_engery_ratios_long, by.x=c("naics", "year"), by.y=c("NAICS", "Year"))
  
  x %>%
    mutate(e_ij=emp*as.numeric(`Consumption per Employee (million Btu)`)) #compute energy consumption of industry i in county j...
  
})

#compute rho
for(i in 1:length(county_data)){
  temp<-county_data[[i]] %>% 
    group_by(fipstate, fipscty) %>% 
    summarise_at(c("e_ij"), sum, na.rm = TRUE) %>%
    dplyr::rename(e_j=e_ij)
  county_data[[i]]<-merge(county_data[[i]], temp, by=c("fipstate", "fipscty"))
  county_data[[i]]$rho<-county_data[[i]]$e_ij/county_data[[i]]$e_j
}
rm(temp)

#now construct county-level elasticities:
county_elasticities=lapply(county_data, function(x){
  x %>%
    mutate(weighted_theta_l=lambda*theta_l,
           weighted_theta_e=rho*theta_e) %>%
    group_by(fipstate, fipscty, year) %>%
    summarize_at(vars(weighted_theta_l, weighted_theta_e, emp, est), sum, na.rm=TRUE) %>%
    dplyr::rename(theta_l_j=weighted_theta_l,
                  theta_e_j=weighted_theta_e)
  
})

#summarize energy data:
county_energy_consumption=lapply(county_data, function(x){
  x %>%
    group_by(fipstate, fipscty, year) %>%
    summarize_at(vars(e_j), mean, na.rm=TRUE)
})

#end
