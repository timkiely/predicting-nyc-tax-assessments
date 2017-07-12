


# http://www1.nyc.gov/assets/finance/jump/property-data-maps/docs/quintos_latenteffect.pdf




rm(list=ls())


library(tidyverse)
source("R/helper-functions.R")
print<-F

# -------------------------------------------------
# -------------------------------------------------

# We have the data for all of NYC, however, Manhattan
# is both the most interesting and one of the smaller
# subsets to work with. This commented part of the script loads 
# and filters the full dataset

# data_pluto <- readRDS("data/pluto_all.rds")

# glimpse(data_pluto)

# Manahttan has comparatively fewer observations, so we will 
# be working with just Manhattan data


# dat <- data_pluto %>% filter(BoroCode==1)
# rm(data_pluto)
# -------------------------------------------------
# -------------------------------------------------



#----------------
# load the Manahttan data and transform
dat <- readRDS("data/pluto-manhat-data.rds")

dat <- Convert_XY(dat)

glimpse(dat)



date_vars <- c(
  'RPADDate'
  ,'DCASDate'
  ,'DOBDate'
  ,'ZoningDate'
  ,'MajPrpDate'
  ,'LandmkDate'
  ,'BaseMpDate'
  ,'MASDate'
)


dat<-
  dat %>% mutate(Landmark_Ind = ifelse(Landmark!="",1,0)
                 ,Landmark_Ind = ifelse(is.na(Landmark_Ind),0,Landmark_Ind)
  ) %>% 
  mutate_at(date_vars,funs(days_since_date,as.Date)) %>%
  select(-matches("_as.Date"))



#----------------


# In aggregate, assessments linearly increase over time
tax_over_time<-
  dat %>% 
  filter(AssessTotal>0) %>% 
  mutate(Year = as.numeric(Year)) %>% 
  mutate(BBL_derive = as.numeric(BBL_derive)) %>% 
  group_by(Year) %>% 
  summarise(AssessTotal = sum(AssessTotal,na.rm=T)) %>% 
  ggplot()+
  aes(x = Year, y = AssessTotal)+
  geom_line()+
  theme_custom()+
  scale_y_continuous(labels=scales::comma)+
  labs(title = "Tax Assessments Increase Linearly Over Time (By Design)"
       ,x = NULL
       ,y = "Tax Assessments (USD)")



if(print==T) print_to_jpeg(tax_over_time, file.out = "img/tax-over-time.jpeg")


# We see most Tax Lots increase gradually, however, the data is full of 
# examples of dramatic increases YoY. This is likely due to development activity

spikes_over_time<-
  dat %>% 
  mutate(Year = as.numeric(Year)) %>% 
  group_by(BBL_derive,Year) %>% 
  summarise(AssessTotal = sum(AssessTotal,na.rm=T)) %>% 
  ungroup() %>% 
  group_by(BBL_derive) %>% 
  mutate(Change_Assesment = (AssessTotal-lag(AssessTotal,1))/lag(AssessTotal,1)) %>% 
  mutate(Change_Assesment = ifelse(is.na(Change_Assesment),0,Change_Assesment)) %>% 
  mutate(Cumulative_Change = cumsum(Change_Assesment)) %>% 
  ggplot()+
  aes(x = Year, y = Cumulative_Change, group = BBL_derive, color = Cumulative_Change)+
  geom_line(alpha=0.1)+
  theme_custom()+
  scale_y_continuous(labels=scales::percent, limits = c(0,100))+
  labs(title = "The Majority of Tax Lots Values Increase Gradually Over Time"
       ,subtitle = "But Some See Dramatic Spikes in Value"
       ,x = NULL
       ,y = "Cumulative Percent Change in Tax Assessments YoY"
       ,color = "Cumlative Change")


if(print==T) print_to_jpeg(spikes_over_time, file.out = "img/spikes-over-time.jpeg")


# there are a handful of tax lots with 
# many buildings on them. 
# we will exclude them for this 
# analysis

build_hist<-
  dat %>% 
  group_by(NumBldgs) %>% 
  count() %>% 
  ggplot()+
  aes(x=NumBldgs,y=n)+
  geom_col()+
  scale_y_log10()+
  theme_custom()+
  labs(title = "Histogram of # of Buildings per Tax Lot"
       ,y="Log of count"
       ,x=NULL)

if(print==T) print_to_jpeg(build_hist, file.out = "img/building-count-hist.jpeg")





# The median percent change is 0.058, but there is significant variability
# We will use an increase above the median as our target variable

dat %>% 
  filter(AssessTotal>0) %>% 
  mutate(Year = as.numeric(Year)) %>% 
  group_by(BBL_derive,Year) %>% 
  summarise(AssessTotal = sum(AssessTotal,na.rm=T)) %>% 
  ungroup() %>% 
  group_by(BBL_derive) %>% 
  mutate(Change_Assesment = (AssessTotal-lag(AssessTotal,1))/lag(AssessTotal,1)) %>% 
  mutate(Change_Assesment = ifelse(is.na(Change_Assesment),0,Change_Assesment)) %>% 
  mutate(Cumulative_Change = cumsum(Change_Assesment)) %>% ungroup() %>% select(Change_Assesment,Cumulative_Change) %>% 
  summary 


# Median change reasonably falls between 4-6%
dat %>% 
  filter(AssessTotal>0) %>% 
  filter(Year>2009) %>% 
  mutate(Year = as.numeric(Year)) %>% 
  group_by(BBL_derive,Year) %>% 
  summarise(AssessTotal = sum(AssessTotal,na.rm=T)) %>% 
  ungroup() %>% 
  group_by(BBL_derive) %>% 
  mutate(Change_Assesment = (AssessTotal-lag(AssessTotal,1))/lag(AssessTotal,1)) %>% 
  mutate(Change_Assesment = ifelse(is.na(Change_Assesment),0,Change_Assesment)) %>% 
  mutate(Cumulative_Change = cumsum(Change_Assesment)) %>% 
  ungroup() %>% 
  group_by(Year) %>% 
  summarise(median(Change_Assesment)) 





# -----------------------------------
# create our target variable

dat_target<-
  dat %>% 
  filter(NumBldgs>0) %>% 
  filter(NumBldgs<2) %>% 
  filter(AssessTotal>0) %>%
  filter(Year>2009) %>% 
  mutate(Year = as.numeric(Year)) %>% 
  group_by(BBL_derive,Year) %>% 
  summarise(AssessTotal = sum(AssessTotal,na.rm=T)) %>% 
  ungroup() %>% 
  group_by(BBL_derive) %>% 
  mutate(Change_Assesment = (AssessTotal-lag(AssessTotal,1))/lag(AssessTotal,1)) %>% 
  mutate(Change_Assesment = ifelse(is.na(Change_Assesment),0,Change_Assesment)) %>% 
  mutate(Cumulative_Change = cumsum(Change_Assesment)) %>% 
  mutate(Target = ifelse(Change_Assesment>0.06,1,0)) %>% 
  ungroup() 

table(dat_target$Change_Assesment>0.06,dat_target$Target)

target_var_chart<-
  dat_target %>% 
  group_by("Target" = as.factor(Target)) %>% 
  count() %>% 
  ggplot()+
  aes(x=Target,y=n)+
  geom_col()+
  theme_custom()+
  scale_y_continuous(labels=scales::comma)+
  labs(title="Target Variable"
       ,subtitle = "1 = YoY % Change in Tax Assessment of >6%"
       ,y=NULL)


if(print==T) print_to_jpeg(target_var_chart, file.out = "img/target-var-counts.jpeg")


num_vars <- c(
  'OfficeArea', 'RetailArea','AreaSource', 'NumBldgs'
  , 'NumFloors', 'UnitsRes', 'UnitsTotal'
  , 'LotFront', 'LotDepth', 'BldgFront'
  , 'BldgDepth', 'ProxCode', 'LotType'
  , 'BsmtCode', 'AssessLand', 'AssessTotal.x'
  , 'ExemptLand', 'YearBuilt'
  , 'YearAlter1', 'YearAlter2', 'BuiltFAR'
  , 'CondoNo', 'XCoord', 'YCoord','lat','lon'
  ,'LandUse','Change_Assesment','Cumulative_Change','Year'
  ,"BBL_derive","Building_Type","Target","BldgClass"
  ,"IrrLotCode","TaxMap","ZoneMap","Landmark_Ind","SchoolDist"
)






dat_model <- 
  dat %>% 
  mutate(Year = as.numeric(Year)) %>% 
  inner_join(dat_target,by=c("BBL_derive","Year")) %>% 
  select_(.dots=num_vars) %>% 
  mutate(Building_Type = factor(Building_Type))


glimpse(dat_model)
dim(dat_model)
summary(dat_model)







dat_fin <- dat_model

# -------------------------------------------
## export our modeling set to XGBoost script:

saveRDS(dat_fin,"data/pluto-xgboost-data.rds")










# -------------------------------------------
## PART II:
## CREATE NEW FEATURE-RICH SET


more_vars <- c(
  'OfficeArea', 'RetailArea','AreaSource', 'NumBldgs'
  , 'NumFloors', 'UnitsRes', 'UnitsTotal'
  , 'LotFront', 'LotDepth', 'BldgFront'
  , 'BldgDepth', 'ProxCode', 'LotType'
  , 'BsmtCode', 'AssessLand', 'AssessTotal.x'
  , 'ExemptLand', 'YearBuilt'
  , 'YearAlter1', 'YearAlter2', 'BuiltFAR'
  , 'CondoNo', 'XCoord', 'YCoord','lat','lon'
  ,'LandUse','Change_Assesment','Cumulative_Change','Year'
  ,"BBL_derive","Building_Type","Target","BldgClass"
  ,"IrrLotCode","OwnerType","TaxMap","ZoneMap","Landmark_Ind","SchoolDist"
  ,"RPADDate_days_since_date","DCASDate_days_since_date","DOBDate_days_since_date"
  ,"ZoningDate_days_since_date", "MajPrpDate_days_since_date" 
  ,"LandmkDate_days_since_date", "BaseMpDate_days_since_date"
  ,"MASDate_days_since_date"
)


dat_model2 <- 
  dat %>% 
  mutate(Year = as.numeric(Year)) %>% 
  inner_join(dat_target,by=c("BBL_derive","Year")) %>% 
  select_(.dots=more_vars) %>% 
  mutate(Building_Type = factor(Building_Type)) %>% 
  mutate(Log_AssessTotal = ifelse(AssessTotal.x>0,log(AssessTotal.x+1),0))




# import radius metrics from 07-par-calculate-radius-metrics
dat_radius_metrics <- readRDS("data/par-geospatial-output.rds")

lagged_radius_metircs<-
  dat_radius_metrics %>% 
  group_by(BBL_derive) %>% 
  arrange(Year) %>% 
  mutate_at(vars(matches("quarter_mile_")),funs(lag)) %>% 
  mutate_at(vars(matches("quarter_mile_")),funs(ifelse(is.na(.),lead(.),.)))
  
  

# merge the lagged radius metrics with the modeling dataset
dat_model3 <- dat_model2 %>% left_join(lagged_radius_metircs,by=c("BBL_derive","Year"))

summary(dat_model3)

# check for duplicates
nrow(dat_model2)==nrow(dat_model3)

dat_model4<-
  dat_model3 %>% 
  group_by(BBL_derive) %>% 
  arrange(Year) %>% 
  mutate(lag_Log_AssessTotal = lag(Log_AssessTotal)
         ,lag_Log_AssessTotal = ifelse(is.na(lag_Log_AssessTotal),lead(lag_Log_AssessTotal),lag_Log_AssessTotal)
         ,lag_AssessTotal = lag(AssessTotal.x)
         ,lag_AssessTotal = ifelse(is.na(lag_AssessTotal),lead(lag_AssessTotal),lag_AssessTotal)
         ) %>% 
  ungroup()



# -------------------------------------------
## export our modeling set to XGBoost script:

saveRDS(dat_model4,"data/pluto-xgboost-data-improved.rds")







