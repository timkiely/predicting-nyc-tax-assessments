

# This script is v1 of an attempt to calculate radius metrics
# for all tax lot data in Manhatta (PLUTO dataset). The issue 
# here is the sheer time consuming nature of applying 
# point-in-polygon operations sequentially. Run end to end, 
# this script would have taken approx 108 hours.
# An v2 of this script uses paralellization to shorten the time considerably
# (13 hours, manageable in an overnight run)


 
rm(list=ls())
library(xgboost)
library(methods)
library(Matrix)
library(tidyverse)
library(caret)
source("R/helper-functions.R")

start_time<-Sys.time()





#----------------------- LOAD DATA -----------------------
# basic data output form the EDA/Transformation script
dat <- readRDS("data/pluto-xgboost-data-improved.rds")
dat <- Convert_XY(dat)
dim(dat)


# function takes lat, lon and radius and calculates
# various summary metric around that point

defineCircle <- function(x, y, r) {
  angles <- seq(0,2*pi,length.out=360)
  points <- data.frame("x"=r*cos(angles)+x
                       ,"y"=r*sin(angles)+y)
  points
}



dat_use <- dat



radius_out <- list()




for(i in 1:nrow(dat)){
  # i<-10
  cat(i," of ",nrow(dat),"\n")
  
  
  tryCatch({
  dat_use$IN <- 0
  dat_use$ID <- i
  data_subset <- dat_use[i,]
  
  
  the_year <- data_subset$Year
  
  x <- data_subset$lon
  y <- data_subset$lat
  
  cirlcePoints <- defineCircle(x,y,r=0.005) #0.005 roughly eqiivalent to 0.5 miles
  pip_vec <- point.in.polygon(dat_use$lon, dat_use$lat, cirlcePoints$x, cirlcePoints$y)
  dat_use$IN <- pip_vec
  
  
  metrics<-
    dat_use %>% 
    filter(IN>0) %>% 
    group_by(Year) %>% 
    dplyr::summarise(`quarter_mile_count` = n()
                     ,quarter_mile_Av_change_last_year = mean(Change_Assesment,1,na.rm=T)
                     ,quarter_mile_med_change_last_year = mean(Change_Assesment,na.rm=T)
                     ,quarter_mile_Tot_Asses_last_year = sum(AssessTotal.x,na.rm=T)
                     ,quarter_mile_Av_Asses_last_year = mean(AssessTotal.x,na.rm=T)
                     ,quarter_mile_Tot_med_last_year = median(AssessTotal.x,na.rm=T)
                     ,quarter_mile_Target_perc_last_year = sum(Target,na.rm=T)/quarter_mile_count
    )
  
  
  dat_out<-
    data.frame(
    "Year"=data_subset$Year
    ,"BBL_derive"=data_subset$BBL_derive
  ,stringsAsFactors = F)
  
  dat_out <- left_join(dat_out,metrics,by=c("Year"))
  
  radius_out[[i]] <- dat_out
  })
  
}

saveRDS(radius_out,"temp-radius-out-list.rds")
# bind_rows(radius_out)

# dat_try <- inner_join(dat,radius_out)


