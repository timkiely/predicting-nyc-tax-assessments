
# This script is a redesign of the radius calculation program.
# Because point-in-polygon tends to be quite slow, applying 
# O(n) p-in-p calculations to the Manhattan tax lot dataset
# (265K rows) takes quite a long time in a brute-force loop.

# Here, we use the foreach package to split the same brute-force
# approach across alla available cores. This allows us to 
# calculate the radius metrics in 13.5 hours across 8 cores, as 
# opposed to 108 hours on a single core. This entire process 
# needs to be optimized considerably, and could make for an interesting
# research project in itself
# Tim Kiely, 11.20.2016



library(xgboost)
library(methods)
library(Matrix)
library(tidyverse)
library(caret)
source("R/helper-functions.R")

library(foreach)
library(doParallel)

cores <- detectCores()
cl<-makeCluster(cores)
registerDoParallel(cl)



# Load the feature-enriched data:
dat <- readRDS("data/pluto-xgboost-data-improved.rds")
# Correction to the lat/lon calc:
dat <- Convert_XY(dat)





dat_use <- dat
radius_out <- data.frame()
start_time <- Sys.time()

out_list<-   
  foreach(i = 1:nrow(dat_use)) %dopar% {
    source("R/helper-functions.R")
    library(tidyr)
    
    # i<-10
    cat(i," of ",nrow(dat),"\n")
    
    
    tryCatch({
      dat_use$IN <- 0
      dat_use$ID <- i
      data_subset <- dat_use[i,]
      
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
      
      dat_out
    })
    
  };stopCluster(cl);closeAllConnections()


end_time <- Sys.time()
end_time-start_time 


# the output from foreach is a list, bind the entire list back together with bind_rows:
dat_maybs_fin<-tbl_df(bind_rows(out_list))


# save the output: 
# saveRDS(out_list,"temp-par-output-list.rds")
# saveRDS(dat_maybs_fin,"temp-par-output-df.rds")






