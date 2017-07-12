
rm(list=ls())
library(tidyverse)
source("R/helper-functions.R")
# ==========================================
# ==========================================
# READ ALL DATA INTO R MEMORY AND CONCAT INTO A DATA FRAME

file_we_want <- dir(paste0("data/PLUTO_ARCHIVES/"))

data_out<-list()
for(i in 1:length(file_we_want)){
  # i<-1
  nmax <- Inf
  path_name <- paste0("data/PLUTO_ARCHIVES/",file_we_want[i])
  
  cat(paste0("20",substr(gsub("nyc_pluto_","", basename(path_name)),0,2)),"...\n")
  
  files_in_yearfolder <- dir(path_name)
  
  manhat_loc <- grepl("mn",files_in_yearfolder,ignore.case = T)
  bk_loc <- grepl("bk",files_in_yearfolder,ignore.case = T)
  bx_loc <- grepl("bx",files_in_yearfolder,ignore.case = T)
  qn_loc <- grepl("qn",files_in_yearfolder,ignore.case = T)
  si_loc <- grepl("si",files_in_yearfolder,ignore.case = T)
  
  
  man_file <- paste0(path_name,"/",files_in_yearfolder[manhat_loc])
  bk_file <- paste0(path_name,"/",files_in_yearfolder[bk_loc])
  bx_file <- paste0(path_name,"/",files_in_yearfolder[bx_loc])
  qn_file <- paste0(path_name,"/",files_in_yearfolder[qn_loc])
  si_file <- paste0(path_name,"/",files_in_yearfolder[si_loc])
  
  suppressWarnings({
  cat("   man...\n")
  
  man_data <- readr::read_csv(man_file,n_max = nmax, col_names = T,progress = F,cols(.default = "c"), locale = locale(encoding = "latin1"))
  
  cat("   bk...\n")
  bk_data <- readr::read_csv(bk_file,n_max = nmax, col_names = T,progress = F,cols(.default = "c"), locale = locale(encoding = "latin1"))
  
  cat("   bx...\n")
  bx_data <- readr::read_csv(bx_file,n_max = nmax, col_names = T,progress = F,cols(.default = "c"), locale = locale(encoding = "latin1"))
  
  cat("   qn...\n")
  qn_data <- readr::read_csv(qn_file,n_max = nmax, col_names = T,progress = F,cols(.default = "c"), locale = locale(encoding = "latin1"))
  
  cat("   si...\n")
  si_data <- readr::read_csv(si_file,n_max = nmax, col_names = T,progress = F
                             #,cols(.default = "c")
                             , locale = locale(encoding = "latin1"))
  })

  man_data[] <- lapply(man_data, as.character)
  bk_data[] <- lapply(bk_data, as.character)
  bx_data[] <- lapply(bx_data, as.character)
  qn_data[] <- lapply(qn_data, as.character)
  si_data[] <- lapply(si_data, as.character)
  
  all_year_data <- bind_rows(list(
    man_data
    ,bk_data
    ,bx_data
    ,qn_data
    ,si_data
  ))
  
  base_name <- paste0("20",substr(gsub("nyc_pluto_","", basename(path_name)),0,2))
  all_year_data$file_version <- base_name
  
  data_out[[i]] <- all_year_data
}

# saveRDS(data_out,"all-pluto-as-list-no-column-names.rds")

# year 2002 and 2006 have no headers and too many variables, easiest to just drop them...
 data_out[[1]]<-NULL
 data_out[[4]]<-NULL


for(p in 1:length(data_out)){
  print(data_out[[p]][1,])
}

data_fin <- bind_rows(data_out)

num <- as.numeric(data_fin$OfficeArea)
num <- as.numeric(data_fin$UnitsTotal)
num <- as.numeric(data_fin$UnitsRes)


# saveRDS(data_fin,"all-pluto-as-list-with-file-version.rds")
# data_fin <- readRDS("all-pluto-as-list-with-file-version.rds")
# ==========================================
# ==========================================






# ==========================================
# ==========================================
# FUNCTION FOR VARIABLE CONVERSION
ID_Building_Type <- function(x){
  dat<-x
  dat<-dplyr::mutate(dat,Building_Type= ifelse(!is.na(BldgClass),substr(BldgClass,1,1)),NA)
}


library(proj4)
# NEW YORK LONG ISLAND STATE PLANE PROJECTION
# see: http://spatialreference.org/ref/esri/102718/

Convert_XY <- function(x) {
  library(proj4)
  # NEW YORK LONG ISLAND STATE PLANE PROJECTION
  # see: http://spatialreference.org/ref/esri/102718/
  proj4string <- "+proj=lcc +lat_1=40.66666666666666 +lat_2=41.03333333333333 +lat_0=40.16666666666666 +lon_0=-74 +x_0=300000 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"
  xy <- data.frame(x=x$XCoord, y=x$YCoord)
  pj <- proj4::project(xy, proj4string, inverse=TRUE)
  latlon <- data.frame(lat=pj$y, lon=pj$x)
  x <- x %>% 
    mutate(lat = latlon$lat
           ,lon = latlon$lon)
  
}
# ==========================================
# ==========================================






# ==========================================
# ==========================================
# ADD lat/lon and Building Type vars
data_refine <- 
  data_fin %>% 
  mutate(AssessTotal = if_else(is.na(AssessTot),AssessTotal,AssessTot)) %>% 
  select(-AssessTot) %>% 
  mutate(BBL_derive = paste0(BoroCode,sprintf("%05s",Block),sprintf("%04s",Lot) )) %>%
  mutate(ExemptTotal = if_else(is.na(ExemptTot),ExemptTotal,ExemptTot)) %>% 
  select(-ExemptTot) %>% 
  mutate(Building_Type = ifelse(!is.na(BldgClass),substr(BldgClass,1,1),NA))
  
data_refine <- Convert_XY(data_refine)
# ==========================================
# ==========================================






# ==========================================
# ==========================================
# CONVERT NUMERIC AND DATE VARIABLES

names(man_data)[sapply(man_data,is.numeric)] %>% paste0(collapse="\', \'")

num_vars <- c(
  'Easements'
  , 'LotArea', 'BldgArea', 'ComArea'
  , 'ResArea', 'OfficeArea', 'RetailArea'
  , 'GarageArea', 'StrgeArea', 'FactryArea'
  , 'OtherArea', 'AreaSource', 'NumBldgs'
  , 'NumFloors', 'UnitsRes', 'UnitsTotal'
  , 'LotFront', 'LotDepth', 'BldgFront'
  , 'BldgDepth', 'ProxCode', 'LotType'
  , 'BsmtCode', 'AssessLand', 'AssessTotal'
  , 'ExemptLand', 'ExemptTotal', 'YearBuilt'
  , 'YearAlter1', 'YearAlter2', 'BuiltFAR'
  , 'ResidFAR', 'CommFAR', 'FacilFAR'
  , 'CondoNo', 'XCoord', 'YCoord','lat','lon'
  ,'PolicePrct','LandUse','CT2010'
  , 'CB2010','Tract2010','MaxAllwFAR'
)

id_vars <- c(
  'Block', 'Lot', 'BoroCode', 'BBL','BBL_derive','PLUTOMapID'
)

date_vars <- c(
  'RPADDate','DCASDate','DOBDate','ZoningDate'
  ,'MajPrpDate','LandmkDate','BaseMpDate'
  ,'LandmkDate','BaseMpDate','MASDate'
  ,'PoliDate','APPDate','DTMDate'
  ,'EDesigDate'
)

parse_yearmon <- function(yearmon){
  yearmon <- paste0("01/",yearmon,sep="")
  yearmon <- as.Date(yearmon,format = "%d/%m/%Y")
  yearmon
}

data_class<-
  data_refine %>% 
  mutate_at(num_vars,as.numeric) %>% 
  mutate_at(date_vars,parse_yearmon)

data_class$Year <- data_class$file_version

# saveRDS(data_class,"pluto_all.rds")
# readr::write_csv(data_class,"pluto_all.csv")


  