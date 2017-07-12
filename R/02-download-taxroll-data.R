


# downlaod all tax class data from http://www1.nyc.gov/site/finance/taxes/property-assessment-roll-archives.page

# directory to download data into:

years <- 2009:2017

base_url <- "http://www1.nyc.gov/assets/finance/downloads/tar/"
file_types <- c("tc1","tc234","all","avroll")


for(year in years){
  base_year <- gsub("20","",year)
  year_dir <- paste0("FY",base_year)
  cat(year_dir,"... ")
  if(!year_dir%in%dir("data/Tax_Rolls")){
    dir.create(paste0("data/Tax_Rolls/",year_dir))
  }
  for(filetype in file_types){
    
    if(sum(grepl(filetype,dir(year_dir)))>0) { cat("...",filetype," in ",year_dir," ","already exists\n"); next} else {
      Sys.sleep(2)
      
      try({
      download_file <- paste0(base_url,filetype,"_",base_year,".zip")
      tmp <- tempdir()
      download.file(download_file,destfile=paste0(tmp,"/data"))
      unzip(paste0(tmp,"/data"),exdir = year_dir)
      cat(year_dir,filetype,"... done \n")
      })
      
    }
    
  }
}

