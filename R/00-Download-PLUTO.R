

# Downloads and unzips the PLUTO archive data from: https://www1.nyc.gov/site/planning/data-maps/open-data/pluto-mappluto-archive.page

source("R/helper-functions.R")

# list of pluto file zips
file_list<-
c('http://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_02a.zip'
,'http://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_03c.zip'
,'http://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_04c.zip'
,'http://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_05d.zip'
,'http://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_06c.zip'
,'http://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_07c.zip'
,'http://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_09v1.zip'
,'http://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_09v2.zip'
,'http://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_10v1.zip'
,'http://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_10v2.zip'
,'http://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_11v1.zip'
,'http://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_11v2.zip'
,'http://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_12v1.zip'
,'http://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_12v2.zip'
,'http://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_13v1.zip'
,'http://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_13v2.zip'
,'http://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_14v1.zip'
,'http://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_14v2.zip'
,'http://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_15v1.zip'
,'http://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_16v1.zip')


# automatically creates a sub directory within specified path
if(!"PLUTO_ARCHIVES"%in%dir("data")){
  dir.create("data/PLUTO_ARCHIVES")
}


for(file in file_list){
  
  fil <- file
  dest_file <- paste0('PLUTO_ARCHIVES/',basename(gsub(".zip","",fil)))
  
  if(exists(dest_file)){
    file.remove(dest_file)
  }
  
  temp <- tempfile()
  download.file(fil,temp)
  unzip(temp, exdir = paste0('PLUTO_ARCHIVES/',basename(gsub(".zip","",fil))))
  unlink(temp)
}


names_list<-list()
for(file in dir("PLUTO_ARCHIVES")){
  names_string <- dir(paste0('PLUTO_ARCHIVES/',file))
  names_string <- names_string[-grep(".pdf",names_string)]
  names_list[[file]] <- names_string
}

as.data.frame(names_list)

