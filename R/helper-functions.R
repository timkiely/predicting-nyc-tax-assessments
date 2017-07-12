


#============================================================
## INSTALL SOME PACKAGES
package.list<-
  c(
    "ggplot2"
    ,"tidyverse"
    ,"ggthemes"
    ,"grid"
    ,"png"
    ,"gridExtra"
    ,"rgdal"
  )

for(i in package.list){
  pck<-i
  if(!i%in%installed.packages()){
    install.packages(i)
  }
}

lapply(package.list,FUN=require,character.only = TRUE)


#============================================================


#============================================================
## SOME BASIC HWE COLORS FOR A STANDARD PALLETTE
hwe_theme_data<-list()
hwe_theme_data$dkgray<-"#3C3C3C"
hwe_theme_data$medgray<-"#D2D2D2"    
hwe_theme_data$ltgray<-"#f2f2f2"
hwe_theme_data$red<-"#FF2700"      
hwe_theme_data$blue<-"#008FD5"
hwe_theme_data$green <-"#77AB43"
hwe_theme_data$palette<-c("#ae0016"#red
                          ,"#0000ae"#blue
                          ,"#2bae00"#green
                          ,"#ff8000"#orange
                          ,"#ae00ae"#purple
                          ,"#00aeae"#skyblue
                          ,"#ebc916"#yellow
                          ,"#e60000"#red2
                          ,"#3C3C3C"#drkgry
                          ,"#D2D2D2"#medgry
                          ,"#d9d9d9"#lytgry
)
#============================================================
#============================================================



#============================================================
## OUR MAIN CUSTOM THEME

# based largely on the fivethirtyeight theme from ggthemes
theme_custom = function(base_size = 14, base_family = "Helvetica") 
{
  (theme_foundation(base_size = base_size, base_family = base_family) + 
     theme(line = element_line(colour = "black")
           , rect = element_rect(fill = "white"
                                 #fill = hwe_theme_data$ltgray
                                 , linetype = 0
                                 , colour = NA)
           , text = element_text(colour = hwe_theme_data$dkgray)
           , axis.title = element_text()
           , axis.text = element_text()
           , axis.ticks = element_blank()
           , axis.line = element_blank()
           , legend.background = element_rect()
           , legend.box = "horizontal"
           , panel.grid = element_line(colour = NULL)
           , panel.grid.major = element_line(colour = hwe_theme_data$medgray)
           , panel.grid.minor = element_blank()
           , plot.title = element_text(size = rel(1.5)
                                       , face = "bold")
           , plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines")
           , strip.background = element_rect()
           , strip.text = element_text(face="bold")
     )
  )
}
#============================================================
#============================================================





#============================================================
## OUR MAIN CUSTOM THEME FOR MAP MAKING

theme_custom_map = function(base_size = 14, base_family = "Helvetica") 
{
  (theme_foundation(base_size = base_size, base_family = base_family) + 
     theme(line = element_line()
           , rect = element_rect(fill = "white"
                                 , linetype = 0
                                 , colour = NA)
           , text = element_text(colour = hwe_theme_data$dkgray)
           , axis.title = element_blank()
           , axis.text = element_text()
           , axis.ticks = element_blank()
           , axis.line = element_blank()
           , legend.background = element_rect()
           , legend.box = "horizontal"
           , legend.position="none"
           , panel.grid = element_line(colour = NULL)
           , panel.grid.major = element_blank()
           , panel.grid.minor = element_blank()
           , plot.title = element_text(size = rel(1.5)
                                       , face = "bold")
           , plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines")
           , strip.background = element_rect()
           , axis.text.x=element_blank()
           , axis.text.y=element_blank()
           , axis.ticks=element_blank()
     )
  )
}
#============================================================
#============================================================




#============================================================
## MAPS TAX DATA TO NEIGHBORHOODS


map_to_hoods <- function(tax_data){
  
  load(file="~/Dropbox (hodgeswardelliott)/Team_NYC/Tim_Kiely/Taxi/GENERAL_EXPLORATION/NYC_Hood_Shapefiles.RData")
  Census_Tracts_shapefile <- readRDS(file="~/Dropbox (hodgeswardelliott)/Team_NYC/Tim_Kiely/Shapefiles/RDS_Files/Census_Tracts_shapefile.rds")
  
  locations<-
    tax_data %>% 
    filter(!is.na(lat),!is.na(lon)) %>% 
    select(lon,lat, BBL)
  
  # Convert the lat/lon to a matrix, for use with SP
  coords<-
    locations %>% 
    select(-BBL) %>% 
    as.matrix()
  
  row.names(coords) <- locations$BBL
  
  
  # create a spatial points object. Note that if the CRS projections do not match 
  # the %over% algo will not work
  library(rgdal)
  proj4 <- proj4string(NYC_Hood_Maps$hood.tracts)
  sp = SpatialPoints(coords,proj4string=CRS(proj4))
  
  # Points-in-polygon. This can take some time:
  hoods <- sp::over(sp, NYC_Hood_Maps$hood.tracts)
  CTracts <-sp::over(sp, Census_Tracts_shapefile)
  
  # add an ID row for merging with the original data
  hoods$id<-as.integer(row.names(coords))
  CTracts$id2<-as.integer(row.names(coords))
  
  unfactorize <- function(df){
    for(i in which(sapply(df, class) == "factor")) df[[i]] = as.character(df[[i]])
    return(df)
  }
  hoods<-unfactorize(hoods)
  CTracts<-unfactorize(CTracts)
  
  map.hoods<-
    inner_join(locations,hoods,by=c("BBL" = "id")) %>% 
    select(BBL,neighborhood,boroughCode,borough)
  
  map.tracts <- 
    inner_join(locations,CTracts,by=c("BBL" = "id2")) %>% 
    select(BBL,CTLabel,CT2010,PUMA)
  
  
  
  # merge with original data
  tax_data<-
    tax_data %>% 
    left_join(map.hoods,by="BBL") %>% 
    left_join(map.tracts,by="BBL") %>% 
    mutate(neighborhood=as.character(neighborhood)
           ,neigh_color = as.character(neighborhood)
    )
  
  
  tax_data
}



#============================================================
## CONVERT XY COORDS TO LAT LON
library(proj4)
# NEW YORK LONG ISLAND STATE PLANE PROJECTION
# see: http://spatialreference.org/ref/esri/102718/

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
  x$lat <- latlon$lat
  x$lon = latlon$lon
  x
}



#============================================================
## PRINT TO JPEG, THEN TURN DEV OFF

print_to_jpeg <- function(image=NULL, file.out = NULL){
  jpeg(filename = file.out, width = 10, height = 8, units = "in", res = 60)
  print(image)
  dev.off()
}


#============================================================
## Convert Dates to Days Since Today
days_since_date <- function(date){
  # we want the results to be
  # reproducible, so we
  # will hardcode a date rather 
  # than have a system generated one
  
  x <- difftime(as.Date("2016-11-19"),date,units="days")
  x <- as.numeric(x)
  x
}


