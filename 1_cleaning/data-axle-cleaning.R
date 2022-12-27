# Script: dataaxle data processing and cleaning
# Author: Seleeke Flingai
# Date Last Modified: December 19, 2022 by RBowers

rm(list=ls())

install.packages('pacman')
pacman::p_load(sp, rgdal, raster, tidyverse) 
library(sp)
library(rgdal)
library(terra)
library(raster)
library(foreign)
library(tidyverse)
options(scipen = 999)

raw.data.path <- "K:/DataServices/Projects/Current_Projects/Arts_and_Culture_Planning/Creative_Economy/Data/Raw"
#update modified path with year
modified.data.path <- "K:/DataServices/Projects/Current_Projects/Arts_and_Culture_Planning/Creative_Economy/Data/Modified/2022"
gis.data.path <- "K:/DataServices/Projects/Current_Projects/Arts_and_Culture_Planning/Creative_Economy/Data/GIS"

# load DataAxle raw files (April 2021)
setwd(raw.data.path)
dataaxle <- read.csv("April_2021 - Massachusetts_New.csv", stringsAsFactors = FALSE) #update with latest DataAxle

#add an OBJECT ID field
dataaxle$OBJECTID <- seq.int(nrow(dataaxle))

####### DATA CLEANING: COMBINE dataaxle RAW FILES AND dplyr::select ON VARIABLES OF INTEREST #######
# dplyr::select our dataaxle variables of interest 
# also, added new variables for the four-digit and six-digit NAICS code 
# (taken from the first four or six digits of the 8-digit NAICS variable)

# 2022 edit - changing field names to reflect data axle fields
dataaxle.mass.temp <- dataaxle %>%
  dplyr::select(OBJECTID, CONAME,STADDR,STCITY, STATE, ZIP, STCODE, 
                CNTYCD, CENSUS, BLKGRP, LATITUDE, LONGITUDE, NAICS, NAICSD, 
                EMPSIZ, LOCEMP, SALVOL, 
                SLSVDT, ZPOPCD, WEALTH, BBUSIN, 
                MBUSIN, SBUSIN, HOME, INDFRM,
                FEMOWN) %>%
  mutate("NAICS_FOUR" = as.character(substr(as.character(NAICS), 1, 4))) %>%
  filter(!is.na(LOCEMP) & STCODE == 25)  



####### DATA CLEANING: AGGREGATING NEIGHBORHOODS/VILLAGES INTO THEIR RESPECTIVE MUNICIPALITY #######
# dataaxle lists establishments in either their municipality (e.g. Boston) or their neighborhood/village (e.g. "Dorchester")
# There are also some names spelled differently than on the MAPC and Massachusetts state government municipality lists
# Therefore, I've gone through the dataaxle data and corrected spellings using the gsub function
# I've also consolidated neighborhoods and villages into their respective municipalities using the ifelse function

dataaxle.mass.temp$STCITY <- gsub("Manchestr By Sea", "Manchester", dataaxle.mass.temp$STCITY)
dataaxle.mass.temp$STCITY <- gsub("Foxboro", "Foxborough", dataaxle.mass.temp$STCITY)
dataaxle.mass.temp$STCITY <- gsub("North Attleboro", "North Attleborough", dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("Roxbury", "Roxbury Crossing", "Mission Hill", "Dorchester", "Grove Hall", "Dorchester Ctr",
                                                                               "Uphams Corner", "Mattapan", "South Boston", "East Boston", "Charlestown", "Jamaica Plain",
                                                                               "Roslindale", "West Roxbury", "Allston", "Brighton", "Hyde Park"), 
                                         "Boston", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("East Arlington", "Arlington Hts"), 
                                         "Arlington", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("North Cambridge", "East Cambridge"), 
                                         "Cambridge", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("Newtonville", "Newton Center", "Newton Highlands", "Newton Lower Fls",
                                                                               "Newton Upper Fls", "West Newton", "Auburndale", "Chestnut Hill", "Waban", "Nonantum"), 
                                         "Newton", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("North Easton", "South Easton"), 
                                         "Easton", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("East Weymouth", "South Weymouth","North Weymouth"), 
                                         "Weymouth", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("Teaticket", "North Falmouth","East Falmouth", "West Falmouth", 
                                                                               "Woods Hole", "Waquoit"), 
                                         "Falmouth", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("North Quincy", "Wollaston"), 
                                         "Quincy", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("Hyannis", "Centerville", "Cotuit", "Cummaquid", 
                                                                               "Hyannis Port", "Marstons Mills", "Osterville", "West Hyannisport",
                                                                               "West Barnstable"), 
                                         "Barnstable", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("West Somerville"), 
                                         "Somerville", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("White Horse Bch", "Manomet"), 
                                         "Plymouth", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("East Sandwich", "Forestdale"), 
                                         "Sandwich", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("South Wellfleet"), 
                                         "Wellfleet", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("North Amherst"), 
                                         "Amherst", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("North Uxbridge"), 
                                         "Uxbridge", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("Housatonic"), 
                                         "Great Barrington", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("Vineyard Haven"), 
                                         "Tisbury", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("Greenbush", "Humarock", "North Scituate"), 
                                         "Scituate", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("Byfield"), 
                                         "Newbury", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("East Walpole", "South Walpole"), 
                                         "Walpole", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("North Grafton", "South Grafton"), 
                                         "Grafton", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("Shattuckville"), 
                                         "Colrain", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("Woronoco"), 
                                         "Russell", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("Milton Village"), 
                                         "Milton", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("Ashley Falls"), 
                                         "Sheffield", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("Attleboro Falls"), 
                                         "North Attleborough", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("Manchaug", "Wilkinsonville"), 
                                         "Sutton", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("West Hatfield", "North Hatfield"), 
                                         "Hatfield", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("East Douglas"), 
                                         "Douglas", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("East Orleans"), 
                                         "Orleans", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("South Attleboro"), 
                                         "Attleboro", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("Baldwinville", "East Templeton"), 
                                         "Templeton", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("Charlton Depot", "Charlton City"), 
                                         "Charlton", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("Assonet", "East Freetown"), 
                                         "Freetown", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("West Millbury"), 
                                         "Millbury", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("West Harwich", "Harwich Port", "East Harwich"), 
                                         "Harwich", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("South Dennis", "West Dennis", "East Dennis", "Dennis Port"), 
                                         "Dennis", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("Lake Pleasant", "Millers Falls", "Turners Falls"), 
                                         "Montague", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("Bass River", "West Yarmouth", "Yarmouth Port", "South Yarmouth"), 
                                         "Yarmouth", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("Marshfield Hills", "North Marshfield", 
                                                                               "Ocean Bluff", "Brant Rock", "Green Harbor"), 
                                         "Marshfield", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("Accord"), 
                                         "Norwell", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("Brookline Vlg"), 
                                         "Brookline", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("Cuttyhunk"), 
                                         "Gosnold", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("Haydenville"), 
                                         "Williamsburg", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("West Medford"), 
                                         "Medford", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("West Whately"), 
                                         "Whately", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("East Otis"), 
                                         "Otis", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("Wellesley Hills", "Babson Park"), 
                                         "Wellesley", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("Feeding Hills"), 
                                         "Agawam", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("Gilbertville", "South Harwich"), 
                                         "Hardwick", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("Buzzards Bay", "Cataumet", "Monument Beach", "Pocasset", "Sagamore", "Sagamore Beach"), 
                                         "Bourne", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("Westport Point"), 
                                         "Westport", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("North Eastham"), 
                                         "Eastham", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("East Wareham", "West Wareham", "Onset"), 
                                         "Wareham", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("Berkshire"), 
                                         "Lanesborough", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("Bondsville", "Thorndike", "Three Rivers"), 
                                         "Palmer", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("Bradford"), 
                                         "Haverhill", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("Bryantville"), 
                                         "Pembroke", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("Chartley"), 
                                         "Norton", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("Cherry Valley", "Rochdale"), 
                                         "Leicester", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("Drury"), 
                                         "Florida", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("East Taunton"), 
                                         "Taunton", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("Elmwood"), 
                                         "East Bridgewater", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("Fiskdale"), 
                                         "Sturbridge", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("Florence", "Leeds"), 
                                         "Northampton", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("Hathorne"), 
                                         "Danvers", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("Indian Orchard"), 
                                         "Springfield", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("Jefferson"), 
                                         "Holden", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("Lenox Dale"), 
                                         "Lenox", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("Glendale"), 
                                         "Stockbridge", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("Menemsha"), 
                                         "Chilmark", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("Mill River"), 
                                         "New Marlborough", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("Monponsett"), 
                                         "Hanson", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("Mt Hermon"), 
                                         "Gill", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("Needham Heights"), 
                                         "Needham", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("North Billerica"), 
                                         "Billerica", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("North Carver" ), 
                                         "Carver", 
                                         dataaxle.mass.temp$STCITY)

dataaxle.mass.temp$STCITY <- ifelse(dataaxle.mass.temp$STCITY %in% c("North Chatham"), 
                                         "Chatham", 
                                         dataaxle.mass.temp$STCITY)


# "Ann Arbor"
# Devens is part of Ayer, Shirley, and Harvard
# East Hartford found in CT
# "Fayville"           
# "Gay Head" 
# "Hanscom Afb"
# "Hollis"
# Linwood is part of Northbridge and Uxbridge
# "Monroe Bridge" is maybe part of Monroe, but Monroe isn't listed elsewhere, so maybe it is Monroe?
# "New Town"
# "North Chelmsford"   
# "North Dartmouth"    
# "North Dighton"      
# "North Oxford"       
# "North Pembroke"
# "North Truro"       
# "North Waltham"
# "Nutting Lake"
# "Pinehurst"
# "Portsmouth"        
# "Prides Crossing"
# "Raynham Center"
# "Readville"
# "Salisbury Beach"
# "Shelburne Falls"
# "Sheldonville"
# "Siasconset"         
# "Smithfield" 
# "South Barre"        
# "South Carver"       
# "South Chatham"      
# "South Chelmsford"   
# "South Dartmouth"   
# "South Deerfield"    
# "South Egremont"
# "South Hamilton"     
# "South Lancaster"    
# "South Lee"          
# "South Orleans"     
# "South Royalston"
# "Southfield"
# "Stevens Point"      
# "Still River"
# "Takoma Park"
# "Terrell"
# "W Chesterfield"
# "Ward Hill"
# "Waterbury"
# "Waverley"
# "Wendell Depot"
# "West Boxford" 
# "West Chatham"       
# "West Groton"
# "West Townsend"      
# "West Warren"
# "Wheelwright"        
# "Whitinsville"
# "Woodville"



# load lists of MAPC municipalities, along with all municipalities in the state
setwd(raw.data.path)
mass.town.types <- read.csv("mass_town_types.csv", header = TRUE)

mass.town.types <- mass.town.types %>%
  dplyr::select(muni_id, municipal, county_id, county, mapc, comm_name, subtype, subregion)

mapc.town.types <- mass.town.types %>%
  filter(mapc == 1) %>%
  dplyr::select(muni_id, municipal, county_id, county, comm_name, subtype, subregion)

############# NEIGHBORHOODS ############
# add neighborhood shape file
setwd(gis.data.path)
ma_nh = readOGR(dsn = ".", "MA_neighborhoods")

# establish coordinate systems for projections
WCS1984_CRS <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
new_CRS <- CRS("+proj=lcc +lat_1=41.71666666666667 +lat_2=42.68333333333333 +lat_0=41 +lon_0=-71.5 +x_0=200000 +y_0=750000
               +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

# project the neighborhood shapefile to NAD83 
proj4string(ma_nh) <- new_CRS

# create SpatialPoints object for infogroup establishments 
estab.points <- SpatialPoints(data.frame(latitude = dataaxle.mass.temp$LONGITUDE, longitude = dataaxle.mass.temp$LATITUDE), # for some reason this only works when latitude is the data's longitude and longitude is data's latitude
                              proj4string = CRS(WCS1984_CRS)) 

# transform projection to NAD83 to match neighborhood file
estab.points <- spTransform(estab.points, new_CRS)

# create dataset that tells us which points fall within which neighborhoods
pnt.estab.shape <- over(estab.points, ma_nh)

# link neighborhood info to establishment info in full dataaxle dataset
dataaxle.mass.nh = cbind(dataaxle.mass.temp, nhood = pnt.estab.shape$nhood_name)

# add other geographical information from mass.town.types dataset
dataaxle.mass.nh.final <- left_join(dataaxle.mass.nh, mass.town.types, by = c("STCITY" = "municipal"))

######### CENSUS TRACTS #############
#tract_2010 = "K:/DataServices/Datasets/U.S. Census and Demographics/Spatial/Census2010/2010_CensusTracts.shp"
#t2010_shp = readOGR(tract_2010) # 2010 census tracts
# plot(t2010_shp)
#proj4string(t2010_shp) <- new_CRS

######### CENSUS TRACTS #############
tract_2020 = "K:/DataServices/Datasets/U.S. Census and Demographics/Census 2020/Data/Processed/Spatial/ct20_2010xw_shp/ct20_2010xw.shp"
t2020_shp = readOGR(tract_2020) # 2020 census tracts
# plot(t2020_shp)
proj4string(t2020_shp) <- new_CRS

# create dataset that tells us which points fall within which neighborhoods
pnt.estab.shape.ct <- over(estab.points, t2020_shp)

# link neighborhood info to establishment info in full dataaxle dataset
dataaxle.mass = cbind(dataaxle.mass.nh.final, ct20_geocode = pnt.estab.shape.ct$ct20_id)
dataaxle.mass$ct20_geocode = as.character(dataaxle.mass$ct20_geocode)

# add 11-digit Census 2010 census tract variable 
dataaxle.mass$ct20_concat = ifelse(dataaxle.mass$CNTYCD < 10 & dataaxle.mass$CENSUS < 1000, 
                                  as.numeric(paste0(dataaxle.mass$STCODE, '00', dataaxle.mass$CNTYCD, '000',
                                                    dataaxle.mass$CENSUS)),
                                  ifelse(dataaxle.mass$CNTYCD < 10 & dataaxle.mass$CENSUS < 10000, 
                                         as.numeric(paste0(dataaxle.mass$STCODE, '00', dataaxle.mass$CNTYCD, '00',
                                                           dataaxle.mass$CENSUS)),
                                         ifelse(dataaxle.mass$CNTYCD < 10 & dataaxle.mass$CENSUS < 100000, 
                                                as.numeric(paste0(dataaxle.mass$STCODE, '00', dataaxle.mass$CNTYCD, '0',
                                                                  dataaxle.mass$CENSUS)),
                                                ifelse(dataaxle.mass$CNTYCD < 10 & dataaxle.mass$CENSUS > 100000, 
                                                       as.numeric(paste0(dataaxle.mass$STCODE, '00', dataaxle.mass$CNTYCD,
                                                                         dataaxle.mass$CENSUS)),
                                                       ifelse(dataaxle.mass$CNTYCD > 10 & dataaxle.mass$CENSUS < 1000, 
                                                              as.numeric(paste0(dataaxle.mass$STCODE, '0', dataaxle.mass$CNTYCD, '000',
                                                                                dataaxle.mass$CENSUS)),
                                                              ifelse(dataaxle.mass$CNTYCD > 10 & dataaxle.mass$CENSUS < 10000, 
                                                                     as.numeric(paste0(dataaxle.mass$STCODE, '0', dataaxle.mass$CNTYCD, '00',
                                                                                       dataaxle.mass$CENSUS)),
                                                                     ifelse(dataaxle.mass$CNTYCD > 10 & dataaxle.mass$CENSUS < 100000, 
                                                                            as.numeric(paste0(dataaxle.mass$STCODE, '0', dataaxle.mass$CNTYCD, '0',
                                                                                              dataaxle.mass$CENSUS)),
                                                                            as.numeric(paste0(dataaxle.mass$STCODE, '0', dataaxle.mass$CNTYCD,
                                                                                              dataaxle.mass$CENSUS)))))))))

dataaxle.mass$ct20_concat = as.character(dataaxle.mass$ct20_concat)

dataaxle.mass$ct_match = ifelse(dataaxle.mass$ct20_geocode == dataaxle.mass$ct20_concat, 1, 0)

# which census tracts were erroneously created in the concatenation step above but don't actually exist in the census tract shapefile
bad_ct_list = unique(dataaxle.mass$ct20_concat[!(dataaxle.mass$ct20_concat %in% dataaxle.mass$ct20_geocode)])
dataaxle.mass$bad_ct_concat = ifelse(dataaxle.mass$ct20_concat %in% bad_ct_list, 1, 0)

#dataaxle.mass$ct20_id = ifelse(dataaxle.mass$ct20_concat %in% bad_ct_list, dataaxle.mass$ct20_geocode, dataaxle.mass$ct20_concat)
#2022 update: eliminating this step and just using the geocoded ID (there were some failed matches that didn't make sense)
dataaxle.mass$ct20_id = dataaxle.mass$ct20_geocode


################### FIX BLOCK GROUPS HERE ##########################
# step 1: upload block group shapefile 
blockgrp_2020 = "K:/DataServices/Datasets/U.S. Census and Demographics/Census 2020/Data/Processed/Spatial/bg20_2010xw_shp/bg20_2010xw.shp" ## REPLACE WITH BLOCK GROUP SHAPEFILE
################### FIX BLOCK GROUPS HERE ##########################

bg2020_shp = readOGR(blockgrp_2020) # DITTO
# plot(bg2020_shp)
proj4string(bg2020_shp) <- new_CRS # REPLACE t2020_shp with block group shapefile

# create dataset that tells us which points fall within which block groups
pnt.estab.shape.bg <- over(estab.points, bg2020_shp) # REPLACE t2020_shp with block group shapefile 

# link block group info to establishment info in full dataaxle dataset
dataaxle.mass = cbind(dataaxle.mass, bg20_geocode = pnt.estab.shape.bg$bg20_id)
dataaxle.mass$bg20_geocode = as.character(dataaxle.mass$bg20_geocode)

# add 12-digit Census 2010 block group variable
#2022 update - removing this because concatenated block groups does not apply for this dataset (DataAxle used 2010 geos)
#dataaxle.mass$bg20_concat = as.numeric(paste0(dataaxle.mass$ct20_id, dataaxle.mass$BLKGRP))

#dataaxle.mass$bg_match = ifelse(dataaxle.mass$bg20_geocode == dataaxle.mass$bg20_concat, 1, 0)

# which block group were erroneously created in the concatenation step above but don't actually exist in the block group shapefile
#bad_bg_list = unique(dataaxle.mass$bg20_concat[!(dataaxle.mass$bg20_concat %in% dataaxle.mass$bg20_geocode)])
#dataaxle.mass$bad_bg_concat = ifelse(dataaxle.mass$bg20_concat %in% bad_bg_list, 1, 0)

#dataaxle.mass$bg20_id = ifelse(dataaxle.mass$bg20_concat %in% bad_bg_list, dataaxle.mass$bg20_geocode, dataaxle.mass$bg20_concat)
dataaxle.mass$bg20_id = dataaxle.mass$bg20_geocode

# Filter dataaxle listings on MAPC municipalities
dataaxle.mapc <- dataaxle.mass %>%
  filter(mapc == 1)

length(unique(dataaxle.mapc$ct20_concat)) # 737 tracts
length(unique(dataaxle.mapc$ct20_geocode)) # 821  tracts
length(unique(dataaxle.mapc$ct20_id)) # 818 tracts

length(unique(dataaxle.mapc$bg20_id)) # 2412 block groups

# save cleaned/filtered datasets for the rest of your analysis
setwd(modified.data.path)
write.csv(dataaxle.mass, "2022_dataaxle_mass_processed.csv", row.names = FALSE)
write.csv(dataaxle.mapc, "2022_dataaxle_mapc_towns_processed.csv", row.names = FALSE)

