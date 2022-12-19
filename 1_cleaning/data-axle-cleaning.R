# Script: InfoUSA data processing and cleaning
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
modified.data.path <- "K:/DataServices/Projects/Current_Projects/Arts_and_Culture_Planning/Creative_Economy/Data/Modified"
gis.data.path <- "K:/DataServices/Projects/Current_Projects/Arts_and_Culture_Planning/Creative_Economy/Data/GIS"

# load InfoUSA raw files (April 2021)
setwd(raw.data.path)
infousa <- read.csv("April_2021 - Massachusetts_New.csv", stringsAsFactors = FALSE)

#add an OBJECT ID field
infousa$OBJECTID <- seq.int(nrow(infousa))

####### DATA CLEANING: COMBINE INFOUSA RAW FILES AND dplyr::select ON VARIABLES OF INTEREST #######
# dplyr::select our infousa variables of interest 
# also, added new variables for the four-digit and six-digit NAICS code 
# (taken from the first four or six digits of the 8-digit NAICS variable)

# 2022 edit - changing field names to reflect data axle fields
infousa.mass.temp <- infousa %>%
  dplyr::select(OBJECTID, CONAME,STADDR,STCITY, STATE, ZIP, STCODE, 
                CNTYCD, CENSUS, BLKGRP, LATITUDE, LONGITUDE, NAICS, NAICSD, 
                EMPSIZ, LOCEMP, SALVOL, 
                SLSVDT, ZPOPCD, WEALTH, BBUSIN, 
                MBUSIN, SBUSIN, HOME, INDFRM,
                FEMOWN) %>%
  mutate("NAICS_FOUR" = as.character(substr(as.character(NAICS), 1, 4))) %>%
  filter(!is.na(LOCEMP) & STCODE == 25)  



####### DATA CLEANING: AGGREGATING NEIGHBORHOODS/VILLAGES INTO THEIR RESPECTIVE MUNICIPALITY #######
# InfoUSA lists establishments in either their municipality (e.g. Boston) or their neighborhood/village (e.g. "Dorchester")
# There are also some names spelled differently than on the MAPC and Massachusetts state government municipality lists
# Therefore, I've gone through the InfoUSA data and corrected spellings using the gsub function
# I've also consolidated neighborhoods and villages into their respective municipalities using the ifelse function

infousa.mass.temp$STCITY <- gsub("Manchestr By Sea", "Manchester", infousa.mass.temp$STCITY)
infousa.mass.temp$STCITY <- gsub("Foxboro", "Foxborough", infousa.mass.temp$STCITY)
infousa.mass.temp$STCITY <- gsub("North Attleboro", "North Attleborough", infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("Roxbury", "Roxbury Crossing", "Mission Hill", "Dorchester", "Grove Hall", "Dorchester Ctr",
                                                                               "Uphams Corner", "Mattapan", "South Boston", "East Boston", "Charlestown", "Jamaica Plain",
                                                                               "Roslindale", "West Roxbury", "Allston", "Brighton", "Hyde Park"), 
                                         "Boston", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("East Arlington", "Arlington Hts"), 
                                         "Arlington", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("North Cambridge", "East Cambridge"), 
                                         "Cambridge", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("Newtonville", "Newton Center", "Newton Highlands", "Newton Lower Fls",
                                                                               "Newton Upper Fls", "West Newton", "Auburndale", "Chestnut Hill", "Waban", "Nonantum"), 
                                         "Newton", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("North Easton", "South Easton"), 
                                         "Easton", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("East Weymouth", "South Weymouth","North Weymouth"), 
                                         "Weymouth", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("Teaticket", "North Falmouth","East Falmouth", "West Falmouth", 
                                                                               "Woods Hole", "Waquoit"), 
                                         "Falmouth", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("North Quincy", "Wollaston"), 
                                         "Quincy", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("Hyannis", "Centerville", "Cotuit", "Cummaquid", 
                                                                               "Hyannis Port", "Marstons Mills", "Osterville", "West Hyannisport",
                                                                               "West Barnstable"), 
                                         "Barnstable", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("West Somerville"), 
                                         "Somerville", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("White Horse Bch", "Manomet"), 
                                         "Plymouth", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("East Sandwich", "Forestdale"), 
                                         "Sandwich", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("South Wellfleet"), 
                                         "Wellfleet", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("North Amherst"), 
                                         "Amherst", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("North Uxbridge"), 
                                         "Uxbridge", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("Housatonic"), 
                                         "Great Barrington", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("Vineyard Haven"), 
                                         "Tisbury", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("Greenbush", "Humarock", "North Scituate"), 
                                         "Scituate", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("Byfield"), 
                                         "Newbury", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("East Walpole", "South Walpole"), 
                                         "Walpole", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("North Grafton", "South Grafton"), 
                                         "Grafton", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("Shattuckville"), 
                                         "Colrain", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("Woronoco"), 
                                         "Russell", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("Milton Village"), 
                                         "Milton", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("Ashley Falls"), 
                                         "Sheffield", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("Attleboro Falls"), 
                                         "North Attleborough", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("Manchaug", "Wilkinsonville"), 
                                         "Sutton", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("West Hatfield", "North Hatfield"), 
                                         "Hatfield", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("East Douglas"), 
                                         "Douglas", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("East Orleans"), 
                                         "Orleans", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("South Attleboro"), 
                                         "Attleboro", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("Baldwinville", "East Templeton"), 
                                         "Templeton", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("Charlton Depot", "Charlton City"), 
                                         "Charlton", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("Assonet", "East Freetown"), 
                                         "Freetown", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("West Millbury"), 
                                         "Millbury", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("West Harwich", "Harwich Port", "East Harwich"), 
                                         "Harwich", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("South Dennis", "West Dennis", "East Dennis", "Dennis Port"), 
                                         "Dennis", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("Lake Pleasant", "Millers Falls", "Turners Falls"), 
                                         "Montague", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("Bass River", "West Yarmouth", "Yarmouth Port", "South Yarmouth"), 
                                         "Yarmouth", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("Marshfield Hills", "North Marshfield", 
                                                                               "Ocean Bluff", "Brant Rock", "Green Harbor"), 
                                         "Marshfield", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("Accord"), 
                                         "Norwell", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("Brookline Vlg"), 
                                         "Brookline", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("Cuttyhunk"), 
                                         "Gosnold", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("Haydenville"), 
                                         "Williamsburg", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("West Medford"), 
                                         "Medford", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("West Whately"), 
                                         "Whately", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("East Otis"), 
                                         "Otis", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("Wellesley Hills", "Babson Park"), 
                                         "Wellesley", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("Feeding Hills"), 
                                         "Agawam", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("Gilbertville", "South Harwich"), 
                                         "Hardwick", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("Buzzards Bay", "Cataumet", "Monument Beach", "Pocasset", "Sagamore", "Sagamore Beach"), 
                                         "Bourne", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("Westport Point"), 
                                         "Westport", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("North Eastham"), 
                                         "Eastham", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("East Wareham", "West Wareham", "Onset"), 
                                         "Wareham", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("Berkshire"), 
                                         "Lanesborough", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("Bondsville", "Thorndike", "Three Rivers"), 
                                         "Palmer", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("Bradford"), 
                                         "Haverhill", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("Bryantville"), 
                                         "Pembroke", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("Chartley"), 
                                         "Norton", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("Cherry Valley", "Rochdale"), 
                                         "Leicester", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("Drury"), 
                                         "Florida", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("East Taunton"), 
                                         "Taunton", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("Elmwood"), 
                                         "East Bridgewater", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("Fiskdale"), 
                                         "Sturbridge", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("Florence", "Leeds"), 
                                         "Northampton", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("Hathorne"), 
                                         "Danvers", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("Indian Orchard"), 
                                         "Springfield", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("Jefferson"), 
                                         "Holden", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("Lenox Dale"), 
                                         "Lenox", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("Glendale"), 
                                         "Stockbridge", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("Menemsha"), 
                                         "Chilmark", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("Mill River"), 
                                         "New Marlborough", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("Monponsett"), 
                                         "Hanson", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("Mt Hermon"), 
                                         "Gill", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("Needham Heights"), 
                                         "Needham", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("North Billerica"), 
                                         "Billerica", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("North Carver" ), 
                                         "Carver", 
                                         infousa.mass.temp$STCITY)

infousa.mass.temp$STCITY <- ifelse(infousa.mass.temp$STCITY %in% c("North Chatham"), 
                                         "Chatham", 
                                         infousa.mass.temp$STCITY)


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
estab.points <- SpatialPoints(data.frame(latitude = infousa.mass.temp$LONGITUDE, longitude = infousa.mass.temp$LATITUDE), # for some reason this only works when latitude is the data's longitude and longitude is data's latitude
                              proj4string = CRS(WCS1984_CRS)) 

# transform projection to NAD83 to match neighborhood file
estab.points <- spTransform(estab.points, new_CRS)

# create dataset that tells us which points fall within which neighborhoods
pnt.estab.shape <- over(estab.points, ma_nh)

# link neighborhood info to establishment info in full infousa dataset
infousa.mass.nh = cbind(infousa.mass.temp, nhood = pnt.estab.shape$nhood_name)

# add other geographical information from mass.town.types dataset
infousa.mass.nh.final <- left_join(infousa.mass.nh, mass.town.types, by = c("STCITY" = "municipal"))

######### CENSUS TRACTS #############
tract_2010 = "K:/DataServices/Datasets/U.S. Census and Demographics/Spatial/Census2010/2010_CensusTracts.shp"
t2010_shp = readOGR(tract_2010) # 2010 census tracts
# plot(t2010_shp)
proj4string(t2010_shp) <- new_CRS

# create dataset that tells us which points fall within which neighborhoods
pnt.estab.shape.ct <- over(estab.points, t2010_shp)

# link neighborhood info to establishment info in full infousa dataset
infousa.mass = cbind(infousa.mass.nh.final, ct10_geocode = pnt.estab.shape.ct$GEOID10)
infousa.mass$ct10_geocode = as.character(infousa.mass$ct10_geocode)

# add 11-digit Census 2010 census tract variable 
infousa.mass$ct10_concat = ifelse(infousa.mass$CNTYCD < 10 & infousa.mass$CENSUS < 1000, 
                                  as.numeric(paste0(infousa.mass$STCODE, '00', infousa.mass$CNTYCD, '000',
                                                    infousa.mass$CENSUS)),
                                  ifelse(infousa.mass$CNTYCD < 10 & infousa.mass$CENSUS < 10000, 
                                         as.numeric(paste0(infousa.mass$STCODE, '00', infousa.mass$CNTYCD, '00',
                                                           infousa.mass$CENSUS)),
                                         ifelse(infousa.mass$CNTYCD < 10 & infousa.mass$CENSUS < 100000, 
                                                as.numeric(paste0(infousa.mass$STCODE, '00', infousa.mass$CNTYCD, '0',
                                                                  infousa.mass$CENSUS)),
                                                ifelse(infousa.mass$CNTYCD < 10 & infousa.mass$CENSUS > 100000, 
                                                       as.numeric(paste0(infousa.mass$STCODE, '00', infousa.mass$CNTYCD,
                                                                         infousa.mass$CENSUS)),
                                                       ifelse(infousa.mass$CNTYCD > 10 & infousa.mass$CENSUS < 1000, 
                                                              as.numeric(paste0(infousa.mass$STCODE, '0', infousa.mass$CNTYCD, '000',
                                                                                infousa.mass$CENSUS)),
                                                              ifelse(infousa.mass$CNTYCD > 10 & infousa.mass$CENSUS < 10000, 
                                                                     as.numeric(paste0(infousa.mass$STCODE, '0', infousa.mass$CNTYCD, '00',
                                                                                       infousa.mass$CENSUS)),
                                                                     ifelse(infousa.mass$CNTYCD > 10 & infousa.mass$CENSUS < 100000, 
                                                                            as.numeric(paste0(infousa.mass$STCODE, '0', infousa.mass$CNTYCD, '0',
                                                                                              infousa.mass$CENSUS)),
                                                                            as.numeric(paste0(infousa.mass$STCODE, '0', infousa.mass$CNTYCD,
                                                                                              infousa.mass$CENSUS)))))))))

infousa.mass$ct10_concat = as.character(infousa.mass$ct10_concat)

infousa.mass$ct_match = ifelse(infousa.mass$ct10_geocode == infousa.mass$ct10_concat, 1, 0)

# which census tracts were erroneously created in the concatenation step above but don't actually exist in the census tract shapefile
bad_ct_list = unique(infousa.mass$ct10_concat[!(infousa.mass$ct10_concat %in% infousa.mass$ct10_geocode)])
infousa.mass$bad_ct_concat = ifelse(infousa.mass$ct10_concat %in% bad_ct_list, 1, 0)

infousa.mass$ct10_id = ifelse(infousa.mass$ct10_concat %in% bad_ct_list, infousa.mass$ct10_geocode, infousa.mass$ct10_concat)



################### FIX BLOCK GROUPS HERE ##########################
# step 1: upload block group shapefile 
blockgrp_2010 = "K:/DataServices/Datasets/U.S. Census and Demographics/Spatial/Census2010/2010_Census_BlockGroups.shp" ## REPLACE WITH BLOCK GROUP SHAPEFILE
################### FIX BLOCK GROUPS HERE ##########################

bg2010_shp = readOGR(blockgrp_2010) # DITTO
# plot(bg2010_shp)
proj4string(bg2010_shp) <- new_CRS # REPLACE t2010_shp with block group shapefile

# create dataset that tells us which points fall within which block groups
pnt.estab.shape.bg <- over(estab.points, bg2010_shp) # REPLACE t2010_shp with block group shapefile 

# link block group info to establishment info in full infousa dataset
infousa.mass = cbind(infousa.mass, bg10_geocode = pnt.estab.shape.bg$GEOID10)
infousa.mass$bg10_geocode = as.character(infousa.mass$bg10_geocode)

# add 12-digit Census 2010 block group variable
infousa.mass$bg10_concat = as.numeric(paste0(infousa.mass$ct10_id, infousa.mass$BLKGRP))

infousa.mass$bg_match = ifelse(infousa.mass$bg10_geocode == infousa.mass$bg10_concat, 1, 0)

# which block group were erroneously created in the concatenation step above but don't actually exist in the block group shapefile
bad_bg_list = unique(infousa.mass$bg10_concat[!(infousa.mass$bg10_concat %in% infousa.mass$bg10_geocode)])
infousa.mass$bad_bg_concat = ifelse(infousa.mass$bg10_concat %in% bad_bg_list, 1, 0)

infousa.mass$bg10_id = ifelse(infousa.mass$bg10_concat %in% bad_bg_list, infousa.mass$bg10_geocode, infousa.mass$bg10_concat)


# Filter infousa listings on MAPC municipalities
infousa.mapc <- infousa.mass %>%
  filter(mapc == 1)

length(unique(infousa.mapc$ct10_concat)) # 909 tracts
length(unique(infousa.mapc$ct10_geocode)) # 884 tracts
length(unique(infousa.mapc$ct10_id)) # 882 tracts

length(unique(infousa.mapc$bg10_id)) # 2671 tracts

# save cleaned/filtered datasets for the rest of your analysis
setwd(modified.data.path)
write.csv(infousa.mass, "2022_dataaxle_mass_processed.csv", row.names = FALSE)
write.csv(infousa.mapc, "2022_dataaxle_mapc_towns_processed.csv", row.names = FALSE)

