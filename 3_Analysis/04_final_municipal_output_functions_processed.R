###################################################################################################
# Code: Final Block Group Output Functions Code
# Purpose: This code contains the functions needed to produce the final block group-specific 
#          tables and graphs for the Creative Economy Indicators Data Project
# Author: Seleeke Flingai
# Date: March 8, 2019
# Modified: December 27, 2022 by RBowers
###################################################################################################
# install and load required packages
# install.packages("pacman")
pacman::p_load(sp, sf, spatstat, rgeos, rgdal, leaflet, leaflet.extras, 
               maptools, GISTools, tmap, tmaptools, ggmap, ggplot2, 
               tidyr, dplyr, RColorBrewer, mapview, webshot, openxlsx, scales)
webshot::install_phantomjs(force = T)

#set tmap_options - note that this might be something to check in on later
tmap_options(check.and.fix = TRUE, basemaps = c(Canvas = "Esri.WorldTopoMap", Imagery = "Esri.WorldImagery"))
tmap_mode("view")
sf::sf_use_s2(FALSE)
# load paths to gather and save files

#update with years where necessary
raw.data.path <- "K:/DataServices/Projects/Current_Projects/Arts_and_Culture_Planning/Creative_Economy/Data/Raw"
modified.data.path <- "K:/DataServices/Projects/Current_Projects/Arts_and_Culture_Planning/Creative_Economy/Data/Modified/2022"
boundaries.path <- "K:/DataServices/Projects/Current_Projects/Arts_and_Culture_Planning/Creative_Economy/Data/GIS"
output.path <- "K:/DataServices/Projects/Current_Projects/Arts_and_Culture_Planning/Creative_Economy/Output/2022"

# read in the required datasets: the municipal data key (for muni-to-community type/subregion linkages), 
# final dataaxle creative economy summary dataset, and the detailed MAPC-specific dataaxle creative economy dataset
setwd(raw.data.path)
mass.town.types <- read.csv("mass_town_types.csv", header = TRUE)

mass.town.types <- mass.town.types %>%
  dplyr::select(muni_id, municipal, county_id, county, mapc, comm_name, subtype, subregion)

mapc.town.types <- mass.town.types %>%
  dplyr::filter(mapc == 1) %>%
  dplyr::select(muni_id, municipal, county_id, county, comm_name, subtype, subregion)

setwd(modified.data.path)
summary.dataaxle.ce.final <- read.csv("summary_dataaxle_ce_final_mapc_processed_all_geographies.csv", stringsAsFactors = FALSE)
dataaxle.nefa.core.mapc <- read.csv("DataAxle_nefa_core_mass_processed.csv", stringsAsFactors = FALSE)


####### MAPPING SETUP: LOADING CENSUS BOUNDARIES AND CHANGING COORDINATE REFERENCE SYSTEMS #########
# Load in census boundaries (municipalities, census tracts, block groups, and blocks)
setwd(boundaries.path)

# load Massachusetts geometries
mapc.towns <- readOGR(dsn = ".", layer = "MAPC_towns", stringsAsFactors = FALSE, encoding = "latin1")
mapc.nhood <- readOGR(dsn = ".", layer = "MA_neighborhoods", stringsAsFactors = FALSE, encoding = "latin1")
mapc.ct <-  readOGR(dsn = ".", layer = "ct20_2010xw", stringsAsFactors = FALSE, encoding = "latin1")
mapc.bg <-  readOGR(dsn = ".", layer = "bg20_2010xw", stringsAsFactors = FALSE, encoding = "latin1")

# transform geometry CRSes to match that of mass.towns (projected, in meters)
mapc.towns <- spTransform(mapc.towns, CRSobj = CRS("+proj=utm +zone=20 ellps=WGS84"))
mapc.nhood <- spTransform(mapc.nhood, CRSobj = CRS("+proj=utm +zone=20 ellps=WGS84"))
mapc.ct <- spTransform(mapc.ct, CRSobj = CRS("+proj=utm +zone=20 ellps=WGS84"))
mapc.bg <- spTransform(mapc.bg, CRSobj = CRS("+proj=utm +zone=20 ellps=WGS84"))

# transform address lat/long to universal transverse mercator (UTM) to allow for distance buffers
latlong <- data.frame(cbind(dataaxle.nefa.core.mapc$LATITUDE, dataaxle.nefa.core.mapc$LONGITUDE))
names(latlong) <- c("X", "Y")
coordinates(latlong) <- ~ Y + X # longitude first
proj4string(latlong) <- CRS("+proj=longlat + ellps=WGS84 +datum=WGS84")

establishments <- SpatialPointsDataFrame(coords = latlong,
                                         data = dataaxle.nefa.core.mapc,
                                         proj4string = CRS(proj4string(latlong)))
establishments <- spTransform(establishments, CRS("+proj=utm +zone=20 ellps=WGS84"))


setwd(output.path)

# Load metadata file for table output later
ce.summary.metadata <- read.csv("ce_summary_metadata.csv", stringsAsFactors = FALSE)

#################################### MUNICIPAL #################################### 
####### MAP: NEFA Core Creative Enterprise Dot Map for a designated municipality (without sole proprietors) #######
municipal.map <- function (municipality) {
  
  establishments.municipal <- establishments[establishments$STCITY==municipality,]
  establishments.municipal.no.soleprop <- establishments.municipal[establishments.municipal@data$LOCEMP > 1,]
  muni <- mapc.towns[mapc.towns@data$town==municipality,]
  
  ce_distribution_map <- 
    tm_basemap("Esri.WorldTopoMap") +
    tm_shape(muni) + 
    tm_borders("black", lwd = 4) +
    tm_shape(mapc.towns) +
    tm_borders("black") +
    tm_shape(establishments.municipal.no.soleprop, size = 1) +
    tm_dots(col = "CREATIVE_CAT", palette = "Paired", border.col = "black", 
            alpha = 0.8,
            jitter = 0,
            title = paste0(municipality, ": Creative Industry Group Type"),
            size = 0.1,
            id = ("Company" = "CONAME"),
            popup.vars = c("Company" = "CONAME", 
                           "Street Address" = "STADDR",
                           "Creative Industry Group" = "CREATIVE_CAT")) +
    tmap_style("white") +
    tm_layout(title = "CONFIDENTIAL - DO NOT SHARE", legend.frame = TRUE) 

  tmap_leaflet(ce_distribution_map, mode = "view", show = TRUE) 
  
  mapshot(tmap_leaflet(ce_distribution_map, mode = "view", show = TRUE), 
          url = paste0(getwd(), "/map_", tolower(municipality), "_ce.html"), 
          file = paste0(getwd(), "/map_", tolower(municipality), "_ce.png"), zoom = 3)
}

####### MAP: NEFA Core Creative Enterprise Heat Map for a designated municipality (without sole proprietors) #######
municipal.heatmap <- function (municipality) {
  
  establishments.municipal <- establishments[establishments$STCITY==municipality,]
  establishments.municipal.no.soleprop <- establishments.municipal[establishments.municipal@data$LOCEMP > 1,]
  lon <- establishments.municipal$LONGITUDE[!is.null(establishments.municipal$LONGITUDE)]
  lat <- establishments.municipal$LATITUDE[!is.null(establishments.municipal$LATITUDE)]
  
  # establishments.municipal.no.soleprop <- establishments.municipal[establishments.municipal@data$LOCEMP > 1,]
  muni <- mapc.towns[mapc.towns@data$town==municipality,]
  muni.wgs84 <- spTransform(muni, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
  mapc.towns.wgs84 <- spTransform(mapc.towns, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
  
  myMap <- leaflet(establishments.municipal) %>% 
    addProviderTiles(providers$Esri.WorldTopoMap, group = "ESRI World Topo Map (default)") %>%
    addProviderTiles(providers$CartoDB.Positron, group = "CartoDB Positron") %>%
    addTiles(group = "OpenStreetMap") %>%
    fitBounds(muni.wgs84@bbox[1,1], muni.wgs84@bbox[2,1],
              muni.wgs84@bbox[1,2], muni.wgs84@bbox[2,2]) %>%
    addPolygons(data = muni.wgs84, weight = 4, color = "black", smoothFactor = 0.3, fill = NA) %>%  
    addPolygons(data = mapc.towns.wgs84, weight = 1, color = "black", smoothFactor = 0.3, fill = NA) %>%
    # addCircles(data = establishments.municipal.no.soleprop, lng = ~lon, lat = ~lat, 
    #            color = "black", weight = 1, fillColor = ~CREATIVE_CAT, group = "Points") %>% # if we want to add dots for each establishment
    addHeatmap(lng = ~lon, lat = ~lat,
               group = "Heat map", blur = 20, max = 1, radius = 10) %>%
    addLayersControl(baseGroups = c("ESRI World Topo Map (default)", "CartoDB Positron", "OpenStreetMap"),
                     # overlayGroups = c("Heat map", "Points"),
                     overlayGroups = "Heat map",
                     options = layersControlOptions(collapsed = FALSE))
  
  mapshot(myMap, 
          url = paste0(getwd(), "/heatmap_", tolower(municipality), "_ce.html"), 
          file = paste0(getwd(), "/heatmap_", tolower(municipality), "_ce.png"), zoom = 3)
}

####### CSV TABLE: Municipal summary data comparison #######
municipal.table <- function(municipality) {
  
  municipal.data <- summary.dataaxle.ce.final %>%
    dplyr::filter(geography == municipality)
  
  comm.type.data <- summary.dataaxle.ce.final %>%
    dplyr::filter(geography == mapc.town.types$comm_name[mapc.town.types$municipal == municipality])
  
  subregion.data <- summary.dataaxle.ce.final %>%
    dplyr::filter(geography == mapc.town.types$subregion[mapc.town.types$municipal == municipality])
  
  #subtype.data <- summary.dataaxle.ce.final[summary.dataaxle.ce.final$geography == municipal.data$subtype,]
  
  #county.data <- summary.dataaxle.ce.final[summary.dataaxle.ce.final$geography == municipal.data$county,]
  
  mapc.data <- summary.dataaxle.ce.final %>%
    dplyr::filter(geography == 'MAPC')
  
  mass.data <- summary.dataaxle.ce.final %>%
    dplyr::filter(geography == 'Massachusetts')
  
  data.output <- rbind(municipal.data,
                       comm.type.data,
                       subregion.data,
                       #subtype.data,
                       #county.data,
                       mapc.data,
                       mass.data)
  
  # Create a blank workbook
  output.xlsx <- createWorkbook()
  
  # Add some sheets to the workbook
  addWorksheet(output.xlsx, paste0(tolower(municipality), "_ce_data"))
  addWorksheet(output.xlsx, "metadata")
  
  # Write the data to the sheets
  writeData(output.xlsx, sheet = paste0(tolower(municipality), "_ce_data"), x = data.output)
  writeData(output.xlsx, sheet = "metadata", x = ce.summary.metadata)
  
  # Export the file
  saveWorkbook(output.xlsx, paste0(tolower(municipality), "_ce_data.xlsx"))
  
  data.output$id <- seq.int(nrow(data.output))
  
  # bar graph
  plot <- ggplot(data.output, aes(x = reorder(geography, id), y = ce.share.core.geography)) + 
    geom_bar(stat = "identity", fill = "steelblue", color = "black") +
    labs(title = paste0(municipality, " Creative Economy Share, Core Enterprises, by Geography, 2021 (DataAxle)"),
         y = "Creative economy core enterprises per 100 total enterprises", x = "") +
    geom_text(aes(label=ce.share.core.geography), vjust=2, size=6, color = "white") +
    theme_minimal() +
    theme(plot.margin = unit(c(1,3,1,1), "cm"),
          axis.text.x  = element_text(size=12))
  ggsave(file=paste0(tolower(municipality), "_ce_core_share_comparison.png"), width = 14, height = 8, units = 'in')
  
}

####### CSV TABLE: Municipal NEFA Core Enterprises list (DataAxle) #######
municipal.dataaxle.ce.core.list <- function(municipality) {
  municipal.ce.core.list <- dataaxle.nefa.core.mapc %>%
    dplyr::filter(STCITY == municipality)
  
  write.csv(municipal.ce.core.list, paste0(tolower(municipality), "_ce_establishment_list.csv"), row.names = FALSE)
  
  summary.dataaxle.ce.core.naics.desc.municipality.df <- municipal.ce.core.list %>%
    dplyr::filter(STCITY == municipality) %>%
    dplyr::select(OBJECTID, STCITY, NAICSD, LOCEMP, SLSVDT,
                  FEMOWN, CREATIVE_CAT) %>%
    dplyr::group_by(NAICSD) %>%
    mutate(FEMOWN = ifelse(FEMOWN == "Y", 1, 0)) %>%
    summarise(ce.core.estab = n(),
              ce.core.employee.count = sum(LOCEMP, na.rm = TRUE),
              ce.core.sales.total = sum(SLSVDT, na.rm = TRUE),
              ce.core.female.owned = mean(FEMOWN),
              ce.core.sole.prop.count = sum(LOCEMP == 1)) %>%
    mutate(cat.core.estab.share = ce.core.estab / sum(ce.core.estab),
           cat.core.employee.share = ce.core.employee.count / sum(ce.core.employee.count),
           cat.core.sales.share = ce.core.sales.total / sum(ce.core.sales.total)) %>%
    dplyr::select(NAICSD, ce.core.estab, cat.core.estab.share, ce.core.employee.count, cat.core.employee.share,
                  ce.core.sales.total, cat.core.sales.share, ce.core.sole.prop.count, ce.core.female.owned)
  
  # write dataframe to a CSV
  write.csv(summary.dataaxle.ce.core.naics.desc.municipality.df, 
            paste0(tolower(municipality), "_ce_NAICSD_breakdown.csv"), row.names = FALSE)
  
  # bar graph of establishment counts
  plot <- ggplot(summary.dataaxle.ce.core.naics.desc.municipality.df, aes(x = reorder(NAICSD, ce.core.estab), y = ce.core.estab)) + 
    geom_bar(stat = "identity", fill = "steelblue", color = "black") +
    labs(title = paste0(municipality, " Creative Economy Core Establishment Counts by dataaxle NAICS description, 2021"),
         y = "Firms", x = "") +
    geom_text(aes(label=ce.core.estab), hjust = -0.2, size=3.5) +
    theme_minimal() +
    coord_flip() +
    theme(plot.margin = unit(c(1,3,1,1), "cm"))
  ggsave(file=paste0(tolower(municipality), "_ce_NAICSD_breakdown.png"), width = 14, height = 8, units = 'in')
  
}

####### CSV TABLE: Municipal NEFA Core Enterprises list (DataAxle) #######
municipal.creative.group.summary <- function(municipality) {
  
  summary.dataaxle.ce.nefa.core.categories.municipality.df <- dataaxle.nefa.core.mapc %>%
    dplyr::filter(STCITY == municipality) %>%
    dplyr::select(OBJECTID, STCITY, LOCEMP, SLSVDT,
                  FEMOWN, CREATIVE_CAT) %>%
    dplyr::group_by(CREATIVE_CAT) %>%
    mutate(FEMOWN = ifelse(FEMOWN == "Y", 1, 0)) %>%
    summarise(ce.core.estab = n(),
              ce.core.employee.count = sum(LOCEMP, na.rm = TRUE),
              ce.core.sales.total = sum(SLSVDT, na.rm = TRUE),
              ce.core.female.owned = mean(FEMOWN),
              ce.core.sole.prop.count = sum(LOCEMP == 1)) %>%
    mutate(cat.core.estab.share = ce.core.estab / sum(ce.core.estab),
           cat.core.employee.share = ce.core.employee.count / sum(ce.core.employee.count),
           cat.core.sales.share = ce.core.sales.total / sum(ce.core.sales.total)) %>%
    dplyr::select(CREATIVE_CAT, ce.core.estab, cat.core.estab.share, ce.core.employee.count, cat.core.employee.share,
                  ce.core.sales.total, cat.core.sales.share, ce.core.sole.prop.count, ce.core.female.owned)
  
  # write dataframe to a CSV
  write.csv(summary.dataaxle.ce.nefa.core.categories.municipality.df, 
            paste0(tolower(municipality), "_ce_creative_group_breakdown.csv"), row.names = FALSE)
  
  # bar graph of establishment counts
  plot <- ggplot(summary.dataaxle.ce.nefa.core.categories.municipality.df, aes(x = reorder(CREATIVE_CAT, ce.core.estab), y = ce.core.estab)) + 
    geom_bar(stat = "identity", fill = "steelblue", color = "black") +
    labs(title = paste0(municipality, " Creative Economy Core Establishment Counts by Industry Group, 2021 (DataAxle)"),
         y = "Firms", x = "") +
    geom_text(aes(label=ce.core.estab), hjust = -0.2, size=4) +
    theme_minimal() +
    coord_flip() +
    theme(plot.margin = unit(c(1,3,1,1), "cm"),
          axis.text.y  = element_text(size=16))
  ggsave(file=paste0(tolower(municipality), "_ce_creative_groups.png"), width = 14, height = 8, units = 'in')
  
}


#################################### NEIGHBORHOOD #################################### 
####### MAP: NEFA Core Creative Enterprise Dot Map for a designated neighborhood (without sole proprietors) #######
nhood.map <- function (nhood_name) {
  
  establishments.nhood <- establishments[establishments$nhood %in% nhood_name,]
  establishments.nhood.no.soleprop <- establishments.nhood[establishments.nhood$LOCEMP > 1,]
  nhood.shape <- mapc.nhood[mapc.nhood$nhood_name %in% nhood_name,]
  
  ce_distribution_map <- 
    tm_basemap("Esri.WorldTopoMap") +
    tm_shape(nhood.shape) + 
    tm_borders("black", lwd = 4) +
    tm_shape(mapc.nhood) +
    tm_borders("black") +
    tm_shape(establishments.nhood.no.soleprop, size = 1) +
    tm_dots(col = "CREATIVE_CAT", palette = "Paired", border.col = "black", 
            alpha = 0.8,
            jitter = 0,
            title = paste0(nhood_name, ": Creative Industry Group Type"),
            size = 0.1,
            id = ("Company" = "CONAME"),
            popup.vars = c("Company" = "CONAME", 
                           "Street Address" = "STADDR",
                           "Creative Industry Group" = "CREATIVE_CAT")) +
    tmap_style("white") +
    tm_layout(title = "CONFIDENTIAL - DO NOT SHARE", legend.frame = TRUE) 
    

  
  tmap_leaflet(ce_distribution_map, mode = "view", show = TRUE) 
  
  mapshot(tmap_leaflet(ce_distribution_map, mode = "view", show = TRUE), 
          url = paste0(getwd(), "/map_", tolower(nhood_name), "_ce.html"), 
          file = paste0(getwd(), "/map_", tolower(nhood_name), "_ce.png"), zoom = 3)
}

####### MAP: NEFA Core Creative Enterprise Heat Map for a designated neighborhood (without sole proprietors) #######
nhood.heatmap <- function (nhood_name) {
  
  establishments.nhood <- establishments[establishments$nhood %in% nhood_name,]
  establishments.nhood.no.soleprop <- establishments.nhood[establishments.nhood@data$LOCEMP > 1,]
  lon <- establishments.nhood$LONGITUDE[!is.null(establishments.nhood$LONGITUDE)]
  lat <- establishments.nhood$LATITUDE[!is.null(establishments.nhood$LATITUDE)]
  
  nhood.shape <- mapc.nhood[mapc.nhood$nhood_name %in% nhood_name,]
  nhood.wgs84 <- spTransform(nhood.shape, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
  mapc.nhood.wgs84 <- spTransform(mapc.nhood, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
  
  myMap <- leaflet(establishments.nhood) %>%
    addProviderTiles(providers$Esri.WorldTopoMap, group = "ESRI World Topo Map (default)") %>%
    addProviderTiles(providers$CartoDB.Positron, group = "CartoDB Positron") %>%
    addTiles(group = "OpenStreetMap") %>%
    fitBounds(nhood.wgs84@bbox[1,1], nhood.wgs84@bbox[2,1],
              nhood.wgs84@bbox[1,2], nhood.wgs84@bbox[2,2]) %>%
    addPolygons(data = nhood.wgs84, weight = 4, color = "black", smoothFactor = 0.3, fill = NA) %>%
    addPolygons(data = mapc.nhood.wgs84, weight = 1, color = "black", smoothFactor = 0.3, fill = NA) %>%
    # addCircles(data = establishments.municipal.no.soleprop, lng = ~lon, lat = ~lat,
    #            color = "black", weight = 1, fillColor = ~CREATIVE_CAT, group = "Points") %>% # if we want to add dots for each establishment
    addHeatmap(lng = ~lon, lat = ~lat,
               group = "Heat map", blur = 20, max = 1, radius = 10) %>%
    addLayersControl(baseGroups = c("ESRI World Topo Map (default)", "CartoDB Positron", "OpenStreetMap"),
                     # overlayGroups = c("Heat map", "Points"),
                     overlayGroups = "Heat map",
                     options = layersControlOptions(collapsed = FALSE))
  
  mapshot(myMap,
          url = paste0(getwd(), "/heatmap_", tolower(nhood_name), "_ce.html"),
          file = paste0(getwd(), "/heatmap_", tolower(nhood_name), "_ce.png"), zoom = 3)
}

####### CSV TABLE: Neighborhood summary data comparison #######
nhood.table <- function(nhood_name, municipality) {
  # create dataset with decimals for graphing
  
  nhood.data <- summary.dataaxle.ce.final %>%
    dplyr::filter(geography == nhood_name) %>%
    select(-c(share.all.mapc, ce.share.all.mapc, share.all.employee.mapc, ce.share.employee.all.mapc, ce.share.core.mapc, ce.share.employee.core.mapc))
  
  
  muni.data <- summary.dataaxle.ce.final %>%
    dplyr::filter(geography == municipality) %>%
    select(-c(share.all.mapc, ce.share.all.mapc, share.all.employee.mapc, ce.share.employee.all.mapc, ce.share.core.mapc, ce.share.employee.core.mapc))
  
  data.output <- rbind(nhood.data,
                       muni.data) 
  
  data.output$id <- seq.int(nrow(data.output))
  
  
  # Create a blank workbook
  output.xlsx <- createWorkbook()
  
  # Add some sheets to the workbook
  addWorksheet(output.xlsx, paste0(tolower(nhood_name), "_ce_data"))
  #addWorksheet(output.xlsx, "metadata")
  
  # Write the data to the sheets
  writeData(output.xlsx, sheet = paste0(tolower(nhood_name), "_ce_data"), x = data.output)
  #writeData(output.xlsx, sheet = "metadata", x = ce.summary.metadata)
  
  # Export the file
  saveWorkbook(output.xlsx, paste0(tolower(nhood_name), "_ce_data.xlsx"))
  return(nhood.data)
}

####### CSV TABLE: Neighborhood NEFA Core Enterprises list (DataAxle) #######
nhood.dataaxle.ce.core.list <- function(nhood_name) {
  nhood.ce.core.list <- dataaxle.nefa.core.mapc %>%
    dplyr::filter(nhood == nhood_name)
  
  write.csv(nhood.ce.core.list, paste0(tolower(nhood_name), "_ce_establishment_list.csv"), row.names = FALSE)
  
  summary.dataaxle.ce.core.naics.desc.nhood.df <- nhood.ce.core.list %>%
    dplyr::filter(nhood == nhood_name) %>%
    dplyr::select(OBJECTID, nhood, NAICSD, LOCEMP, SLSVDT,
                  FEMOWN, CREATIVE_CAT) %>%
    dplyr::group_by(nhood, NAICSD) %>%
    mutate(FEMOWN = ifelse(FEMOWN == "Y", 1, 0)) %>%
    summarise(ce.core.estab = n(),
              ce.core.employee.count = sum(LOCEMP, na.rm = TRUE),
              ce.core.sales.total = sum(SLSVDT, na.rm = TRUE),
              ce.core.female.owned = mean(FEMOWN),
              ce.core.sole.prop.count = sum(LOCEMP == 1)) %>%
    mutate(cat.core.estab.share = ce.core.estab / sum(ce.core.estab),
           cat.core.employee.share = ce.core.employee.count / sum(ce.core.employee.count),
           cat.core.sales.share = ce.core.sales.total / sum(ce.core.sales.total)) %>%
    dplyr::select(nhood, NAICSD, ce.core.estab, cat.core.estab.share, ce.core.employee.count, cat.core.employee.share,
                  ce.core.sales.total, cat.core.sales.share, ce.core.sole.prop.count, ce.core.female.owned)
  
  # write dataframe to a CSV
  write.csv(summary.dataaxle.ce.core.naics.desc.nhood.df, 
            paste0(tolower(nhood_name), "_ce_NAICSD_breakdown.csv"), row.names = FALSE)
}

####### CSV TABLE: Neighborhood NEFA Core Enterprises list (DataAxle) #######
nhood.creative.group.summary <- function(nhood_name) {
  
  summary.dataaxle.ce.nefa.core.categories.nhood.df <- dataaxle.nefa.core.mapc %>%
    dplyr::filter(nhood == nhood_name) %>%
    dplyr::select(OBJECTID, nhood, LOCEMP, SLSVDT,
                  FEMOWN, CREATIVE_CAT) %>%
    dplyr::group_by(nhood, CREATIVE_CAT) %>%
    mutate(FEMOWN = ifelse(FEMOWN == "Y", 1, 0)) %>%
    summarise(ce.core.estab = n(),
              ce.core.employee.count = sum(LOCEMP, na.rm = TRUE),
              ce.core.sales.total = sum(SLSVDT, na.rm = TRUE),
              ce.core.female.owned = mean(FEMOWN),
              ce.core.sole.prop.count = sum(LOCEMP == 1)) %>%
    mutate(cat.core.estab.share = ce.core.estab / sum(ce.core.estab),
           cat.core.employee.share = ce.core.employee.count / sum(ce.core.employee.count),
           cat.core.sales.share = ce.core.sales.total / sum(ce.core.sales.total)) %>%
    dplyr::select(nhood, CREATIVE_CAT, ce.core.estab, cat.core.estab.share, ce.core.employee.count, cat.core.employee.share,
                  ce.core.sales.total, cat.core.sales.share, ce.core.sole.prop.count, ce.core.female.owned)
  
  # write dataframe to a CSV
  write.csv(summary.dataaxle.ce.nefa.core.categories.nhood.df, 
            paste0(tolower(nhood_name), "_ce_creative_group_breakdown.csv"), row.names = FALSE)
  
}

#################################### CENSUS TRACT #################################### 
####### MAP: NEFA Core Creative Enterprise Dot Map for a designated collection of census tracts (without sole proprietors) #######
ct.map <- function (ct_vector, agg_name) {
  
  establishments.ct <- establishments[establishments$ct20_id %in% ct_vector,]
  establishments.ct.no.soleprop <- establishments.ct[establishments.ct$LOCEMP > 1,]
  ctracts <- mapc.ct[mapc.ct$ct20_id %in% ct_vector,]
  
  ce_distribution_map <- tm_shape(ctracts) + 
    tm_borders("black", lwd = 4) +
    tm_shape(mapc.ct) +
    tm_borders("black") +
    tm_shape(establishments.ct.no.soleprop, size = 1) +
    tm_dots(col = "CREATIVE_CAT", palette = "Paired", border.col = "black", 
            jitter = 0,
            title = paste0(agg_name, ": Creative Industry Group Type"),
            size = 0.1,
            popup.vars = c("Company" = "CONAME", 
                           "Street Address" = "STADDR",
                           "Creative Industry Group" = "CREATIVE_CAT")) +
    tmap_style("white") +
    tm_layout(title = "CONFIDENTIAL - DO NOT SHARE", legend.frame = TRUE) +
    # tm_view(basemaps = "CartoDB.Positron")
    tm_view(basemaps = "Esri.WorldTopoMap")
  
  
  tmap_leaflet(ce_distribution_map, mode = "view", show = TRUE) 
  
  mapshot(tmap_leaflet(ce_distribution_map, mode = "view", show = TRUE), 
          url = paste0(getwd(), "/map_", tolower(agg_name), "_ct_ce.html"), 
          file = paste0(getwd(), "/map_", tolower(agg_name), "_ct_ce.png"), zoom = 3)
}

####### MAP: NEFA Core Creative Enterprise Heat Map for a designated collection of census tracts (without sole proprietors) #######
ct.heatmap <- function (ct_vector, agg_name) {
  
  establishments.ct <- establishments[establishments$ct20_id %in% ct_vector,]
  establishments.ct.no.soleprop <- establishments.ct[establishments.ct@data$LOCEMP > 1,]
  lon <- establishments.ct$LONGITUDE[!is.null(establishments.ct$LONGITUDE)]
  lat <- establishments.ct$LATITUDE[!is.null(establishments.ct$LATITUDE)]
  
  ctracts <- mapc.ct[mapc.ct$ct20_id %in% ct_vector,]
  ct.wgs84 <- spTransform(ctracts, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
  mapc.ct.wgs84 <- spTransform(mapc.ct, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
  
  myMap <- leaflet(establishments.ct) %>% 
    addProviderTiles(providers$Esri.WorldTopoMap, group = "ESRI World Topo Map (default)") %>%
    addProviderTiles(providers$CartoDB.Positron, group = "CartoDB Positron") %>%
    addTiles(group = "OpenStreetMap") %>%
    fitBounds(ct.wgs84@bbox[1,1], ct.wgs84@bbox[2,1],
              ct.wgs84@bbox[1,2], ct.wgs84@bbox[2,2]) %>%
    addPolygons(data = ct.wgs84, weight = 4, color = "black", smoothFactor = 0.3, fill = NA) %>%  
    addPolygons(data = mapc.ct.wgs84, weight = 1, color = "black", smoothFactor = 0.3, fill = NA) %>%
    # addCircles(data = establishments.municipal.no.soleprop, lng = ~lon, lat = ~lat, 
    #            color = "black", weight = 1, fillColor = ~CREATIVE_CAT, group = "Points") %>% # if we want to add dots for each establishment
    addHeatmap(lng = ~lon, lat = ~lat,
               group = "Heat map", blur = 20, max = 1, radius = 10) %>%
    addLayersControl(baseGroups = c("ESRI World Topo Map (default)", "CartoDB Positron", "OpenStreetMap"),
                     # overlayGroups = c("Heat map", "Points"),
                     overlayGroups = "Heat map",
                     options = layersControlOptions(collapsed = FALSE))
  
  mapshot(myMap, 
          url = paste0(getwd(), "/heatmap_", tolower(agg_name), "_ct_ce.html"), 
          file = paste0(getwd(), "/heatmap_", tolower(agg_name), "_ct_ce.png"), zoom = 3)
}

####### CSV TABLE: Census tract summary data comparison #######
ct.table <- function(ct_vector, agg_name, municipality) {
  # create dataset with decimals for graphing
  
  ct.data <- summary.dataaxle.ce.final %>%
    dplyr::filter(geography %in% ct_vector) %>%
    select(-c(share.all.mapc, ce.share.all.mapc, share.all.employee.mapc, ce.share.employee.all.mapc, ce.share.core.mapc, ce.share.employee.core.mapc)) %>%
    mutate(ce.core.sales.total = ifelse(is.na(ce.core.sales.total), 0, ce.core.sales.total),
           ce.core.sole.prop.count = ifelse(is.na(ce.core.sole.prop.count), 0, ce.core.sole.prop.count),
           Architecture.and.Design = ifelse(ce.core.estab == 0, 0, Architecture.and.Design),
           Art.related.Retail = ifelse(ce.core.estab == 0, 0, Art.related.Retail),
           Arts.and.architectural.manufacturing = ifelse(ce.core.estab == 0, 0, Arts.and.architectural.manufacturing),
           Culture.and.Preservation = ifelse(ce.core.estab == 0, 0, Culture.and.Preservation),
           Machinery.and.communications.manufacturing = ifelse(ce.core.estab == 0, 0, Machinery.and.communications.manufacturing),
           Marketing = ifelse(ce.core.estab == 0, 0, Marketing),
           Materials.manufacturing = ifelse(ce.core.estab == 0, 0, Materials.manufacturing),
           Media = ifelse(ce.core.estab == 0, 0, Media),
           Motion.picture.and.teleproduction = ifelse(ce.core.estab == 0, 0, Motion.picture.and.teleproduction),
           Music.Recording = ifelse(ce.core.estab == 0, 0, Music.Recording),
           Printing = ifelse(ce.core.estab == 0, 0, Printing),
           Publishing = ifelse(ce.core.estab == 0, 0, Publishing),
           Visual.Arts..Music.and.Other.Performing.Arts = ifelse(ce.core.estab == 0, 0, Visual.Arts..Music.and.Other.Performing.Arts),
           Wholesale.art.stores = ifelse(ce.core.estab == 0, 0, Wholesale.art.stores))
  
  
  ## construct creative economy category summaries (e.g., Arts-related retail) for the neighborhood aggregation of block groups
  by.agg.dataaxle.ct = dataaxle.nefa.core.mapc %>%
    dplyr::select(ct20_id, CREATIVE_CAT) %>%
    mutate("indicator" = rep(1, nrow(dataaxle.nefa.core.mapc))) %>%
    dplyr::filter(ct20_id %in% ct_vector) %>%
    dplyr::group_by(CREATIVE_CAT) %>%
    summarise("estab" = sum(indicator)) %>%
    mutate(geography = agg_name,
           "share" = estab / sum(estab),
           "percent" = percent(estab / sum(estab))) %>%
    dplyr::select(geography, CREATIVE_CAT, estab, share, percent)
  
  # number of each creative industry
  by.agg.dataaxle.estab <- by.agg.dataaxle.ct %>%
    dplyr::select(geography, CREATIVE_CAT, estab) %>%
    spread(key = CREATIVE_CAT, value = estab)
  
  missing.cat.test.estab = unique(dataaxle.nefa.core.mapc$CREATIVE_CAT) %in% names(by.agg.dataaxle.estab)
  missing.cat.estab = unique(dataaxle.nefa.core.mapc$CREATIVE_CAT)[which(missing.cat.test.estab == FALSE)]
  
  by.agg.dataaxle.estab[missing.cat.estab] = NA # adds missing categories as new columns with NA values
  
  by.agg.dataaxle.estab[is.na(by.agg.dataaxle.estab)] <- 0 # turns all NA values to 0
  
  
  # share of each creative industry amongst all creative industries
  by.agg.dataaxle.share <- by.agg.dataaxle.ct %>%
    dplyr::select(geography, CREATIVE_CAT, share) %>%
    spread(key = CREATIVE_CAT, value = share)
  
  missing.cat.test.share = unique(dataaxle.nefa.core.mapc$CREATIVE_CAT) %in% names(by.agg.dataaxle.share)
  missing.cat.share = unique(dataaxle.nefa.core.mapc$CREATIVE_CAT)[which(missing.cat.test.share == FALSE)]
  
  by.agg.dataaxle.share[missing.cat.share] = NA # adds missing categories as new columns with NA values
  
  by.agg.dataaxle.share[is.na(by.agg.dataaxle.share)] <- 0 # turns all NA values to 0
  
  agg.data <- ct.data %>%
    summarise(all.estab = sum(all.estab),
              ce.all.estab = sum(ce.all.estab),
              all.employee.count = sum(all.employee.count),
              ce.all.employee.count = sum(ce.all.employee.count),
              ce.all.sales.total = sum(ce.all.sales.total),
              ce.all.sole.prop.count = sum(ce.all.sole.prop.count),
              ce.core.estab = sum(ce.core.estab),
              ce.core.employee.count = sum(ce.core.employee.count),
              ce.core.sales.total = sum(ce.core.sales.total),                         
              ce.core.sole.prop.count = sum(ce.core.sole.prop.count)) %>%
    mutate(ce.core.estab = ifelse(is.na(ce.core.estab), 0, ce.core.estab),
           ce.core.employee.count  = ifelse(is.na(ce.core.employee.count ), 0, ce.core.employee.count),
           ce.core.sales.total = ifelse(is.na(ce.core.sales.total), 0, ce.core.sales.total),
           ce.core.sole.prop.count = ifelse(is.na(ce.core.sole.prop.count), 0, ce.core.sole.prop.count)) %>%
    mutate(geography = agg_name,
           share.all.mapc = round((all.estab / sum(all.estab))*100, digits = 2),
           ce.share.all.municipality = round((ce.all.estab / all.estab)*100, digits = 2),
           ce.share.all.mapc = round((ce.all.estab / sum(ce.all.estab))*100, digits = 2),
           share.all.employee.mapc = round((all.employee.count / sum(all.employee.count))*100, digits = 2),
           ce.share.employee.all.municipality = round((ce.all.employee.count / all.employee.count)*100, digits = 2),
           ce.share.employee.all.mapc = round((ce.all.employee.count / sum(ce.all.employee.count))*100, digits = 2),                   
           ce.share.core.municipality = round((ce.core.estab / all.estab)*100, digits = 2),
           ce.share.core.mapc = round((ce.core.estab / sum(ce.core.estab))*100, digits = 2),                          
           ce.share.employee.core.municipality = round((ce.core.employee.count / all.employee.count)*100, digits = 2),
           ce.share.employee.core.mapc = round((ce.core.employee.count / sum(ce.core.employee.count))*100, digits = 2)) %>%
    left_join(by.agg.dataaxle.share, by = "geography") %>%
    dplyr::select(geography,
                  all.estab,                                    
                  share.all.mapc,                               
                  ce.all.estab,                                
                  "ce.share.all.geography" = ce.share.all.municipality,                    
                  ce.share.all.mapc,                            
                  all.employee.count,                           
                  share.all.employee.mapc,                     
                  ce.all.employee.count,                        
                  "ce.share.employee.all.geography" = ce.share.employee.all.municipality,           
                  ce.share.employee.all.mapc,                   
                  ce.all.sales.total,                          
                  ce.all.sole.prop.count,                       
                  ce.core.estab,                                
                  "ce.share.core.geography" = ce.share.core.municipality,                   
                  ce.share.core.mapc,                          
                  ce.core.employee.count,                       
                  "ce.share.employee.core.geography" = ce.share.employee.core.municipality,          
                  ce.share.employee.core.mapc,                  
                  ce.core.sales.total,                         
                  ce.core.sole.prop.count,
                  Architecture.and.Design = "Architecture and Design", 
                  Art.related.Retail = "Art-related Retail",
                  Arts.and.architectural.manufacturing = "Arts and architectural manufacturing",
                  Culture.and.Preservation = "Culture and Preservation",
                  Machinery.and.communications.manufacturing = "Machinery and communications manufacturing",
                  Marketing = "Marketing",
                  Materials.manufacturing = "Materials manufacturing",
                  Media = "Media",
                  Motion.picture.and.teleproduction = "Motion picture and teleproduction",
                  Music.Recording = "Music Recording", 
                  Printing = "Printing",
                  Publishing = "Publishing",
                  Visual.Arts..Music.and.Other.Performing.Arts = "Visual Arts, Music and Other Performing Arts",
                  Wholesale.art.stores = "Wholesale art stores") %>%
    mutate(Architecture.and.Design = ifelse(ce.core.estab == 0, 0, Architecture.and.Design),
           Art.related.Retail = ifelse(ce.core.estab == 0, 0, Art.related.Retail),
           Arts.and.architectural.manufacturing = ifelse(ce.core.estab == 0, 0, Arts.and.architectural.manufacturing),
           Culture.and.Preservation = ifelse(ce.core.estab == 0, 0, Culture.and.Preservation),
           Machinery.and.communications.manufacturing = ifelse(ce.core.estab == 0, 0, Machinery.and.communications.manufacturing),
           Marketing = ifelse(ce.core.estab == 0, 0, Marketing),
           Materials.manufacturing = ifelse(ce.core.estab == 0, 0, Materials.manufacturing),
           Media = ifelse(ce.core.estab == 0, 0, Media),
           Motion.picture.and.teleproduction = ifelse(ce.core.estab == 0, 0, Motion.picture.and.teleproduction),
           Music.Recording = ifelse(ce.core.estab == 0, 0, Music.Recording),
           Printing = ifelse(ce.core.estab == 0, 0, Printing),
           Publishing = ifelse(ce.core.estab == 0, 0, Publishing),
           Visual.Arts..Music.and.Other.Performing.Arts = ifelse(ce.core.estab == 0, 0, Visual.Arts..Music.and.Other.Performing.Arts),
           Wholesale.art.stores = ifelse(ce.core.estab == 0, 0, Wholesale.art.stores)) %>%
    select(-c(share.all.mapc, ce.share.all.mapc, share.all.employee.mapc, ce.share.employee.all.mapc, ce.share.core.mapc, ce.share.employee.core.mapc))
  
  
  # get the names of neighborhoods from the municipality of choice
  nhood_names = mapc.nhood@data$nhood_name[mapc.nhood@data$municipal == municipality]
  
  # get the neighborhood summary data for the desired neighborhoods
  nhood.data = summary.dataaxle.ce.final %>%
    dplyr::filter(geography %in% nhood_names)  %>%
    select(-c(share.all.mapc, ce.share.all.mapc, share.all.employee.mapc, ce.share.employee.all.mapc, ce.share.core.mapc, ce.share.employee.core.mapc))
  
  
  muni.data <- summary.dataaxle.ce.final %>%
    dplyr::filter(geography == municipality)%>%
    select(-c(share.all.mapc, ce.share.all.mapc, share.all.employee.mapc, ce.share.employee.all.mapc, ce.share.core.mapc, ce.share.employee.core.mapc))
  
  
  data.output <- rbind(ct.data,
                       agg.data,
                       nhood.data,
                       muni.data) 
  
  data.output$id <- seq.int(nrow(data.output))
  
  
  # Create a blank workbook
  output.xlsx <- createWorkbook()
  
  # Add some sheets to the workbook
  addWorksheet(output.xlsx, paste0(tolower(agg_name), "_ct_ce_data"))
  #addWorksheet(output.xlsx, "metadata")
  
  # Write the data to the sheets
  writeData(output.xlsx, sheet = paste0(tolower(agg_name), "_ct_ce_data"), x = data.output)
  #writeData(output.xlsx, sheet = "metadata", x = ce.summary.metadata)
  
  # Export the file
  saveWorkbook(output.xlsx, paste0(tolower(agg_name), "_ct_ce_data.xlsx"))
  return(agg.data)
}

####### CSV TABLE: Census tract NEFA Core Enterprises list (DataAxle) #######
ct.dataaxle.ce.core.list <- function(ct_vector, agg_name) {
  ct.ce.core.list <- dataaxle.nefa.core.mapc %>%
    dplyr::filter(ct20_id %in% ct_vector)
  
  write.csv(ct.ce.core.list, paste0(tolower(agg_name), "_ct_ce_establishment_list.csv"), row.names = FALSE)
  
  summary.dataaxle.ce.core.naics.desc.ct.df <- ct.ce.core.list %>%
    dplyr::filter(ct20_id %in% ct_vector) %>%
    dplyr::select(OBJECTID, ct20_id, NAICSD, LOCEMP, SLSVDT,
                  FEMOWN, CREATIVE_CAT) %>%
    dplyr::group_by(ct20_id, NAICSD) %>%
    mutate(FEMOWN = ifelse(FEMOWN == "Y", 1, 0)) %>%
    summarise(ce.core.estab = n(),
              ce.core.employee.count = sum(LOCEMP, na.rm = TRUE),
              ce.core.sales.total = sum(SLSVDT, na.rm = TRUE),
              ce.core.female.owned = mean(FEMOWN),
              ce.core.sole.prop.count = sum(LOCEMP == 1)) %>%
    mutate(cat.core.estab.share = ce.core.estab / sum(ce.core.estab),
           cat.core.employee.share = ce.core.employee.count / sum(ce.core.employee.count),
           cat.core.sales.share = ce.core.sales.total / sum(ce.core.sales.total)) %>%
    dplyr::select(ct20_id, NAICSD, ce.core.estab, cat.core.estab.share, ce.core.employee.count, cat.core.employee.share,
                  ce.core.sales.total, cat.core.sales.share, ce.core.sole.prop.count, ce.core.female.owned)
  
  # write dataframe to a CSV
  write.csv(summary.dataaxle.ce.core.naics.desc.ct.df, 
            paste0(tolower(agg_name), "_ct_ce_NAICSD_breakdown.csv"), row.names = FALSE)
}

####### CSV TABLE: Census tract NEFA Core Enterprises list (DataAxle) #######
ct.creative.group.summary <- function(ct_vector, agg_name) {
  
  summary.dataaxle.ce.nefa.core.categories.ct.df <- dataaxle.nefa.core.mapc %>%
    dplyr::filter(ct20_id %in% ct_vector) %>%
    dplyr::select(OBJECTID, ct20_id, LOCEMP, SLSVDT,
                  FEMOWN, CREATIVE_CAT) %>%
    dplyr::group_by(ct20_id, CREATIVE_CAT) %>%
    mutate(FEMOWN = ifelse(FEMOWN == "Y", 1, 0)) %>%
    summarise(ce.core.estab = n(),
              ce.core.employee.count = sum(LOCEMP, na.rm = TRUE),
              ce.core.sales.total = sum(SLSVDT, na.rm = TRUE),
              ce.core.female.owned = mean(FEMOWN),
              ce.core.sole.prop.count = sum(LOCEMP == 1)) %>%
    mutate(cat.core.estab.share = ce.core.estab / sum(ce.core.estab),
           cat.core.employee.share = ce.core.employee.count / sum(ce.core.employee.count),
           cat.core.sales.share = ce.core.sales.total / sum(ce.core.sales.total)) %>%
    dplyr::select(ct20_id, CREATIVE_CAT, ce.core.estab, cat.core.estab.share, ce.core.employee.count, cat.core.employee.share,
                  ce.core.sales.total, cat.core.sales.share, ce.core.sole.prop.count, ce.core.female.owned)
  
  # write dataframe to a CSV
  write.csv(summary.dataaxle.ce.nefa.core.categories.ct.df, 
            paste0(tolower(agg_name), "_ct_ce_creative_group_breakdown.csv"), row.names = FALSE)
}

#################################### BLOCK GROUP #################################### 
####### MAP: NEFA Core Creative Enterprise Dot Map for a designated collection of block groups (without sole proprietors) #######
bg.map <- function (bg_vector, agg_name) {
  
  establishments.bg <- establishments[establishments$bg20_id %in% bg_vector,]
  establishments.bg.no.soleprop <- establishments.bg[establishments.bg$LOCEMP > 1,]
  bgroups <- mapc.bg[mapc.bg$bg20_id %in% bg_vector,]
  
  ce_distribution_map <- tm_shape(bgroups) + 
    tm_borders("black", lwd = 4) +
    tm_shape(mapc.bg) +
    tm_borders("black") +
    tm_shape(establishments.bg.no.soleprop, size = 1) +
    tm_dots(col = "CREATIVE_CAT", palette = "Paired", border.col = "black", 
            jitter = 0,
            title = paste0(agg_name, ": Creative Industry Group Type"),
            size = 0.1,
            popup.vars = c("Company" = "CONAME", 
                           "Street Address" = "STADDR",
                           "Creative Industry Group" = "CREATIVE_CAT")) +
    tmap_style("white") +
    tm_layout(title = "CONFIDENTIAL - DO NOT SHARE", legend.frame = TRUE) +
    # tm_view(basemaps = "CartoDB.Positron")
    tm_view(basemaps = "Esri.WorldTopoMap")
  
  
  tmap_leaflet(ce_distribution_map, mode = "view", show = TRUE) 
  
  mapshot(tmap_leaflet(ce_distribution_map, mode = "view", show = TRUE), 
          url = paste0(getwd(), "/map_", tolower(agg_name), "_bg_ce.html"), 
          file = paste0(getwd(), "/map_", tolower(agg_name), "_bg_ce.png"), zoom = 3)
}

####### MAP: NEFA Core Creative Enterprise Heat Map for a designated collection of block groups (without sole proprietors) #######
bg.heatmap <- function (bg_vector, agg_name) {
  
  establishments.bg <- establishments[establishments$bg20_id %in% bg_vector,]
  establishments.bg.no.soleprop <- establishments.bg[establishments.bg@data$LOCEMP > 1,]
  lon <- establishments.bg$LONGITUDE[!is.null(establishments.bg$LONGITUDE)]
  lat <- establishments.bg$LATITUDE[!is.null(establishments.bg$LATITUDE)]
  
  bgroups <- mapc.bg[mapc.bg@data$bg20_id %in% bg_vector,]
  bgroups.wgs84 <- spTransform(bgroups, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
  mapc.bg.wgs84 <- spTransform(mapc.bg, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
  
  myMap <- leaflet(establishments.bg) %>% 
    addProviderTiles(providers$Esri.WorldTopoMap, group = "ESRI World Topo Map (default)") %>%
    addProviderTiles(providers$CartoDB.Positron, group = "CartoDB Positron") %>%
    addTiles(group = "OpenStreetMap") %>%
    fitBounds(bgroups.wgs84@bbox[1,1], bgroups.wgs84@bbox[2,1],
              bgroups.wgs84@bbox[1,2], bgroups.wgs84@bbox[2,2]) %>%
    addPolygons(data = bgroups.wgs84, weight = 4, color = "black", smoothFactor = 0.3, fill = NA) %>%  
    addPolygons(data = mapc.bg.wgs84, weight = 1, color = "black", smoothFactor = 0.3, fill = NA) %>%
    # addCircles(data = establishments.municipal.no.soleprop, lng = ~lon, lat = ~lat, 
    #            color = "black", weight = 1, fillColor = ~CREATIVE_CAT, group = "Points") %>% # if we want to add dots for each establishment
    addHeatmap(lng = ~lon, lat = ~lat,
               group = "Heat map", blur = 20, max = 1, radius = 10) %>%
    addLayersControl(baseGroups = c("ESRI World Topo Map (default)", "CartoDB Positron", "OpenStreetMap"),
                     # overlayGroups = c("Heat map", "Points"),
                     overlayGroups = "Heat map",
                     options = layersControlOptions(collapsed = FALSE))
  
  mapshot(myMap, 
          url = paste0(getwd(), "/heatmap_", tolower(agg_name), "_bg_ce.html"), 
          file = paste0(getwd(), "/heatmap_", tolower(agg_name), "_bg_ce.png"), zoom = 3)
}

####### CSV TABLE: Block group summary data comparison #######
bg.table <- function(bg_vector, agg_name, municipality) {
  # create dataset with decimals for graphing
  
  bg.data <- summary.dataaxle.ce.final %>%
    dplyr::filter(geography %in% bg_vector) %>%
    select(-c(share.all.mapc, ce.share.all.mapc, share.all.employee.mapc, ce.share.employee.all.mapc, ce.share.core.mapc, ce.share.employee.core.mapc)) %>%
    mutate(ce.core.sales.total = ifelse(is.na(ce.core.sales.total), 0, ce.core.sales.total),
           ce.core.sole.prop.count = ifelse(is.na(ce.core.sole.prop.count), 0, ce.core.sole.prop.count),
           Architecture.and.Design = ifelse(ce.core.estab == 0, 0, Architecture.and.Design),
           Art.related.Retail = ifelse(ce.core.estab == 0, 0, Art.related.Retail),
           Arts.and.architectural.manufacturing = ifelse(ce.core.estab == 0, 0, Arts.and.architectural.manufacturing),
           Culture.and.Preservation = ifelse(ce.core.estab == 0, 0, Culture.and.Preservation),
           Machinery.and.communications.manufacturing = ifelse(ce.core.estab == 0, 0, Machinery.and.communications.manufacturing),
           Marketing = ifelse(ce.core.estab == 0, 0, Marketing),
           Materials.manufacturing = ifelse(ce.core.estab == 0, 0, Materials.manufacturing),
           Media = ifelse(ce.core.estab == 0, 0, Media),
           Motion.picture.and.teleproduction = ifelse(ce.core.estab == 0, 0, Motion.picture.and.teleproduction),
           Music.Recording = ifelse(ce.core.estab == 0, 0, Music.Recording),
           Printing = ifelse(ce.core.estab == 0, 0, Printing),
           Publishing = ifelse(ce.core.estab == 0, 0, Publishing),
           Visual.Arts..Music.and.Other.Performing.Arts = ifelse(ce.core.estab == 0, 0, Visual.Arts..Music.and.Other.Performing.Arts),
           Wholesale.art.stores = ifelse(ce.core.estab == 0, 0, Wholesale.art.stores))
  
  
  ## construct creative economy category summaries (e.g., Arts-related retail) for the neighborhood aggregation of block groups
  by.agg.dataaxle.bg = dataaxle.nefa.core.mapc %>%
    dplyr::select(bg20_id, CREATIVE_CAT) %>%
    mutate("indicator" = rep(1, nrow(dataaxle.nefa.core.mapc))) %>%
    dplyr::filter(bg20_id %in% bg_vector) %>%
    dplyr::group_by(CREATIVE_CAT) %>%
    summarise("estab" = sum(indicator)) %>%
    mutate(geography = agg_name,
           "share" = estab / sum(estab),
           "percent" = percent(estab / sum(estab))) %>%
    dplyr::select(geography, CREATIVE_CAT, estab, share, percent)
  
  # number of each creative industry
  by.agg.dataaxle.estab <- by.agg.dataaxle.bg %>%
    dplyr::select(geography, CREATIVE_CAT, estab) %>%
    spread(key = CREATIVE_CAT, value = estab)
  
  missing.cat.test.estab = unique(dataaxle.nefa.core.mapc$CREATIVE_CAT) %in% names(by.agg.dataaxle.estab)
  missing.cat.estab = unique(dataaxle.nefa.core.mapc$CREATIVE_CAT)[which(missing.cat.test.estab == FALSE)]
  
  by.agg.dataaxle.estab[missing.cat.estab] = NA # adds missing categories as new columns with NA values
  
  by.agg.dataaxle.estab[is.na(by.agg.dataaxle.estab)] <- 0 # turns all NA values to 0
  
  
  # share of each creative industry amongst all creative industries
  by.agg.dataaxle.share <- by.agg.dataaxle.bg %>%
    dplyr::select(geography, CREATIVE_CAT, share) %>%
    spread(key = CREATIVE_CAT, value = share)
  
  missing.cat.test.share = unique(dataaxle.nefa.core.mapc$CREATIVE_CAT) %in% names(by.agg.dataaxle.share)
  missing.cat.share = unique(dataaxle.nefa.core.mapc$CREATIVE_CAT)[which(missing.cat.test.share == FALSE)]
  
  by.agg.dataaxle.share[missing.cat.share] = NA # adds missing categories as new columns with NA values
  
  by.agg.dataaxle.share[is.na(by.agg.dataaxle.share)] <- 0 # turns all NA values to 0
  
  agg.data <- bg.data %>%
    summarise(all.estab = sum(all.estab),
              ce.all.estab = sum(ce.all.estab),
              all.employee.count = sum(all.employee.count),
              ce.all.employee.count = sum(ce.all.employee.count),
              ce.all.sales.total = sum(ce.all.sales.total),
              ce.all.sole.prop.count = sum(ce.all.sole.prop.count),
              ce.core.estab = sum(ce.core.estab),
              ce.core.employee.count = sum(ce.core.employee.count),
              ce.core.sales.total = sum(ce.core.sales.total),                         
              ce.core.sole.prop.count = sum(ce.core.sole.prop.count)) %>%
    mutate(ce.core.estab = ifelse(is.na(ce.core.estab), 0, ce.core.estab),
           ce.core.employee.count  = ifelse(is.na(ce.core.employee.count ), 0, ce.core.employee.count),
           ce.core.sales.total = ifelse(is.na(ce.core.sales.total), 0, ce.core.sales.total),
           ce.core.sole.prop.count = ifelse(is.na(ce.core.sole.prop.count), 0, ce.core.sole.prop.count)) %>%
    mutate(geography = agg_name,
           share.all.mapc = round((all.estab / sum(all.estab))*100, digits = 2),
           ce.share.all.municipality = round((ce.all.estab / all.estab)*100, digits = 2),
           ce.share.all.mapc = round((ce.all.estab / sum(ce.all.estab))*100, digits = 2),
           share.all.employee.mapc = round((all.employee.count / sum(all.employee.count))*100, digits = 2),
           ce.share.employee.all.municipality = round((ce.all.employee.count / all.employee.count)*100, digits = 2),
           ce.share.employee.all.mapc = round((ce.all.employee.count / sum(ce.all.employee.count))*100, digits = 2),                   
           ce.share.core.municipality = round((ce.core.estab / all.estab)*100, digits = 2),
           ce.share.core.mapc = round((ce.core.estab / sum(ce.core.estab))*100, digits = 2),                          
           ce.share.employee.core.municipality = round((ce.core.employee.count / all.employee.count)*100, digits = 2),
           ce.share.employee.core.mapc = round((ce.core.employee.count / sum(ce.core.employee.count))*100, digits = 2)) %>%
    left_join(by.agg.dataaxle.share, by = "geography") %>%
    dplyr::select(geography,
                  all.estab,                                    
                  share.all.mapc,                               
                  ce.all.estab,                                
                  "ce.share.all.geography" = ce.share.all.municipality,                    
                  ce.share.all.mapc,                            
                  all.employee.count,                           
                  share.all.employee.mapc,                     
                  ce.all.employee.count,                        
                  "ce.share.employee.all.geography" = ce.share.employee.all.municipality,           
                  ce.share.employee.all.mapc,                   
                  ce.all.sales.total,                          
                  ce.all.sole.prop.count,                       
                  ce.core.estab,                                
                  "ce.share.core.geography" = ce.share.core.municipality,                   
                  ce.share.core.mapc,                          
                  ce.core.employee.count,                       
                  "ce.share.employee.core.geography" = ce.share.employee.core.municipality,          
                  ce.share.employee.core.mapc,                  
                  ce.core.sales.total,                         
                  ce.core.sole.prop.count,
                  Architecture.and.Design = "Architecture and Design", 
                  Art.related.Retail = "Art-related Retail",
                  Arts.and.architectural.manufacturing = "Arts and architectural manufacturing",
                  Culture.and.Preservation = "Culture and Preservation",
                  Machinery.and.communications.manufacturing = "Machinery and communications manufacturing",
                  Marketing = "Marketing",
                  Materials.manufacturing = "Materials manufacturing",
                  Media = "Media",
                  Motion.picture.and.teleproduction = "Motion picture and teleproduction",
                  Music.Recording = "Music Recording", 
                  Printing = "Printing",
                  Publishing = "Publishing",
                  Visual.Arts..Music.and.Other.Performing.Arts = "Visual Arts, Music and Other Performing Arts",
                  Wholesale.art.stores = "Wholesale art stores") %>%
    mutate(Architecture.and.Design = ifelse(ce.core.estab == 0, 0, Architecture.and.Design),
           Art.related.Retail = ifelse(ce.core.estab == 0, 0, Art.related.Retail),
           Arts.and.architectural.manufacturing = ifelse(ce.core.estab == 0, 0, Arts.and.architectural.manufacturing),
           Culture.and.Preservation = ifelse(ce.core.estab == 0, 0, Culture.and.Preservation),
           Machinery.and.communications.manufacturing = ifelse(ce.core.estab == 0, 0, Machinery.and.communications.manufacturing),
           Marketing = ifelse(ce.core.estab == 0, 0, Marketing),
           Materials.manufacturing = ifelse(ce.core.estab == 0, 0, Materials.manufacturing),
           Media = ifelse(ce.core.estab == 0, 0, Media),
           Motion.picture.and.teleproduction = ifelse(ce.core.estab == 0, 0, Motion.picture.and.teleproduction),
           Music.Recording = ifelse(ce.core.estab == 0, 0, Music.Recording),
           Printing = ifelse(ce.core.estab == 0, 0, Printing),
           Publishing = ifelse(ce.core.estab == 0, 0, Publishing),
           Visual.Arts..Music.and.Other.Performing.Arts = ifelse(ce.core.estab == 0, 0, Visual.Arts..Music.and.Other.Performing.Arts),
           Wholesale.art.stores = ifelse(ce.core.estab == 0, 0, Wholesale.art.stores)) %>%
    select(-c(share.all.mapc, ce.share.all.mapc, share.all.employee.mapc, ce.share.employee.all.mapc, ce.share.core.mapc, ce.share.employee.core.mapc))
  
  
  # get the names of neighborhoods from the municipality of choice
  nhood_names = mapc.nhood@data$nhood_name[mapc.nhood@data$municipal == municipality]
  
  # get the neighborhood summary data for the desired neighborhoods
  nhood.data = summary.dataaxle.ce.final %>%
    dplyr::filter(geography %in% nhood_names)  %>%
    select(-c(share.all.mapc, ce.share.all.mapc, share.all.employee.mapc, ce.share.employee.all.mapc, ce.share.core.mapc, ce.share.employee.core.mapc))
  
  
  muni.data <- summary.dataaxle.ce.final %>%
    dplyr::filter(geography == municipality) %>%
    select(-c(share.all.mapc, ce.share.all.mapc, share.all.employee.mapc, ce.share.employee.all.mapc, ce.share.core.mapc, ce.share.employee.core.mapc))
  
  
  data.output <- rbind(bg.data,
                       agg.data,
                       nhood.data,
                       muni.data) 
  
  data.output$id <- seq.int(nrow(data.output))
  
  
  # Create a blank workbook
  output.xlsx <- createWorkbook()
  
  # Add some sheets to the workbook
  addWorksheet(output.xlsx, paste0(tolower(agg_name), "_bg_ce_data"))
  #addWorksheet(output.xlsx, "metadata")
  
  # Write the data to the sheets
  writeData(output.xlsx, sheet = paste0(tolower(agg_name), "_bg_ce_data"), x = data.output)
  #writeData(output.xlsx, sheet = "metadata", x = ce.summary.metadata)
  
  # Export the file
  saveWorkbook(output.xlsx, paste0(tolower(agg_name), "_bg_ce_data.xlsx"))
  return(agg.data)
}

####### CSV TABLE: Block Group NEFA Core Enterprises list (DataAxle) #######
bg.dataaxle.ce.core.list <- function(bg_vector, agg_name) {
  bg.ce.core.list <- dataaxle.nefa.core.mapc %>%
    dplyr::filter(bg20_id %in% bg_vector)
  
  write.csv(bg.ce.core.list, paste0(tolower(agg_name), "_bg_ce_establishment_list.csv"), row.names = FALSE)
  
  summary.dataaxle.ce.core.naics.desc.bg.df <- bg.ce.core.list %>%
    dplyr::filter(bg20_id %in% bg_vector) %>%
    dplyr::select(OBJECTID, bg20_id, NAICSD, LOCEMP, SLSVDT,
                  FEMOWN, CREATIVE_CAT) %>%
    dplyr::group_by(bg20_id, NAICSD) %>%
    mutate(FEMOWN = ifelse(FEMOWN == "Y", 1, 0)) %>%
    summarise(ce.core.estab = n(),
              ce.core.employee.count = sum(LOCEMP, na.rm = TRUE),
              ce.core.sales.total = sum(SLSVDT, na.rm = TRUE),
              ce.core.female.owned = mean(FEMOWN),
              ce.core.sole.prop.count = sum(LOCEMP == 1)) %>%
    mutate(cat.core.estab.share = ce.core.estab / sum(ce.core.estab),
           cat.core.employee.share = ce.core.employee.count / sum(ce.core.employee.count),
           cat.core.sales.share = ce.core.sales.total / sum(ce.core.sales.total)) %>%
    dplyr::select(bg20_id, NAICSD, ce.core.estab, cat.core.estab.share, ce.core.employee.count, cat.core.employee.share,
                  ce.core.sales.total, cat.core.sales.share, ce.core.sole.prop.count, ce.core.female.owned)
  
  # write dataframe to a CSV
  write.csv(summary.dataaxle.ce.core.naics.desc.bg.df, 
            paste0(tolower(agg_name), "_bg_ce_NAICSD_breakdown.csv"), row.names = FALSE)
  
}

####### CSV TABLE: Block group NEFA Core Enterprises list (DataAxle) #######
bg.creative.group.summary <- function(bg_vector, agg_name) {
  
  summary.dataaxle.ce.nefa.core.categories.bg.df <- dataaxle.nefa.core.mapc %>%
    dplyr::filter(bg20_id %in% bg_vector) %>%
    dplyr::select(OBJECTID, bg20_id, LOCEMP, SLSVDT,
                  FEMOWN, CREATIVE_CAT) %>%
    dplyr::group_by(bg20_id, CREATIVE_CAT) %>%
    mutate(FEMOWN = ifelse(FEMOWN == "Y", 1, 0)) %>%
    summarise(ce.core.estab = n(),
              ce.core.employee.count = sum(LOCEMP, na.rm = TRUE),
              ce.core.sales.total = sum(SLSVDT, na.rm = TRUE),
              ce.core.female.owned = mean(FEMOWN),
              ce.core.sole.prop.count = sum(LOCEMP == 1)) %>%
    mutate(cat.core.estab.share = ce.core.estab / sum(ce.core.estab),
           cat.core.employee.share = ce.core.employee.count / sum(ce.core.employee.count),
           cat.core.sales.share = ce.core.sales.total / sum(ce.core.sales.total)) %>%
    dplyr::select(bg20_id, CREATIVE_CAT, ce.core.estab, cat.core.estab.share, ce.core.employee.count, cat.core.employee.share,
                  ce.core.sales.total, cat.core.sales.share, ce.core.sole.prop.count, ce.core.female.owned)
  
  # write dataframe to a CSV
  write.csv(summary.dataaxle.ce.nefa.core.categories.bg.df, 
            paste0(tolower(agg_name), "_bg_ce_creative_group_breakdown.csv"), row.names = FALSE)
  
}

####### FINAL OUTPUT FUNCTION: Map, summary data table, and core establishment table #######
municipal.output <- function(municipality) {
  
  dir.create(file.path(output.path, municipality), showWarnings = TRUE)
  setwd(file.path(output.path, municipality))
  
  municipal.table (municipality)
  municipal.dataaxle.ce.core.list (municipality)
  municipal.map(municipality)
  municipal.heatmap(municipality)
  municipal.creative.group.summary (municipality)
  
}

nhood.output <- function(nhood_name, municipality) {
  
  dir.create(file.path(output.path, nhood_name), showWarnings = TRUE)
  setwd(file.path(output.path, nhood_name))
  
  nhood.table (nhood_name, municipality)
  nhood.dataaxle.ce.core.list (nhood_name)
  nhood.map(nhood_name)
  nhood.heatmap(nhood_name)
  nhood.creative.group.summary (nhood_name)
  
}

ct.output <- function(ct_vector, agg_name, municipality) {
  
  dir.create(file.path(output.path, agg_name), showWarnings = TRUE)
  setwd(file.path(output.path, agg_name))
  
  ct.table (ct_vector, agg_name, municipality)
  ct.dataaxle.ce.core.list (ct_vector, agg_name)
  ct.map(ct_vector, agg_name)
  ct.heatmap(ct_vector, agg_name)
  ct.creative.group.summary (ct_vector, agg_name)
  
}

bg.output <- function(bg_vector, agg_name, municipality) {
  
  dir.create(file.path(output.path, agg_name), showWarnings = TRUE)
  setwd(file.path(output.path, agg_name))
  
  bg.table (bg_vector, agg_name, municipality)
  bg.dataaxle.ce.core.list (bg_vector, agg_name)
  bg.map(bg_vector, agg_name)
  bg.heatmap(bg_vector, agg_name)
  bg.creative.group.summary (bg_vector, agg_name)
  
}

