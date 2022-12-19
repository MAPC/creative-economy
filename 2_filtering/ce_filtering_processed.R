# Script: InfoUSA data filtering and creative economy category assignment
# Author: Seleeke Flingai
# Date Last Modified: December 19, 2022 by RBowers

install.packages('pacman')
pacman::p_load(tidyr, dplyr, reshape2)

modified.data.path <- "K:/DataServices/Projects/Current_Projects/Arts_and_Culture_Planning/Creative_Economy/Data/Modified"
naics.path <- "K:/DataServices/Projects/Current_Projects/Arts_and_Culture_Planning/Creative_Economy/NAICS/Converted NAICS CE code tables/Conversions to 2012"

# load InfoUSA data for Massachusetts and its municipalities (2016)
setwd(modified.data.path)
infousa.mass <- read.csv("2022_dataaxle_mass_processed.csv", stringsAsFactors = FALSE)

####### DATA WRANGLING: ASSIGN NEFA CREATIVE CATEGORIES TO EACH ESTABLISHMENT #######
# From Appendix D (NEFA Core Industries by Creative Category) of 2017 NEFA Report "The Jobs in New England's Creative Economy and Why They Matter"
# Creation of industry groups by six digit NAICS code (assign to establishments with ifelse function)
arch.and.design <- c('541310', '541320', '541340', '541410', '541420', '541430', '541490')
#art.electronic.retail <- c('443142', '448310', '451130', '451140', '451211', '453920', '812921', '812922')
art.retail <- c('448310', '451130', '451140', '451211', '453920', '812921', '812922') # no electronics-related retail
art.arch.manu <- c('332323', '337212', '339910', '339992')
culture.pres <- c('712110', '712120', '712130', '712190')
mach.comm.manu <- c('333244', '334310', '334614')
marketing <- c('541810', '541830', '541840', '541850')
mat.manu <- c('325992', '327110', '327212', '339940')
media <- c('515111', '515112', '515120', '515210', '517110', '519110', '519120', '519130', '532230')
motion.pic.teleprod <- c('512110', '512120', '512131', '512132', '512191', '512199')
music.rec <- c('512210', '512220', '512230', '512240', '512290')
printing <- c('323111', '323113', '323117', '323120')
publishing <- c('511110', '511120', '511130', '511191', '511199')
visual.music.perf.arts <- c('541921', '541922', '611610', '711110', '711120', '711130', '711190', '711510')
wholesale.art.store <- c('423410', '423940', '424110', '424920')

#create a NAICSSIX code by extracting first 6 digits from the 8-digit NAICS code
infousa.mass$NAICSSIX <- as.numeric(substr(infousa.mass$NAICS, 1, 6))

infousa.mass$CREATIVE_CAT <- as.factor(ifelse(infousa.mass$NAICSSIX %in% arch.and.design, "Architecture and Design",
                                              #ifelse (infousa.mass$NAICSSIX %in% art.electronic.retail , "Art and electronics-related Retail",
                                              ifelse (infousa.mass$NAICSSIX %in% art.retail , "Art-related Retail",
                                                      ifelse(infousa.mass$NAICSSIX %in% art.arch.manu, "Arts and architectural manufacturing", 
                                                             ifelse(infousa.mass$NAICSSIX %in% culture.pres, "Culture and Preservation", 
                                                                    ifelse(infousa.mass$NAICSSIX %in% mach.comm.manu, "Machinery and communications manufacturing",
                                                                           ifelse(infousa.mass$NAICSSIX %in% marketing, "Marketing",
                                                                                  ifelse(infousa.mass$NAICSSIX %in% mat.manu, "Materials manufacturing",
                                                                                         ifelse(infousa.mass$NAICSSIX %in% media, "Media",
                                                                                                ifelse(infousa.mass$NAICSSIX %in% motion.pic.teleprod, "Motion picture and teleproduction",
                                                                                                       ifelse(infousa.mass$NAICSSIX %in% music.rec, "Music Recording",
                                                                                                              ifelse(infousa.mass$NAICSSIX %in% printing, "Printing",
                                                                                                                     ifelse(infousa.mass$NAICSSIX %in% publishing, "Publishing",
                                                                                                                            ifelse(infousa.mass$NAICSSIX %in% visual.music.perf.arts, "Visual Arts, Music and Other Performing Arts",
                                                                                                                                   ifelse(infousa.mass$NAICSSIX %in% wholesale.art.store, "Wholesale art stores", NA)))))))))))))))



####### DATA FILTERING: CREATIVE ECONOMY FILTERING OF INFOUSA DATA ########
# load NAICS creative economy lists from NEFA
setwd(naics.path)
nefa.all.naics <- read.csv("naics_12_nefa.csv", stringsAsFactors = FALSE)
nefa.core.naics <- read.csv("naics_12_nefa_core.csv", stringsAsFactors = FALSE)

# isolate the creative economy NAICS codes (2012) from NEFA core and all (core + peripheral) industries
nefa.all.12 <- nefa.all.naics$naics
nefa.core.12 <- nefa.core.naics$naics

# Flag each establishment in the infousa database as creative (core or all (core + peripheral)) or not
# by their 6-digit NAICS code
infousa.mass$CE_ALL_FLAG <- ifelse(infousa.mass$NAICSSIX %in% nefa.all.12, 1, 0)
infousa.mass$CE_CORE_FLAG <- ifelse(infousa.mass$NAICSSIX %in% nefa.core.12, 1, 0)


####### DATA FILTERING: FILTER INFOUSA DATA (ALL AND CREATIVE ECONOMY ONLY) ON MAPC MUNICIPALITIES ###########
infousa.nefa.all.mapc <- infousa.mass %>%
  filter(mapc == 1,
         CE_ALL_FLAG == 1)

infousa.nefa.all.mass <- infousa.mass %>%
  filter(CE_ALL_FLAG == 1)

infousa.nefa.core.mapc <- infousa.mass %>%
  filter(mapc == 1,
         CE_CORE_FLAG == 1)

infousa.nefa.core.mass <- infousa.mass %>%
  filter(CE_CORE_FLAG == 1)

# Remove electronic stores from core establishments dataset
infousa.nefa.core.mapc <- infousa.nefa.core.mapc %>%
  filter(complete.cases(CREATIVE_CAT)) 

infousa.nefa.core.mass <- infousa.nefa.core.mass %>%
  filter(complete.cases(CREATIVE_CAT))

setwd(modified.data.path)

write.csv(infousa.nefa.all.mapc, "2022_DataAxle_nefa_all_mapc_processed.csv", row.names = FALSE)
write.csv(infousa.nefa.all.mass, "2022_DataAxle_nefa_all_mass_processed.csv", row.names = FALSE)
write.csv(infousa.nefa.core.mapc, "2022_DataAxle_nefa_core_mapc_processed.csv", row.names = FALSE)
write.csv(infousa.nefa.core.mass, "2022_DataAxle_nefa_core_mass_processed.csv", row.names = FALSE)
