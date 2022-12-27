# Script: Data Axle (formerly InfoGroup) creative economy summary statistics by geography
# Author: Seleeke Flingai
# Date Last Modified: December 27, 2022 by RBowers

# install.packages('pacman')
pacman::p_load(tidyr, dplyr, reshape2, scales, data.table, bit64)

#update both with year
modified.data.path <- "K:/DataServices/Projects/Current_Projects/Arts_and_Culture_Planning/Creative_Economy/Data/Modified/2022"
output.path <- "K:/DataServices/Projects/Current_Projects/Arts_and_Culture_Planning/Creative_Economy/Output/2022"

# load dataaxle and ES-202 data for Massachusetts and its municipalities (2016)
setwd(modified.data.path)
dataaxle.nefa.core.mapc <- fread("20200430_dataaxle_nefa_core_mapc_processed.csv", stringsAsFactors = FALSE)
dataaxle.nefa.core.mass <- fread("20200430_dataaxle_nefa_core_mass_processed.csv", stringsAsFactors = FALSE)
dataaxle.nefa.all.mapc <- fread("20200430_dataaxle_nefa_all_mapc_processed.csv", stringsAsFactors = FALSE)
dataaxle.nefa.all.mass <- fread("20200430_dataaxle_nefa_all_mass_processed.csv", stringsAsFactors = FALSE)
dataaxle.mapc.towns <-  fread("20200430_dataaxle_mapc_towns_processed.csv", stringsAsFactors = FALSE)
dataaxle.mass <- fread("20200430_dataaxle_mass_processed.csv", stringsAsFactors = FALSE)


########################################## MUNICIPAL ##########################################
####### SUMMARY STATISTICS, BY MUNICIPALITY AND NEFA CREATIVE INDUSTRY GROUP 
#### summary statistics, by municipality, using NEFA Core Creative Enterprise NAICS codes
summary.dataaxle.ce.nefa.core.municipality.df <- dataaxle.nefa.core.mapc %>%
  dplyr::select(OBJECTID, PRIMARY_CITY, ACTUAL_LOCATION_EMPLOYMENT_SIZE, ACTUAL_LOCATION_SALES_VOLUME,
                FEMALE_OWNER_EXEC) %>%
  group_by(PRIMARY_CITY) %>%
  mutate(FEMALE_OWNER_EXEC = ifelse(FEMALE_OWNER_EXEC == "Y", 1, 0)) %>%   # change FEMALE_OWNER_EXEC to indicator variable (0 or 1) for summary stats
  summarise(ce.core.estab = n(),
            ce.core.employee.count = sum(ACTUAL_LOCATION_EMPLOYMENT_SIZE, na.rm = TRUE),
            ce.core.sales.total = sum(ACTUAL_LOCATION_SALES_VOLUME, na.rm = TRUE),
            ce.core.female.owned = mean(FEMALE_OWNER_EXEC),
            ce.core.sole.prop.count = sum(ACTUAL_LOCATION_EMPLOYMENT_SIZE == 1))

# add column for the total number of establishments and employees for all industries in each municipality
summary.dataaxle.ce.nefa.core.municipality.df <- inner_join (summary.dataaxle.ce.nefa.core.municipality.df, 
                                                            data.frame(table(dataaxle.mapc.towns$PRIMARY_CITY)), by = c("PRIMARY_CITY" = "Var1")) %>%
  left_join (unique(dataaxle.nefa.core.mapc[,c("PRIMARY_CITY", "county", "comm_name", "subtype", "subregion")]),
             by = "PRIMARY_CITY") %>%
  left_join(dataaxle.mapc.towns %>%
              group_by(PRIMARY_CITY) %>%
              summarise(all.employee.count = sum(ACTUAL_LOCATION_EMPLOYMENT_SIZE)), by = "PRIMARY_CITY")

summary.dataaxle.ce.nefa.core.municipality.df <- summary.dataaxle.ce.nefa.core.municipality.df %>%
  mutate("all.estab" = Freq,
         share.all.mapc = round(all.estab / sum(all.estab), digits = 3),
         ce.share.core.geography = round(ce.core.estab / all.estab, digits = 3),
         ce.share.core.geography.perc = percent(ce.share.core.geography),
         ce.share.core.mapc = round(ce.core.estab / sum(ce.core.estab), digits = 3),
         ce.share.core.mapc.perc = percent(ce.share.core.mapc),
         share.all.employee.mapc = round(all.employee.count / sum(all.employee.count), digits = 3),
         ce.share.employee.core.geography = round(ce.core.employee.count / all.employee.count, digits = 3),
         ce.share.employee.core.mapc = round(ce.core.employee.count / sum(ce.core.employee.count), digits = 3)) %>%
  dplyr::select("geography" = PRIMARY_CITY, county, "comm.type" = comm_name, subtype, subregion, 
                all.estab, share.all.mapc, ce.core.estab, ce.share.core.geography, ce.share.core.geography.perc, 
                ce.share.core.mapc, ce.share.core.mapc.perc, all.employee.count, share.all.employee.mapc,
                ce.core.employee.count, ce.share.employee.core.geography, 
                ce.share.employee.core.mapc, ce.core.sales.total, ce.core.sole.prop.count, ce.core.female.owned)


#### summary statistics, by municipality, using NEFA All (Core + Peripheral) Creative Enterprise NAICS codes
summary.dataaxle.ce.nefa.all.municipality.df <- dataaxle.nefa.all.mapc %>%
  dplyr::select(OBJECTID, PRIMARY_CITY, ACTUAL_LOCATION_EMPLOYMENT_SIZE, ACTUAL_LOCATION_SALES_VOLUME,
                FEMALE_OWNER_EXEC) %>%
  group_by(PRIMARY_CITY) %>%
  mutate(FEMALE_OWNER_EXEC = ifelse(FEMALE_OWNER_EXEC == "Y", 1, 0)) %>%   # change FEMALE_OWNER_EXEC to indicator variable (0 or 1) for summary stats
  summarise(ce.all.estab = n(),
            ce.all.employee.count = sum(ACTUAL_LOCATION_EMPLOYMENT_SIZE, na.rm = TRUE),
            ce.all.sales.total = sum(ACTUAL_LOCATION_SALES_VOLUME, na.rm = TRUE),
            ce.all.female.owned = mean(FEMALE_OWNER_EXEC),
            ce.all.sole.prop.count = sum(ACTUAL_LOCATION_EMPLOYMENT_SIZE == 1))

# add column for the total number of establishments and employees for all industries in each municipality
summary.dataaxle.ce.nefa.all.municipality.df <- inner_join (summary.dataaxle.ce.nefa.all.municipality.df, 
                                                           data.frame(table(dataaxle.mapc.towns$PRIMARY_CITY)), by = c("PRIMARY_CITY" = "Var1")) %>%
  left_join (unique(dataaxle.nefa.all.mapc[,c("PRIMARY_CITY", "county", "comm_name", "subtype", "subregion")]),
             by = "PRIMARY_CITY") %>%
  left_join(dataaxle.mapc.towns %>%
              group_by(PRIMARY_CITY) %>%
              summarise(all.employee.count = sum(ACTUAL_LOCATION_EMPLOYMENT_SIZE)), by = "PRIMARY_CITY")

summary.dataaxle.ce.nefa.all.municipality.df <- summary.dataaxle.ce.nefa.all.municipality.df %>%
  mutate("all.estab" = Freq,
         share.all.mapc = round(all.estab / sum(all.estab), digits = 3),
         ce.share.all.geography = round(ce.all.estab / all.estab, digits = 3),
         ce.share.all.geography.perc = percent(ce.share.all.geography),
         ce.share.all.mapc = round(ce.all.estab / sum(ce.all.estab), digits = 3),
         ce.share.all.mapc.perc = percent(ce.share.all.mapc),
         share.all.employee.mapc = round(all.employee.count / sum(all.employee.count), digits = 3),
         ce.share.employee.all.geography = round(ce.all.employee.count / all.employee.count, digits = 3),
         ce.share.employee.all.mapc = round(ce.all.employee.count / sum(ce.all.employee.count), digits = 3)) %>%
  dplyr::select("geography" = PRIMARY_CITY, county, "comm.type" = comm_name, subtype, subregion, 
                all.estab, share.all.mapc, ce.all.estab, ce.share.all.geography, ce.share.all.geography.perc,
                ce.share.all.mapc, ce.share.all.mapc.perc, all.employee.count, share.all.employee.mapc, 
                ce.all.employee.count, ce.share.employee.all.geography, ce.share.employee.all.mapc, 
                ce.all.sales.total, ce.all.sole.prop.count, ce.all.female.owned)

#### summary statistics, by NEFA Creative Industry Group categorization, MAPC region
summary.dataaxle.ce.nefa.core.categories.mapc.detailed.df <- dataaxle.nefa.core.mapc %>%
  dplyr::select(OBJECTID, PRIMARY_CITY, ACTUAL_LOCATION_EMPLOYMENT_SIZE, ACTUAL_LOCATION_SALES_VOLUME,
                FEMALE_OWNER_EXEC, CREATIVE_CAT) %>%
  filter(complete.cases(CREATIVE_CAT)) %>%
  group_by(CREATIVE_CAT) %>%
  mutate(FEMALE_OWNER_EXEC = ifelse(FEMALE_OWNER_EXEC == "Y", 1, 0)) %>%
  summarise(ce.core.estab = n(),
            ce.core.employee.count = sum(ACTUAL_LOCATION_EMPLOYMENT_SIZE, na.rm = TRUE),
            ce.core.sales.total = sum(ACTUAL_LOCATION_SALES_VOLUME, na.rm = TRUE),
            ce.core.female.owned = mean(FEMALE_OWNER_EXEC),
            ce.core.sole.prop.count = sum(ACTUAL_LOCATION_EMPLOYMENT_SIZE == 1)) %>%
  mutate(cat.core.estab.share = ce.core.estab / sum(ce.core.estab),
         cat.core.estab.share.perc = percent(cat.core.estab.share),
         cat.core.employee.share = ce.core.employee.count / sum(ce.core.employee.count),
         cat.core.sales.share = ce.core.sales.total / sum(ce.core.sales.total)) %>%
  dplyr::select(CREATIVE_CAT, ce.core.estab, cat.core.estab.share, cat.core.estab.share.perc, ce.core.employee.count, cat.core.employee.share,
                ce.core.sales.total, cat.core.sales.share, ce.core.sole.prop.count, ce.core.female.owned)

#### summary statistics, by NEFA Creative Industry Group categorization, Massachusetts
summary.dataaxle.ce.nefa.core.categories.mass.detailed.df <- dataaxle.nefa.core.mass %>%
  dplyr::select(OBJECTID, PRIMARY_CITY, ACTUAL_LOCATION_EMPLOYMENT_SIZE, ACTUAL_LOCATION_SALES_VOLUME,
                FEMALE_OWNER_EXEC, CREATIVE_CAT) %>%
  filter(complete.cases(CREATIVE_CAT)) %>%
  group_by(CREATIVE_CAT) %>%
  mutate(FEMALE_OWNER_EXEC = ifelse(FEMALE_OWNER_EXEC == "Y", 1, 0)) %>%
  summarise(ce.core.estab = n(),
            ce.core.employee.count = sum(ACTUAL_LOCATION_EMPLOYMENT_SIZE, na.rm = TRUE),
            ce.core.sales.total = sum(ACTUAL_LOCATION_SALES_VOLUME, na.rm = TRUE),
            ce.core.female.owned = mean(FEMALE_OWNER_EXEC),
            ce.core.sole.prop.count = sum(ACTUAL_LOCATION_EMPLOYMENT_SIZE == 1)) %>%
  mutate(cat.core.estab.share = ce.core.estab / sum(ce.core.estab),
         cat.core.estab.share.perc = percent(cat.core.estab.share),
         cat.core.employee.share = ce.core.employee.count / sum(ce.core.employee.count),
         cat.core.sales.share = ce.core.sales.total / sum(ce.core.sales.total)) %>%
  dplyr::select(CREATIVE_CAT, ce.core.estab, cat.core.estab.share, cat.core.estab.share.perc, ce.core.employee.count, cat.core.employee.share,
                ce.core.sales.total, cat.core.sales.share, ce.core.sole.prop.count, ce.core.female.owned)


#### summary statistics, by NEFA Creative Industry Group categorization, MAPC Region -- dataaxle data 

# Select dataaxle data for MAPC region
summary.dataaxle.ce.nefa.core.categories.mapc.df <- summary.dataaxle.ce.nefa.core.categories.mapc.detailed.df %>%
  dplyr::select(CREATIVE_CAT, ce.core.estab, cat.core.estab.share, cat.core.estab.share.perc, ce.core.employee.count,
                cat.core.employee.share) %>%
  mutate(source = "dataaxle")


#### summary statistics, by NEFA Creative Industry Group categorization, Massachusetts -- dataaxle data 
# Select dataaxle data for Massachusetts
summary.dataaxle.ce.nefa.core.categories.mass.df <- summary.dataaxle.ce.nefa.core.categories.mass.detailed.df %>%
  dplyr::select(CREATIVE_CAT, ce.core.estab, cat.core.estab.share, cat.core.estab.share.perc, ce.core.employee.count,
                cat.core.employee.share) %>%
  mutate(source = "dataaxle")


####### SUMMARY STAT: NUMBER AND SHARE OF EACH CREATIVE INDUSTRY COMPARED TO ALL CREATIVE INDUSTRIES, BY MUNICIPALITY 
# number and share of each creative industry
# dataaxle data
by.town.dataaxle.mapc <- dataaxle.nefa.core.mapc %>%
  dplyr::select("geography" = PRIMARY_CITY, CREATIVE_CAT) %>%
  mutate("indicator" = rep(1, nrow(dataaxle.nefa.core.mapc))) %>%
  group_by(geography, CREATIVE_CAT) %>%
  summarise("estab" = sum(indicator)) %>%
  mutate("share" = estab / sum(estab),
         "percent" = percent(estab / sum(estab))) %>%
  dplyr::select(geography, CREATIVE_CAT, estab, share, percent)

# number of each creative industry
by.town.dataaxle.estab <- by.town.dataaxle.mapc %>%
  dplyr::select(geography, CREATIVE_CAT, estab) %>%
  spread(key = CREATIVE_CAT, value = estab)

by.town.dataaxle.estab[is.na(by.town.dataaxle.estab)] <- 0


# share of each creative industry amongst all creative industries
by.town.dataaxle.share <- by.town.dataaxle.mapc %>%
  dplyr::select(geography, CREATIVE_CAT, share) %>%
  spread(key = CREATIVE_CAT, value = share)

by.town.dataaxle.share[is.na(by.town.dataaxle.share)] <- 0



########################################## MASSACHUSETTS ##########################################
#### summary statistics, by state, using NEFA Core Creative Enterprise NAICS codes
summary.dataaxle.ce.nefa.core.mass.df <- dataaxle.nefa.core.mass %>%
  dplyr::select(OBJECTID, PRIMARY_CITY, PRIMARY_STATE, ACTUAL_LOCATION_EMPLOYMENT_SIZE, ACTUAL_LOCATION_SALES_VOLUME,
                FEMALE_OWNER_EXEC) %>%
  group_by(PRIMARY_STATE) %>%
  mutate(FEMALE_OWNER_EXEC = ifelse(FEMALE_OWNER_EXEC == "Y", 1, 0)) %>%   # change FEMALE_OWNER_EXEC to indicator variable (0 or 1) for summary stats
  summarise(ce.core.estab = n(),
            ce.core.employee.count = sum(ACTUAL_LOCATION_EMPLOYMENT_SIZE, na.rm = TRUE),
            ce.core.sales.total = sum(ACTUAL_LOCATION_SALES_VOLUME, na.rm = TRUE),
            ce.core.female.owned = mean(FEMALE_OWNER_EXEC),
            ce.core.sole.prop.count = sum(ACTUAL_LOCATION_EMPLOYMENT_SIZE == 1))

# add column for the total number of establishments and employees for all industries in the state
summary.dataaxle.ce.nefa.core.mass.temp <- left_join (summary.dataaxle.ce.nefa.core.mass.df, 
                                                     data.frame(table(dataaxle.mass$PRIMARY_STATE[dataaxle.mass$PRIMARY_STATE=="MA"])), 
                                                     by = c("PRIMARY_STATE" = "Var1")) %>%
  left_join (unique(dataaxle.nefa.core.mass[,c("PRIMARY_STATE", "county", "comm_name", "subtype", "subregion")]),
             by = "PRIMARY_STATE") %>%
  left_join(dataaxle.mass %>%
              group_by(PRIMARY_STATE) %>%
              summarise(all.employee.count = sum(ACTUAL_LOCATION_EMPLOYMENT_SIZE)), by = "PRIMARY_STATE")

summary.dataaxle.ce.nefa.core.mass.df <- summary.dataaxle.ce.nefa.core.mass.temp %>%
  mutate("all.estab" = Freq,
         share.all.mapc = NA,
         ce.share.core.geography = round(ce.core.estab / all.estab, digits = 3),
         ce.share.core.mapc = NA,
         share.all.employee.mapc = NA,
         ce.share.employee.core.geography = round(ce.core.employee.count / all.employee.count, digits = 3),
         ce.share.employee.core.mapc = NA) %>%
  dplyr::select("geography" = PRIMARY_STATE, all.estab, share.all.mapc, ce.core.estab, ce.share.core.geography, 
                ce.share.core.mapc, all.employee.count, share.all.employee.mapc, ce.core.employee.count, 
                ce.share.employee.core.geography, ce.share.employee.core.mapc, ce.core.sales.total, 
                ce.core.sole.prop.count, ce.core.female.owned) %>%
  mutate(geography = "Massachusetts")

summary.dataaxle.ce.nefa.core.mass.df <- unique(summary.dataaxle.ce.nefa.core.mass.df)

#### summary statistics, by state, using NEFA All Creative Enterprise NAICS codes
summary.dataaxle.ce.nefa.all.mass.df <- dataaxle.nefa.all.mass %>%
  dplyr::select(OBJECTID, PRIMARY_CITY, PRIMARY_STATE, ACTUAL_LOCATION_EMPLOYMENT_SIZE, ACTUAL_LOCATION_SALES_VOLUME,
                FEMALE_OWNER_EXEC) %>%
  group_by(PRIMARY_STATE) %>%
  mutate(FEMALE_OWNER_EXEC = ifelse(FEMALE_OWNER_EXEC == "Y", 1, 0)) %>%   # change FEMALE_OWNER_EXEC to indicator variable (0 or 1) for summary stats
  summarise(ce.all.estab = n(),
            ce.all.employee.count = sum(ACTUAL_LOCATION_EMPLOYMENT_SIZE, na.rm = TRUE),
            ce.all.sales.total = sum(ACTUAL_LOCATION_SALES_VOLUME, na.rm = TRUE),
            ce.all.female.owned = mean(FEMALE_OWNER_EXEC),
            ce.all.sole.prop.count = sum(ACTUAL_LOCATION_EMPLOYMENT_SIZE == 1))

# add column for the total number of establishments and employees for all industries in the state
summary.dataaxle.ce.nefa.all.mass.temp <- left_join (summary.dataaxle.ce.nefa.all.mass.df, 
                                                    data.frame(table(dataaxle.mass$PRIMARY_STATE[dataaxle.mass$PRIMARY_STATE=="MA"])), 
                                                    by = c("PRIMARY_STATE" = "Var1")) %>%
  left_join (unique(dataaxle.nefa.all.mass[,c("PRIMARY_STATE", "county", "comm_name", "subtype", "subregion")]),
             by = "PRIMARY_STATE") %>%
  left_join(dataaxle.mass %>%
              group_by(PRIMARY_STATE) %>%
              summarise(all.employee.count = sum(ACTUAL_LOCATION_EMPLOYMENT_SIZE)), by = "PRIMARY_STATE")

summary.dataaxle.ce.nefa.all.mass.df <- summary.dataaxle.ce.nefa.all.mass.temp %>%
  mutate("all.estab" = Freq,
         share.all.mapc = NA,
         ce.share.all.geography = round(ce.all.estab / all.estab, digits = 3),
         ce.share.all.mapc = NA,
         share.all.employee.mapc = NA,
         ce.share.employee.all.geography = round(ce.all.employee.count / all.employee.count, digits = 3),
         ce.share.employee.all.mapc = NA) %>%
  dplyr::select("geography" = PRIMARY_STATE, 
                all.estab, share.all.mapc, ce.all.estab, ce.share.all.geography, ce.share.all.mapc, 
                all.employee.count, share.all.employee.mapc, ce.all.employee.count, ce.share.employee.all.geography, 
                ce.share.employee.all.mapc, ce.all.sales.total, ce.all.sole.prop.count, ce.all.female.owned) %>%
  mutate(geography = "Massachusetts")

summary.dataaxle.ce.nefa.all.mass.df <- unique(summary.dataaxle.ce.nefa.all.mass.df)

####### SUMMARY STAT: NUMBER AND SHARE OF EACH CREATIVE INDUSTRY COMPARED TO ALL CREATIVE INDUSTRIES, MASSACHUSETTS 
# number and share of each creative industry
# dataaxle data
by.mass.dataaxle.mass <- dataaxle.nefa.core.mass %>%
  dplyr::select(CREATIVE_CAT) %>%
  mutate("indicator" = rep(1, nrow(dataaxle.nefa.core.mass))) %>%
  group_by(CREATIVE_CAT) %>%
  summarise("estab" = sum(indicator)) %>%
  mutate("share" = estab / sum(estab),
         "percent" = percent(estab / sum(estab)),
         "geography" = "Massachusetts") %>%
  dplyr::select(geography, CREATIVE_CAT, estab, share)

# number of each creative industry
by.mass.dataaxle.estab <- by.mass.dataaxle.mass %>%
  dplyr::select(geography, CREATIVE_CAT, estab) %>%
  spread(key = CREATIVE_CAT, value = estab)

by.mass.dataaxle.estab[is.na(by.mass.dataaxle.estab)] <- 0


# share of each creative industry amongst all creative industries
by.mass.dataaxle.share <- by.mass.dataaxle.mass %>%
  dplyr::select(geography, CREATIVE_CAT, share) %>%
  spread(key = CREATIVE_CAT, value = share)

by.mass.dataaxle.share[is.na(by.mass.dataaxle.share)] <- 0

########################################## MAPC ##########################################
#### SUMMARY STATISTICS FOR MAPC REGION, BY NEFA CREATIVE INDUSTRY GROUP 
#### summary statistics, for MAPC, using NEFA Core Creative Enterprise NAICS codes
summary.dataaxle.ce.nefa.core.mapc <- dataaxle.nefa.core.mapc %>%
  dplyr::select(OBJECTID, PRIMARY_CITY, mapc, ACTUAL_LOCATION_EMPLOYMENT_SIZE, ACTUAL_LOCATION_SALES_VOLUME,
                FEMALE_OWNER_EXEC) %>%
  group_by(mapc) %>%
  mutate(FEMALE_OWNER_EXEC = ifelse(FEMALE_OWNER_EXEC == "Y", 1, 0)) %>%   # change FEMALE_OWNER_EXEC to indicator variable (0 or 1) for summary stats
  summarise(ce.core.estab = n(),
            ce.core.employee.count = sum(ACTUAL_LOCATION_EMPLOYMENT_SIZE, na.rm = TRUE),
            ce.core.sales.total = sum(ACTUAL_LOCATION_SALES_VOLUME, na.rm = TRUE),
            ce.core.female.owned = mean(FEMALE_OWNER_EXEC),
            ce.core.sole.prop.count = sum(ACTUAL_LOCATION_EMPLOYMENT_SIZE == 1)) %>%
  mutate(geography = "MAPC")

# add column for the total number of establishments and employees for all industries in MAPC
estab.count.mapc = dataaxle.mapc.towns %>%
  group_by(mapc) %>%
  mutate(all.estab = n()) %>%
  dplyr::select(mapc, all.estab) %>%
  filter(!duplicated(mapc)) %>%
  mutate(geography = "MAPC")

employee.count.mapc = dataaxle.mapc.towns %>%
  group_by(mapc) %>%
  summarize(all.employee.count = sum(ACTUAL_LOCATION_EMPLOYMENT_SIZE)) %>%
  mutate(geography = "MAPC")

estab.employee.count.mapc = left_join(estab.count.mapc, employee.count.mapc, by = 'geography')

summary.dataaxle.ce.nefa.core.mapc.temp <- left_join(summary.dataaxle.ce.nefa.core.mapc, 
                                                    estab.employee.count.mapc, by = "geography")

summary.dataaxle.ce.nefa.core.mapc.df <- summary.dataaxle.ce.nefa.core.mapc.temp %>%
  mutate(share.all.mapc = round(all.estab / sum(all.estab), digits = 3),
         ce.share.core.geography = round(ce.core.estab / all.estab, digits = 3),
         ce.share.core.geography.perc = percent(ce.share.core.geography),
         ce.share.core.mapc = round(ce.core.estab / sum(ce.core.estab), digits = 3),
         ce.share.core.mapc.perc = percent(ce.share.core.mapc),
         share.all.employee.mapc = round(all.employee.count / sum(all.employee.count), digits = 3),
         ce.share.employee.core.geography = round(ce.core.employee.count / all.employee.count, digits = 3),
         ce.share.employee.core.mapc = round(ce.core.employee.count / sum(ce.core.employee.count), digits = 3)) %>%
  dplyr::select(geography,  
                all.estab, share.all.mapc, ce.core.estab, ce.share.core.geography, ce.share.core.geography.perc, 
                ce.share.core.mapc, ce.share.core.mapc.perc, all.employee.count, share.all.employee.mapc,
                ce.core.employee.count, ce.share.employee.core.geography, 
                ce.share.employee.core.mapc, ce.core.sales.total, ce.core.sole.prop.count, ce.core.female.owned)


#### summary statistics, MAPC, using NEFA All (Core + Peripheral) Creative Enterprise NAICS codes
summary.dataaxle.ce.nefa.all.mapc <- dataaxle.nefa.all.mapc %>%
  dplyr::select(OBJECTID, PRIMARY_CITY, mapc, ACTUAL_LOCATION_EMPLOYMENT_SIZE, ACTUAL_LOCATION_SALES_VOLUME,
                FEMALE_OWNER_EXEC) %>%
  group_by(mapc) %>%
  mutate(FEMALE_OWNER_EXEC = ifelse(FEMALE_OWNER_EXEC == "Y", 1, 0)) %>%   # change FEMALE_OWNER_EXEC to indicator variable (0 or 1) for summary stats
  summarise(ce.all.estab = n(),
            ce.all.employee.count = sum(ACTUAL_LOCATION_EMPLOYMENT_SIZE, na.rm = TRUE),
            ce.all.sales.total = sum(ACTUAL_LOCATION_SALES_VOLUME, na.rm = TRUE),
            ce.all.female.owned = mean(FEMALE_OWNER_EXEC),
            ce.all.sole.prop.count = sum(ACTUAL_LOCATION_EMPLOYMENT_SIZE == 1)) %>%
  mutate(geography = 'MAPC')

# add column for the total number of establishments and employees for all industries in MAPC
summary.dataaxle.ce.nefa.all.mapc.temp <- left_join(summary.dataaxle.ce.nefa.all.mapc, 
                                                   estab.employee.count.mapc, by = "geography")

summary.dataaxle.ce.nefa.all.mapc.df <- summary.dataaxle.ce.nefa.all.mapc.temp %>%
  mutate(share.all.mapc = round(all.estab / sum(all.estab), digits = 3),
         ce.share.all.geography = round(ce.all.estab / all.estab, digits = 3),
         ce.share.all.geography.perc = percent(ce.share.all.geography),
         ce.share.all.mapc = round(ce.all.estab / sum(ce.all.estab), digits = 3),
         ce.share.all.mapc.perc = percent(ce.share.all.mapc),
         share.all.employee.mapc = round(all.employee.count / sum(all.employee.count), digits = 3),
         ce.share.employee.all.geography = round(ce.all.employee.count / all.employee.count, digits = 3),
         ce.share.employee.all.mapc = round(ce.all.employee.count / sum(ce.all.employee.count), digits = 3)) %>%
  dplyr::select(geography, all.estab, share.all.mapc, ce.all.estab, ce.share.all.geography, ce.share.all.geography.perc,
                ce.share.all.mapc, ce.share.all.mapc.perc, all.employee.count, share.all.employee.mapc, 
                ce.all.employee.count, ce.share.employee.all.geography, ce.share.employee.all.mapc, 
                ce.all.sales.total, ce.all.sole.prop.count, ce.all.female.owned)


####### SUMMARY STAT: NUMBER AND SHARE OF EACH CREATIVE INDUSTRY COMPARED TO ALL CREATIVE INDUSTRIES, FOR MAPC 
# number and share of each creative industry
# dataaxle data
by.mapc.dataaxle.mapc <- dataaxle.nefa.core.mapc %>%
  dplyr::select(mapc, CREATIVE_CAT) %>%
  mutate("indicator" = rep(1, nrow(dataaxle.nefa.core.mapc))) %>%
  group_by(mapc, CREATIVE_CAT) %>%
  summarise("estab" = sum(indicator)) %>%
  mutate("share" = estab / sum(estab),
         "percent" = percent(estab / sum(estab)),
         geography = 'MAPC') %>%
  dplyr::select(geography, CREATIVE_CAT, estab, share, percent)

# number of each creative industry
by.mapc.dataaxle.estab <- by.mapc.dataaxle.mapc %>%
  dplyr::select(geography, CREATIVE_CAT, estab) %>%
  spread(key = CREATIVE_CAT, value = estab)

by.mapc.dataaxle.estab[is.na(by.mapc.dataaxle.estab)] <- 0


# share of each creative industry amongst all creative industries
by.mapc.dataaxle.share <- by.mapc.dataaxle.mapc %>%
  dplyr::select(geography, CREATIVE_CAT, share) %>%
  spread(key = CREATIVE_CAT, value = share)

by.mapc.dataaxle.share[is.na(by.mapc.dataaxle.share)] <- 0

########################################## COMMUNITY TYPE ##########################################
#### SUMMARY STATISTICS, BY COMMUNITY TYPE AND NEFA CREATIVE INDUSTRY GROUP 
#### summary statistics, by community type, using NEFA Core Creative Enterprise NAICS codes
summary.dataaxle.ce.nefa.core.commtype <- dataaxle.nefa.core.mapc %>%
  dplyr::select(OBJECTID, PRIMARY_CITY, comm_name, ACTUAL_LOCATION_EMPLOYMENT_SIZE, ACTUAL_LOCATION_SALES_VOLUME,
                FEMALE_OWNER_EXEC) %>%
  group_by("geography" = comm_name) %>%
  mutate(FEMALE_OWNER_EXEC = ifelse(FEMALE_OWNER_EXEC == "Y", 1, 0)) %>%   # change FEMALE_OWNER_EXEC to indicator variable (0 or 1) for summary stats
  summarise(ce.core.estab = n(),
            ce.core.employee.count = sum(ACTUAL_LOCATION_EMPLOYMENT_SIZE, na.rm = TRUE),
            ce.core.sales.total = sum(ACTUAL_LOCATION_SALES_VOLUME, na.rm = TRUE),
            ce.core.female.owned = mean(FEMALE_OWNER_EXEC),
            ce.core.sole.prop.count = sum(ACTUAL_LOCATION_EMPLOYMENT_SIZE == 1))

# add column for the total number of establishments and employees for all industries in each community type
estab.count.commtype = dataaxle.mapc.towns %>%
  group_by("geography" = comm_name) %>%
  mutate(all.estab = n()) %>%
  dplyr::select(geography, all.estab) %>%
  filter(!duplicated(geography))

employee.count.commtype = dataaxle.mapc.towns %>%
  group_by("geography" = comm_name) %>%
  summarize(all.employee.count = sum(ACTUAL_LOCATION_EMPLOYMENT_SIZE))

estab.employee.count.commtype = left_join(estab.count.commtype, employee.count.commtype, by = 'geography')

summary.dataaxle.ce.nefa.core.commtype.temp <- left_join(summary.dataaxle.ce.nefa.core.commtype, 
                                                        estab.employee.count.commtype, by = "geography")

summary.dataaxle.ce.nefa.core.commtype.df <- summary.dataaxle.ce.nefa.core.commtype.temp %>%
  mutate(share.all.mapc = round(all.estab / sum(all.estab), digits = 3),
         ce.share.core.geography = round(ce.core.estab / all.estab, digits = 3),
         ce.share.core.geography.perc = percent(ce.share.core.geography),
         ce.share.core.mapc = round(ce.core.estab / sum(ce.core.estab), digits = 3),
         ce.share.core.mapc.perc = percent(ce.share.core.mapc),
         share.all.employee.mapc = round(all.employee.count / sum(all.employee.count), digits = 3),
         ce.share.employee.core.geography = round(ce.core.employee.count / all.employee.count, digits = 3),
         ce.share.employee.core.mapc = round(ce.core.employee.count / sum(ce.core.employee.count), digits = 3)) %>%
  dplyr::select(geography,  
                all.estab, share.all.mapc, ce.core.estab, ce.share.core.geography, ce.share.core.geography.perc, 
                ce.share.core.mapc, ce.share.core.mapc.perc, all.employee.count, share.all.employee.mapc,
                ce.core.employee.count, ce.share.employee.core.geography, 
                ce.share.employee.core.mapc, ce.core.sales.total, ce.core.sole.prop.count, ce.core.female.owned)


#### summary statistics, by community type, using NEFA All (Core + Peripheral) Creative Enterprise NAICS codes
summary.dataaxle.ce.nefa.all.commtype <- dataaxle.nefa.all.mapc %>%
  dplyr::select(OBJECTID, PRIMARY_CITY, comm_name, ACTUAL_LOCATION_EMPLOYMENT_SIZE, ACTUAL_LOCATION_SALES_VOLUME,
                FEMALE_OWNER_EXEC) %>%
  group_by("geography" = comm_name) %>%
  mutate(FEMALE_OWNER_EXEC = ifelse(FEMALE_OWNER_EXEC == "Y", 1, 0)) %>%   # change FEMALE_OWNER_EXEC to indicator variable (0 or 1) for summary stats
  summarise(ce.all.estab = n(),
            ce.all.employee.count = sum(ACTUAL_LOCATION_EMPLOYMENT_SIZE, na.rm = TRUE),
            ce.all.sales.total = sum(ACTUAL_LOCATION_SALES_VOLUME, na.rm = TRUE),
            ce.all.female.owned = mean(FEMALE_OWNER_EXEC),
            ce.all.sole.prop.count = sum(ACTUAL_LOCATION_EMPLOYMENT_SIZE == 1))

# add column for the total number of establishments and employees for all industries in each community type
summary.dataaxle.ce.nefa.all.commtype.temp <- left_join(summary.dataaxle.ce.nefa.all.commtype, 
                                                       estab.employee.count.commtype, by = "geography")

summary.dataaxle.ce.nefa.all.commtype.df <- summary.dataaxle.ce.nefa.all.commtype.temp %>%
  mutate(share.all.mapc = round(all.estab / sum(all.estab), digits = 3),
         ce.share.all.geography = round(ce.all.estab / all.estab, digits = 3),
         ce.share.all.geography.perc = percent(ce.share.all.geography),
         ce.share.all.mapc = round(ce.all.estab / sum(ce.all.estab), digits = 3),
         ce.share.all.mapc.perc = percent(ce.share.all.mapc),
         share.all.employee.mapc = round(all.employee.count / sum(all.employee.count), digits = 3),
         ce.share.employee.all.geography = round(ce.all.employee.count / all.employee.count, digits = 3),
         ce.share.employee.all.mapc = round(ce.all.employee.count / sum(ce.all.employee.count), digits = 3)) %>%
  dplyr::select(geography, all.estab, share.all.mapc, ce.all.estab, ce.share.all.geography, ce.share.all.geography.perc,
                ce.share.all.mapc, ce.share.all.mapc.perc, all.employee.count, share.all.employee.mapc, 
                ce.all.employee.count, ce.share.employee.all.geography, ce.share.employee.all.mapc, 
                ce.all.sales.total, ce.all.sole.prop.count, ce.all.female.owned)

####### SUMMARY STAT: NUMBER AND SHARE OF EACH CREATIVE INDUSTRY COMPARED TO ALL CREATIVE INDUSTRIES, BY COMMUNITY TYPE 
# number and share of each creative industry
# dataaxle data
by.comm.type.dataaxle.mapc <- dataaxle.nefa.core.mapc %>%
  dplyr::select("geography" = comm_name, CREATIVE_CAT) %>%
  mutate("indicator" = rep(1, nrow(dataaxle.nefa.core.mapc))) %>%
  group_by(geography, CREATIVE_CAT) %>%
  summarise("estab" = sum(indicator)) %>%
  mutate("share" = estab / sum(estab),
         "percent" = percent(estab / sum(estab))) %>%
  dplyr::select(geography, CREATIVE_CAT, estab, share, percent)

# number of each creative industry
by.comm.type.dataaxle.estab <- by.comm.type.dataaxle.mapc %>%
  dplyr::select(geography, CREATIVE_CAT, estab) %>%
  spread(key = CREATIVE_CAT, value = estab)

by.comm.type.dataaxle.estab[is.na(by.comm.type.dataaxle.estab)] <- 0


# share of each creative industry amongst all creative industries
by.comm.type.dataaxle.share <- by.comm.type.dataaxle.mapc %>%
  dplyr::select(geography, CREATIVE_CAT, share) %>%
  spread(key = CREATIVE_CAT, value = share)

by.comm.type.dataaxle.share[is.na(by.comm.type.dataaxle.share)] <- 0


########################################## SUBREGION ##########################################
#### SUMMARY STATISTICS, BY SUBREGION AND NEFA CREATIVE INDUSTRY GROUP 
#### summary statistics, by subregion, using NEFA Core Creative Enterprise NAICS codes
summary.dataaxle.ce.nefa.core.subregion <- dataaxle.nefa.core.mapc %>%
  dplyr::select(OBJECTID, PRIMARY_CITY, subregion, ACTUAL_LOCATION_EMPLOYMENT_SIZE, ACTUAL_LOCATION_SALES_VOLUME,
                FEMALE_OWNER_EXEC) %>%
  group_by("geography" = subregion) %>%
  mutate(FEMALE_OWNER_EXEC = ifelse(FEMALE_OWNER_EXEC == "Y", 1, 0)) %>%   # change FEMALE_OWNER_EXEC to indicator variable (0 or 1) for summary stats
  summarise(ce.core.estab = n(),
            ce.core.employee.count = sum(ACTUAL_LOCATION_EMPLOYMENT_SIZE, na.rm = TRUE),
            ce.core.sales.total = sum(ACTUAL_LOCATION_SALES_VOLUME, na.rm = TRUE),
            ce.core.female.owned = mean(FEMALE_OWNER_EXEC),
            ce.core.sole.prop.count = sum(ACTUAL_LOCATION_EMPLOYMENT_SIZE == 1))

# add column for the total number of establishments and employees for all industries in each neighborhood
estab.count.subregion = dataaxle.mapc.towns %>%
  group_by("geography" = subregion) %>%
  mutate(all.estab = n()) %>%
  dplyr::select(geography, all.estab) %>%
  filter(!duplicated(geography))

employee.count.subregion = dataaxle.mapc.towns %>%
  group_by("geography" = subregion) %>%
  summarize(all.employee.count = sum(ACTUAL_LOCATION_EMPLOYMENT_SIZE))

estab.employee.count.subregion = left_join(estab.count.subregion, employee.count.subregion, by = 'geography')

summary.dataaxle.ce.nefa.core.subregion.temp <- left_join(summary.dataaxle.ce.nefa.core.subregion, 
                                                         estab.employee.count.subregion, by = "geography")

summary.dataaxle.ce.nefa.core.subregion.df <- summary.dataaxle.ce.nefa.core.subregion.temp %>%
  mutate(share.all.mapc = round(all.estab / sum(all.estab), digits = 3),
         ce.share.core.geography = round(ce.core.estab / all.estab, digits = 3),
         ce.share.core.geography.perc = percent(ce.share.core.geography),
         ce.share.core.mapc = round(ce.core.estab / sum(ce.core.estab), digits = 3),
         ce.share.core.mapc.perc = percent(ce.share.core.mapc),
         share.all.employee.mapc = round(all.employee.count / sum(all.employee.count), digits = 3),
         ce.share.employee.core.geography = round(ce.core.employee.count / all.employee.count, digits = 3),
         ce.share.employee.core.mapc = round(ce.core.employee.count / sum(ce.core.employee.count), digits = 3)) %>%
  dplyr::select(geography,  
                all.estab, share.all.mapc, ce.core.estab, ce.share.core.geography, ce.share.core.geography.perc, 
                ce.share.core.mapc, ce.share.core.mapc.perc, all.employee.count, share.all.employee.mapc,
                ce.core.employee.count, ce.share.employee.core.geography, 
                ce.share.employee.core.mapc, ce.core.sales.total, ce.core.sole.prop.count, ce.core.female.owned)


#### summary statistics, by municipality, using NEFA All (Core + Peripheral) Creative Enterprise NAICS codes
summary.dataaxle.ce.nefa.all.subregion <- dataaxle.nefa.all.mapc %>%
  dplyr::select(OBJECTID, PRIMARY_CITY, subregion, ACTUAL_LOCATION_EMPLOYMENT_SIZE, ACTUAL_LOCATION_SALES_VOLUME,
                FEMALE_OWNER_EXEC) %>%
  group_by("geography" = subregion) %>%
  mutate(FEMALE_OWNER_EXEC = ifelse(FEMALE_OWNER_EXEC == "Y", 1, 0)) %>%   # change FEMALE_OWNER_EXEC to indicator variable (0 or 1) for summary stats
  summarise(ce.all.estab = n(),
            ce.all.employee.count = sum(ACTUAL_LOCATION_EMPLOYMENT_SIZE, na.rm = TRUE),
            ce.all.sales.total = sum(ACTUAL_LOCATION_SALES_VOLUME, na.rm = TRUE),
            ce.all.female.owned = mean(FEMALE_OWNER_EXEC),
            ce.all.sole.prop.count = sum(ACTUAL_LOCATION_EMPLOYMENT_SIZE == 1))

# add column for the total number of establishments and employees for all industries in each neighborhood
summary.dataaxle.ce.nefa.all.subregion.temp <- left_join(summary.dataaxle.ce.nefa.all.subregion, 
                                                        estab.employee.count.subregion, by = "geography")

summary.dataaxle.ce.nefa.all.subregion.df <- summary.dataaxle.ce.nefa.all.subregion.temp %>%
  mutate(share.all.mapc = round(all.estab / sum(all.estab), digits = 3),
         ce.share.all.geography = round(ce.all.estab / all.estab, digits = 3),
         ce.share.all.geography.perc = percent(ce.share.all.geography),
         ce.share.all.mapc = round(ce.all.estab / sum(ce.all.estab), digits = 3),
         ce.share.all.mapc.perc = percent(ce.share.all.mapc),
         share.all.employee.mapc = round(all.employee.count / sum(all.employee.count), digits = 3),
         ce.share.employee.all.geography = round(ce.all.employee.count / all.employee.count, digits = 3),
         ce.share.employee.all.mapc = round(ce.all.employee.count / sum(ce.all.employee.count), digits = 3)) %>%
  dplyr::select(geography, all.estab, share.all.mapc, ce.all.estab, ce.share.all.geography, ce.share.all.geography.perc,
                ce.share.all.mapc, ce.share.all.mapc.perc, all.employee.count, share.all.employee.mapc, 
                ce.all.employee.count, ce.share.employee.all.geography, ce.share.employee.all.mapc, 
                ce.all.sales.total, ce.all.sole.prop.count, ce.all.female.owned)

####### SUMMARY STAT: NUMBER AND SHARE OF EACH CREATIVE INDUSTRY COMPARED TO ALL CREATIVE INDUSTRIES, BY SUBREGION 
# number and share of each creative industry
# dataaxle data
by.subregion.dataaxle.mapc <- dataaxle.nefa.core.mapc %>%
  dplyr::select("geography" = subregion, CREATIVE_CAT) %>%
  mutate("indicator" = rep(1, nrow(dataaxle.nefa.core.mapc))) %>%
  group_by(geography, CREATIVE_CAT) %>%
  summarise("estab" = sum(indicator)) %>%
  mutate("share" = estab / sum(estab),
         "percent" = percent(estab / sum(estab))) %>%
  dplyr::select(geography, CREATIVE_CAT, estab, share, percent)

# number of each creative industry
by.subregion.dataaxle.estab <- by.subregion.dataaxle.mapc %>%
  dplyr::select(geography, CREATIVE_CAT, estab) %>%
  spread(key = CREATIVE_CAT, value = estab)

by.subregion.dataaxle.estab[is.na(by.subregion.dataaxle.estab)] <- 0


# share of each creative industry amongst all creative industries
by.subregion.dataaxle.share <- by.subregion.dataaxle.mapc %>%
  dplyr::select(geography, CREATIVE_CAT, share) %>%
  spread(key = CREATIVE_CAT, value = share)

by.subregion.dataaxle.share[is.na(by.subregion.dataaxle.share)] <- 0



########################################## NEIGHBORHOOD ##########################################
#### SUMMARY STATISTICS, BY NEIGHBORHOOD AND NEFA CREATIVE INDUSTRY GROUP 
#### summary statistics, by neighborhood, using NEFA Core Creative Enterprise NAICS codes
summary.dataaxle.ce.nefa.core.nhood <- dataaxle.nefa.core.mapc %>%
  dplyr::select(OBJECTID, PRIMARY_CITY, nhood, ACTUAL_LOCATION_EMPLOYMENT_SIZE, ACTUAL_LOCATION_SALES_VOLUME,
                FEMALE_OWNER_EXEC) %>%
  group_by("geography" = nhood) %>%
  mutate(FEMALE_OWNER_EXEC = ifelse(FEMALE_OWNER_EXEC == "Y", 1, 0)) %>%   # change FEMALE_OWNER_EXEC to indicator variable (0 or 1) for summary stats
  summarise(ce.core.estab = n(),
            ce.core.employee.count = sum(ACTUAL_LOCATION_EMPLOYMENT_SIZE, na.rm = TRUE),
            ce.core.sales.total = sum(ACTUAL_LOCATION_SALES_VOLUME, na.rm = TRUE),
            ce.core.female.owned = mean(FEMALE_OWNER_EXEC),
            ce.core.sole.prop.count = sum(ACTUAL_LOCATION_EMPLOYMENT_SIZE == 1))

# add column for the total number of establishments and employees for all industries in each neighborhood
estab.count.nhood = dataaxle.mapc.towns %>%
  group_by("geography" = nhood) %>%
  mutate(all.estab = n()) %>%
  dplyr::select(geography, all.estab) %>%
  filter(!duplicated(geography))

employee.count.nhood = dataaxle.mapc.towns %>%
  group_by("geography" = nhood) %>%
  summarize(all.employee.count = sum(ACTUAL_LOCATION_EMPLOYMENT_SIZE))

estab.employee.count.nhood = left_join(estab.count.nhood, employee.count.nhood, by = 'geography')

summary.dataaxle.ce.nefa.core.nhood.temp <- left_join(summary.dataaxle.ce.nefa.core.nhood, 
                                                     estab.employee.count.nhood, by = "geography")

summary.dataaxle.ce.nefa.core.nhood.df <- summary.dataaxle.ce.nefa.core.nhood.temp %>%
  mutate(share.all.mapc = round(all.estab / sum(all.estab), digits = 3),
         ce.share.core.geography = round(ce.core.estab / all.estab, digits = 3),
         ce.share.core.geography.perc = percent(ce.share.core.geography),
         ce.share.core.mapc = round(ce.core.estab / sum(ce.core.estab), digits = 3),
         ce.share.core.mapc.perc = percent(ce.share.core.mapc),
         share.all.employee.mapc = round(all.employee.count / sum(all.employee.count), digits = 3),
         ce.share.employee.core.geography = round(ce.core.employee.count / all.employee.count, digits = 3),
         ce.share.employee.core.mapc = round(ce.core.employee.count / sum(ce.core.employee.count), digits = 3)) %>%
  dplyr::select(geography,  
                all.estab, share.all.mapc, ce.core.estab, ce.share.core.geography, ce.share.core.geography.perc, 
                ce.share.core.mapc, ce.share.core.mapc.perc, all.employee.count, share.all.employee.mapc,
                ce.core.employee.count, ce.share.employee.core.geography, 
                ce.share.employee.core.mapc, ce.core.sales.total, ce.core.sole.prop.count, ce.core.female.owned)


#### summary statistics, by municipality, using NEFA All (Core + Peripheral) Creative Enterprise NAICS codes
summary.dataaxle.ce.nefa.all.nhood <- dataaxle.nefa.all.mapc %>%
  dplyr::select(OBJECTID, PRIMARY_CITY, nhood, ACTUAL_LOCATION_EMPLOYMENT_SIZE, ACTUAL_LOCATION_SALES_VOLUME,
                FEMALE_OWNER_EXEC) %>%
  group_by("geography" = nhood) %>%
  mutate(FEMALE_OWNER_EXEC = ifelse(FEMALE_OWNER_EXEC == "Y", 1, 0)) %>%   # change FEMALE_OWNER_EXEC to indicator variable (0 or 1) for summary stats
  summarise(ce.all.estab = n(),
            ce.all.employee.count = sum(ACTUAL_LOCATION_EMPLOYMENT_SIZE, na.rm = TRUE),
            ce.all.sales.total = sum(ACTUAL_LOCATION_SALES_VOLUME, na.rm = TRUE),
            ce.all.female.owned = mean(FEMALE_OWNER_EXEC),
            ce.all.sole.prop.count = sum(ACTUAL_LOCATION_EMPLOYMENT_SIZE == 1))

# add column for the total number of establishments and employees for all industries in each neighborhood
summary.dataaxle.ce.nefa.all.nhood.temp <- left_join(summary.dataaxle.ce.nefa.all.nhood, 
                                                    estab.employee.count.nhood, by = "geography")

summary.dataaxle.ce.nefa.all.nhood.df <- summary.dataaxle.ce.nefa.all.nhood.temp %>%
  mutate(share.all.mapc = round(all.estab / sum(all.estab), digits = 3),
         ce.share.all.geography = round(ce.all.estab / all.estab, digits = 3),
         ce.share.all.geography.perc = percent(ce.share.all.geography),
         ce.share.all.mapc = round(ce.all.estab / sum(ce.all.estab), digits = 3),
         ce.share.all.mapc.perc = percent(ce.share.all.mapc),
         share.all.employee.mapc = round(all.employee.count / sum(all.employee.count), digits = 3),
         ce.share.employee.all.geography = round(ce.all.employee.count / all.employee.count, digits = 3),
         ce.share.employee.all.mapc = round(ce.all.employee.count / sum(ce.all.employee.count), digits = 3)) %>%
  dplyr::select(geography, all.estab, share.all.mapc, ce.all.estab, ce.share.all.geography, ce.share.all.geography.perc,
                ce.share.all.mapc, ce.share.all.mapc.perc, all.employee.count, share.all.employee.mapc, 
                ce.all.employee.count, ce.share.employee.all.geography, ce.share.employee.all.mapc, 
                ce.all.sales.total, ce.all.sole.prop.count, ce.all.female.owned)


####### SUMMARY STAT: NUMBER AND SHARE OF EACH CREATIVE INDUSTRY COMPARED TO ALL CREATIVE INDUSTRIES, BY NEIGHBORHOOD
# number and share of each creative industry
# dataaxle data
by.nhood.dataaxle.nhood <- dataaxle.nefa.core.mapc %>%
  dplyr::select("geography" = nhood, CREATIVE_CAT) %>%
  mutate("indicator" = rep(1, nrow(dataaxle.nefa.core.mapc))) %>%
  group_by(geography, CREATIVE_CAT) %>%
  summarise("estab" = sum(indicator)) %>%
  mutate("share" = estab / sum(estab),
         "percent" = percent(estab / sum(estab))) %>%
  dplyr::select(geography, CREATIVE_CAT, estab, share, percent)

# number of each creative industry
by.nhood.dataaxle.estab <- by.nhood.dataaxle.nhood %>%
  dplyr::select(geography, CREATIVE_CAT, estab) %>%
  spread(key = CREATIVE_CAT, value = estab)

by.nhood.dataaxle.estab[is.na(by.nhood.dataaxle.estab$geography),2:ncol(by.nhood.dataaxle.estab)] <- 0


# share of each creative industry amongst all creative industries
by.nhood.dataaxle.share <- by.nhood.dataaxle.nhood %>%
  dplyr::select(geography, CREATIVE_CAT, share) %>%
  spread(key = CREATIVE_CAT, value = share)

by.nhood.dataaxle.share[is.na(by.nhood.dataaxle.share$geography),2:ncol(by.nhood.dataaxle.share)] <- 0


########################################## CENSUS TRACT ##########################################
####### SUMMARY STATISTICS, BY CENSUS TRACT AND NEFA CREATIVE INDUSTRY GROUP 
#### summary statistics, by census tract, using NEFA Core Creative Enterprise NAICS codes
summary.dataaxle.ce.nefa.core.ct <- dataaxle.nefa.core.mapc %>%
  dplyr::select(OBJECTID, PRIMARY_CITY, ct10_id, ACTUAL_LOCATION_EMPLOYMENT_SIZE, ACTUAL_LOCATION_SALES_VOLUME,
                FEMALE_OWNER_EXEC) %>%
  group_by("geography" = ct10_id) %>%
  mutate(FEMALE_OWNER_EXEC = ifelse(FEMALE_OWNER_EXEC == "Y", 1, 0)) %>%   # change FEMALE_OWNER_EXEC to indicator variable (0 or 1) for summary stats
  summarise(ce.core.estab = n(),
            ce.core.employee.count = sum(ACTUAL_LOCATION_EMPLOYMENT_SIZE, na.rm = TRUE),
            ce.core.sales.total = sum(ACTUAL_LOCATION_SALES_VOLUME, na.rm = TRUE),
            ce.core.female.owned = mean(FEMALE_OWNER_EXEC),
            ce.core.sole.prop.count = sum(ACTUAL_LOCATION_EMPLOYMENT_SIZE == 1))

# add column for the total number of establishments and employees for all industries in each census tract
estab.count.ct = dataaxle.mapc.towns %>%
  group_by("geography" = ct10_id) %>%
  mutate(all.estab = n()) %>%
  dplyr::select(geography, all.estab) %>%
  filter(!duplicated(geography))

employee.count.ct = dataaxle.mapc.towns %>%
  group_by("geography" = ct10_id) %>%
  summarize(all.employee.count = sum(ACTUAL_LOCATION_EMPLOYMENT_SIZE))

estab.employee.count.ct = left_join(estab.count.ct, employee.count.ct, by = 'geography')

summary.dataaxle.ce.nefa.core.ct.temp <- left_join(summary.dataaxle.ce.nefa.core.ct, 
                                                  estab.employee.count.ct, by = "geography")

summary.dataaxle.ce.nefa.core.ct.df <- summary.dataaxle.ce.nefa.core.ct.temp %>%
  mutate(share.all.mapc = round(all.estab / sum(all.estab), digits = 3),
         ce.share.core.geography = round(ce.core.estab / all.estab, digits = 3),
         ce.share.core.geography.perc = percent(ce.share.core.geography),
         ce.share.core.mapc = round(ce.core.estab / sum(ce.core.estab), digits = 3),
         ce.share.core.mapc.perc = percent(ce.share.core.mapc),
         share.all.employee.mapc = round(all.employee.count / sum(all.employee.count), digits = 3),
         ce.share.employee.core.geography = round(ce.core.employee.count / all.employee.count, digits = 3),
         ce.share.employee.core.mapc = round(ce.core.employee.count / sum(ce.core.employee.count), digits = 3)) %>%
  dplyr::select(geography,  
                all.estab, share.all.mapc, ce.core.estab, ce.share.core.geography, ce.share.core.geography.perc, 
                ce.share.core.mapc, ce.share.core.mapc.perc, all.employee.count, share.all.employee.mapc,
                ce.core.employee.count, ce.share.employee.core.geography, 
                ce.share.employee.core.mapc, ce.core.sales.total, ce.core.sole.prop.count, ce.core.female.owned)


#### summary statistics, by census tract, using NEFA All (Core + Peripheral) Creative Enterprise NAICS codes
summary.dataaxle.ce.nefa.all.ct <- dataaxle.nefa.all.mapc %>%
  dplyr::select(OBJECTID, PRIMARY_CITY, ct10_id, ACTUAL_LOCATION_EMPLOYMENT_SIZE, ACTUAL_LOCATION_SALES_VOLUME,
                FEMALE_OWNER_EXEC) %>%
  group_by("geography" = ct10_id) %>%
  mutate(FEMALE_OWNER_EXEC = ifelse(FEMALE_OWNER_EXEC == "Y", 1, 0)) %>%   # change FEMALE_OWNER_EXEC to indicator variable (0 or 1) for summary stats
  summarise(ce.all.estab = n(),
            ce.all.employee.count = sum(ACTUAL_LOCATION_EMPLOYMENT_SIZE, na.rm = TRUE),
            ce.all.sales.total = sum(ACTUAL_LOCATION_SALES_VOLUME, na.rm = TRUE),
            ce.all.female.owned = mean(FEMALE_OWNER_EXEC),
            ce.all.sole.prop.count = sum(ACTUAL_LOCATION_EMPLOYMENT_SIZE == 1))

# add column for the total number of establishments and employees for all industries in each census tract
summary.dataaxle.ce.nefa.all.ct.temp <- left_join(summary.dataaxle.ce.nefa.all.ct, estab.employee.count.ct, by = "geography")

summary.dataaxle.ce.nefa.all.ct.df <- summary.dataaxle.ce.nefa.all.ct.temp %>%
  mutate(share.all.mapc = round(all.estab / sum(all.estab), digits = 3),
         ce.share.all.geography = round(ce.all.estab / all.estab, digits = 3),
         ce.share.all.geography.perc = percent(ce.share.all.geography),
         ce.share.all.mapc = round(ce.all.estab / sum(ce.all.estab), digits = 3),
         ce.share.all.mapc.perc = percent(ce.share.all.mapc),
         share.all.employee.mapc = round(all.employee.count / sum(all.employee.count), digits = 3),
         ce.share.employee.all.geography = round(ce.all.employee.count / all.employee.count, digits = 3),
         ce.share.employee.all.mapc = round(ce.all.employee.count / sum(ce.all.employee.count), digits = 3)) %>%
  dplyr::select(geography, all.estab, share.all.mapc, ce.all.estab, ce.share.all.geography, ce.share.all.geography.perc,
                ce.share.all.mapc, ce.share.all.mapc.perc, all.employee.count, share.all.employee.mapc, 
                ce.all.employee.count, ce.share.employee.all.geography, ce.share.employee.all.mapc, 
                ce.all.sales.total, ce.all.sole.prop.count, ce.all.female.owned)


####### SUMMARY STAT: NUMBER AND SHARE OF EACH CREATIVE INDUSTRY COMPARED TO ALL CREATIVE INDUSTRIES, BY CENSUS TRACT 
# number and share of each creative industry
# dataaxle data
by.ct.dataaxle.ct <- dataaxle.nefa.core.mapc %>%
  dplyr::select("geography" = ct10_id, CREATIVE_CAT) %>%
  mutate("indicator" = rep(1, nrow(dataaxle.nefa.core.mapc))) %>%
  group_by(geography, CREATIVE_CAT) %>%
  summarise("estab" = sum(indicator)) %>%
  mutate("share" = estab / sum(estab),
         "percent" = percent(estab / sum(estab))) %>%
  dplyr::select(geography, CREATIVE_CAT, estab, share, percent)

# number of each creative industry
by.ct.dataaxle.estab <- by.ct.dataaxle.ct %>%
  dplyr::select(geography, CREATIVE_CAT, estab) %>%
  spread(key = CREATIVE_CAT, value = estab)

by.ct.dataaxle.estab[is.na(by.ct.dataaxle.estab)] <- 0


# share of each creative industry amongst all creative industries
by.ct.dataaxle.share <- by.ct.dataaxle.ct %>%
  dplyr::select(geography, CREATIVE_CAT, share) %>%
  spread(key = CREATIVE_CAT, value = share)

by.ct.dataaxle.share[is.na(by.ct.dataaxle.share)] <- 0


########################################## BLOCK GROUP ##########################################
####### SUMMARY STATISTICS, BY BLOCK GROUP AND NEFA CREATIVE INDUSTRY GROUP 
#### summary statistics, by block group, using NEFA Core Creative Enterprise NAICS codes
summary.dataaxle.ce.nefa.core.bg <- dataaxle.nefa.core.mapc %>%
  dplyr::select(OBJECTID, PRIMARY_CITY, bg10_id, ACTUAL_LOCATION_EMPLOYMENT_SIZE, ACTUAL_LOCATION_SALES_VOLUME,
                FEMALE_OWNER_EXEC) %>%
  group_by("geography" = bg10_id) %>%
  mutate(FEMALE_OWNER_EXEC = ifelse(FEMALE_OWNER_EXEC == "Y", 1, 0)) %>%   # change FEMALE_OWNER_EXEC to indicator variable (0 or 1) for summary stats
  summarise(ce.core.estab = n(),
            ce.core.employee.count = sum(ACTUAL_LOCATION_EMPLOYMENT_SIZE, na.rm = TRUE),
            ce.core.sales.total = sum(ACTUAL_LOCATION_SALES_VOLUME, na.rm = TRUE),
            ce.core.female.owned = mean(FEMALE_OWNER_EXEC),
            ce.core.sole.prop.count = sum(ACTUAL_LOCATION_EMPLOYMENT_SIZE == 1))

# add column for the total number of establishments and employees for all industries in each block group
estab.count.bg = dataaxle.mapc.towns %>%
  group_by("geography" = bg10_id) %>%
  mutate(all.estab = n()) %>%
  dplyr::select(geography, all.estab) %>%
  filter(!duplicated(geography))

employee.count.bg = dataaxle.mapc.towns %>%
  group_by("geography" = bg10_id) %>%
  summarize(all.employee.count = sum(ACTUAL_LOCATION_EMPLOYMENT_SIZE))

estab.employee.count.bg = left_join(estab.count.bg, employee.count.bg, by = 'geography')

summary.dataaxle.ce.nefa.core.bg.temp <- left_join(summary.dataaxle.ce.nefa.core.bg, estab.employee.count.bg, by = "geography")

summary.dataaxle.ce.nefa.core.bg.df <- summary.dataaxle.ce.nefa.core.bg.temp %>%
  mutate(share.all.mapc = round(all.estab / sum(all.estab), digits = 3),
         ce.share.core.geography = round(ce.core.estab / all.estab, digits = 3),
         ce.share.core.geography.perc = percent(ce.share.core.geography),
         ce.share.core.mapc = round(ce.core.estab / sum(ce.core.estab), digits = 3),
         ce.share.core.mapc.perc = percent(ce.share.core.mapc),
         share.all.employee.mapc = round(all.employee.count / sum(all.employee.count), digits = 3),
         ce.share.employee.core.geography = round(ce.core.employee.count / all.employee.count, digits = 3),
         ce.share.employee.core.mapc = round(ce.core.employee.count / sum(ce.core.employee.count), digits = 3)) %>%
  dplyr::select(geography,  
                all.estab, share.all.mapc, ce.core.estab, ce.share.core.geography, ce.share.core.geography.perc, 
                ce.share.core.mapc, ce.share.core.mapc.perc, all.employee.count, share.all.employee.mapc,
                ce.core.employee.count, ce.share.employee.core.geography, 
                ce.share.employee.core.mapc, ce.core.sales.total, ce.core.sole.prop.count, ce.core.female.owned)


#### summary statistics, by municipality, using NEFA All (Core + Peripheral) Creative Enterprise NAICS codes
summary.dataaxle.ce.nefa.all.bg <- dataaxle.nefa.all.mapc %>%
  dplyr::select(OBJECTID, PRIMARY_CITY, bg10_id, ACTUAL_LOCATION_EMPLOYMENT_SIZE, ACTUAL_LOCATION_SALES_VOLUME,
                FEMALE_OWNER_EXEC) %>%
  group_by("geography" = bg10_id) %>%
  mutate(FEMALE_OWNER_EXEC = ifelse(FEMALE_OWNER_EXEC == "Y", 1, 0)) %>%   # change FEMALE_OWNER_EXEC to indicator variable (0 or 1) for summary stats
  summarise(ce.all.estab = n(),
            ce.all.employee.count = sum(ACTUAL_LOCATION_EMPLOYMENT_SIZE, na.rm = TRUE),
            ce.all.sales.total = sum(ACTUAL_LOCATION_SALES_VOLUME, na.rm = TRUE),
            ce.all.female.owned = mean(FEMALE_OWNER_EXEC),
            ce.all.sole.prop.count = sum(ACTUAL_LOCATION_EMPLOYMENT_SIZE == 1))

# add column for the total number of establishments and employees for all industries in each block group
summary.dataaxle.ce.nefa.all.bg.temp <- left_join(summary.dataaxle.ce.nefa.all.bg, estab.employee.count.bg, by = "geography")

summary.dataaxle.ce.nefa.all.bg.df <- summary.dataaxle.ce.nefa.all.bg.temp %>%
  mutate(share.all.mapc = round(all.estab / sum(all.estab), digits = 3),
         ce.share.all.geography = round(ce.all.estab / all.estab, digits = 3),
         ce.share.all.geography.perc = percent(ce.share.all.geography),
         ce.share.all.mapc = round(ce.all.estab / sum(ce.all.estab), digits = 3),
         ce.share.all.mapc.perc = percent(ce.share.all.mapc),
         share.all.employee.mapc = round(all.employee.count / sum(all.employee.count), digits = 3),
         ce.share.employee.all.geography = round(ce.all.employee.count / all.employee.count, digits = 3),
         ce.share.employee.all.mapc = round(ce.all.employee.count / sum(ce.all.employee.count), digits = 3)) %>%
  dplyr::select(geography, all.estab, share.all.mapc, ce.all.estab, ce.share.all.geography, ce.share.all.geography.perc,
                ce.share.all.mapc, ce.share.all.mapc.perc, all.employee.count, share.all.employee.mapc, 
                ce.all.employee.count, ce.share.employee.all.geography, ce.share.employee.all.mapc, 
                ce.all.sales.total, ce.all.sole.prop.count, ce.all.female.owned)


####### SUMMARY STAT: NUMBER AND SHARE OF EACH CREATIVE INDUSTRY COMPARED TO ALL CREATIVE INDUSTRIES, BY BLOCK GROUP 
# number and share of each creative industry
# dataaxle data
by.bg.dataaxle.bg <- dataaxle.nefa.core.mapc %>%
  dplyr::select("geography" = bg10_id, CREATIVE_CAT) %>%
  mutate("indicator" = rep(1, nrow(dataaxle.nefa.core.mapc))) %>%
  group_by(geography, CREATIVE_CAT) %>%
  summarise("estab" = sum(indicator)) %>%
  mutate("share" = estab / sum(estab),
         "percent" = percent(estab / sum(estab))) %>%
  dplyr::select(geography, CREATIVE_CAT, estab, share, percent)

# number of each creative industry
by.bg.dataaxle.estab <- by.bg.dataaxle.bg %>%
  dplyr::select(geography, CREATIVE_CAT, estab) %>%
  spread(key = CREATIVE_CAT, value = estab)

by.bg.dataaxle.estab[is.na(by.bg.dataaxle.estab)] <- 0


# share of each creative industry amongst all creative industries
by.bg.dataaxle.share <- by.bg.dataaxle.bg %>%
  dplyr::select(geography, CREATIVE_CAT, share) %>%
  spread(key = CREATIVE_CAT, value = share)

by.bg.dataaxle.share[is.na(by.bg.dataaxle.share)] <- 0



######## BAR GRAPH: Creative Industry Groups establishment counts, dataaxle data ########
setwd(output.path)
ggplot(summary.dataaxle.ce.nefa.core.categories.mass.df, aes(x = reorder(CREATIVE_CAT, ce.core.estab), y = ce.core.estab)) + 
  geom_bar(stat = "identity", fill = "steelblue", color = "black") +
  labs(title = "Massachusetts Creative Economy Establishment Counts by Industry Group, 2016 (dataaxle)",
       y = "Firms", x = "") +
  geom_text(aes(label=ce.core.estab), hjust = -0.2, size=3.5) +
  theme_minimal() +
  coord_flip() +
  theme(plot.margin = unit(c(1,3,1,1), "cm"),
        axis.text.y= element_text(size=15))
ggsave(file="mass_ce_establishment-counts_industry-group_dataaxle_processed.png", width = 14, height = 8, units = 'in')

ggplot(summary.dataaxle.ce.nefa.core.categories.mapc.df, aes(x = reorder(CREATIVE_CAT, ce.core.estab), y = ce.core.estab)) + 
  geom_bar(stat = "identity", fill = "steelblue", color = "black") +
  labs(title = "MAPC Creative Economy Establishment Counts by Industry Group, 2016 (dataaxle)",
       y = "Firms", x = "") +
  geom_text(aes(label=ce.core.estab), hjust = -0.2, size=3.5) +
  theme_minimal() +
  coord_flip() +
  theme(plot.margin = unit(c(1,3,1,1), "cm"),
        axis.text.y  = element_text(size=15))
ggsave(file="mapc_ce_establishment-counts_industry-group_dataaxle_processed.png", width = 14, height = 8, units = 'in')

ggplot(summary.dataaxle.ce.nefa.core.categories.mapc.df, aes(x = reorder(CREATIVE_CAT, cat.core.estab.share), y = cat.core.estab.share)) + 
  geom_bar(stat = "identity", fill = "steelblue", color = "black") +
  labs(title = "MAPC Creative Economy Establishment Share by Industry Group, 2016 (dataaxle)",
       y = "", x = "") +
  geom_text(aes(label=cat.core.estab.share.perc), hjust = -0.2, size=3.5) +
  theme_minimal() +
  coord_flip() +
  theme(plot.margin = unit(c(1,0,1,0), "cm"),
        axis.text.y  = element_text(size=11),
        axis.text.x = element_blank(),
        panel.grid = element_blank())
ggsave(file="mapc_ce_establishment-share_industry-group_dataaxle_processed.png", width = 14, height = 8, units = 'in')

ranked.muni.core.estab <- summary.dataaxle.ce.nefa.core.municipality.df$geography [order(summary.dataaxle.ce.nefa.core.municipality.df$ce.core.estab, decreasing = TRUE)]

ggplot(data = subset(summary.dataaxle.ce.nefa.core.municipality.df,
                     geography %in% ranked.muni.core.estab[1:10]), 
       aes(x = reorder(geography, ce.core.estab), y = ce.core.estab)) + 
  geom_bar(stat = "identity", fill = "steelblue", color = "black") +
  labs(title = "Top Ten MAPC Municipalities by Core Creative Establishment Count",
       y = "Firms", x = "") +
  geom_text(aes(label=ce.core.estab), hjust = -0.2, size=3.5) +
  theme_minimal() +
  coord_flip() +
  theme(plot.margin = unit(c(1,3,1,1), "cm"),
        axis.text.y  = element_text(size=15))
ggsave(file="mapc_ce_establishment_counts_top_ten_municipalities_dataaxle_processed.png", width = 14, height = 8, units = 'in')


ranked.muni.core.share <- summary.dataaxle.ce.nefa.core.municipality.df$geography [order((summary.dataaxle.ce.nefa.core.municipality.df$ce.share.core.geography), decreasing = TRUE)]

ggplot(data = subset(summary.dataaxle.ce.nefa.core.municipality.df,
                     geography %in% ranked.muni.core.share[1:10]), 
       aes(x = reorder(geography, ce.share.core.geography), y = ce.share.core.geography)) + 
  geom_bar(stat = "identity", fill = "steelblue", color = "black") +
  labs(title = "Top Ten MAPC Municipalities by Core Creative Establishment Share (of All Establishments)",
       y = "", x = "") +
  geom_text(aes(label=ce.share.core.geography.perc), hjust = -0.2, size=3.5) +
  theme_minimal() +
  coord_flip() +
  theme(plot.margin = unit(c(1,0,1,0), "cm"),
        axis.text.y  = element_text(size=11),
        axis.text.x = element_blank(),
        panel.grid = element_blank())
ggsave(file="mapc_ce_establishment_share_top_ten_municipalities_share_dataaxle_processed.png", width = 14, height = 8, units = 'in')

