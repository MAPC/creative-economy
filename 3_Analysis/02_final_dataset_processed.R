########### ONLY FOR MAPC MUNICIPALITIES ############

rm(list=ls())

# install.packages('pacman')
pacman::p_load(tidyr, dplyr, reshape2, scales, foreign)

#update with years
analysis.path <- "K:/DataServices/Projects/Current_Projects/Arts_and_Culture_Planning/Creative_Economy/R code/git/creative-economy/3_Analysis"
modified.data.path <- "K:/DataServices/Projects/Current_Projects/Arts_and_Culture_Planning/Creative_Economy/Data/Modified/2022"
output.path <- "K:/DataServices/Projects/Current_Projects/Arts_and_Culture_Planning/Creative_Economy/Output/2022"


setwd(analysis.path)
source("01_summary_stats.R")

####### EXCEL SPREADSHEET #######

summary.infousa.ce.all.variables <- c("geography", 
                                      "all.estab",
                                      "share.all.mapc", 
                                      "ce.all.estab",
                                      "ce.share.all.geography",
                                      "ce.share.all.mapc",
                                      "all.employee.count", 
                                      "share.all.employee.mapc",
                                      "ce.all.employee.count",
                                      "ce.share.employee.all.geography",
                                      "ce.share.employee.all.mapc",
                                      "ce.all.sales.total",
                                      "ce.all.sole.prop.count")

summary.infousa.ce.core.variables <- c("geography",
                                       "ce.core.estab",
                                       "ce.share.core.geography",
                                       "ce.share.core.mapc",
                                       "ce.core.employee.count",
                                       "ce.share.employee.core.geography",
                                       "ce.share.employee.core.mapc",
                                       "ce.core.sales.total",
                                       "ce.core.sole.prop.count")


summary.infousa.ce.mass <- left_join(summary.infousa.ce.nefa.all.mass.df[,summary.infousa.ce.all.variables],
                                     summary.infousa.ce.nefa.core.mass.df[,summary.infousa.ce.core.variables],
                                     by = "geography") %>%
  left_join(by.mass.infousa.share, by = "geography") %>%
  mutate(share.all.mapc = NA,
         ce.share.all.geography = round((ce.all.estab / all.estab)*100, digits = 2),
         ce.share.all.mapc = NA,
         share.all.employee.mapc = NA,
         ce.share.employee.all.geography = round((ce.all.employee.count / all.employee.count)*100, digits = 2),
         ce.share.employee.all.mapc = NA,                   
         ce.share.core.geography = round((ce.core.estab / all.estab)*100, digits = 2),
         ce.share.core.mapc = NA,                          
         ce.share.employee.core.geography = round((ce.core.employee.count / all.employee.count)*100, digits = 2),
         ce.share.employee.core.mapc = NA)

summary.infousa.ce.mapc <- left_join(summary.infousa.ce.nefa.all.mapc.df[,summary.infousa.ce.all.variables],
                                     summary.infousa.ce.nefa.core.mapc.df[,summary.infousa.ce.core.variables],
                                     by = "geography") %>%
  left_join(by.mapc.infousa.share, by = "geography") %>%
  mutate(share.all.mapc = round((all.estab / sum(all.estab))*100, digits = 2),
         ce.share.all.geography = round((ce.all.estab / all.estab)*100, digits = 2),
         ce.share.all.mapc = round((ce.all.estab / sum(ce.all.estab))*100, digits = 2),
         share.all.employee.mapc = round((all.employee.count / sum(all.employee.count))*100, digits = 2),
         ce.share.employee.all.geography = round((ce.all.employee.count / all.employee.count)*100, digits = 2),
         ce.share.employee.all.mapc = round((ce.all.employee.count / sum(ce.all.employee.count))*100, digits = 2) ,                   
         ce.share.core.geography = round((ce.core.estab / all.estab)*100, digits = 2),
         ce.share.core.mapc = round((ce.core.estab / sum(ce.core.estab))*100, digits = 2),                          
         ce.share.employee.core.geography = round((ce.core.employee.count / all.employee.count)*100, digits = 2),
         ce.share.employee.core.mapc = round((ce.core.employee.count / sum(ce.core.employee.count))*100, digits = 2)) %>%
  dplyr::select(-mapc) 

summary.infousa.ce.muni <- left_join(summary.infousa.ce.nefa.all.municipality.df[,summary.infousa.ce.all.variables],
                                     summary.infousa.ce.nefa.core.municipality.df[,summary.infousa.ce.core.variables],
                                     by = "geography") %>%
  left_join(by.town.infousa.share, by = "geography") %>%
  mutate(share.all.mapc = round((all.estab / sum(all.estab))*100, digits = 2),
         ce.share.all.geography = round((ce.all.estab / all.estab)*100, digits = 2),
         ce.share.all.mapc = round((ce.all.estab / sum(ce.all.estab))*100, digits = 2),
         share.all.employee.mapc = round((all.employee.count / sum(all.employee.count))*100, digits = 2),
         ce.share.employee.all.geography = round((ce.all.employee.count / all.employee.count)*100, digits = 2),
         ce.share.employee.all.mapc = round((ce.all.employee.count / sum(ce.all.employee.count))*100, digits = 2) ,                   
         ce.share.core.geography = round((ce.core.estab / all.estab)*100, digits = 2),
         ce.share.core.mapc = round((ce.core.estab / sum(ce.core.estab))*100, digits = 2),                          
         ce.share.employee.core.geography = round((ce.core.employee.count / all.employee.count)*100, digits = 2),
         ce.share.employee.core.mapc = round((ce.core.employee.count / sum(ce.core.employee.count))*100, digits = 2)) 

summary.infousa.ce.subregion <- left_join(summary.infousa.ce.nefa.all.subregion.df[,summary.infousa.ce.all.variables],
                                          summary.infousa.ce.nefa.core.subregion.df[,summary.infousa.ce.core.variables],
                                          by = "geography") %>%
  left_join(by.subregion.infousa.share, by = "geography") %>%
  mutate(share.all.mapc = round((all.estab / sum(all.estab))*100, digits = 2),
         ce.share.all.geography = round((ce.all.estab / all.estab)*100, digits = 2),
         ce.share.all.mapc = round((ce.all.estab / sum(ce.all.estab))*100, digits = 2),
         share.all.employee.mapc = round((all.employee.count / sum(all.employee.count))*100, digits = 2),
         ce.share.employee.all.geography = round((ce.all.employee.count / all.employee.count)*100, digits = 2),
         ce.share.employee.all.mapc = round((ce.all.employee.count / sum(ce.all.employee.count))*100, digits = 2) ,                   
         ce.share.core.geography = round((ce.core.estab / all.estab)*100, digits = 2),
         ce.share.core.mapc = round((ce.core.estab / sum(ce.core.estab))*100, digits = 2),                          
         ce.share.employee.core.geography = round((ce.core.employee.count / all.employee.count)*100, digits = 2),
         ce.share.employee.core.mapc = round((ce.core.employee.count / sum(ce.core.employee.count))*100, digits = 2)) 

summary.infousa.ce.commtype <- left_join(summary.infousa.ce.nefa.all.commtype.df[,summary.infousa.ce.all.variables],
                                         summary.infousa.ce.nefa.core.commtype.df[,summary.infousa.ce.core.variables],
                                         by = "geography") %>%
  left_join(by.comm.type.infousa.share, by = "geography") %>%
  mutate(share.all.mapc = round((all.estab / sum(all.estab))*100, digits = 2),
         ce.share.all.geography = round((ce.all.estab / all.estab)*100, digits = 2),
         ce.share.all.mapc = round((ce.all.estab / sum(ce.all.estab))*100, digits = 2),
         share.all.employee.mapc = round((all.employee.count / sum(all.employee.count))*100, digits = 2),
         ce.share.employee.all.geography = round((ce.all.employee.count / all.employee.count)*100, digits = 2),
         ce.share.employee.all.mapc = round((ce.all.employee.count / sum(ce.all.employee.count))*100, digits = 2) ,                   
         ce.share.core.geography = round((ce.core.estab / all.estab)*100, digits = 2),
         ce.share.core.mapc = round((ce.core.estab / sum(ce.core.estab))*100, digits = 2),                          
         ce.share.employee.core.geography = round((ce.core.employee.count / all.employee.count)*100, digits = 2),
         ce.share.employee.core.mapc = round((ce.core.employee.count / sum(ce.core.employee.count))*100, digits = 2)) 

summary.infousa.ce.nhood <- left_join(summary.infousa.ce.nefa.all.nhood.df[,summary.infousa.ce.all.variables],
                                      summary.infousa.ce.nefa.core.nhood.df[,summary.infousa.ce.core.variables],
                                      by = "geography") %>%
  left_join(by.nhood.infousa.share, by = "geography") %>%
  mutate(share.all.mapc = round((all.estab / sum(all.estab))*100, digits = 2),
         ce.share.all.geography = round((ce.all.estab / all.estab)*100, digits = 2),
         ce.share.all.mapc = round((ce.all.estab / sum(ce.all.estab))*100, digits = 2),
         share.all.employee.mapc = round((all.employee.count / sum(all.employee.count))*100, digits = 2),
         ce.share.employee.all.geography = round((ce.all.employee.count / all.employee.count)*100, digits = 2),
         ce.share.employee.all.mapc = round((ce.all.employee.count / sum(ce.all.employee.count))*100, digits = 2) ,                   
         ce.share.core.geography = round((ce.core.estab / all.estab)*100, digits = 2),
         ce.share.core.mapc = round((ce.core.estab / sum(ce.core.estab))*100, digits = 2),                          
         ce.share.employee.core.geography = round((ce.core.employee.count / all.employee.count)*100, digits = 2),
         ce.share.employee.core.mapc = round((ce.core.employee.count / sum(ce.core.employee.count))*100, digits = 2)) 

summary.infousa.ce.ct <- left_join(summary.infousa.ce.nefa.all.ct.df[,summary.infousa.ce.all.variables],
                                   summary.infousa.ce.nefa.core.ct.df[,summary.infousa.ce.core.variables],
                                   by = "geography") %>%
  left_join(by.ct.infousa.share, by = "geography") %>%
  mutate(ce.core.estab = ifelse(is.na(ce.core.estab), 0, ce.core.estab),
         ce.core.employee.count  = ifelse(is.na(ce.core.employee.count ), 0, ce.core.employee.count)) %>%
  mutate(share.all.mapc = round((all.estab / sum(all.estab))*100, digits = 2),
         ce.share.all.geography = round((ce.all.estab / all.estab)*100, digits = 2),
         ce.share.all.mapc = round((ce.all.estab / sum(ce.all.estab))*100, digits = 2),
         share.all.employee.mapc = round((all.employee.count / sum(all.employee.count))*100, digits = 2),
         ce.share.employee.all.geography = round((ce.all.employee.count / all.employee.count)*100, digits = 2),
         ce.share.employee.all.mapc = round((ce.all.employee.count / sum(ce.all.employee.count))*100, digits = 2) ,                   
         ce.share.core.geography = round((ce.core.estab / all.estab)*100, digits = 2),
         ce.share.core.mapc = round((ce.core.estab / sum(ce.core.estab))*100, digits = 2),                          
         ce.share.employee.core.geography = round((ce.core.employee.count / all.employee.count)*100, digits = 2),
         ce.share.employee.core.mapc = round((ce.core.employee.count / sum(ce.core.employee.count))*100, digits = 2)) %>%
  mutate(geography = as.character(geography))

summary.infousa.ce.bg <- left_join(summary.infousa.ce.nefa.all.bg.df[,summary.infousa.ce.all.variables],
                                   summary.infousa.ce.nefa.core.bg.df[,summary.infousa.ce.core.variables],
                                   by = "geography") %>%
  left_join(by.bg.infousa.share, by = "geography") %>%
  mutate(ce.core.estab = ifelse(is.na(ce.core.estab), 0, ce.core.estab),
         ce.core.employee.count  = ifelse(is.na(ce.core.employee.count ), 0, ce.core.employee.count)) %>%
  mutate(share.all.mapc = round((all.estab / sum(all.estab))*100, digits = 2),
         ce.share.all.geography = round((ce.all.estab / all.estab)*100, digits = 2),
         ce.share.all.mapc = round((ce.all.estab / sum(ce.all.estab))*100, digits = 2),
         share.all.employee.mapc = round((all.employee.count / sum(all.employee.count))*100, digits = 2),
         ce.share.employee.all.geography = round((ce.all.employee.count / all.employee.count)*100, digits = 2),
         ce.share.employee.all.mapc = round((ce.all.employee.count / sum(ce.all.employee.count))*100, digits = 2) ,                   
         ce.share.core.geography = round((ce.core.estab / all.estab)*100, digits = 2),
         ce.share.core.mapc = round((ce.core.estab / sum(ce.core.estab))*100, digits = 2),                          
         ce.share.employee.core.geography = round((ce.core.employee.count / all.employee.count)*100, digits = 2),
         ce.share.employee.core.mapc = round((ce.core.employee.count / sum(ce.core.employee.count))*100, digits = 2))%>%
  mutate(geography = as.character(geography))

summary.infousa.ce.final <- rbind(summary.infousa.ce.muni,
                                  summary.infousa.ce.mapc,
                                  summary.infousa.ce.mass,
                                  summary.infousa.ce.subregion,
                                  summary.infousa.ce.commtype, 
                                  summary.infousa.ce.nhood,
                                  summary.infousa.ce.ct,
                                  summary.infousa.ce.bg)

setwd(modified.data.path)
summary.infousa.ce.final$geography <- as.character(summary.infousa.ce.final$geography)
write.csv(summary.infousa.ce.final, paste0(output.path, "/20200430_summary_infousa_ce_final_mapc_processed_all_geographies.csv"), row.names = FALSE)

