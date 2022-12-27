########### ONLY FOR MAPC MUNICIPALITIES ############

rm(list=ls())

# install.packages('pacman')
pacman::p_load(tidyverse, reshape2, scales, foreign)

#update with years
analysis.path <- "K:/DataServices/Projects/Current_Projects/Arts_and_Culture_Planning/Creative_Economy/R code/git/creative-economy/3_Analysis"
modified.data.path <- "K:/DataServices/Projects/Current_Projects/Arts_and_Culture_Planning/Creative_Economy/Data/Modified/2022"

setwd(analysis.path)
source("01_summary_stats.R")

####### EXCEL SPREADSHEET #######

summary.dataaxle.ce.all.variables.muni <- c("geography", 
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

summary.dataaxle.ce.all.variables.nhood <- c("geography", 
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

summary.dataaxle.ce.all.variables.ct <- c("geography", 
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

summary.dataaxle.ce.all.variables.bg <- c("geography", 
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

summary.dataaxle.ce.core.variables.muni <- c("geography",
                                            "ce.core.estab",
                                            "ce.share.core.geography",
                                            "ce.share.core.mapc",
                                            "ce.core.employee.count",
                                            "ce.share.employee.core.geography",
                                            "ce.share.employee.core.mapc",
                                            "ce.core.sales.total",
                                            "ce.core.sole.prop.count")

summary.dataaxle.ce.core.variables.nhood <- c("geography",
                                             "ce.core.estab",
                                             "ce.share.core.geography",
                                             "ce.share.core.mapc",
                                             "ce.core.employee.count",
                                             "ce.share.employee.core.geography",
                                             "ce.share.employee.core.mapc",
                                             "ce.core.sales.total",
                                             "ce.core.sole.prop.count")

summary.dataaxle.ce.core.variables.ct <- c("geography",
                                          "ce.core.estab",
                                          "ce.share.core.geography",
                                          "ce.share.core.mapc",
                                          "ce.core.employee.count",
                                          "ce.share.employee.core.geography",
                                          "ce.share.employee.core.mapc",
                                          "ce.core.sales.total",
                                          "ce.core.sole.prop.count")

summary.dataaxle.ce.core.variables.bg <- c("geography",
                                          "ce.core.estab",
                                          "ce.share.core.geography",
                                          "ce.share.core.mapc",
                                          "ce.core.employee.count",
                                          "ce.share.employee.core.geography",
                                          "ce.share.employee.core.mapc",
                                          "ce.core.sales.total",
                                          "ce.core.sole.prop.count")


summary.dataaxle.ce.muni <- left_join(summary.dataaxle.ce.nefa.all.municipality.df[,summary.dataaxle.ce.all.variables.muni],
                                     summary.dataaxle.ce.nefa.core.municipality.df[,summary.dataaxle.ce.core.variables.muni],
                                     by = "geography") %>%
  left_join(by.town.dataaxle.share, by = "geography") %>%
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


summary.dataaxle.ce.nhood <- left_join(summary.dataaxle.ce.nefa.all.nhood.df[,summary.dataaxle.ce.all.variables.nhood],
                                      summary.dataaxle.ce.nefa.core.nhood.df[,summary.dataaxle.ce.core.variables.nhood],
                                      by = "geography") %>%
  left_join(by.nhood.dataaxle.share, by = "geography") %>%
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

summary.dataaxle.ce.ct <- left_join(summary.dataaxle.ce.nefa.all.ct.df[,summary.dataaxle.ce.all.variables.ct],
                                   summary.dataaxle.ce.nefa.core.ct.df[,summary.dataaxle.ce.core.variables.ct],
                                   by = "geography") %>%
  left_join(by.ct.dataaxle.share, by = "geography") %>%
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
         ce.share.employee.core.mapc = round((ce.core.employee.count / sum(ce.core.employee.count))*100, digits = 2)) 

summary.dataaxle.ce.bg <- left_join(summary.dataaxle.ce.nefa.all.bg.df[,summary.dataaxle.ce.all.variables.bg],
                                   summary.dataaxle.ce.nefa.core.bg.df[,summary.dataaxle.ce.core.variables.bg],
                                   by = "geography") %>%
  left_join(by.bg.dataaxle.share, by = "geography") %>%
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
         ce.share.employee.core.mapc = round((ce.core.employee.count / sum(ce.core.employee.count))*100, digits = 2))

summary.dataaxle.ce.final <- rbind(summary.dataaxle.ce.muni,
                                  summary.dataaxle.ce.nhood,
                                  summary.dataaxle.ce.ct,
                                  summary.dataaxle.ce.bg)

setwd(modified.data.path)
write.csv(summary.dataaxle.ce.final, "summary_dataaxle_ce_final_mapc_processed_all_geographies.csv", row.names = FALSE)

