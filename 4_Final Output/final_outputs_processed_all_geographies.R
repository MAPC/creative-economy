###################################################################################################
# Code: Final Output Code for All Geographies
# Purpose: This code uses the functions created in "final_outputs_functions_processed_all_geographies.R" 
#          to output data summaries and a map of core creative economy establishments
# Geographies: Block Group(s), Census Tract(s), Neighborhoods (where available), and Municipalities
# Author: Seleeke Flingai
# Date: April 2, 2019
# Updated: December 27, 2022 by RBowers
#
###################################################################################################

# clear workspace
rm(list=ls())
options(scipen = 999)
# load in the final municipal output functions R file
analysis.path <- "K:/DataServices/Projects/Current_Projects/Arts_and_Culture_Planning/Creative_Economy/R code/git/creative-economy/3_Analysis"
setwd(analysis.path)
source("04_final_municipal_output_functions_processed.R")

pacman::p_load(readxl)
#################################### MUNICIPAL #################################### 
# Place name of municipality in quotation marks for function below
# output is store in Output folder of data project
municipal.output("Dedham")


#################################### NEIGHBORHOOD #################################### 
# Place name of neighborhood in quotation marks for function below
# USE NEIGHBORHOOD LOOKUP TABLE IN THIS FOLDER FOR NEIGHBORHOOD NAMES: K:\DataServices\Projects\Current_Projects\Arts_and_Culture_Planning\Creative Economy\Input
# output is store in Output folder of data project
nhood.output("Ball Square", "Somerville")


#################################### CENSUS TRACT #################################### 
######## OPEN INPUT FILE ########

# open a dialog box that allows you to choose a file with addresses
file.to.load <- file.choose(new=TRUE)

# read the csv files into a variable (and do not read strings as factors)
location <- read_excel(file.to.load)


# Places the census tract vector and aggregated tract name and municipality from the loaded file into the function below
ct_vector = location$ct20_id
agg_name = location$agg_name[1]
municipality = location$muni[1]

ct.output(ct_vector, agg_name, municipality) # output is store in Output folder of data project



#################################### BLOCK GROUP #################################### 
######## OPEN INPUT FILE ########

# open a dialog box that allows you to choose a file with addresses
file.to.load <- file.choose(new=TRUE)

# read the csv files into a variable (and do not read strings as factors)
location <- read_excel(file.to.load, stringsAsFactors = FALSE)


# Places the block group vector and aggregated block group name and municipality from the loaded file into the function below
bg_vector = location$bg20_id
agg_name = location$agg_name[1]
municipality = location$muni[1]

bg.output(bg_vector, agg_name, municipality) # output is store in Output folder of data project