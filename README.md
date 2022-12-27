# Creative Economy Indicator Project Technical Memo

Written by Seleeke Flingai, updated by Rachel Bowers
Date: December 27, 2022

## Introduction
The R scripts folder is broken down into four subfolders – Cleaning, Filtering, Analysis, and Mapping – that were used to go from raw DataAxle 2021 master file to municipal- and regional-level summaries and maps of creative economy data. 
While each script folder and its contents will be described below, it is likely that the cleaning and filtering scripts won’t need to be touched for any future analysis unless updated DataAxle data is purchased or an alternative data source is considered. 



### 1_Cleaning
Cleans up DataAxle 2021 dataset and selects the most relevant variables for the rest of the analysis
- Input: DataAxle 2021 data processed by Susan Brunton for the state of Massachusetts (found in Raw data folder under DataAxle2016.csv)
- Output: two .csv files (found in Modified data folder): 
    - `DataAxle_mass_processed.csv` (includes DataAxle data for all of Massachusetts)
    - `DataAxle_mapc_towns_processed.csv` (only includes data from MAPC municipalities)

### 2_Filtering
Filters the DataAxle data to isolate creative economy establishments (per NEFA’s definition). Also designates which creative category each establishment is a part of by six-digit NAICS code (creative categories also from NEFA’s creative economy work).
- Input: `DataAxle_mass_processed.csv` (found in Modified data folder, created through the Cleaning script)
- Output: four .csv files (found in Modified data folder):
    - `DataAxle_nefa_all_mapc_processed.csv` (includes all creative economy establishments in MAPC)
    - `DataAxle_nefa_all_mass_processed.csv` (includes all creative economy establishments across Massachusetts)
    - `DataAxle_nefa_core_mapc_processed.csv` (includes core (as opposed to peripheral) creative economy establishments in MAPC – these are most important!)
    - `DataAxle_nefa_core_mass_processed.csv` (includes core creative economy establishments throughout Massachusetts)

### 3_Analysis
This folder contains seven scripts which summarize the data for a desired neighborhood (useful for intra-municipal analyses) or for each municipality (useful for regional analyses and comparisons between municipalities).

##### Scripts: 
1. `summary_stats_processed.R` – creates summary statistics for each municipality, community type, subregion, subtype, and for MAPC and Massachusetts as a whole. Summary statistic variable list can be found in the Summary Statistics Metadata csv. 
    - A comparable script, summary_stats_processed_block-group.R, exists specifically for block group-level summary statistic generation.
2. `final_dataset_processed.R` – uses the `summary_stats_processed.R` file (via the ‘source’ function in R) to create clean excel spreadsheets of the summary statistics. The “final datasets” produced from this script are then used for the final regional- and municipal-level data outputs (graphs, maps, charts)
    - A comparable script, `final_dataset_processed_block-group.R`, exists specifically for block group-level final dataset generation.
3. `final_dataset_percents.R` – same as `final_dataset_processed.R`, but transforms all proportions to percentages. The “final datasets” produced from this script are also used in the final regional- and municipal-level data outputs where necessary
    -	Because the percents R code is only used to turn the proportions calculated in `final_dataset_processed.R` to percentages, and this information is only used for bar graphs generated in R, a comparable `final_dataset_percents_block-group.R` was NOT made. Those who wish to create bar graphs of the data can use the numbers calculated from the `final-dataset_processed_block-group.R` in whichever data visualization program they wish.
4. `final_municipal_output_functions_processed.R` – this script builds functions to
    - Create both online (leaflet) and png dot maps and heat maps. Each dot in the dot map is a creative economy establishment, colored by NEFA creative category. The heat map illustrates the density of the creative economy establishments within a municipality. 
    - Produce summary statistics excel spreadsheets and charts (outlined in greater detail below)
    - A comparable script, `final_block-group_output_functions_processed.R`, exists specifically for block group-level final output functions, which includes a function that aggregates desired block groups into a designated neighborhood. Charts are not made in the block group function code. 

 
Figure 1 -- R code from final_municipal_outputs_processed.R. Simply replace "Arlington" in the municipal.output function with desired municipality to produce municipal-specific data folder containing maps, graphs, and datasets

### 4_Final Output
This folder contains three scripts, which produce maps and data summaries of the desired municipality (useful for municipality-specific interests) or neighborhood (as delineated by block groups that the user inputs), or produces regional maps using the summary stats data analyzed in the “Analysis” R scripts

##### Outputs of `final_municipal_outputs_processed.R` or `final_block-group_outputs_processed.R` script (found in the Output folder)
1.	`heatmap_[neighborhood/municipality]_ce.html` or .png: Heat map of the core creative economy in the neighborhood/municipality (online leaflet and png file) – 
2.	`map_[neighborhood/municipality]_ce.html` or .png: Dot map of each core creative economy establishment in the neighborhood/municipality (online leaflet and png file). GENERALLY ONLY USED FOR INTERNAL PURPOSES (map labeled to illustrate this point)
3.	`[municipality]_ce_core_share_comparison.png` Bar graph comparing share of creative economy in desired municipality to its community type, county, subtype, subregion, and MAPC and Massachusetts as a whole. Bar graph is not produced for the block group/neighborhood analysis
4.	Bar graph breaking down the number of creative establishments in the desired municipality, by NEFA creative category (e.g., “Architecture and Design” or “Visual Arts, Music, and Other Performing Arts”). Bar graph is not produced for the block group/neighborhood analysis
5. Bar graph breaking down the number of creative establishments in the desired municipality, by NAICS description (e.g., “Graphic Design Services” or “Museums”). Bar graph is not produced for the block group/neighborhood analysis
6.	The following spreadsheets:
    -	`[neighborhood/municipality]_ce_establishment_list.csv`: the list of creative economy establishments. Essentially a filter of the DataAxle creative economy dataset (produced from the “Filtering” R script) that only shows the creative establishments from the desired block group/neighborhood/ municipality
    -	`[municipality]_ce_data.csv`: a comparison of the neighborhood/municipality’s creative economy to that of its respective community type, county, subregion, subtype, as well as MAPC and Massachusetts. for the block group/neighborhood analysis, the comparison is only to the municipality of the given block group/neighborhood
    -	`[neighborhood/municipality]_ce_naics_desc_breakdown.csv`: A breakdown of the neighborhood/municipality’s creative economy by NAICS description (e.g., “Graphic Design Services” or “Museums”). includes number of establishments in each category and the share of that category within the neighborhood/municipality’s total core creative economy
    -	`[neighborhood/municipality]_ce_creative_group_breakdown.csv`: A breakdown of the neighborhood/municipality’s creative economy by NEFA creative category (e.g., “Architecture and Design” or “Visual Arts, Music, and Other Performing Arts”) 

##### Output of `regional_ce_maps_processed.R` (found in Output folder
1.	Creates regional maps that show the share of establishments that are in the creative economy by each municipality. 
    -	Creates leaflet (online) map and pdf map. 
    -	Core, peripheral, and All (core + peripheral) maps are produced
2.	Output: three .pdf and three .html files (found in Output folder):
    -	`map_nefa_core_ce.pdf` or html (share of establishments that are core creative establishments)
    -	`map_nefa_peri_ce.pdf` or html (share of establishments that are peripheral creative establishments)
    -	`map_nefa_all_ce.pdf` or html (share of establishments that are creative establishments of either core or  peripheral status)

