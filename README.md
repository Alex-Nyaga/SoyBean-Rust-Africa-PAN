
# Pan-African Soybean Rust (SBR) Dataset

This repo contains the raw Pan-African Soybean Rust trial data, the R script used to process it, and the cleaned/derived database.

Files
## PAN_SoyBean_Rust_New.csv

The raw trial dataset as downloaded from the PAN (Pan-African Network) / Soybean Innovation Lab Figshare repository. This is the untouched source file.

## PAN_SoyBean_Rust_Old.csv

An earlier version of the raw dataset. Not yet processed/analyzed.
## SBR.R

The R script used to go from PAN_SoyBean_Rust_New.csv → Soybean_Rust_Database.csv. Broadly, it:

Splits the raw data by season (Season 1 / Season 2).
Checks for location–genotype–year consistency (flags any location + genotype + year combination that shows up under more than one trial code).
Collapses replicate rows into a single row per year + location (lat/lon) + season + genotype.
Merges the two season subsets back into one final dataset.
Soybean_Rust_Database.csv

The cleaned, analysis-ready dataset derived from PAN_SoyBean_Rust_New.csv using SBR.R. Each row is now unique no repeated/duplicate plot-level records because replicates for the same year + location + season + genotype have been collapsed into one representative row.

Since the data is now collapsed to a single row per trial combination, there is no rep (replicate) column in this file each row already represents one aggregated sample, replacing what used to be multiple replicate rows.

Columns were aggregated in one of two ways during collapsing:

Categorical / rating columns → mode (most frequent value kept): LOD, R6_BB_Sev, R6_BP_Sev, R6_BS_Sev, R6_BTS_Sev, R6_CLB_Sev, R6_DM_Sev, R6_MLS_Sev, R6_FELS_Se, R6_RLB_Sev, R6_RUST_Se, R6_RUST_RR, R6_Others_, COUNTRY, env, loc, FLW_CL, PUB_CL, SOWING, HARVEST, RAINFED, SOURCE, COMPANY

Quantitative columns → mean (numeric average taken): check, PL_EMERG_C, PL_EMERG_P, FLW_DAYS, NDM, PODDING_MA, Pod_Shatte, PH_R8, W100G, GY, PROT,

## Column_Dctionary.png

 This image shows the metadata/data dictionary describing what each column in Soybean_Rust_Database.csv represents.

## Season.png

Reference table (macro-environment classification for soybean across Africa) used to interpret the SEASON column — i.e. which African regions/countries correspond to Season 1 vs Season 2 growing periods.

## Map 

The Soybean Rust Map (New).png plots the sample points for locations after analysis. Plots the file Soybean_Rust_Database.csv.
