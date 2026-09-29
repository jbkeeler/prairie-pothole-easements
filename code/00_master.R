# Entry point: run this to reproduce the full pipeline in order.
# Rscript code/00_master.R

library(data.table)
library(arrow)
library(kableExtra)
library(rnassqs)
library(sf)
library(terra)
library(tigris)
library(ggplot2)
library(colorspace)
library(scales)
library(ggthemes)
library(Cairo)
library(archive)




source("code/constants.R")
source("code/utils.R")

# source("code/01_import_data.R")
# source("code/02_clean_data.R")
# source("code/03_analysis.R")
# source("code/04_figures_tables.R")
