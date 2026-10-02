# Shared setup: packages, constants, and helpers.
# Sourced at the top of every numbered script so each step can also be run on
# its own (from the project root).

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
library(readxl)

source("code/constants.R")
source("code/utils.R")
