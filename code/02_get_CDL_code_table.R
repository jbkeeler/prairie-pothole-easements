# Step 02: Cropland Data Layer legend (code, class name, colors).
#
# In:  NASS CDL legend web page (scraped)
# Out: data/processed/cdl_legend.rds   used by steps 05 and 06 to name and color
#                                     CDL classes

rm(list = ls())
gc()

source("code/setup.R")

library(rvest)

url <- "https://www.nass.usda.gov/Research_and_Science/Cropland/docs/cdl_legend_colors.html"

cdl_legend <- rvest::read_html(url) |>
  rvest::html_element("table") |>
  rvest::html_table() |>
  as.data.table()

cdl_legend[, Color := NULL]                     # empty swatch column
setnames(cdl_legend, tolower(names(cdl_legend)))

saveRDS(cdl_legend,"data/processed/cdl_legend.rds")
