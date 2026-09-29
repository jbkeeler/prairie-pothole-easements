library(rvest)

url <- "https://www.nass.usda.gov/Research_and_Science/Cropland/docs/cdl_legend_colors.html"

cdl_legend <- rvest::read_html(url) |>
  rvest::html_element("table") |>
  rvest::html_table() |>
  as.data.table()

cdl_legend[, Color := NULL]                     # empty swatch column
setnames(cdl_legend, tolower(names(cdl_legend)))

saveRDS(cdl_legend,"data/processed/cdl_legend.rds")
