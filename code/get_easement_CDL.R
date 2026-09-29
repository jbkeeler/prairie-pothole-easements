rm(list = ls())
gc()

source("code/00_master.R")


nd <- states(cb = FALSE, year = 2024)[states$STUSPS == "ND",] |>
  st_transform(5070)

nd_cnties <- counties(state = "ND", year = 2024)

fws_tracts_nd <- readRDS("data/processed/fws_tracts_nd.rds") |>
  st_transform(5070)


cdl_legend <- readRDS("data/processed/cdl_legend.rds")

years <- 2021:2025

y <- 2025
cdl <- rast(file.path("data/raw",paste0(y,"_30m_cdls.tif"))) |>
  project("EPSG:5070")


ease_cdl <- crop(cdl, fws_tracts_nd, mask = TRUE)

levels(ease_cdl) <- cdl_legend[, .(id = code, category = class_name)] |> as.data.frame()
coltab(ease_cdl) <- cdl_legend[, .(value = code, red = esri_red, green = esri_green, blue = esri_blue)] |>
  as.data.frame()

# terra attachments

