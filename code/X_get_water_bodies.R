rm(list = ls())
gc()

source("code/setup.R")


states <- states(cb = FALSE, year = 2024)

nd <- states[states$STUSPS == "ND",] |>
  st_transform(5070)


ppr <- read_sf("data/raw/prairie_pothole.json")|>
  st_transform(5070)

ppr_nd <- st_intersection(ppr,st_geometry(nd))

write_sf(nd, "data/processed/nd.shp")
write_sf(ppr_nd, "data/processed/ppr_nd.shp")


nwi_nd <- read_sf("data/raw/NWI/ND_geopackage_wetlands.gpkg",
                  query = "SELECT ATTRIBUTE, WETLAND_TYPE, NWI_ID, Shape FROM ND_Wetlands") |>
  st_set_geometry("geometry") |>
  st_transform(5070)

# drop wetlands that don't touch a tract before the (slow) validity repair
nwi_nd <- st_filter(nwi_nd, nd) |>
  st_make_valid()

# wetland x tract pieces; shared edges can come back as lines/points, so keep polygons only
nwi_ease <- st_intersection(nwi_nd, st_geometry(nd)) |>
  st_collection_extract("POLYGON")

write_sf(nd, "data/processed/nwi_nd/nwi_nd.shp")

fws_tracts_nd <- readRDS("data/processed/fws_tracts_nd.rds") |>
  st_transform(5070)

write_sf(nd, "data/processed/fws_tracts_nd/fws_tracts_nd.shp")


# add cropland to figure 3
