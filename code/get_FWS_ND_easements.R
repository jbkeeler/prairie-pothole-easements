source("code/00_master.R")


# ppr <-  read_sf("data/raw/prairie_pothole.json")|>
#   st_transform(4326)


states <- states(cb = FALSE, year = 2024)
nd <- states[states$STUSPS == "ND",] |>
  st_transform(4326)

# ppr_nd <- st_intersection(ppr,nd)



fws_tracts <- read_sf("data/raw/FWS_National_Realty_Tracts_View_Test_2629062379573146599.gpkg") |>
  st_set_geometry("geometry") |>
  st_transform(5070) |>
  st_make_valid()

fws_tracts_nd <- st_intersection(fws_tracts, st_transform(nd, 5070)) |>
  st_transform(4326)

fws_tracts_nd <- fws_tracts_nd[fws_tracts_nd$INTTYPE1 == "E" & (fws_tracts_nd$INTTYPE2 %in% c("X", "FX")),]

saveRDS(fws_tracts_nd,"data/processed/fws_tracts_nd.rds")


# # Acreage: measure in an equal-area CRS (5070); 1 acre = 4046.8564224 m^2
# fws_tracts_nd_5070 <- st_transform(fws_tracts_nd, 5070)
# fws_tracts_nd$acres <- as.numeric(st_area(fws_tracts_nd_5070)) / 4046.8564224

# # Dissolve before measuring so overlapping tracts aren't double counted
# easement_acres_total <- as.numeric(st_area(st_union(fws_tracts_nd_5070))) / 4046.8564224
# easement_acres_sum <- sum(fws_tracts_nd$acres)

