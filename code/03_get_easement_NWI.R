# Step 03: NWI wetlands inside FWS easement tracts.
#
# In:  data/processed/fws_tracts_nd.rds (step 01)
#      data/raw/NWI/ND_geopackage_wetlands.gpkg
# Out: data/processed/nwi_tracts_nd.rds   NWI wetland x tract polygons, EPSG:5070,
#                                        used by steps 04 and 05
#      figures/stutsman_easement_nwi.pdf  (not used in the memo)
#
# MEMO: "NWI wetlands inside easement tracts" in the acreage table is the sum of
#       wet_acres in nwi_tracts_nd.rds (hand-entered; step 05's buffer acreage
#       CSV gives the same total over tracts with setbacks).
# MEMO: The memo's NWI maps (figures/nd_nwi_system.png,
#       figures/stutsman_easement_nwi_system.png) and the statewide NWI total
#       (4.23M ac) are not produced by any script in this repo.

rm(list = ls())
gc()

source("code/setup.R")


nd_cnties <- counties(state = "ND", year = 2024) |>
  st_transform(5070)

fws_tracts_nd <- readRDS("data/processed/fws_tracts_nd.rds") |>
  st_transform(5070)

# row id, since the first tract column has duplicates; matches steps 04 and 05
fws_tracts_nd$tract_uid <- seq_len(nrow(fws_tracts_nd))

# st_layers("data/raw/NWI/ND_geopackage_wetlands.gpkg")

# ~2.1M polygons; only read the columns we need
nwi_nd <- read_sf("data/raw/NWI/ND_geopackage_wetlands.gpkg",
                  query = "SELECT ATTRIBUTE, WETLAND_TYPE, NWI_ID, Shape FROM ND_Wetlands") |>
  st_set_geometry("geometry") |>
  st_transform(5070)

# drop wetlands that don't touch a tract before the (slow) validity repair
nwi_nd <- st_filter(nwi_nd, fws_tracts_nd) |>
  st_make_valid()

# wetland x tract pieces; shared edges can come back as lines/points, so keep polygons only
nwi_ease <- st_intersection(nwi_nd, fws_tracts_nd[, c("tract_uid", "TRACTNO")]) |>
  st_collection_extract("POLYGON")

nwi_ease$wet_acres <- as.numeric(st_area(nwi_ease)) / 4046.8564224

saveRDS(nwi_ease, "data/processed/nwi_tracts_nd.rds")

# MEMO: Key Insights "Most wetland basins are small": share of basins under
#       1 acre (~3 in 4) and their share of wetland acres (~14%), printed below.
# basin size: a basin is one NWI polygon within one tract (multi-part pieces summed)
basins <- as.data.table(st_drop_geometry(nwi_ease))[
  , .(acres = sum(wet_acres)), by = .(NWI_ID, tract_uid)]
small <- basins$acres < 1
cat(sprintf("Basins under 1 acre: %s of %s (%.1f%%), %.1f%% of wetland acres\n",
            comma(sum(small)), comma(nrow(basins)), 100 * mean(small),
            100 * sum(basins$acres[small]) / sum(basins$acres)))


# Stutsman County example map (not used in the memo; the memo's Stutsman
# figure is stutsman_easement_nwi_system.png, made outside this repo)
stutsman <- nd_cnties[nd_cnties$COUNTYFP == "093",] |>
  st_transform(5070)

stutsman_ease <- st_intersection(fws_tracts_nd, st_geometry(stutsman))
stutsman_nwi  <- st_intersection(nwi_ease, st_geometry(stutsman))

# standard NWI Wetlands Mapper colors
nwi_colors <- c("Freshwater Emergent Wetland"       = "#7FC31C",
                "Freshwater Forested/Shrub Wetland" = "#008837",
                "Freshwater Pond"                   = "#688CC0",
                "Lake"                              = "#13007C",
                "Riverine"                          = "#0190BF",
                "Other"                             = "#B28653")

p_nwi <- ggplot() +
  # easement footprint, so non-wetland easement land still shows
  geom_sf(data = stutsman_ease, fill = "grey90", color = NA) +
  geom_sf(data = stutsman_nwi, aes(fill = WETLAND_TYPE), color = NA) +
  geom_sf(data = stutsman, fill = NA, color = "grey30", linewidth = 0.4) +
  scale_fill_manual(values = nwi_colors, name = "NWI wetland type") +
  coord_sf(datum = NA) +
  labs(title = "NWI wetlands on FWS easements, Stutsman County, ND",
       caption = "Grey: easement land outside NWI wetlands.\nSource: USFWS NWI, USFWS realty tracts.") +
  theme_void() +
  theme(plot.margin = margin(10, 10, 10, 10))

ggsave("figures/stutsman_easement_nwi.pdf", p_nwi,
       width = 7, height = 6.5, device = cairo_pdf, bg = "white")
