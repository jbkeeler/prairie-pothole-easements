# Step 04: gSSURGO soils on each easement tract, split into wetland and
# non-wetland (field) parts.
#
# In:  data/processed/fws_tracts_nd.rds (step 01)
#      data/processed/nwi_tracts_nd.rds (step 03)
#      data/raw/gSSURGO_ND.gdb
# Out: data/processed/gssurgo_tract_mukey_nd.rds, gssurgo_tract_soils_nd.rds,
#      gssurgo_tract_lcc_nd.rds
#      data/processed/nwi_basin_soils_nd.rds        dominant map unit of each wetland
#                                                   basin (used by step 05)
#      data/processed/gssurgo_tract_mupoly_nd.rds   map unit polygons clipped to
#                                                   tracts (used by step 05)
#      data/processed/gssurgo_soil_pairs_nd.csv   every county x wetland soil x
#        field soil that occurs together in a tract; the input to the external
#        lateral effect calculation that produces LateralEffect_Soils_LE.csv
#        (see 00_master.R)
#      figures/stutsman_easement_hydric.pdf  (not used in the memo)
#
# MEMO: "Drain Tile Setbacks" section: the field and wetland soil types used to
#       compute setbacks come from the CSV written here. The memo's setback
#       distances (tables/setback_county.tex, the 160 ft vs 70 ft medians, the
#       32% vs 1.4% of tracts over 500 ft, the ~300 tracts with missing soil
#       data) were computed from the earlier one-row-per-tract version of that
#       CSV (dominant field and wetland soil only), and no script in this repo
#       builds that table or those numbers.

rm(list = ls())
gc()

source("code/setup.R")

states <- states(cb = FALSE, year = 2024)

nd <- states[states$STUSPS == "ND",] |>
  st_transform(5070)

nd_cnties <- counties(state = "ND", year = 2024) |>
  st_transform(5070)

fws_tracts_nd <- readRDS("data/processed/fws_tracts_nd.rds") |>
  st_transform(5070)


gssurgo_gdb  <- "data/raw/gSSURGO_ND.gdb"

# 10 m map-unit raster; cell values are MUKEY
mu_r <- rast(gssurgo_gdb)
levels(mu_r) <- NULL   # drop the attribute table so values are plain MUKEY integers

# map-unit-level soil summaries (one row per mukey)
valu1    <- st_read(gssurgo_gdb, "Valu1",    quiet = TRUE) |> as.data.table()
muaggatt <- st_read(gssurgo_gdb, "muaggatt", quiet = TRUE) |> as.data.table()
mapunit  <- st_read(gssurgo_gdb, "mapunit",  quiet = TRUE) |> as.data.table()
valu1[, mukey := as.integer(mukey)]
mapunit[, mukey := as.integer(mukey)]
muaggatt[, `:=`(
  mukey = as.integer(mukey), 
  hydclprs = as.numeric(hydclprs) # hydclprs is stored as text
  )]   

# row id, since the first tract column has duplicates
fws_tracts_nd$tract_uid <- seq_len(nrow(fws_tracts_nd))
# GISACRES is 0 for every tract, so use polygon area
fws_tracts_nd$poly_acres <- as.numeric(st_area(fws_tracts_nd)) / 4046.8564224

# NWI wetland x tract polygons from step 03 (same tract_uid)
nwi_ease <- readRDS("data/processed/nwi_tracts_nd.rds")

mu_r    <- crop(mu_r, vect(fws_tracts_nd))
tract_r <- rasterize(vect(fws_tracts_nd), mu_r, field = "tract_uid")
# 1 = NWI wetland, 0 = non-wetland; cells assigned by center, so wetlands < 1 cell can drop out
wet_r   <- rasterize(vect(nwi_ease), mu_r, field = 1, background = 0)

# pixel counts for each tract x wetland x mukey combination (NA/masked cells dropped)
tract_mu_wet <- crosstab(c(tract_r, wet_r, mu_r), long = TRUE, useNA = FALSE) |> as.data.table()
setnames(tract_mu_wet, c("tract_uid", "wetland", "mukey", "n_pix"))

# 10 m x 10 m cell -> acres
tract_mu_wet[, acres := n_pix * prod(res(mu_r)) / 4046.8564224]   # ~0.0247 ac

tract_mu <- tract_mu_wet[, .(n_pix = sum(n_pix), acres = sum(acres)), by = .(tract_uid, mukey)]

tract_mu <- merge(tract_mu,
                  valu1[, .(mukey, nccpi3all, aws0_100, soc0_100, rootznemc, droughty)],
                  by = "mukey", all.x = TRUE)
tract_mu <- merge(tract_mu,
                  muaggatt[, .(mukey, niccdcd, drclassdcd, hydclprs, slopegraddcp)],
                  by = "mukey", all.x = TRUE)

saveRDS(tract_mu, "data/processed/gssurgo_tract_mukey_nd.rds")

# area-weighted means for continuous attributes
tract_soils <- tract_mu[, .(
  acres      = sum(acres),
  nccpi      = weighted.mean(nccpi3all,    acres, na.rm = TRUE),
  aws0_100   = weighted.mean(aws0_100,     acres, na.rm = TRUE),
  soc0_100   = weighted.mean(soc0_100,     acres, na.rm = TRUE),
  pct_hydric = weighted.mean(hydclprs,     acres, na.rm = TRUE),
  slope      = weighted.mean(slopegraddcp, acres, na.rm = TRUE)
), by = tract_uid]

# attach tract identifiers; overlapping tracts (~26k ac statewide, mostly slivers)
# get each cell only once in rasterize(), so acres < poly_acres flags those tracts
tract_soils <- merge(tract_soils,
                     as.data.table(st_drop_geometry(fws_tracts_nd))[, .(tract_uid, TRACTNO, poly_acres)],
                     by = "tract_uid")

# county of each tract; tracts crossing a county line get the county with the most overlap
tract_cnty <- st_join(fws_tracts_nd[, "tract_uid"], nd_cnties[, c("GEOID", "NAME")], largest = TRUE) |>
  st_drop_geometry() |>
  as.data.table()
setnames(tract_cnty, c("GEOID", "NAME"), c("county_fips", "county_name"))
tract_soils <- merge(tract_soils, tract_cnty, by = "tract_uid", all.x = TRUE)

# dominant map unit: the mukey with the most acres in each tract, with its symbol and name;
# columns are prefixed, e.g. dom_musym, wet_dom_musym
dominant_mu <- function(dt, prefix) {
  dom <- dt[dt[, .I[which.max(acres)], by = tract_uid]$V1, .(tract_uid, mukey, mu_acres = acres)]
  dom <- merge(dom, mapunit[, .(mukey, musym, muname)], by = "mukey", all.x = TRUE)
  cols <- c("mukey", "mu_acres", "musym", "muname")
  setnames(dom, cols, paste0(prefix, cols))
  dom
}

tract_soils <- merge(tract_soils, dominant_mu(tract_mu, "dom_"), by = "tract_uid", all.x = TRUE)

# wetland vs non-wetland parts of each tract; tracts without NWI wetlands get wet_acres = 0, NA wet_dom_*
tract_wet_acres <- tract_mu_wet[, .(wet_acres    = sum(acres[wetland == 1]),
                                    nonwet_acres = sum(acres[wetland == 0])), by = tract_uid]
tract_soils <- merge(tract_soils, tract_wet_acres, by = "tract_uid", all.x = TRUE)
tract_soils <- merge(tract_soils, dominant_mu(tract_mu_wet[wetland == 1], "wet_dom_"),
                     by = "tract_uid", all.x = TRUE)
tract_soils <- merge(tract_soils, dominant_mu(tract_mu_wet[wetland == 0], "nonwet_dom_"),
                     by = "tract_uid", all.x = TRUE)

# categorical attributes: acres by class, e.g. land capability class
tract_lcc <- tract_mu[, .(acres = sum(acres)), by = .(tract_uid, niccdcd)]

saveRDS(tract_soils, "data/processed/gssurgo_tract_soils_nd.rds")
saveRDS(tract_lcc,   "data/processed/gssurgo_tract_lcc_nd.rds")


# wetland basins: a basin is one NWI polygon within one tract (a row of nwi_ease).
# Each basin gets the map unit covering most of its cells.
nwi_ease$basin_uid <- seq_len(nrow(nwi_ease))   # row id; matches step 05

basin_r  <- rasterize(vect(nwi_ease[, "basin_uid"]), mu_r, field = "basin_uid")
basin_mu <- crosstab(c(basin_r, mu_r), long = TRUE, useNA = FALSE) |> as.data.table()
setnames(basin_mu, c("basin_uid", "mukey", "n_pix"))
basin_soils <- basin_mu[basin_mu[, .I[which.max(n_pix)], by = basin_uid]$V1, .(basin_uid, mukey = as.integer(mukey))]

# basins too small to hold a cell center get the map unit at a point inside them
small_uid <- setdiff(nwi_ease$basin_uid, basin_soils$basin_uid)
small_pts <- st_point_on_surface(st_geometry(nwi_ease)[small_uid])
basin_soils <- rbind(basin_soils,
                     data.table(basin_uid = small_uid, mukey = as.integer(extract(mu_r, vect(small_pts))[, 2])))

basin_soils <- merge(as.data.table(st_drop_geometry(nwi_ease))[, .(basin_uid, tract_uid, NWI_ID, wet_acres)],
                     basin_soils, by = "basin_uid", all.x = TRUE)
setnames(basin_soils, "mukey", "wet_mukey")

saveRDS(basin_soils, "data/processed/nwi_basin_soils_nd.rds")


# map unit polygons clipped to tracts, one (multi)polygon per tract x map unit;
# step 05 clips each soil's setback to these
mupoly <- read_sf(gssurgo_gdb, query = "SELECT MUKEY FROM MUPOLYGON") |>
  st_set_geometry("geometry") |>
  st_transform(5070)
mupoly$mukey <- as.integer(mupoly$MUKEY)

mupoly <- st_filter(mupoly[, "mukey"], fws_tracts_nd) |>
  st_make_valid()

# shared edges can come back as lines/points, so keep polygons only
tract_mupoly <- st_intersection(mupoly, fws_tracts_nd[, "tract_uid"]) |>
  st_collection_extract("POLYGON")

tract_mupoly <- aggregate(vect(tract_mupoly), by = c("tract_uid", "mukey")) |>
  st_as_sf()
tract_mupoly <- st_cast(tract_mupoly[, c("tract_uid", "mukey")], "MULTIPOLYGON", warn = FALSE)
tract_mupoly <- merge(tract_mupoly, mapunit[, .(mukey, musym)], by = "mukey", all.x = TRUE)
# the tract's county, which step 05 needs to look up setback distances
tract_mupoly$county_fips <- tract_cnty$county_fips[match(tract_mupoly$tract_uid, tract_cnty$tract_uid)]
tract_mupoly <- tract_mupoly[order(tract_mupoly$tract_uid, tract_mupoly$mukey),
                             c("tract_uid", "county_fips", "mukey", "musym")]

saveRDS(tract_mupoly, "data/processed/gssurgo_tract_mupoly_nd.rds")


# input to the external lateral effect (setback) calculation: one row per
# county x wetland soil x field soil that occurs together in a tract. wet_* is a
# basin's dominant map unit; field_* is any map unit in the same tract (wetland
# or not), so each piece step 05 buffers has a distance. A field soil can match
# the wetland soil, since the basin's map unit usually extends past the basin.
# Rows are unique pairs within a county; dropping county would only remove ~1%,
# since map units rarely cross survey areas.
tract_wet_soils   <- unique(basin_soils[!is.na(wet_mukey), .(tract_uid, wet_mukey)])
tract_field_soils <- as.data.table(st_drop_geometry(tract_mupoly))[, .(tract_uid, field_mukey = mukey)]

soil_pairs <- merge(tract_wet_soils, tract_field_soils, by = "tract_uid", allow.cartesian = TRUE)
soil_pairs <- merge(soil_pairs, tract_cnty, by = "tract_uid")
soil_pairs <- unique(soil_pairs[, .(county_fips, county_name, wet_mukey, field_mukey)])

mu_names <- mapunit[, .(mukey, musym, muname)]
soil_pairs <- merge(soil_pairs, setNames(mu_names, c("wet_mukey", "wet_musym", "wet_muname")),
                    by = "wet_mukey", all.x = TRUE)
soil_pairs <- merge(soil_pairs, setNames(mu_names, c("field_mukey", "field_musym", "field_muname")),
                    by = "field_mukey", all.x = TRUE)
setorder(soil_pairs, county_fips, wet_mukey, field_mukey)

write.csv(soil_pairs[, .(county_fips, county_name,
                         wet_mukey, wet_musym, wet_muname,
                         field_mukey, field_musym, field_muname)],
          "data/processed/gssurgo_soil_pairs_nd.csv", row.names = FALSE)



# Stutsman County hydric soils map (not used in the memo)
stutsman <- nd_cnties[nd_cnties$COUNTYFP == "093",] |>
  st_transform(5070)

stutsman_ease <- st_intersection(fws_tracts_nd, st_geometry(stutsman))

# % of each map unit's components rated hydric, masked to Stutsman easements
stutsman_mu <- crop(mu_r, vect(stutsman_ease), mask = TRUE)
stutsman_hydric <- classify(stutsman_mu,
                            as.matrix(muaggatt[, .(mukey, hydclprs)]),
                            others = NA)

# 10 m is more detail than a county map can show; 30 m keeps the data frame small
stutsman_hydric_df <- aggregate(stutsman_hydric, fact = 3, fun = "mean", na.rm = TRUE) |>
  as.data.frame(xy = TRUE, na.rm = TRUE) |>
  setNames(c("x", "y", "pct_hydric"))

p_hydric <- ggplot() +
  geom_raster(data = stutsman_hydric_df, aes(x, y, fill = pct_hydric)) +
  geom_sf(data = stutsman, fill = NA, color = "grey30", linewidth = 0.4) +
  # begin > 0 so 0% hydric easement land stays visible against the white background
  scale_fill_continuous_sequential(palette = "Blues 3", begin = 0.15, limits = c(0, 100),
                                   name = "Hydric soil\n(% of map unit)") +
  coord_sf(datum = NA) +
  labs(title = "Hydric soils on FWS easements, Stutsman County, ND",
       caption = "Source: gSSURGO (muaggatt.hydclprs), USFWS realty tracts.") +
  theme_void() +
  theme(plot.margin = margin(10, 10, 10, 10))

ggsave("figures/stutsman_easement_hydric.pdf", p_hydric,
       width = 7, height = 6.5, device = cairo_pdf)


# nass tiling acreage by county
# paper on tile drainage using satellite data



