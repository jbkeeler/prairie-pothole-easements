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

mu_r    <- crop(mu_r, vect(fws_tracts_nd))
tract_r <- rasterize(vect(fws_tracts_nd), mu_r, field = "tract_uid")

# pixel counts for each tract x mukey pair (NA/masked cells dropped)
tract_mu <- crosstab(c(tract_r, mu_r), long = TRUE, useNA = FALSE) |> as.data.table()
setnames(tract_mu, c("tract_uid", "mukey", "n_pix"))

# 10 m x 10 m cell -> acres
tract_mu[, acres := n_pix * prod(res(mu_r)) / 4046.8564224]   # ~0.0247 ac

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

# dominant map unit: the mukey with the most acres in each tract
tract_dom_mu <- tract_mu[tract_mu[, .I[which.max(acres)], by = tract_uid]$V1,
                         .(tract_uid, dom_mukey = mukey, dom_mu_acres = acres)]
tract_dom_mu <- merge(tract_dom_mu,
                      mapunit[, .(dom_mukey = mukey, dom_musym = musym, dom_muname = muname)],
                      by = "dom_mukey", all.x = TRUE)
tract_soils <- merge(tract_soils, tract_dom_mu, by = "tract_uid", all.x = TRUE)

# categorical attributes: acres by class, e.g. land capability class
tract_lcc <- tract_mu[, .(acres = sum(acres)), by = .(tract_uid, niccdcd)]

saveRDS(tract_soils, "data/processed/gssurgo_tract_soils_nd.rds")
saveRDS(tract_lcc,   "data/processed/gssurgo_tract_lcc_nd.rds")


write.csv(tract_soils[,.(tract_uid,dom_mukey,dom_mu_acres,dom_musym,dom_muname)], "data/processed/gssurgo_tract_dom_soils_nd.csv")
