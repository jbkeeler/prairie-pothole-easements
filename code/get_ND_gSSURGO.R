rm(list = ls())
gc()

source("code/00_master.R")

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
valu1[, mukey := as.integer(mukey)]
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

# categorical attributes: acres by class, e.g. land capability class
tract_lcc <- tract_mu[, .(acres = sum(acres)), by = .(tract_uid, niccdcd)]

saveRDS(tract_soils, "data/processed/gssurgo_tract_soils_nd.rds")
saveRDS(tract_lcc,   "data/processed/gssurgo_tract_lcc_nd.rds")


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
