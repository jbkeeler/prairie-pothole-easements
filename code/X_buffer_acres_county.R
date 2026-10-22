# Extra: setback buffer acres by county under the FWS (1.2 in drawdown) and
# NRCS (12 in drawdown) setbacks. Buffer only: the wetland itself is excluded,
# and all land in the setback counts, cropped or not.
#
# Each tract is assigned whole to the county it overlaps most (step 04), which
# is also the county whose setback distances step 05 used for it. The buffer
# acres are step 05's per-tract buffer_acres: the sum of the buffer rings, one
# per tract x field soil, each at that soil's setback distance from each
# wetland soil. Rings don't overlap each other or the wetlands, so their sum
# is the buffer area (checked below against the wetland + setback union).
#
# In:  data/processed/nwi_buffer_acres_<LE>_nd.csv (step 05)
#      data/processed/nwi_wet_buf_<LE>_nd.rds (step 05, for the check)
#      data/processed/gssurgo_tract_mupoly_nd.rds (step 04, tract county)
# Out: data/processed/buffer_acres_county_nd.csv
#   where <LE> is LE_2ft_3in_1p2in_ft (FWS) or LE_2ft_3in_12in_ft (NRCS)

rm(list = ls())
gc()

source("code/setup.R")


le_vars <- c(fws = "LE_2ft_3in_1p2in_ft", nrcs = "LE_2ft_3in_12in_ft")

nd_counties <- counties(state = "ND", year = 2022)
nd_counties <- data.table(county_fips = as.integer(nd_counties$GEOID),
                          county_name = nd_counties$NAME)

# county of each tract (majority overlap), from step 04
tract_cnty <- readRDS("data/processed/gssurgo_tract_mupoly_nd.rds") |>
  st_drop_geometry() |>
  as.data.table()
tract_cnty <- unique(tract_cnty[, .(tract_uid, county_fips = as.integer(county_fips))])
stopifnot(!anyDuplicated(tract_cnty$tract_uid))


# buffer acres per county for one LE_* setback, tracts assigned whole to
# their majority county.
# For option 2 (buffers split at county lines), swap this for a version that
# intersects the zone == "buffer" rows of nwi_buffers_<le_var>_nd.rds with the
# county polygons and sums st_area by county; it should return the same
# columns so the rest of the script is unchanged.
buffer_acres_by_county <- function(le_var) {
  ac <- fread(paste0("data/processed/nwi_buffer_acres_", le_var, "_nd.csv"))

  # rings should add up to the wetland + setback union less the wetland; a gap
  # would mean rings overlap (double counting) or cracks carry real area
  u <- readRDS(paste0("data/processed/nwi_wet_buf_", le_var, "_nd.rds"))
  ac$union_buf_acres <- u$acres[match(ac$tract_uid, u$tract_uid)] - ac$wet_acres
  off <- ac[abs(buffer_acres - union_buf_acres) > 0.01]
  if (nrow(off) > 0)
    warning(le_var, ": ", nrow(off), " tract(s) where ring acres differ from the wetland + setback union (tract_uid ",
            paste(off$tract_uid, collapse = ", "), "); ring acres are used")

  ac <- merge(ac, tract_cnty, by = "tract_uid", all.x = TRUE)
  stopifnot(!anyNA(ac$county_fips))
  ac[, .(buffer_acres = sum(buffer_acres)), by = county_fips]
}


cnty_acres <- copy(nd_counties)
for (s in names(le_vars)) {
  a <- buffer_acres_by_county(le_vars[[s]])
  cnty_acres[, (paste0(s, "_buffer_acres")) := a$buffer_acres[match(county_fips, a$county_fips)]]
}
# counties with no setback buffers on easements
for (col in grep("_buffer_acres$", names(cnty_acres), value = TRUE))
  set(cnty_acres, which(is.na(cnty_acres[[col]])), col, 0)

setorder(cnty_acres, county_name)

# county sums must match the statewide tract totals
for (s in names(le_vars)) {
  tot <- sum(fread(paste0("data/processed/nwi_buffer_acres_", le_vars[[s]], "_nd.csv"))$buffer_acres)
  stopifnot(isTRUE(all.equal(sum(cnty_acres[[paste0(s, "_buffer_acres")]]), tot)))
}

fwrite(cnty_acres, "data/processed/buffer_acres_county_nd.csv")
