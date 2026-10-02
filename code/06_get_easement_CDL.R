# Step 06: cropped acres inside wetlands + setbacks (CDL 2021-2025), and their
# value at county cash rents, land values, and gross crop revenue.
#
# In:  data/processed/nwi_wet_buf_LE_2ft_3in_1p2in_ft_nd.rds (step 05, FWS)
#      data/processed/nwi_wet_buf_LE_2ft_3in_12in_ft_nd.rds  (step 05, NRCS)
#      data/processed/cdl_legend.rds (step 02), data/raw/CDL/<year>_30m_cdls.tif
#      data/processed/NDcountyRents_prices2026.xlsx (ND Dept. of Trust Lands)
#      NASS Quick Stats API (needs NASSQS_TOKEN set in the environment)
# Out: data/processed/cdl_tract_buf_acres_nd.rds                 FWS, county x crop x year
#      data/processed/cdl_tract_buf_acres_LE_2ft_3in_12in_ft_nd.rds  NRCS
#      data/processed/crop_revenue_per_acre_nd.rds, tract_buf_crop_revenue_nd.rds,
#      tract_buf_cropland_rents_nd.rds
#      tables/revenue_summary_crop_mean.tex, tables/cropland_rent_summary_year.tex
#      figures/stutsman_easement_cdl.pdf  (not used in the memo)
#
# MEMO: "Acreage Measures" table rows for cropped acres inside wetlands plus
#       setbacks (1.57M FWS, 0.62M NRCS, 2021-2025 average) are hand-entered
#       from the two cdl_tract_buf_acres*.rds files (mean over years of summed
#       acres).
# MEMO: "Potential Lost Returns" uses the FWS-standard tables built here. The
#       NRCS-standard figures in the text ($41M rent, $1.5B land value, $200M
#       revenue) are not computed here: this script only tabulates NRCS acres.

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

tract_buffers <- readRDS("data/processed/nwi_wet_buf_LE_2ft_3in_1p2in_ft_nd.rds") |>
  st_transform(5070)


cdl_legend <- readRDS("data/processed/cdl_legend.rds")

years <- 2021:2025


get_county_CDL_acreage <- function(y,poly){

  cdl <- rast(file.path("data/raw/CDL",paste0(y,"_30m_cdls.tif")))

  poly_cdl <- crop(cdl, poly, mask = TRUE, touches = FALSE)

  drop_ids <- c(61,81:83,87,88,92,111,112,121:124,131,141:143,152,176,190,195)

  poly_cdl <- ifel(poly_cdl %in% drop_ids,NA,poly_cdl)

  levels(poly_cdl) <- cdl_legend[, .(id = code, category = class_name)] |> as.data.frame()
  coltab(poly_cdl) <- cdl_legend[, .(value = code, red = esri_red, green = esri_green, blue = esri_blue)] |>
    as.data.frame()


  # integer field so cell values are the county codes, not factor indices
  nd_cnties$cnty_id <- as.integer(nd_cnties$COUNTYFP)
  nd_cnties_r <- rasterize(vect(nd_cnties), poly_cdl, field = "cnty_id")

  # pixel counts for each county x crop pair (NA/masked cells dropped)
  nd_cnties_tab <- crosstab(c(nd_cnties_r, poly_cdl), long = TRUE, useNA = FALSE) |>
    as.data.table()
  # ease_cdl is categorical, so crosstab returns class names, not codes
  setnames(nd_cnties_tab, c("cnty_id", "class_name", "n_pix"))

  # 30 m x 30 m cell -> acres
  cell_ac <- prod(res(poly_cdl)) / 4046.8564224   # ~0.2224 ac
  nd_cnties_tab[, acres := n_pix * cell_ac]

  # attach county and crop names
  nd_cnties_tab <- merge(nd_cnties_tab,
              as.data.table(st_drop_geometry(nd_cnties))[, .(cnty_id, COUNTYFP, GEOID, NAME)],
              by = "cnty_id")
  nd_cnties_tab <- merge(nd_cnties_tab, cdl_legend[, .(code, class_name)], by = "class_name", all.x = TRUE)

  nd_cnties_tab[, year := y]
  setorder(nd_cnties_tab, GEOID, -acres)

  nd_cnties_tab[]
}


tract_buf_tab <- lapply(years, get_county_CDL_acreage, poly = tract_buffers) |>
  rbindlist()

# MEMO: cropped acres inside wetlands plus FWS setbacks (1.57M ac average)
saveRDS(tract_buf_tab, "data/processed/cdl_tract_buf_acres_nd.rds")

# MEMO: cropped acres inside wetlands plus NRCS setbacks (0.62M ac average)
# cropped acres under NRCS-standard setbacks, for the memo's acreage range
nrcs_buffers <- readRDS("data/processed/nwi_wet_buf_LE_2ft_3in_12in_ft_nd.rds") |>
  st_transform(5070)
nrcs_buf_tab <- lapply(years, get_county_CDL_acreage, poly = nrcs_buffers) |>
  rbindlist()
saveRDS(nrcs_buf_tab, "data/processed/cdl_tract_buf_acres_LE_2ft_3in_12in_ft_nd.rds")


# Stutsman County 2025 crop map (not used in the memo)
y <- 2025
cdl <- rast(file.path("data/raw/CDL",paste0(y,"_30m_cdls.tif")))


stutsman <- nd_cnties[nd_cnties$COUNTYFP == "093",] |>
  st_transform(5070)


stutsman_ease <- st_intersection(tract_buffers, st_geometry(stutsman))


stutsman_cdl <- crop(cdl, stutsman_ease, mask = TRUE, touches = FALSE)

drop_ids <- c(61,81:83,87,88,92,111,112,121:124,131,141:143,152,176,190,195)

# drop non-crop classes while values are still plain CDL codes
stutsman_cdl <- ifel(stutsman_cdl %in% drop_ids,NA,stutsman_cdl)

stutsman_cdl_df <- as.data.frame(stutsman_cdl, xy = TRUE, na.rm = TRUE) |>
  setNames(c("x", "y", "code")) |>
  as.data.table()
stutsman_cdl_df <- merge(stutsman_cdl_df, cdl_legend[, .(code, class_name, colorhex)], by = "code")

# top classes by area keep their CDL colors; the rest (~3% of cropped acres) lumped
n_top <- 8
top_crops <- stutsman_cdl_df[, .N, by = .(class_name, colorhex)][order(-N)][1:n_top]
stutsman_cdl_df[, crop := fifelse(class_name %in% top_crops$class_name, class_name, "Other crops")]
stutsman_cdl_df[, crop := factor(crop, levels = c(top_crops$class_name, "Other crops"))]
crop_colors <- c(setNames(top_crops$colorhex, top_crops$class_name), "Other crops" = "grey45")


p_cdl <- ggplot() +
  # easement footprint, so non-crop easement land still shows
  geom_sf(data = stutsman_ease, fill = "grey90", color = NA) +
  geom_raster(data = stutsman_cdl_df, aes(x, y, fill = crop)) +
  geom_sf(data = stutsman, fill = NA, color = "grey30", linewidth = 0.4) +
  scale_fill_manual(values = crop_colors, name = paste(y, "crop type")) +
  coord_sf(datum = NA) +
  labs(title = "Crop types on FWS easements, Stutsman County, ND",
       caption = "Grey: easement land not in crops. Source: USDA NASS CDL, USFWS realty tracts.") +
  theme_void() +
  theme(plot.margin = margin(10, 10, 10, 10))

ggsave("figures/stutsman_easement_cdl.pdf", p_cdl,
       width = 7, height = 6.5, device = cairo_pdf, bg = "white")




nassqs_auth(key = Sys.getenv("NASSQS_TOKEN"))

# Query NASS Quick Stats and keep the columns we need
# drops state code since all queries will be for ND in this project
get_nass <- function(params) {
  cols <- c("agg_level_desc","county_code","commodity_desc","short_desc","year","Value") # "state_fips_code",
  dt <- nassqs(params, progress_bar = FALSE) |>
    as.data.table()
  dt[, year := as.integer(year)]
  dt[, ..cols]
}

corn_prices <- get_nass(list(
  commodity_desc = "CORN",
  short_desc = "CORN, GRAIN - PRICE RECEIVED, MEASURED IN $ / BU",
  agg_level_desc = "STATE",
  state_fips_code = "38",
  year = years,
  freq_desc = "ANNUAL",
  reference_period_desc = "MARKETING YEAR"
  ))

corn_yields <- get_nass(list(
  commodity_desc = "CORN",
  short_desc = "CORN, GRAIN - YIELD, MEASURED IN BU / ACRE",
  agg_level_desc = c("COUNTY", "STATE"),
  state_fips_code = "38",
  year = years,
  freq_desc = "ANNUAL",
  reference_period_desc = "YEAR"
  ))

soy_prices <- get_nass(list(
  commodity_desc = "SOYBEANS",
  short_desc = "SOYBEANS - PRICE RECEIVED, MEASURED IN $ / BU",
  agg_level_desc = "STATE",
  state_fips_code = "38",
  year = years,
  freq_desc = "ANNUAL",
  reference_period_desc = "MARKETING YEAR"
  ))

soy_yields <- get_nass(list(
  commodity_desc = "SOYBEANS",
  short_desc = "SOYBEANS - YIELD, MEASURED IN BU / ACRE",
  agg_level_desc = c("COUNTY", "STATE"),
  state_fips_code = "38",
  year = years,
  freq_desc = "ANNUAL",
  reference_period_desc = "YEAR"
  ))

spring_wheat_prices <- get_nass(list(
  commodity_desc = "WHEAT",
  short_desc = "WHEAT, SPRING, (EXCL DURUM) - PRICE RECEIVED, MEASURED IN $ / BU",
  agg_level_desc = "STATE",
  state_fips_code = "38",
  year = years,
  freq_desc = "ANNUAL",
  reference_period_desc = "MARKETING YEAR"
  ))

spring_wheat_yields <- get_nass(list(
  commodity_desc = "WHEAT",
  short_desc = "WHEAT, SPRING, (EXCL DURUM) - YIELD, MEASURED IN BU / ACRE",
  agg_level_desc = c("COUNTY", "STATE"),
  state_fips_code = "38",
  year = years,
  freq_desc = "ANNUAL",
  reference_period_desc = "YEAR"
  ))

durum_wheat_prices <- get_nass(list(
  commodity_desc = "WHEAT",
  short_desc = "WHEAT, SPRING, DURUM - PRICE RECEIVED, MEASURED IN $ / BU",
  agg_level_desc = "STATE",
  state_fips_code = "38",
  year = years,
  freq_desc = "ANNUAL",
  reference_period_desc = "MARKETING YEAR"
  ))

durum_wheat_yields <- get_nass(list(
  commodity_desc = "WHEAT",
  short_desc = "WHEAT, SPRING, DURUM - YIELD, MEASURED IN BU / ACRE",
  agg_level_desc = c("COUNTY", "STATE"),
  state_fips_code = "38",
  year = years,
  freq_desc = "ANNUAL",
  reference_period_desc = "YEAR"
  ))

canola_prices <- get_nass(list(
  commodity_desc = "CANOLA",
  short_desc = "CANOLA - PRICE RECEIVED, MEASURED IN $ / CWT",
  agg_level_desc = "STATE",
  state_fips_code = "38",
  year = years,
  freq_desc = "ANNUAL",
  reference_period_desc = "MARKETING YEAR"
  ))

canola_yields <- get_nass(list(
  commodity_desc = "CANOLA",
  short_desc = "CANOLA - YIELD, MEASURED IN LB / ACRE",
  agg_level_desc = c("COUNTY", "STATE"),
  state_fips_code = "38",
  year = years,
  freq_desc = "ANNUAL",
  reference_period_desc = "YEAR"
  ))

bean_prices <- get_nass(list(
  commodity_desc = "BEANS",
  short_desc = "BEANS, DRY EDIBLE, (EXCL CHICKPEAS) - PRICE RECEIVED, MEASURED IN $ / CWT",
  agg_level_desc = "STATE",
  state_fips_code = "38",
  year = years,
  freq_desc = "ANNUAL",
  reference_period_desc = "MARKETING YEAR"
  ))

bean_yields <- get_nass(list(
  commodity_desc = "BEANS",
  short_desc = "BEANS, DRY EDIBLE, (EXCL CHICKPEAS) - YIELD, MEASURED IN LB / ACRE",
  agg_level_desc = c("COUNTY", "STATE"),
  state_fips_code = "38",
  year = years,
  freq_desc = "ANNUAL",
  reference_period_desc = "YEAR"
  ))

alfalfa_prices <- get_nass(list(
  commodity_desc = "HAY",
  short_desc = "HAY, ALFALFA - PRICE RECEIVED, MEASURED IN $ / TON",
  agg_level_desc = "STATE",
  state_fips_code = "38",
  year = years,
  freq_desc = "ANNUAL",
  reference_period_desc = "MARKETING YEAR"
  ))

alfalfa_yields <- get_nass(list(
  commodity_desc = "HAY",
  short_desc = "HAY, ALFALFA - YIELD, MEASURED IN TONS / ACRE",
  agg_level_desc = c("COUNTY", "STATE"),
  state_fips_code = "38",
  year = years,
  freq_desc = "ANNUAL",
  reference_period_desc = "YEAR"
  ))

other_hay_prices <- get_nass(list(
  commodity_desc = "HAY",
  short_desc = "HAY, (EXCL ALFALFA) - PRICE RECEIVED, MEASURED IN $ / TON",
  agg_level_desc = "STATE",
  state_fips_code = "38",
  year = years,
  freq_desc = "ANNUAL",
  reference_period_desc = "MARKETING YEAR"
  ))

other_hay_yields <- get_nass(list(
  commodity_desc = "HAY",
  short_desc = "HAY, (EXCL ALFALFA) - YIELD, MEASURED IN TONS / ACRE",
  agg_level_desc = c("COUNTY", "STATE"),
  state_fips_code = "38",
  year = years,
  freq_desc = "ANNUAL",
  reference_period_desc = "YEAR"
  ))

barley_prices <- get_nass(list(
  commodity_desc = "BARLEY",
  short_desc = "BARLEY - PRICE RECEIVED, MEASURED IN $ / BU",
  agg_level_desc = "STATE",
  state_fips_code = "38",
  year = years,
  freq_desc = "ANNUAL",
  reference_period_desc = "MARKETING YEAR"
  ))

barley_yields <- get_nass(list(
  commodity_desc = "BARLEY",
  short_desc = "BARLEY - YIELD, MEASURED IN BU / ACRE",
  agg_level_desc = c("COUNTY", "STATE"),
  state_fips_code = "38",
  year = years,
  freq_desc = "ANNUAL",
  reference_period_desc = "YEAR"
  ))

pea_prices <- get_nass(list(
  commodity_desc = "PEAS",
  short_desc = "PEAS, DRY EDIBLE - PRICE RECEIVED, MEASURED IN $ / CWT",
  agg_level_desc = "STATE",
  state_fips_code = "38",
  year = years,
  freq_desc = "ANNUAL",
  reference_period_desc = "MARKETING YEAR"
  ))

pea_yields <- get_nass(list(
  commodity_desc = "PEAS",
  short_desc = "PEAS, DRY EDIBLE - YIELD, MEASURED IN LB / ACRE",
  agg_level_desc = c("COUNTY", "STATE"),
  state_fips_code = "38",
  year = years,
  freq_desc = "ANNUAL",
  reference_period_desc = "YEAR"
  ))

oat_prices <- get_nass(list(
  commodity_desc = "OATS",
  short_desc = "OATS - PRICE RECEIVED, MEASURED IN $ / BU",
  agg_level_desc = "STATE",
  state_fips_code = "38",
  year = years,
  freq_desc = "ANNUAL",
  reference_period_desc = "MARKETING YEAR"
  ))

oat_yields <- get_nass(list(
  commodity_desc = "OATS",
  short_desc = "OATS - YIELD, MEASURED IN BU / ACRE",
  agg_level_desc = c("COUNTY", "STATE"),
  state_fips_code = "38",
  year = years,
  freq_desc = "ANNUAL",
  reference_period_desc = "YEAR"
  ))

# assumes all sunflowers are oil type (which should be a lower bound?)
sunflower_prices <- get_nass(list(
  commodity_desc = "SUNFLOWER",
  short_desc = "SUNFLOWER, OIL TYPE - PRICE RECEIVED, MEASURED IN $ / CWT",
  agg_level_desc = "STATE",
  state_fips_code = "38",
  year = years,
  freq_desc = "ANNUAL",
  reference_period_desc = "MARKETING YEAR"
  ))

sunflower_yields <- get_nass(list(
  commodity_desc = "SUNFLOWER",
  short_desc = "SUNFLOWER, OIL TYPE - YIELD, MEASURED IN LB / ACRE",
  agg_level_desc = c("COUNTY", "STATE"),
  state_fips_code = "38",
  year = years,
  freq_desc = "ANNUAL",
  reference_period_desc = "YEAR"
  ))

lentils_prices <- get_nass(list(
  commodity_desc = "LENTILS",
  short_desc = "LENTILS - PRICE RECEIVED, MEASURED IN $ / CWT",
  agg_level_desc = "STATE",
  state_fips_code = "38",
  year = years,
  freq_desc = "ANNUAL",
  reference_period_desc = "MARKETING YEAR"
  ))

lentils_yields <- get_nass(list(
  commodity_desc = "LENTILS",
  short_desc = "LENTILS - YIELD, MEASURED IN LB / ACRE",
  agg_level_desc = c("COUNTY", "STATE"),
  state_fips_code = "38",
  year = years,
  freq_desc = "ANNUAL",
  reference_period_desc = "YEAR"
  ))

flaxseed_prices <- get_nass(list(
  commodity_desc = "FLAXSEED",
  short_desc = "FLAXSEED - PRICE RECEIVED, MEASURED IN $ / BU",
  agg_level_desc = "STATE",
  state_fips_code = "38",
  year = years,
  freq_desc = "ANNUAL",
  reference_period_desc = "MARKETING YEAR"
  ))

flaxseed_yields <- get_nass(list(
  commodity_desc = "FLAXSEED",
  short_desc = "FLAXSEED - YIELD, MEASURED IN BU / ACRE",
  agg_level_desc = c("COUNTY", "STATE"),
  state_fips_code = "38",
  year = years,
  freq_desc = "ANNUAL",
  reference_period_desc = "YEAR"
  ))


# Combine prices and yields into one county x year x crop table ----------
# names match CDL class_name for merging with the easement CDL acres
# CLAUDE WROTE THIS

prices <- rbindlist(list(
  "Corn" = corn_prices,
  "Soybeans" = soy_prices,
  "Spring Wheat" = spring_wheat_prices,
  "Durum Wheat" = durum_wheat_prices,
  "Canola" = canola_prices,
  "Dry Beans" = bean_prices,
  "Alfalfa" = alfalfa_prices,
  "Other Hay/Non Alfalfa" = other_hay_prices,
  "Barley" = barley_prices,
  "Peas" = pea_prices,
  "Oats" = oat_prices,
  "Sunflower" = sunflower_prices,
  "Lentils" = lentils_prices,
  "Flaxseed" = flaxseed_prices
  ), idcol = "class_name")

yields <- rbindlist(list(
  "Corn" = corn_yields,
  "Soybeans" = soy_yields,
  "Spring Wheat" = spring_wheat_yields,
  "Durum Wheat" = durum_wheat_yields,
  "Canola" = canola_yields,
  "Dry Beans" = bean_yields,
  "Alfalfa" = alfalfa_yields,
  "Other Hay/Non Alfalfa" = other_hay_yields,
  "Barley" = barley_yields,
  "Peas" = pea_yields,
  "Oats" = oat_yields,
  "Sunflower" = sunflower_yields,
  "Lentils" = lentils_yields,
  "Flaxseed" = flaxseed_yields
  ), idcol = "class_name")

prices[, price_unit := sub(".*MEASURED IN ", "", short_desc)]
yields[, yield_unit := sub(".*MEASURED IN ", "", short_desc)]

# yields in lb/acre are priced in $/cwt, so convert to cwt/acre
yields[yield_unit == "LB / ACRE", `:=`(Value = Value / 100, yield_unit = "CWT / ACRE")]

# check that price x yield gives $/acre for every crop (e.g. $ / BU with BU / ACRE)
units <- unique(prices[, .(class_name, price_unit)])[unique(yields[, .(class_name, yield_unit)]), on = "class_name"]
stopifnot(units[, sub("^\\$ / ", "", price_unit) == sub("S? / ACRE$", "", yield_unit)])

# NASS doesn't publish every county every year. Fill gaps with the pooled
# "other counties" yield (county_code 998, all unpublished ND counties), then
# the state yield for years with no county estimates at all.
cnty_yields <- yields[agg_level_desc == "COUNTY" & county_code != "998",
                      .(class_name, county_code, year, cnty_yield = Value)]
other_yields <- yields[county_code == "998", .(class_name, year, other_yield = Value)]
state_yields <- yields[agg_level_desc == "STATE", .(class_name, year, state_yield = Value)]

crop_revenue <- CJ(class_name = unique(prices$class_name),
                   county_code = nd_cnties$COUNTYFP,
                   year = years)
crop_revenue <- cnty_yields[crop_revenue, on = .(class_name, county_code, year)]
crop_revenue <- other_yields[crop_revenue, on = .(class_name, year)]
crop_revenue <- state_yields[crop_revenue, on = .(class_name, year)]

crop_revenue[, yield := fcoalesce(cnty_yield, other_yield, state_yield)]
crop_revenue[, yield_source := fcase(!is.na(cnty_yield), "county",
                                     !is.na(other_yield), "other counties",
                                     !is.na(state_yield), "state")]

crop_revenue <- prices[, .(class_name, year, price = Value, price_unit)][crop_revenue, on = .(class_name, year)]
crop_revenue <- unique(yields[, .(class_name, yield_unit)])[crop_revenue, on = "class_name"]

# MEMO: the 15% yield discount for wetland ground (Boldt et al., 2025)
wetland_discount_factor <- 0.85

crop_revenue[, revenue_per_acre := price * yield * wetland_discount_factor]

setnames(crop_revenue, "county_code", "COUNTYFP")
crop_revenue <- crop_revenue[, .(class_name, COUNTYFP, year, price, price_unit,
                                 yield, yield_unit, yield_source, revenue_per_acre)]
setorder(crop_revenue, class_name, COUNTYFP, year)


# County cash rents and rented land values -------------------------------
# ND Dept. of Trust Lands annual survey (source URL on Sheet2). The sheet has
# no year column: it stacks 13 survey years, 2014-2026 oldest first, each a
# block of 53 cropland rows then 53 pastureland rows. Checked against the
# 2022 and 2026 PDFs (Stutsman cropland: $83.60 / $3,015 and $98.80 / $3,614).
rents <- read_excel("data/processed/NDcountyRents_prices2026.xlsx", sheet = "Sheet1") |>
  as.data.table()
setnames(rents, c("county", "GEOID", "land_type", "rent_per_acre", "land_value_per_acre"))
stopifnot(nrow(rents) == 13 * 2 * 53)
rents[, year := 2014L + (.I - 1L) %/% (2L * 53L)]
rents[, COUNTYFP := sprintf("%03d", as.integer(GEOID) %% 1000L)]

cropland_rents <- rents[land_type == "cropland" & year %in% years,
                        .(COUNTYFP, year, rent_per_acre, land_value_per_acre)]

crop_revenue <- cropland_rents[crop_revenue, on = .(COUNTYFP, year)]
setcolorder(crop_revenue, c("class_name", "COUNTYFP", "year"))


saveRDS(crop_revenue,"data/processed/crop_revenue_per_acre_nd.rds")


# Total revenue on tract buffer acres by county x crop x year -------------
# CDL classes without NASS prices (fallow, minor crops) drop out here
tract_buf_revenue <- crop_revenue[tract_buf_tab[, .(class_name, COUNTYFP, GEOID, NAME, year, acres)],
                                  on = .(class_name, COUNTYFP, year), nomatch = NULL]
tract_buf_revenue[, total_revenue := acres * revenue_per_acre]
tract_buf_revenue[, total_rent := acres * rent_per_acre]
tract_buf_revenue[, total_land_value := acres * land_value_per_acre]
setorder(tract_buf_revenue, GEOID, year, -total_revenue)

# buffer acres left unvalued, by crop
unvalued_acres <- tract_buf_tab[!crop_revenue, on = .(class_name, COUNTYFP, year)][
  , .(acres = sum(acres)), by = .(class_name, year)][order(year, -acres)]

saveRDS(tract_buf_revenue, "data/processed/tract_buf_crop_revenue_nd.rds")


revenue_summary_crop_year <- tract_buf_revenue[, .(total_revenue = sum(total_revenue, na.rm = TRUE)),
                                               by = .(class_name, year)][order(year, -total_revenue)]
revenue_summary_year <- revenue_summary_crop_year[, .(class_name = "Total",
                                                      total_revenue = sum(total_revenue)), by = year]
revenue_summary_crop_year <- rbind(revenue_summary_crop_year, revenue_summary_year)[
  order(year, class_name == "Total", -total_revenue)]
revenue_summary_crop_year[, total_revenue_fmt := label_dollar()(total_revenue)]

# mean annual revenue by crop and total, 2021-2025 (crop absent in a year counts as $0)
revenue_summary_crop_mean <- revenue_summary_crop_year[, .(mean_revenue = sum(total_revenue) / length(years)),
                                                       by = class_name][
  order(class_name == "Total", -mean_revenue)]
revenue_summary_crop_mean[, mean_revenue_fmt := label_dollar()(mean_revenue)]

# MEMO: Table "Mean Crop Revenue (2021--2025) for Acreage Within FWS Buffers"
#       (tab:revenue_crop_mean). The text's $509M per year comes from its Total
#       row. The text's ~$330 per cropped acre and the 87% share for soybeans,
#       corn, spring wheat, and canola are derived from it by hand.
spec1 <- "L{2.8cm} R{2.6cm}"

labs1 <- c("Crop", "Mean Annual Revenue")

note1 <- paste0(
  "Revenue is cropped acres within wetland buffers on FWS easement tracts, ",
  "from the Cropland Data Layer, times county yield and state marketing-year ",
  "price from NASS. Where NASS does not publish a county yield, the pooled ",
  "other-counties yield is used, then the state yield. Crops absent in a year ",
  "count as \\$0 in the mean. CDL classes without a NASS price are excluded. ",
  "County yields are multiplied by ", wetland_discount_factor, " to reflect ",
  "lower yields in and near wetlands (Boldt et al., 2025)."
)

revenue_summary_crop_mean_tex <- revenue_summary_crop_mean[
  , .(class_name, mean_revenue = label_dollar(prefix = "\\$")(mean_revenue))]

save_latex_table_bands(
  revenue_summary_crop_mean_tex,
  file_path = "tables/revenue_summary_crop_mean.tex",
  caption   = "Mean Crop Revenue (2021--2025) for Acreage Within FWS Buffers",
  label     = "revenue_crop_mean",
  note      = note1,
  colspec   = spec1,
  labels    = labs1,
  size_cmd  = "\\footnotesize",
  tabcolsep_pt = 3,
  rule_rows = "^Total$",
  bold_rows = "^Total$"
)


# Total value of cropland rents on tract buffer acres ---------------------
# Uses every cropped CDL acre in the buffers, including crops without a NASS
# price, since cash rent does not depend on what is planted.
tract_buf_rents <- cropland_rents[tract_buf_tab[, .(acres = sum(acres)), by = .(COUNTYFP, GEOID, NAME, year)],
                                  on = .(COUNTYFP, year)]
stopifnot(!anyNA(tract_buf_rents$rent_per_acre))
tract_buf_rents[, `:=`(total_rent = acres * rent_per_acre,
                       total_land_value = acres * land_value_per_acre)]
setorder(tract_buf_rents, GEOID, year)

saveRDS(tract_buf_rents, "data/processed/tract_buf_cropland_rents_nd.rds")

rent_summary_year <- tract_buf_rents[, .(acres = sum(acres),
                                         total_rent = sum(total_rent),
                                         total_land_value = sum(total_land_value)),
                                     by = year][order(year)]
rent_summary_year <- rbind(rent_summary_year[, year := as.character(year)],
                           rent_summary_year[, .(year = "Average",
                                                 acres = mean(acres),
                                                 total_rent = mean(total_rent),
                                                 total_land_value = mean(total_land_value))])
# acre-weighted county averages
rent_summary_year[, `:=`(rent_per_acre = total_rent / acres,
                         land_value_per_acre = total_land_value / acres)]

rent_summary_year_tex <- rent_summary_year[, .(
  year,
  acres               = label_comma(accuracy = 1)(acres),
  rent_per_acre       = label_dollar(prefix = "\\$", accuracy = 0.01)(rent_per_acre),
  total_rent          = label_dollar(prefix = "\\$")(total_rent),
  land_value_per_acre = label_dollar(prefix = "\\$")(land_value_per_acre),
  total_land_value    = label_dollar(prefix = "\\$")(total_land_value)
)]

# MEMO: Table "Cropland Cash Rent and Land Value for Acreage Within FWS
#       Buffers" (tab:cropland_rent_year). The text's $102M per year, $65 per
#       acre, $3.8B, and $2,400 per acre come from its Average row.
spec2 <- "L{1.3cm} R{1.5cm} R{1.5cm} R{2.1cm} R{1.7cm} R{2.5cm}"

labs2 <- c("Year", "Cropped Acres", "Rent per Acre", "Total Cash Rent",
           "Land Value per Acre", "Total Land Value")

note2 <- paste0(
  "Cropped acres are all Cropland Data Layer crop classes within wetland ",
  "buffers on FWS easement tracts. Each county's acres are valued at that ",
  "county's average cropland cash rent and average price of rented cropland ",
  "for the survey year. Per-acre columns are acre-weighted averages across ",
  "counties."
)

save_latex_table_bands(
  rent_summary_year_tex,
  file_path = "tables/cropland_rent_summary_year.tex",
  caption   = "Cropland Cash Rent and Land Value for Acreage Within FWS Buffers",
  label     = "cropland_rent_year",
  note      = note2,
  colspec   = spec2,
  labels    = labs2,
  size_cmd  = "\\footnotesize",
  tabcolsep_pt = 3,
  rule_rows = "^Average$",
  bold_rows = "^Average$",
  source    = "ARPC calculations using data from USDA and the North Dakota Department of Trust Lands."
)


# yield
# ers costs of return studies to get operating costs

# forgone revenues
# rental rates


# STATE: We believe that these numbers are upper bounds of wetland crop production. 

# highlight acres, then what if they weren't cropped

# reduced 

# yield loss adjacent to wetland
# per acre returns
# additional diesel cost from 

# field boundaries - satellite derived - from usda

# check whether easements and clus are the same

# do water body analysis - feasibility

# farmable impacts 
# easement acres vs farmable
