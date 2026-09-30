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

saveRDS(tract_buf_tab, "data/processed/cdl_tract_buf_acres_nd.rds")


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

crop_revenue[, revenue_per_acre := price * yield]

setnames(crop_revenue, "county_code", "COUNTYFP")
crop_revenue <- crop_revenue[, .(class_name, COUNTYFP, year, price, price_unit,
                                 yield, yield_unit, yield_source, revenue_per_acre)]
setorder(crop_revenue, class_name, COUNTYFP, year)


saveRDS(crop_revenue,"data/processed/crop_revenue_per_acre_nd.rds")


# Total revenue on tract buffer acres by county x crop x year -------------
# CDL classes without NASS prices (fallow, minor crops) drop out here
tract_buf_revenue <- crop_revenue[tract_buf_tab[, .(class_name, COUNTYFP, GEOID, NAME, year, acres)],
                                  on = .(class_name, COUNTYFP, year), nomatch = NULL]
tract_buf_revenue[, total_revenue := acres * revenue_per_acre]
setorder(tract_buf_revenue, GEOID, year, -total_revenue)

# buffer acres left unvalued, by crop
unvalued_acres <- tract_buf_tab[!crop_revenue, on = .(class_name, COUNTYFP, year)][
  , .(acres = sum(acres)), by = .(class_name, year)][order(year, -acres)]

saveRDS(tract_buf_revenue, "data/processed/tract_buf_crop_revenue_nd.rds")

# What crops to include
nd_cnties_tab <- readRDS("data/processed/cdl_county_acres_nd.rds")

acres_nd <- nd_cnties_tab[, .(acres = round(sum(acres,na.rm=TRUE))), by = .(class_name,year)]

setorder(acres_nd,-year,-acres)

# top 12 crops by acres in each year
top_crops_nd <- acres_nd[, head(.SD, 12), by = year]


# yield loss adjacent to wetland
# per acre returns
# additional diesel cost from 

# field boundaries - satellite derived - from usda

# check whether easements and clus are the same

# do water body analysis - feasibility

# farmable impacts 
# easement acres vs farmable
