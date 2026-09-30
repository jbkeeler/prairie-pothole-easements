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


cdl_legend <- readRDS("data/processed/cdl_legend.rds")

years <- 2021:2025


get_county_CDL_acreage <- function(y){

  cdl <- rast(file.path("data/raw/CDL",paste0(y,"_30m_cdls.tif")))

  ease_cdl <- crop(cdl, fws_tracts_nd, mask = TRUE, touches = FALSE)

  drop_ids <- c(61,81:83,87,88,92,111,112,121:124,131,141:143,152,176,190,195)

  ease_cdl <- ifel(ease_cdl %in% drop_ids,NA,ease_cdl)

  levels(ease_cdl) <- cdl_legend[, .(id = code, category = class_name)] |> as.data.frame()
  coltab(ease_cdl) <- cdl_legend[, .(value = code, red = esri_red, green = esri_green, blue = esri_blue)] |>
    as.data.frame()


  # integer field so cell values are the county codes, not factor indices
  nd_cnties$cnty_id <- as.integer(nd_cnties$COUNTYFP)
  nd_cnties_r <- rasterize(vect(nd_cnties), ease_cdl, field = "cnty_id")

  # pixel counts for each county x crop pair (NA/masked cells dropped)
  nd_cnties_tab <- crosstab(c(nd_cnties_r, ease_cdl), long = TRUE, useNA = FALSE) |>
    as.data.table()
  # ease_cdl is categorical, so crosstab returns class names, not codes
  setnames(nd_cnties_tab, c("cnty_id", "class_name", "n_pix"))

  # 30 m x 30 m cell -> acres
  cell_ac <- prod(res(ease_cdl)) / 4046.8564224   # ~0.2224 ac
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


nd_cnties_tab <- lapply(years, get_county_CDL_acreage) |>
  rbindlist()

saveRDS(nd_cnties_tab, "data/processed/cdl_county_acres_nd.rds")


y <- 2025
cdl <- rast(file.path("data/raw/CDL",paste0(y,"_30m_cdls.tif")))


stutsman <- nd_cnties[nd_cnties$COUNTYFP == "093",] |>
  st_transform(5070)


stutsman_ease <- st_intersection(fws_tracts_nd, st_geometry(stutsman))


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

# Parameters to query on and data call
params <- list(
  commodity_desc = "CORN", 
  short_desc = "CORN, GRAIN - PRICE RECEIVED, MEASURED IN $ / BU",
  agg_level_desc = "STATE",
  state_fips_code = "38",
  year = years,
  freq_desc = "ANNUAL"
  )

corn_prices <- nassqs(params) |>
  as.data.table()

cols <- c("state_fips_code","county_code","year","Value")

cnty_pasture_rents <- cnty_pasture_rents[, ..cols]


# What crops to include
nd_cnties_tab <- readRDS("data/processed/cdl_county_acres_nd.rds")

acres_nd <- nd_cnties_tab[, .(acres = sum(acres,na.rm=TRUE)), by = .(class_name,year)]

setorder(acres_nd,-year,-acres)


# yield loss adjacent to wetland
# per acre returns
# additional diesel cost from 

# field boundaries - satellite derived - from usda

# check whether easements and clus are the same

# do water body analysis - feasibility

# farmable impacts 
# easement acres vs farmable
