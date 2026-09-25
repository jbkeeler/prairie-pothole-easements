

rm(list = ls())
gc()

source("code/00_master.R")


fsa_clu_nd_geometry_file <-  "D:/data/processed/FSA/clu/state/ND/fy25_geometry.rds"
fsa_clu_nd_attribute_file <-  "D:/data/processed/FSA/clu/state/ND/fy25_attributes.parquet"

fsa_clu_nd_geometry <- readRDS(fsa_clu_nd_geometry_file) |>
  st_set_geometry("geometry") |>
  st_transform(5070)

fsa_clu_nd_attributes <- as.data.table(
  read_parquet(fsa_clu_nd_attribute_file)  # , col_select = cols
)


fws_tracts_nd <- readRDS("data/processed/fws_tracts_nd.rds") |>
  st_transform(5070)

easements_nd <- st_intersection(fsa_clu_nd_geometry,fws_tracts_nd)

easements_nd <- merge(easements_nd,fsa_clu_nd_attributes,by =c("clu_identifier"))


easements_nd$acres <- as.numeric(st_area(easements_nd)) / 4046.8564224

# Dissolve before measuring so overlapping tracts aren't double counted
easement_acres_total <- as.numeric(st_area(st_union(easements_nd))) / 4046.8564224
easement_acres_sum <- sum(easements_nd$acres)

acreage_by_crop_nd <- as.data.table(
  read_parquet("D:/data/processed/FSA/panel/acreage_by_crop/ND/part-0.parquet")
)

cols <- c("farm_number", "state_fsa_code", "county_fsa_code", "clu_identifier", "program_year", "fsa_crop_code",
          "crop_name","crop_type_name","intended_use_desc","irrigation_practice_code", "reported_acres",
          "determined_acres","farmland_total_acreage", "cropland_total_acreage")

acreage_by_crop_nd <- unique(
  acreage_by_crop_nd[, ..cols])




farm_panel_nd <- as.data.table(
  read_parquet("D:/data/processed/FSA/panel/farm_panel/ND/part-0.parquet")
)

cols <- c("farm_number", "state_fsa_code", "county_fsa_code", "program_year", 
          "base_acres_2023", "total_reported_acres")


farm_panel_nd <- unique(
  farm_panel_nd[, ..cols])


farm_panel_nd <- merge(farm_panel_nd,acreage_by_crop_nd,by=c("farm_number", "state_fsa_code", "county_fsa_code", "program_year"),all.x=TRUE)

# farm_panel_nd <- merge(farm_panel_nd,easements_nd,by=c("clu_identifier","farm_number"))

easement_acres_sum <- sum(easements_nd$acres)

stutsman_farms <- farm_panel_nd[farm_panel_nd$county_fsa_code == "093",]

stutsman_farms <- stutsman_farms[!is.na(stutsman_farms$clu_identifier)]

stutsman_farms_ease <- stutsman_farms[!is.na(stutsman_farms$INTTYPE2)]

check <- merge(stutsman_farms,easements_nd,by=c("clu_identifier"))




stutsman <- counties(state = "ND", year = 2022)
stutsman <- stutsman[stutsman$COUNTYFP == "093",] |>
  st_transform(5070)

stutsman_farms <- st_intersection(fsa_clu_nd_geometry, stutsman)
stutsman_farms <- sf::st_drop_geometry(stutsman_farms)  # returns a plain data.frame
setDT(stutsman_farms)

stutsman_farms <- merge(stutsman_farms,farm_panel_nd,by=c("clu_identifier"))

stutsman_farms <- stutsman_farms[program_year == 2022]

cols <- c("clu_identifier")
stutsman_farms <- stutsman_farms[, ..cols]

check <- merge(stutsman_farms,easements_nd,by=c("clu_identifier"))

check <- st_as_sf(check)

plot(check$geometry)

# from khashi
# NIW diff