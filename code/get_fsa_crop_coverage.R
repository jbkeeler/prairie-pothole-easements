

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

# Keep the spatial work in sf and the tabular work in data.table; only pass IDs between them
clu_ids_2022 <- unique(
  farm_panel_nd[program_year == 2022 & !is.na(clu_identifier), clu_identifier]
)

stutsman_ease <- st_filter(easements_nd, stutsman)
stutsman_ease <- stutsman_ease[stutsman_ease$clu_identifier %in% clu_ids_2022, ]

plot(st_geometry(stutsman), border = "grey40")
plot(st_geometry(stutsman_ease), col = "darkgreen", border = NA, add = TRUE)

# from khashi
# NIW diff