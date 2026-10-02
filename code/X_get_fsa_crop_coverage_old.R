

rm(list = ls())
gc()

source("code/setup.R")


fsa_clu_nd_geometry_file <-  "D:/data/processed/FSA/clu/state/ND/fy25_geometry.rds"
fsa_clu_nd_attribute_file <-  "D:/data/processed/FSA/clu/state/ND/fy25_attributes.parquet"

fsa_clu_nd_geometry <- readRDS(fsa_clu_nd_geometry_file) |>
  st_set_geometry("geometry") |>
  st_transform(5070)

fsa_clu_nd_attributes <- as.data.table(
  read_parquet(fsa_clu_nd_attribute_file)  # , col_select = cols
)


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



stutsman <- counties(state = "ND", year = 2022)
stutsman <- stutsman[stutsman$COUNTYFP == "093",] |>
  st_transform(5070)


fws_tracts_nd <- readRDS("data/processed/fws_tracts_nd.rds") |>
  st_transform(5070)

easements_nd <- st_intersection(fsa_clu_nd_geometry,fws_tracts_nd)

easements_nd_dt <- st_drop_geometry(easements_nd)

fsa_clu <- merge(easements_nd_dt,fsa_clu_nd_attributes,by =c("clu_identifier"))
setDT(fsa_clu)



stutsman_fn <- unique(
  fsa_clu[county_code == "093",.(farm_number,state_code,county_code)]
)

stutsman_fn$farm_number <- sprintf("%07d", as.integer(stutsman_fn$farm_number))

setnames(stutsman_fn,c("state_code","county_code"),c("state_fsa_code","county_fsa_code"))


check <- farm_panel_nd[county_fsa_code == "093"]

stutsman_ease_data <- farm_panel_nd[
  stutsman_fn,
  on = .(farm_number, state_fsa_code, county_fsa_code),
  nomatch = NULL
]


stutsman_clu <- unique(
  stutsman_ease_data[!is.na(clu_identifier),.(clu_identifier,farm_number,state_fsa_code,county_fsa_code)]
)


stutsman_ease <- st_filter(easements_nd, stutsman)
stutsman_ease <- stutsman_ease[stutsman_ease$clu_identifier %in% stutsman_clu, ]


plot(st_geometry(stutsman), border = "grey40")
plot(st_geometry(stutsman_ease), col = "darkgreen", border = NA, add = TRUE)


# cols <- c("farm_number", "state_fsa_code", "county_fsa_code", "program_year", 
#           "base_acres_2023", "total_reported_acres")
# 
# 
# farm_panel_nd <- unique(
#   farm_panel_nd[, ..cols])
