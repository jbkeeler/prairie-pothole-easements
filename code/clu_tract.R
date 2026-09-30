
rm(list = ls())
gc()

source("code/00_master.R")


fsa_clu_nd_geometry_file <-  "D:/data/processed/FSA/clu/state/ND/fy25_geometry.rds"
fsa_clu_nd_attribute_file <-  "D:/data/processed/FSA/clu/state/ND/fy25_attributes.parquet"

fsa_clu_nd_geometry <- readRDS(fsa_clu_nd_geometry_file) |>
  st_set_geometry("geometry") |>
  st_transform(5070)


stutsman <- counties(state = "ND", year = 2022)
stutsman <- stutsman[stutsman$COUNTYFP == "093",] |>
  st_transform(5070)


fws_tracts_nd <- readRDS("data/processed/fws_tracts_nd.rds") |>
  st_transform(5070)

stutsman_ease <- st_intersection(fws_tracts_nd,st_geometry(stutsman))

stutsman_clu <- st_intersection(fsa_clu_nd_geometry,st_geometry(stutsman))


p_cdl <- ggplot() +
  # easement footprint, so non-crop easement land still shows
  geom_sf(data = stutsman_ease, fill = "forestgreen", color = NA) +
  geom_sf(data = stutsman_clu, fill = "black", color = "grey30", linewidth = 0.4) +
  geom_sf(data = stutsman, fill = NA, color = "grey30", linewidth = 0.4) +
  coord_sf(datum = NA) +
  labs(title = "",
       caption = "") +
  theme_void() +
  theme(plot.margin = margin(10, 10, 10, 10))

ggsave("figures/stutsman_eas_clu.pdf", p_cdl,
       width = 7, height = 6.5, device = cairo_pdf, bg = "white")