# Extra: statewide map of every FWS easement tract with its NWI wetlands and
# FWS-standard (1.2 in drawdown) setback buffers, from step 05's output, and
# the same map for Stutsman County.
#
# In:  data/processed/fws_tracts_nd.rds (step 01)
#      data/processed/nwi_buffers_LE_2ft_3in_1p2in_ft_nd.rds (step 05)
#      data/processed/nwi_wet_buf_LE_2ft_3in_1p2in_ft_nd.rds (step 05)
# Out: figures/nd_tracts_nwi_buffers.png
#      figures/stutsman_tracts_nwi_buffers.png
#      data/processed/nwi_setback_<LE>_nd.gpkg  setback buffer only (no wetland),
#                       one polygon per tract x field soil
#   where <LE> is LE_2ft_3in_1p2in_ft (FWS) or LE_2ft_3in_12in_ft (NRCS)

rm(list = ls())
gc()

source("code/setup.R")


fws_tracts_nd <- readRDS("data/processed/fws_tracts_nd.rds") |>
  st_transform(5070)

nwi_buffers <- readRDS("data/processed/nwi_buffers_LE_2ft_3in_1p2in_ft_nd.rds")
nwi_wet     <- nwi_buffers[nwi_buffers$zone == "wetland", ]

# setback buffer areas as GeoPackages (a shapefile would pass its 2 GB limit),
# EPSG:5070; acres sum to step 05's buffer_acres
write_setback <- function(buffers, le_var) {
  st_write(buffers[buffers$zone == "buffer",
                   c("tract_uid", "TRACTNO", "mukey", "musym", "buffer_ft", "acres")],
           paste0("data/processed/nwi_setback_", le_var, "_nd.gpkg"), delete_dsn = TRUE, quiet = TRUE)
}
write_setback(nwi_buffers, "LE_2ft_3in_1p2in_ft")
write_setback(readRDS("data/processed/nwi_buffers_LE_2ft_3in_12in_ft_nd.rds"), "LE_2ft_3in_12in_ft")
gc()

# wetland + setback, one crack-free polygon per tract; outlining this rather
# than the buffer rings keeps the soil boundaries between rings off the map
nwi_wet_buf <- readRDS("data/processed/nwi_wet_buf_LE_2ft_3in_1p2in_ft_nd.rds")

nd_counties <- counties(state = "ND", year = 2022) |>
  st_transform(5070)

nwi_wet$layer       <- "NWI wetland"
fws_tracts_nd$layer <- "Easement tract"
nwi_wet_buf$layer   <- "Wetland + setback buffer"
line_levels <- c("Easement tract", "Wetland + setback buffer")

# wetlands are filled; tracts and buffers are outlines only, drawn after the
# wetlands so the borders show where they overlap (buffer outline on top)
plot_zones <- function(base, wet, tracts, wet_buf, lw) {
  ggplot() +
    geom_sf(data = base, fill = "white", color = "grey75", linewidth = 0.15) +
    geom_sf(data = wet, aes(fill = layer), color = NA) +
    geom_sf(data = tracts, aes(color = layer), fill = NA, linewidth = lw) +
    geom_sf(data = wet_buf, aes(color = layer), fill = NA, linewidth = lw) +
    scale_fill_manual(values = c("NWI wetland" = "#688CC0"), name = NULL) +
    scale_color_manual(values = setNames(c("grey70", "black"), line_levels),
                       limits = line_levels, name = NULL) +
    guides(fill  = guide_legend(order = 1),
           color = guide_legend(order = 2, override.aes = list(fill = NA, linewidth = 0.5))) +
    coord_sf(datum = NA) +
    labs(caption = paste0("Setback buffers under the FWS standard (1.2 in drawdown); the buffer outline encloses the wetland plus its setback.\n",
                          "Source: USFWS NWI, USFWS realty tracts, lateral effect distances.")) +
    theme_void() +
    theme(legend.position = "bottom",
          plot.caption = element_text(size = 8, hjust = 0),
          plot.margin = margin(6, 6, 6, 6))
}

p_all <- plot_zones(nd_counties, nwi_wet, fws_tracts_nd, nwi_wet_buf, lw = 0.05)

# PNG rather than PDF: hundreds of thousands of polygons make a vector file unwieldy
ggsave("figures/nd_tracts_nwi_buffers.png", p_all,
       width = 11, height = 7, dpi = 800, bg = "white")


# Stutsman County, everything clipped to the county
stutsman <- nd_counties[nd_counties$COUNTYFP == "093", ]
st_agr(nwi_wet)       <- "constant"
st_agr(fws_tracts_nd) <- "constant"
st_agr(nwi_wet_buf)   <- "constant"

p_stutsman <- plot_zones(stutsman,
                         st_intersection(nwi_wet, st_geometry(stutsman)),
                         st_intersection(fws_tracts_nd, st_geometry(stutsman)),
                         st_intersection(nwi_wet_buf, st_geometry(stutsman)),
                         lw = 0.15)

ggsave("figures/stutsman_tracts_nwi_buffers.png", p_stutsman,
       width = 8, height = 8, dpi = 800, bg = "white")
