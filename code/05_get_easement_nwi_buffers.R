# Step 05: wetland + setback buffer polygons within each easement tract, under
# the FWS-approximation (1.2 in drawdown) and NRCS (12 in drawdown) setbacks.
#
# In:  data/processed/fws_tracts_nd.rds (step 01)
#      data/processed/nwi_tracts_nd.rds (step 03)
#      data/processed/LateralEffect_Tract_LE.csv (external, from step 04's soils)
#      data/processed/cdl_legend.rds (step 02), data/raw/CDL/2025_30m_cdls.tif
# Out: data/processed/nwi_buffers_<LE>_nd.rds      wetland and buffer-ring polygons
#      data/processed/nwi_buffer_acres_<LE>_nd.csv  per-tract acres
#      data/processed/nwi_wet_buf_<LE>_nd.rds       wetland + buffer, one polygon per
#                                                   tract (used by step 06)
#      figures/example_tract_nwi_buffer.pdf
#   where <LE> is LE_2ft_3in_1p2in_ft (FWS) or LE_2ft_3in_12in_ft (NRCS)
#
# MEMO: Table "Acreage Measures" (hand-entered in arpc-memo.tex) rows for
#       NWI wetlands inside easement tracts, setback buffer, and wetlands plus
#       setback buffer under each standard are column sums of
#       nwi_buffer_acres_<LE>_nd.csv (wet_acres, buffer_acres, wet_buf_acres).
#       The same totals give the 4.0M / 2.3M acre figures in Key Insights.
# MEMO: Key Insights "about 27% of tracts" fully covered by wetlands plus FWS
#       setbacks is the share of tracts with wet_buf_share ~ 1 in the FWS CSV
#       (computed by hand, not in this script).

rm(list = ls())
gc()

source("code/setup.R")


fws_tracts_nd <- readRDS("data/processed/fws_tracts_nd.rds") |>
  st_transform(5070)

# row id, since the first tract column has duplicates; matches steps 03 and 04
fws_tracts_nd$tract_uid <- seq_len(nrow(fws_tracts_nd))

# wetland x tract pieces from step 03 (already clipped to tracts, EPSG:5070)
nwi_ease <- readRDS("data/processed/nwi_tracts_nd.rds")

# lateral effect setback distances (ft) by tract_uid; NA where the soil has no LE
ease_buffers <- fread("data/processed/LateralEffect_Tract_LE.csv")

le_vars <- grep("^LE_", names(ease_buffers), value = TRUE)


# one (multi)polygon of all NWI wetland per tract
nwi_tract <- aggregate(vect(nwi_ease[, "tract_uid"]), by = "tract_uid") |>
  st_as_sf()
nwi_tract <- st_cast(nwi_tract[, "tract_uid"], "MULTIPOLYGON", warn = FALSE)


# element-wise geometry op on two equal-length sfc; st_intersection/st_difference
# on sfc objects do all pairs, which blows up when big buffers span many tracts
pairwise_op <- function(x, y, op) {
  out <- mapply(function(a, b) {
    g <- op(st_sfc(a, crs = 5070), st_sfc(b, crs = 5070))
    # clipping can leave collections with lines/points along shared edges;
    # keep polygons only, and an empty geometry (not a dropped row) if none
    polys <- Filter(function(p) inherits(p, c("POLYGON", "MULTIPOLYGON")) && !st_is_empty(p),
                    if (length(g) && inherits(g[[1]], "GEOMETRYCOLLECTION")) as.list(g[[1]]) else as.list(g))
    if (length(polys) == 0) st_multipolygon() else st_cast(st_union(st_sfc(polys)), "MULTIPOLYGON")[[1]]
  }, x, y, SIMPLIFY = FALSE)
  st_sfc(out, crs = 5070)
}


# wetland and setback buffer polygons for each tract, buffer clipped to the tract;
# le_var is any LE_* column in ease_buffers (setback distance in feet)
get_wetland_buffers <- function(le_var) {

  wet <- merge(nwi_tract,
               ease_buffers[, .(tract_uid, buffer_ft = get(le_var))],
               by = "tract_uid")
  wet <- wet[!is.na(wet$buffer_ft), ]

  tract_geom <- st_geometry(fws_tracts_nd)[match(wet$tract_uid, fws_tracts_nd$tract_uid)]

  # wetland + setback, kept within the easement tract
  wet_buf <- st_buffer(st_geometry(wet), dist = wet$buffer_ft * 0.3048) |>
    pairwise_op(tract_geom, st_intersection)

  # setback ring only (buffer area outside the wetland)
  buf_ring <- pairwise_op(wet_buf, st_geometry(wet), st_difference)

  out <- rbind(
    st_sf(tract_uid = wet$tract_uid, zone = "wetland", geometry = st_geometry(wet)),
    st_sf(tract_uid = wet$tract_uid, zone = "buffer",  geometry = buf_ring)
  )
  out <- out[!st_is_empty(out), ]

  out$TRACTNO   <- fws_tracts_nd$TRACTNO[match(out$tract_uid, fws_tracts_nd$tract_uid)]
  out$le_var    <- le_var
  out$buffer_ft <- wet$buffer_ft[match(out$tract_uid, wet$tract_uid)]
  out$acres     <- as.numeric(st_area(out)) / 4046.8564224

  out <- out[order(out$tract_uid, out$zone != "wetland"),
             c("tract_uid", "TRACTNO", "zone", "le_var", "buffer_ft", "acres")]
  out
}


# setback buffer currently used in the analysis; swap in other le_vars as needed
nwi_buffers <- get_wetland_buffers("LE_2ft_3in_1p2in_ft")

saveRDS(nwi_buffers, "data/processed/nwi_buffers_LE_2ft_3in_1p2in_ft_nd.rds")


# per-tract acreage: wetland + setback buffer together is the protected area
tract_buffer_acres <- function(buffers) {
  d <- dcast(as.data.table(st_drop_geometry(buffers)),
             tract_uid + TRACTNO + le_var + buffer_ft ~ zone,
             value.var = "acres", fill = 0)
  setnames(d, c("wetland", "buffer"), c("wet_acres", "buffer_acres"))
  d[, wet_buf_acres := wet_acres + buffer_acres]
  d[, tract_acres := as.numeric(st_area(fws_tracts_nd))[tract_uid] / 4046.8564224]
  d[, wet_buf_share := wet_buf_acres / tract_acres]
  d[]
}

nwi_buffer_acres <- tract_buffer_acres(nwi_buffers)

# MEMO: source of the FWS-standard rows in the "Acreage Measures" table
fwrite(nwi_buffer_acres, "data/processed/nwi_buffer_acres_LE_2ft_3in_1p2in_ft_nd.csv")


# combined wetland + setback buffer, one polygon per tract
union_wet_buf <- function(buffers) {
  u <- aggregate(vect(buffers[, "tract_uid"]), by = "tract_uid") |>
    st_as_sf()
  u <- st_cast(u[, "tract_uid"], "MULTIPOLYGON", warn = FALSE)

  attrs <- unique(st_drop_geometry(buffers)[, c("tract_uid", "TRACTNO", "le_var", "buffer_ft")])
  u <- merge(u, attrs, by = "tract_uid")
  u$acres <- as.numeric(st_area(u)) / 4046.8564224

  u[order(u$tract_uid), c("tract_uid", "TRACTNO", "le_var", "buffer_ft", "acres")]
}

nwi_wet_buf <- union_wet_buf(nwi_buffers)

saveRDS(nwi_wet_buf, "data/processed/nwi_wet_buf_LE_2ft_3in_1p2in_ft_nd.rds")

# NRCS standard (12 in drawdown), same tile design, for the memo's acreage range
# MEMO: the NRCS acreage CSV is the source of the NRCS-standard rows in the
#       "Acreage Measures" table
nrcs_buffers <- get_wetland_buffers("LE_2ft_3in_12in_ft")
saveRDS(nrcs_buffers, "data/processed/nwi_buffers_LE_2ft_3in_12in_ft_nd.rds")
fwrite(tract_buffer_acres(nrcs_buffers), "data/processed/nwi_buffer_acres_LE_2ft_3in_12in_ft_nd.csv")
saveRDS(union_wet_buf(nrcs_buffers), "data/processed/nwi_wet_buf_LE_2ft_3in_12in_ft_nd.rds")

# all LE distances:
# for (v in le_vars) {
#   b <- get_wetland_buffers(v)
#   saveRDS(b, paste0("data/processed/nwi_buffers_", v, "_nd.rds"))
#   fwrite(tract_buffer_acres(b), paste0("data/processed/nwi_buffer_acres_", v, "_nd.csv"))
#   saveRDS(union_wet_buf(b), paste0("data/processed/nwi_wet_buf_", v, "_nd.rds"))
# }


# example tract maps: easement, wetlands, FWS and NRCS setbacks, and crops
# MEMO: Figure "Easement Tract 112X: Wetlands and 180 ft Setback Buffer"
#       (fig:tract_buffer). The text's numbers for this tract (22.7 wetland ac,
#       76.6 setback ac, 99 ac / 62% of the 160-ac tract) are in ex_ac
#       (wet_acres, buffer_acres, wet_buf_acres, wet_buf_share, tract_acres);
#       the plot subtitle prints the totals.
# TRACTNO repeats across WMDs, so pick tracts by tract_uid
ex_uids <- c(101, 11948, 11179)   # TRACTNO 112X (Valley City), 197X and 17X (Tewaukon)
ex_year <- 2025

nrcs_buffer_acres <- tract_buffer_acres(nrcs_buffers)

# CDL crops on the tract; same non-crop classes dropped as in step 06
cdl_legend <- readRDS("data/processed/cdl_legend.rds")
drop_ids   <- c(61,81:83,87,88,92,111,112,121:124,131,141:143,152,176,190,195)
cdl_ex_year <- rast(file.path("data/raw/CDL", paste0(ex_year, "_30m_cdls.tif")))

setback_levels <- c("FWS setback", "NRCS setback")

ex_data <- lapply(ex_uids, function(uid) {
  tract <- fws_tracts_nd[fws_tracts_nd$tract_uid == uid, ]
  ac      <- nwi_buffer_acres[tract_uid == uid]
  ac_nrcs <- nrcs_buffer_acres[tract_uid == uid]

  # wetland + setback outline for each standard
  setbacks <- rbind(
    st_sf(standard = "FWS setback",
          geometry = st_union(nwi_buffers[nwi_buffers$tract_uid == uid, ])),
    st_sf(standard = "NRCS setback",
          geometry = st_union(nrcs_buffers[nrcs_buffers$tract_uid == uid, ]))
  )
  setbacks$standard <- factor(setbacks$standard, levels = setback_levels)

  cdl <- crop(cdl_ex_year, tract, mask = TRUE, touches = FALSE)
  cdl <- ifel(cdl %in% drop_ids, NA, cdl)
  # stray Other Hay/Non Alfalfa (37) pixel in 11179
  if (uid == 11179) cdl <- ifel(cdl == 37, NA, cdl)
  # cropped share of wetland + FWS setback, the same measure as the memo's acreage table
  wet_fws <- setbacks[setbacks$standard == "FWS setback", ]
  crop_ac <- sum(!is.na(values(mask(cdl, vect(wet_fws), touches = FALSE)))) *
    prod(res(cdl)) / 4046.8564224
  crop_pct <- 100 * crop_ac / (as.numeric(st_area(wet_fws)) / 4046.8564224)

  cdl_df <- as.data.frame(cdl, xy = TRUE, na.rm = TRUE) |>
    setNames(c("x", "y", "code")) |>
    as.data.table()
  cdl_df <- merge(cdl_df, cdl_legend[, .(code, class_name, colorhex)], by = "code")

  # shares of the tract; FWS and NRCS setback rings measured separately
  pct <- function(x) 100 * x / ac$tract_acres
  subtitle <- sprintf(paste0("Wetland %.0f%%\n",
                             "FWS setback %s ft: %.0f%%\n",
                             "NRCS setback %s ft: %.0f%%\n",
                             "Cropped: %.0f%% of wetland + FWS setback"),
                      pct(ac$wet_acres),
                      ac$buffer_ft, pct(ac$buffer_acres),
                      ac_nrcs$buffer_ft, pct(ac_nrcs$buffer_acres),
                      crop_pct)

  list(tract = tract,
       wet = nwi_buffers[nwi_buffers$tract_uid == uid & nwi_buffers$zone == "wetland", ],
       setbacks = setbacks, cdl_df = cdl_df, subtitle = subtitle)
})

# one fill scale across panels so the crop legend can be collected
crop_colors <- unique(rbindlist(lapply(ex_data, `[[`, "cdl_df"))[, .(class_name, colorhex)])
# alpha set in the palette, not the layer, so legend keys match the map
crop_colors <- setNames(alpha(crop_colors$colorhex, 0.55), crop_colors$class_name)
fill_colors <- c(crop_colors, "NWI wetland" = "#688CC0")

plot_ex_tract <- function(d) {
  ggplot() +
    geom_sf(data = d$tract, fill = "grey92", color = NA) +
    geom_raster(data = d$cdl_df, aes(x, y, fill = class_name),
                show.legend = c(fill = TRUE)) +   # draw every fill key so patchwork collects one legend
    geom_sf(data = d$wet, aes(fill = "NWI wetland"), color = NA, show.legend = FALSE) +
    geom_sf(data = d$setbacks, aes(color = standard, linetype = standard),
            fill = NA, linewidth = 0.6) +
    geom_sf(data = d$tract, fill = NA, color = "grey20", linewidth = 0.5) +
    scale_fill_manual(values = fill_colors,
                      breaks = c(names(crop_colors), "NWI wetland"),
                      limits = names(fill_colors),
                      name = NULL) +
    scale_color_manual(values = setNames(c("#B2582B", NDSU_COLORS[["ndsu_night"]]), setback_levels),
                       limits = setback_levels, name = NULL) +
    scale_linetype_manual(values = setNames(c("solid", "22"), setback_levels),
                          limits = setback_levels, name = NULL) +
    guides(color = guide_legend(override.aes = list(fill = NA))) +
    coord_sf(datum = NA) +
    labs(subtitle = d$subtitle) +
    theme_void() +
    theme(plot.subtitle = element_text(size = 8.5, lineheight = 1.1,
                                       margin = margin(t = 2, b = 6)))
}

p_ex <- patchwork::wrap_plots(lapply(ex_data, plot_ex_tract), nrow = 1) +
  patchwork::plot_layout(guides = "collect") +
  patchwork::plot_annotation(
    caption = paste0("Wetland and setback percentages are shares of tract acres; setback shares exclude the wetland itself.\n",
                     "Cropped share is of the wetland + FWS setback area, from the ", ex_year, " CDL.\n",
                     "Grey: easement land not in crops. Outlines enclose wetlands plus setback.\n",
                     "Source: USFWS NWI, USFWS realty tracts, USDA NASS CDL, lateral effect distances."),
    theme = theme(plot.caption = element_text(size = 8, hjust = 0))
  ) &
  theme(legend.position = "bottom",
        legend.box = "vertical",
        legend.text = element_text(size = 8),
        plot.margin = margin(6, 6, 6, 6))

ggsave("figures/example_tract_nwi_buffer.pdf", p_ex,
       width = 9, height = 4.8, device = cairo_pdf, bg = "white")
