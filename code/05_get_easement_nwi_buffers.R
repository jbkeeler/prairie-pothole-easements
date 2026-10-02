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


# example tract map: easement, wetlands, FWS and NRCS setbacks, and crops
# MEMO: Figure "Easement Tract 112X: Wetlands and 180 ft Setback Buffer"
#       (fig:tract_buffer). The text's numbers for this tract (22.7 wetland ac,
#       76.6 setback ac, 99 ac / 62% of the 160-ac tract) are in ex_ac
#       (wet_acres, buffer_acres, wet_buf_acres, wet_buf_share, tract_acres);
#       the plot subtitle prints the totals.
ex_uid  <- 101   # TRACTNO 112X; 160 ac, 20 wetland polygons, 180 ft setback
ex_year <- 2025

ex_tract <- fws_tracts_nd[fws_tracts_nd$tract_uid == ex_uid, ]
ex_wet   <- nwi_buffers[nwi_buffers$tract_uid == ex_uid & nwi_buffers$zone == "wetland", ]
ex_ac    <- nwi_buffer_acres[tract_uid == ex_uid]
ex_ac_nrcs <- tract_buffer_acres(nrcs_buffers)[tract_uid == ex_uid]

# wetland + setback outline for each standard
ex_setbacks <- rbind(
  st_sf(standard = sprintf("FWS setback (%s ft)", ex_ac$buffer_ft),
        geometry = st_union(nwi_buffers[nwi_buffers$tract_uid == ex_uid, ])),
  st_sf(standard = sprintf("NRCS setback (%s ft)", ex_ac_nrcs$buffer_ft),
        geometry = st_union(nrcs_buffers[nrcs_buffers$tract_uid == ex_uid, ]))
)
ex_setbacks$standard <- factor(ex_setbacks$standard, levels = ex_setbacks$standard)

# CDL crops on the tract; same non-crop classes dropped as in step 06
cdl_legend <- readRDS("data/processed/cdl_legend.rds")
drop_ids   <- c(61,81:83,87,88,92,111,112,121:124,131,141:143,152,176,190,195)

ex_cdl <- rast(file.path("data/raw/CDL", paste0(ex_year, "_30m_cdls.tif"))) |>
  crop(ex_tract, mask = TRUE, touches = FALSE)
ex_cdl <- ifel(ex_cdl %in% drop_ids, NA, ex_cdl)

cell_ac <- prod(res(ex_cdl)) / 4046.8564224

# cropped acres inside each standard's wetland + setback area
ex_crop_ac <- sapply(seq_len(nrow(ex_setbacks)), function(i) {
  sum(!is.na(values(mask(ex_cdl, vect(ex_setbacks[i, ]), touches = FALSE)))) * cell_ac
})

ex_cdl_df <- as.data.frame(ex_cdl, xy = TRUE, na.rm = TRUE) |>
  setNames(c("x", "y", "code")) |>
  as.data.table()
ex_cdl_df <- merge(ex_cdl_df, cdl_legend[, .(code, class_name, colorhex)], by = "code")

crop_colors <- unique(ex_cdl_df[, .(class_name, colorhex)])
crop_colors <- setNames(crop_colors$colorhex, crop_colors$class_name)
fill_colors <- c(crop_colors, "NWI wetland" = "#688CC0")

p_ex <- ggplot() +
  geom_sf(data = ex_tract, fill = "grey92", color = NA) +
  geom_raster(data = ex_cdl_df, aes(x, y, fill = class_name), alpha = 0.55) +
  geom_sf(data = ex_wet, aes(fill = "NWI wetland"), color = NA) +
  geom_sf(data = ex_setbacks, aes(color = standard, linetype = standard),
          fill = NA, linewidth = 0.8) +
  geom_sf(data = ex_tract, fill = NA, color = "grey20", linewidth = 0.6) +
  scale_fill_manual(values = fill_colors,
                    breaks = c(names(crop_colors), "NWI wetland"),
                    name = NULL) +
  scale_color_manual(values = c("#B2582B", NDSU_COLORS[["ndsu_night"]]), name = NULL) +
  scale_linetype_manual(values = c("solid", "22"), name = NULL) +
  coord_sf(datum = NA) +
  labs(title = paste0("Easement tract ", ex_ac$TRACTNO, ": wetlands, setbacks, and ",
                      ex_year, " crops"),
       subtitle = sprintf(paste0("FWS: wetland + setback %.1f ac (%.0f%% of %.1f ac tract), %.1f ac cropped\n",
                                 "NRCS: wetland + setback %.1f ac (%.0f%% of tract), %.1f ac cropped"),
                          ex_ac$wet_buf_acres, 100 * ex_ac$wet_buf_share, ex_ac$tract_acres,
                          ex_crop_ac[1],
                          ex_ac_nrcs$wet_buf_acres, 100 * ex_ac_nrcs$wet_buf_share,
                          ex_crop_ac[2]),
       caption = paste0("Grey: easement land not in crops. Outlines enclose wetlands plus setback.\n",
                        "Source: USFWS NWI, USFWS realty tracts, USDA NASS CDL, lateral effect distances.")) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.box = "vertical",
        plot.title = element_blank(),
        plot.margin = margin(10, 10, 10, 10))

ggsave("figures/example_tract_nwi_buffer.pdf", p_ex,
       width = 7, height = 7.5, device = cairo_pdf, bg = "white")
