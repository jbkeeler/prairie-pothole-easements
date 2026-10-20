# Step 05: wetland + setback buffer polygons within each easement tract, under
# the FWS-approximation (1.2 in drawdown) and NRCS (12 in drawdown) setbacks.
# The setback distance varies within a tract: each field soil gets the distance
# for that soil paired with the soil of the wetland basin it is measured from.
#
# In:  data/processed/fws_tracts_nd.rds (step 01)
#      data/processed/nwi_tracts_nd.rds (step 03)
#      data/processed/nwi_basin_soils_nd.rds, gssurgo_tract_mupoly_nd.rds (step 04)
#      data/processed/LateralEffect_Soils_LE.csv (external, from step 04's soil pairs)
#      data/processed/cdl_legend.rds (step 02), data/raw/CDL/2025_30m_cdls.tif
# Out: data/processed/nwi_buffers_<LE>_nd.rds      wetland polygons (one per tract)
#                                                   and buffer-ring polygons (one
#                                                   per tract x field soil)
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

# dominant map unit of each wetland basin from step 04; basin_uid is the row of nwi_ease
basin_soils <- readRDS("data/processed/nwi_basin_soils_nd.rds")
nwi_ease$wet_mukey <- basin_soils$wet_mukey[match(seq_len(nrow(nwi_ease)), basin_soils$basin_uid)]

# map unit polygons clipped to tracts, one (multi)polygon per tract x map unit,
# with the tract's county
tract_mupoly <- readRDS("data/processed/gssurgo_tract_mupoly_nd.rds")
tract_mupoly$county_fips <- as.integer(tract_mupoly$county_fips)
st_agr(tract_mupoly) <- "constant"   # no per-tract warnings when clipping

# lateral effect setback distances (ft) for each county x wetland soil x field
# soil; NA where the pair has no LE
ease_buffers <- fread("data/processed/soil_pairs_le.csv")
setnames(ease_buffers, "field_mukey", "mukey")
ease_buffers[, county_fips := as.integer(county_fips)]
stopifnot(!anyDuplicated(ease_buffers[, .(county_fips, wet_mukey, mukey)]))

le_vars <- grep("^LE_", names(ease_buffers), value = TRUE)


# (multi)polygons of NWI wetland dissolved by the given columns
dissolve_wetlands <- function(x, by) {
  u <- aggregate(vect(x[, by]), by = by) |>
    st_as_sf()
  st_cast(u[, by], "MULTIPOLYGON", warn = FALSE)
}

# all NWI wetland per tract, and per tract x wetland soil (the setback source);
# basins without a map unit are wetland but get no setback
nwi_tract      <- dissolve_wetlands(nwi_ease, "tract_uid")
nwi_tract_soil <- dissolve_wetlands(nwi_ease[!is.na(nwi_ease$wet_mukey), ], c("tract_uid", "wet_mukey"))

# rows of each tract, for the per-tract loop
wet_rows  <- split(seq_len(nrow(nwi_tract_soil)), nwi_tract_soil$tract_uid)
soil_rows <- split(seq_len(nrow(tract_mupoly)), tract_mupoly$tract_uid)


# setback rings of one tract, one (multi)polygon per field soil: the part of
# each soil within its setback distance of a wetland, less the wetlands.
# le_t has the tract's wet_mukey, mukey (field soil) and buffer_ft.
tract_buffer_rings <- function(uid, le_t) {
  wet_t  <- nwi_tract_soil[wet_rows[[as.character(uid)]], ]
  soil_t <- tract_mupoly[soil_rows[[as.character(uid)]], ]

  # one buffer per wetland soil x distance; field soils often share a distance
  bufs <- unique(le_t[, .(wet_mukey, buffer_ft)])
  bufs[, buf_id := .I]
  buf_geom <- st_buffer(st_geometry(wet_t)[match(bufs$wet_mukey, wet_t$wet_mukey)],
                        dist = bufs$buffer_ft * 0.3048)
  le_t <- merge(le_t, bufs, by = c("wet_mukey", "buffer_ft"))

  # field soils reached by the same set of buffers are clipped together
  reach <- le_t[, .(buf_ids = paste(sort(buf_id), collapse = " "), buffer_ft = max(buffer_ft)), by = mukey]
  rings <- lapply(split(reach, by = "buf_ids"), function(r) {
    ids <- as.integer(strsplit(r$buf_ids[1], " ")[[1]])
    st_intersection(soil_t[soil_t$mukey %in% r$mukey, ],
                    if (length(ids) > 1) st_union(buf_geom[ids]) else buf_geom[ids])
  })
  rings <- do.call(rbind, rings)
  st_agr(rings) <- "constant"
  rings <- st_difference(rings, st_geometry(nwi_tract)[match(uid, nwi_tract$tract_uid)])

  # clipping can leave lines/points along shared edges; keep polygons only
  rings <- rings[st_dimension(rings) %in% 2, ]
  if (nrow(rings) == 0) return(NULL)
  if (any(st_is(rings, "GEOMETRYCOLLECTION"))) rings <- st_collection_extract(rings, "POLYGON")
  rings <- st_cast(rings, "MULTIPOLYGON", warn = FALSE)

  # with wetlands of several soil types in a tract, a field soil's buffer_ft is its largest setback
  rings$buffer_ft <- reach$buffer_ft[match(rings$mukey, reach$mukey)]
  rings
}


# wetland and setback buffer polygons for each tract, buffer clipped to the tract;
# le_var is any LE_* column in ease_buffers (setback distance in feet).
# Tracts where no soil pair has an LE are dropped, wetlands included.
get_wetland_buffers <- function(le_var) {

  # every wetland soil x field soil pair in each tract, with its county's distance
  le <- merge(as.data.table(st_drop_geometry(nwi_tract_soil)),
              as.data.table(st_drop_geometry(tract_mupoly))[, .(tract_uid, county_fips, mukey)],
              by = "tract_uid", allow.cartesian = TRUE)
  le <- merge(le, ease_buffers[, .(county_fips, wet_mukey, mukey, buffer_ft = get(le_var))],
              by = c("county_fips", "wet_mukey", "mukey"))
  le <- split(le[!is.na(buffer_ft)], by = "tract_uid")

  # rbindlist, since rbind on thousands of sf objects is very slow; the geometry
  # comes back as a plain list column and is rebuilt as an sfc below
  rings <- lapply(names(le), function(uid) tract_buffer_rings(as.integer(uid), le[[uid]])) |>
    rbindlist()

  wet <- nwi_tract[nwi_tract$tract_uid %in% as.integer(names(le)), ]

  out <- rbind(
    st_sf(tract_uid = wet$tract_uid, zone = "wetland", mukey = NA_integer_, musym = NA_character_,
          buffer_ft = NA_real_, geometry = st_geometry(wet)),
    st_sf(tract_uid = rings$tract_uid, zone = "buffer", mukey = rings$mukey, musym = rings$musym,
          buffer_ft = rings$buffer_ft, geometry = st_sfc(rings$geometry, crs = 5070))
  )

  out$TRACTNO <- fws_tracts_nd$TRACTNO[match(out$tract_uid, fws_tracts_nd$tract_uid)]
  out$le_var  <- le_var
  out$acres   <- as.numeric(st_area(out)) / 4046.8564224

  out <- out[order(out$tract_uid, out$zone != "wetland", out$mukey),
             c("tract_uid", "TRACTNO", "zone", "le_var", "mukey", "musym", "buffer_ft", "acres")]
  out
}


# setback buffer currently used in the analysis; swap in other le_vars as needed
nwi_buffers <- get_wetland_buffers("LE_2ft_3in_1p2in_ft")

saveRDS(nwi_buffers, "data/processed/nwi_buffers_LE_2ft_3in_1p2in_ft_nd.rds")


# per-tract acreage: wetland + setback buffer together is the protected area;
# buffer_ft_min/max are the range of setback distances over the tract's field
# soils (NA if no setback falls inside the tract)
tract_buffer_acres <- function(buffers) {
  d <- as.data.table(st_drop_geometry(buffers))[, .(
    wet_acres     = sum(acres[zone == "wetland"]),
    buffer_acres  = sum(acres[zone == "buffer"]),
    buffer_ft_min = if (any(zone == "buffer")) min(buffer_ft, na.rm = TRUE) else NA_real_,
    buffer_ft_max = if (any(zone == "buffer")) max(buffer_ft, na.rm = TRUE) else NA_real_
  ), by = .(tract_uid, TRACTNO, le_var)]
  d[, wet_buf_acres := wet_acres + buffer_acres]
  d[, tract_acres := as.numeric(st_area(fws_tracts_nd))[tract_uid] / 4046.8564224]
  d[, wet_buf_share := wet_buf_acres / tract_acres]
  d[]
}

nwi_buffer_acres <- tract_buffer_acres(nwi_buffers)

# MEMO: source of the FWS-standard rows in the "Acreage Measures" table
fwrite(nwi_buffer_acres, "data/processed/nwi_buffer_acres_LE_2ft_3in_1p2in_ft_nd.csv")


# neighbouring rings are clipped from different buffers, so their shared edges
# don't match exactly; their union keeps zero-area cracks and slivers that plot
# as stray lines. A 1 cm closing (grow, then shrink) seals them; mitre joins
# keep the vertex count and the area change is effectively zero.
close_cracks <- function(g, eps = 0.01) {
  g |>
    st_buffer(eps,  joinStyle = "MITRE", mitreLimit = 2) |>
    st_buffer(-eps, joinStyle = "MITRE", mitreLimit = 2)
}

# combined wetland + setback buffer, one polygon per tract
union_wet_buf <- function(buffers) {
  u <- aggregate(vect(buffers[, "tract_uid"]), by = "tract_uid") |>
    st_as_sf()
  u <- st_cast(u[, "tract_uid"], "MULTIPOLYGON", warn = FALSE)
  st_agr(u) <- "constant"
  u <- st_cast(close_cracks(u), "MULTIPOLYGON", warn = FALSE)

  attrs <- unique(st_drop_geometry(buffers)[, c("tract_uid", "TRACTNO", "le_var")])
  u <- merge(u, attrs, by = "tract_uid")
  u$acres <- as.numeric(st_area(u)) / 4046.8564224

  u[order(u$tract_uid), c("tract_uid", "TRACTNO", "le_var", "acres")]
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
          geometry = close_cracks(st_union(nwi_buffers[nwi_buffers$tract_uid == uid, ]))),
    st_sf(standard = "NRCS setback",
          geometry = close_cracks(st_union(nrcs_buffers[nrcs_buffers$tract_uid == uid, ])))
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
  # setback distance, or its range over the tract's field soils
  ft <- function(a) paste(unique(c(a$buffer_ft_min, a$buffer_ft_max)), collapse = "-")
  subtitle <- sprintf(paste0("Wetland %.0f%%\n",
                             "FWS setback %s ft: %.0f%%\n",
                             "NRCS setback %s ft: %.0f%%\n",
                             "Cropped: %.0f%% of wetland + FWS setback"),
                      pct(ac$wet_acres),
                      ft(ac), pct(ac$buffer_acres),
                      ft(ac_nrcs), pct(ac_nrcs$buffer_acres),
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
