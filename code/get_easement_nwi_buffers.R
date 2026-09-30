rm(list = ls())
gc()

source("code/00_master.R")


fws_tracts_nd <- readRDS("data/processed/fws_tracts_nd.rds") |>
  st_transform(5070)

# row id, since the first tract column has duplicates; matches get_ND_gSSURGO.R
fws_tracts_nd$tract_uid <- seq_len(nrow(fws_tracts_nd))

# wetland x tract pieces from get_easement_NWI.R (already clipped to tracts, EPSG:5070)
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

# all LE distances:
# for (v in le_vars) {
#   b <- get_wetland_buffers(v)
#   saveRDS(b, paste0("data/processed/nwi_buffers_", v, "_nd.rds"))
#   fwrite(tract_buffer_acres(b), paste0("data/processed/nwi_buffer_acres_", v, "_nd.csv"))
#   saveRDS(union_wet_buf(b), paste0("data/processed/nwi_wet_buf_", v, "_nd.rds"))
# }


# example tract map: easement, wetland, and setback buffer
ex_uid <- 101   # TRACTNO 112X; 160 ac, 20 wetland polygons, 180 ft setback

ex_tract <- fws_tracts_nd[fws_tracts_nd$tract_uid == ex_uid, ]
ex_buf   <- nwi_buffers[nwi_buffers$tract_uid == ex_uid, ]
ex_ac    <- nwi_buffer_acres[tract_uid == ex_uid]

ex_buf$zone <- factor(ex_buf$zone, levels = c("wetland", "buffer"),
                      labels = c("NWI wetland", "Setback buffer"))

p_ex <- ggplot() +
  geom_sf(data = ex_tract, fill = "grey92", color = NA) +
  geom_sf(data = ex_buf, aes(fill = zone), color = NA) +
  # outline of the combined wetland + buffer area
  geom_sf(data = st_union(ex_buf), fill = NA, color = "#B2582B", linewidth = 0.4) +
  geom_sf(data = ex_tract, fill = NA, color = "grey20", linewidth = 0.6) +
  scale_fill_manual(values = c("NWI wetland" = "#688CC0", "Setback buffer" = "#F4C28F"),
                    name = NULL) +
  coord_sf(datum = NA) +
  labs(title = paste0("Easement tract ", ex_ac$TRACTNO, ": wetlands and ",
                      ex_ac$buffer_ft, " ft setback buffer"),
       subtitle = sprintf("Wetland %.1f ac + buffer %.1f ac = %.1f ac (%.0f%% of %.1f ac tract)",
                          ex_ac$wet_acres, ex_ac$buffer_acres, ex_ac$wet_buf_acres,
                          100 * ex_ac$wet_buf_share, ex_ac$tract_acres),
       caption = paste0("Grey: easement land outside the buffer. Setback: ", ex_ac$le_var, ".\n",
                        "Source: USFWS NWI, USFWS realty tracts, lateral effect distances.")) +
  theme_void() +
  theme(legend.position = "bottom",
        plot.margin = margin(10, 10, 10, 10))

ggsave("figures/example_tract_nwi_buffer.pdf", p_ex,
       width = 7, height = 7, device = cairo_pdf, bg = "white")
