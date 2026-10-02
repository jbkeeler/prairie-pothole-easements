# Entry point: run this from the project root to reproduce the full pipeline in order.
# Rscript code/00_master.R
#
# Each step runs in a fresh R session: the scripts clear the workspace, read
# their inputs from data/processed/, and hold multi-GB spatial objects, so
# isolating them keeps memory down. Every script sources code/setup.R itself.
#
# Inputs needed in data/raw/ (not tracked in git):
#   FWS_National_Realty_Tracts_View_Test_*.gpkg   USFWS realty tracts
#   NWI/ND_geopackage_wetlands.gpkg               National Wetlands Inventory, ND
#   gSSURGO_ND.gdb                                gridded soil survey, ND
#   CDL/<year>_30m_cdls.tif, 2021-2025            Cropland Data Layer
# and in data/processed/:
#   NDcountyRents_prices2026.xlsx                 ND Trust Lands county rents (step 06)
#   LateralEffect_Tract_LE.csv                    setback distances (see note below)
# Step 06 also needs a NASS Quick Stats key in the NASSQS_TOKEN environment variable.
#
# Memo components are tagged "# MEMO:" in each script; grep for that tag to
# find where a table, figure, or number in arpc-memo.tex comes from.

steps <- c(
  "code/01_get_FWS_ND_easements.R",   # FWS wetland easement tracts in ND
  "code/02_get_CDL_code_table.R",     # CDL class codes, names, colors
  "code/03_get_easement_NWI.R",       # NWI wetlands clipped to tracts; small-basin share
  "code/04_get_ND_gSSURGO.R",         # dominant field/wetland soils per tract

  # EXTERNAL STEP (not in this repo): data/processed/gssurgo_tract_dom_soils_nd.csv
  # from step 04 is run through the NRCS van Schilfgaarde lateral effect
  # calculation to produce data/processed/LateralEffect_Tract_LE.csv (setback
  # distances by tract under the NRCS and FWS drawdown assumptions). Steps 05-06
  # need that file.

  "code/05_get_easement_nwi_buffers.R", # wetland + setback buffers; example tract figure
  "code/06_get_easement_CDL.R"          # cropped acres, revenue, and rent tables
)

for (s in steps) {
  message("Running ", s)
  status <- system2("Rscript", s)
  if (status != 0) stop(s, " failed with exit status ", status)
}
