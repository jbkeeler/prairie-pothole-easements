
- Easement Tracts are from FWS National Realty Tracts map.
  - filtered to X and FX for easements (FX not in ND, should we include grassland?)
  - Easements tracts (verify and update with FWS definition) are land that FWS has an agreement in place with the landowner where easements for wetland areas exist. They do not represent polygons of the eased wetland areas (eased wetlands are subsets of tracts), but the total land that's covered by FWS agreement (need to verify). The GIS data only provides this tract polygons and the total acreage of wetlands inside the tract
  - ~1.03M acres of eased wetland are reported within FWS realty tracts
  - ~6.4 acres of FWS realty tracts with wetland easements
  - ~4.23M wetland acres in ND (includes bodies of water, wetlands (or porttions of them) that arent' covered by easements)
  - We recalculate the acres of eased wetland by crossing FWS realty tracts with NWI wetlands, then use FWS and NRCS (for comparison) setback formulas to estimate the total acreage including setback buffer areas around wetlands.
- We compute setbacks using the FWS and NRCS formulas, but they are only a rough approximation
- Using SSURGO soil survey map units, we identify the dominant (largest share of tract acreage) field (non-wetland) and wetland soil types, then plug into setback equations for high/low drainage and FWS/NRCS rule scenarios. We focus our analysis in the memo on the low drainage with FCS rules, which generates the larges setbacks. Many of these setbacks completly cover the realty tracts.
  - in practice/reality, setbacks would be determined using complete soil survey maps in a tract, so setback buffers would very between soil types within a tract (See NRCS handbook ch 19)
  - our approximation is roughly the "most likely" setback distance.
- we present an example of a realty tract, its wetlands, and the computed buffer area used in our analysis.

- Report the total acreage of wetlands + buffer areas in easement tracts

- What is the value of land potentially impacted by these easements and setbacks? What are the opportunity costs of not farming cropland in these eased wetlands and buffers around them?
  - report rental rates and values table
  - We estimate crop revenues lost as follows
    - Get polygons of wetland areas + setback buffer within easements and cross them with annual Cropland Data Layer remotely sensed crop acreage maps from 2021-2025
    - compute total revenues by crop using 0.85 times county yields times, where 0.85 is a rough discount factor for crop yields in wetlands in the prairie pothole region based on Bold_et_al.pdf (pdf is in the workspace root)
    - NOTE: any land value, yield, or revenue estimates in this analysis in the are rough upper bounds of wetland crop production. These may not be indicative of true crop, field, or farm performance.

- Next steps
  - add costs of production
    - true opportunity cost of wetland setbacks are lost crop revenues - production costs for land no longer cropped + increased costs of operating around irregular field boundaries
    - cite references for rough cost multiples for irregular field boundaries?
    - we could simulate a tractor's path on each tract with vs without buffers, then calculate additional labor, input use (diesel, double seeding, etc.)
    - use a rough measure of rectangularity of each field
  - analysis of what wetlands are transient or have been dry for an extended period
    - use high resolution satellite data (examples of 10m products or others)
    - NWI data does not necessarily represent current wetland extents
  - analysis of policies consolidating wetland acreage
    - farmers want to merge many small wetlands into fewer large wetlands, what would the costs and benefits be?
