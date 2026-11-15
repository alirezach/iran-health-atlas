# Methodology

## Question

What proportion of Iran's population can reach a hospital by car within 15, 30, 60 and 120 minutes, and how unequal is that access across provinces?

## Study area and projections

- **Area:** the whole territory of Iran as defined by geoBoundaries ADM1 (31 provinces), with ADM2 counties for finer summaries.
- **Analysis CRS:** EPSG:4326 (WGS 84). Every analytical layer and every GeoParquet output is stored in longitude/latitude; the QA report fails the build otherwise.
- **Visualisation CRS:** EPSG:3857 (Web Mercator). Raster overlays are re-projected to 3857 before being written as PNG so they align exactly with web map tiles. Metric operations (polygon simplification) are done in 3857 and transformed back.
- **Distances** are great-circle (haversine) metres.

## Data sources

| Layer | Source | Notes | Licence |
|---|---|---|---|
| Road network | OpenStreetMap via Geofabrik `iran-latest.osm.pbf` | downloaded with osmextract; md5 checked monthly | ODbL 1.0 |
| Hospitals | same extract | `amenity=hospital` or `healthcare=hospital`, nodes and areas | ODbL 1.0 |
| Population | WorldPop 2020 unconstrained, UN-adjusted, 30 arc-second (~1 km) | the analytical grid | CC BY 4.0 |
| Centroid weights | WorldPop 2020 UN-adjusted, 3 arc-second (~100 m) | only used to place centroids | CC BY 4.0 |
| Boundaries | geoBoundaries gbOpen ADM1, ADM2 | via the geoBoundaries API | CC BY 4.0 |

Provenance for each run (URL, version, retrieval time, bytes, md5, licence) is in `metadata.csv`.

## Processing workflow

The pipeline is a `targets` graph; each stage is one script in `scripts/`.

1. **Download.** OSM extract (osmextract), WorldPop rasters, geoBoundaries GeoJSON and API metadata.
2. **Hospitals.** Read the OSM points and multipolygons layers with an SQL filter on `amenity`/`healthcare`. Deduplicate (below), keep facilities inside Iran, attach province and county.
3. **Population grid.** One row per populated WorldPop 1 km cell (`cell_id` = raster cell index). Province/county are assigned by rasterising boundaries onto the grid; coastal and border cells outside every polygon take the nearest unit. exactextractr computes independent provincial population totals for reconciliation.
4. **Road network.** Drivable OSM ways become a directed graph.
5. **Accessibility.** Fastest car route from each cell to its nearest hospital.
6. **Indicators.** Province, county and national summaries.
7. **Web export.** GeoJSON, PNG overlays, JSON summaries and download copies.
8. **Validation.** Automated checks and the HTML QA report. Validation runs *before* the web export, so a failing build never reaches the website.

### Population-weighted centroids

Each 1 km cell is represented by the population-weighted mean location of the ~100 m WorldPop cells inside it:

$$x_c = \frac{\sum_i p_i x_i}{\sum_i p_i}, \qquad y_c = \frac{\sum_i p_i y_i}{\sum_i p_i}$$

computed with `terra::aggregate` (factor 10). This moves the origin of a half-desert cell to the village inside it instead of its geometric centre. If the 100 m raster is unavailable or a cell has no fine-scale population, the cell centre is used and the cell is flagged (`centroid_method`).

### Hospital deduplication

OSM often maps one hospital several times (a node plus a building outline, or several nodes for departments).

1. A node inside a hospital area is the same facility; its name fills a nameless area.
2. Areas are reduced to a point on their surface.
3. Candidates closer than 150 m (transitively) are merged into one facility. The representative prefers an area, then a named feature, then the most complete tags; `n_merged` and `merged_osm_ids` record what was merged.
4. `hospital_id` is the OSM type/id of the representative (e.g. `way/123456`), stable across refreshes and linkable to openstreetmap.org.

Features tagged only `building=hospital`, clinics (`amenity=clinic`) and doctors are excluded.

### Road network

| OSM class | Speed (km/h) |
|---|---|
| motorway | 100 |
| trunk | 85 |
| primary | 70 |
| secondary | 60 |
| tertiary | 50 |
| unclassified | 40 |
| road | 30 |
| motorway / trunk / primary / secondary / tertiary links | 60 / 50 / 45 / 40 / 35 |

Speeds are free-flow assumptions below Iran's legal limits (120 km/h freeways, 95 to 110 km/h highways, 50 to 60 km/h urban) to reflect real driving. Every consecutive pair of OSM nodes becomes an edge; ways join at shared nodes. One-way rules: `oneway=yes|true|1` forward only, `oneway=-1` reverse only, roundabouts and motorways one-way unless tagged otherwise. Fragments with fewer than 500 edges are removed; larger islands (e.g. Qeshm, Kish) are kept.

**Residential and service streets are excluded in v1.** Including them roughly triples the national graph and exceeds GitHub runner memory. In built-up areas the off-network connector stands in for the last few hundred metres. Set `residential: 25` in `config.yml` to include them on larger machines.

### Routing

- **Engine:** dodgr (Dijkstra). For each batch of 50 hospitals, shortest paths run from the hospitals over the **reversed** graph, which gives the time *from* every origin *to* each hospital on the real one-way network. A running minimum keeps the nearest hospital per origin, so memory stays at batch size x origins.
- **Contraction:** the graph is contracted (`dodgr_contract_graph`) keeping every snapped origin and hospital vertex.
- **Time and distance:** routing minimises time. Distance is the length of that fastest route (a second pass on the same contracted graph with lengths as the reported quantity).
- **Snapping and connectors:** origins and hospitals snap to the nearest graph vertex. The straight-line gap is added at 15 km/h at both ends.
- **Outputs per cell:** `travel_time_min`, `distance_km`, `nearest_hospital`. Cells with no road route to any hospital get `NA` and are counted as *not covered*.
- **r5r migration:** `route_nearest()` dispatches on `routing$engine`. `route_nearest_r5r()` already implements the same input/output contract with `r5r::travel_time_matrix(mode = "CAR")`; switching engines is a one-line config change once Java 21 is available.

## Indicators

Let $p_i$ be the population and $t_i$ the drive time of cell $i$.

- **Coverage within $T$:** $\sum_{t_i \le T} p_i / \sum p_i$. The denominator is the *whole* population, including people with no road route.
- **Mean / median drive time:** population-weighted, over the reachable population.
- **Percentiles:** p10, p25, p50, p75, p90, p95, p99, population-weighted (smallest $t$ whose cumulative population share reaches $q$).
- **Gini coefficient:** $G = 1 - \sum_k (P_k - P_{k-1})(L_k + L_{k-1})$ over cells sorted by $t$, with $P$ the cumulative population share and $L$ the cumulative share of person-minutes. 0 means everyone faces the same drive time.
- **Theil T index** of drive time, the **p90/p10 ratio**, and the **gap, range and Gini of provincial 60-minute coverage** capture inequality between and within regions.
- **Hospitals per 100,000** and a **coverage rank** (1 = best at 60 minutes) per province.

## Validation

Run on every build; results in `data-quality-report.html` and `qa_checks.csv`.

| Check | Rule |
|---|---|
| CRS consistency | hospitals, provinces, counties and WorldPop are EPSG:4326; GeoParquet files carry valid `geo` metadata (WKB, CRS84) |
| Geometry | no invalid or empty geometries (GEOS) |
| Grid | unique cells, centroids inside their cell and the raster, no negative population |
| Duplicate hospitals | unique ids; no two hospitals within 100 m after deduplication |
| Network | no missing, zero or negative edge lengths or times |
| Population | grid and exactextractr totals within 2% of the WorldPop total |
| Impossible travel times | no negative times or times above 24 h; no crow-flies speed above 130 km/h; route never shorter than the straight line; every reachable cell references a known hospital |

Any `fail` stops the pipeline before the website is updated; `warn` items are published in the report.

## Web delivery

- Cell-level results become **dissolved category polygons per province** (`travel_time.geojson`), simplified at 750 m with coordinates rounded to 4 decimals, so the browser loads a few hundred features instead of 1.6 million points.
- Continuous travel time and population are shipped as **pre-rendered PNG overlays** in EPSG:3857.
- Cells with fewer than one person are hidden from map layers (they remain in every dataset).
- Full-resolution data is offered as Parquet (zstd) and gzip CSV on the Downloads page and in GitHub releases.

## Assumptions and limitations

- **Free-flow car travel.** No congestion, time of day, road quality, seasonal closures or border crossings. Urban drive times are likely underestimated, mountain roads possibly overestimated.
- **Car access is assumed.** Households without a vehicle depend on public transport or ambulances, which are not modelled.
- **OSM completeness.** Missing or mis-tagged hospitals and roads translate directly into longer estimated times. OSM coverage in Iran is good on the trunk network and uneven on rural roads and facility tags.
- **All hospitals are treated as equal.** No capacity, specialty, emergency department, ownership or public/private distinction.
- **Nearest facility, not chosen facility.** People may bypass the nearest hospital.
- **Population.** WorldPop 2020 is a modelled, UN-adjusted estimate, not a census; unconstrained data spreads small populations into uninhabited areas.
- **Residential streets** are excluded in v1 (see above).
- **Administrative boundaries** follow geoBoundaries and may differ from current official divisions.

## Future work

- r5r engine with realistic road speeds and turn restrictions.
- Facility attributes from official sources (Ministry of Health registry): beds, emergency, specialties.
- Time-of-day congestion and ambulance response models.
- Walking and public-transport scenarios for cities.
- Primary care (clinics, health houses) as a second destination layer.
- Newer WorldPop releases (2025, constrained) and population projections.
- Two-step floating catchment (2SFCA) indicators that account for hospital capacity.
