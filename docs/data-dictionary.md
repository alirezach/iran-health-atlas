# Data dictionary

GeoParquet files follow GeoParquet 1.0.0: geometry in a WKB `geometry` column, longitude/latitude (EPSG:4326 / OGC:CRS84). Times are minutes, distances kilometres unless stated.

### population_grid.parquet

| Column | Type | Description |
|---|---|---|
| cell_id | integer | Cell index in the WorldPop 1 km raster (row-major, 1-based) |
| row, col | integer | Raster row and column |
| cell_lon, cell_lat | double | Cell centre |
| lon, lat | double | Population-weighted centroid (routing origin) |
| population | double | People in the cell (WorldPop 2020, UN-adjusted) |
| centroid_method | string | `population_weighted` or `cell_centre` |
| province_id, province | string | geoBoundaries ADM1 id and name |
| county_id, county | string | geoBoundaries ADM2 id and name |
| geometry | Point | Centroid |

### hospitals.parquet / hospitals.csv / hospitals.geojson

| Column | Type | Description |
|---|---|---|
| hospital_id | string | OSM type/id of the representative feature, e.g. `node/123` |
| osm_id, osm_type | string | Same id and its OSM type |
| name, name_fa, name_en | string | OSM `name`, `name:fa`, `name:en` |
| amenity, healthcare, emergency, operator, beds | string | OSM tags |
| source_geom | string | `point` (node) or `polygon` (area reduced to a point on its surface) |
| n_merged | integer | Candidate features merged into this facility |
| merged_osm_ids | string | Semicolon-separated ids of merged features |
| lon, lat | double | Facility location |
| province_id, province, county_id, county | string | Administrative units |

### travel_times.parquet / travel_times.csv.gz

| Column | Type | Description |
|---|---|---|
| cell_id | integer | Joins to `population_grid` |
| population | double | People in the cell |
| travel_time_min | double | Drive time to the nearest hospital; `NA` = no road route |
| distance_km | double | Length of that fastest route, including connectors |
| nearest_hospital | string | `hospital_id` of the nearest hospital |
| category_id | integer | 1 = 0-15, 2 = 15-30, 3 = 30-60, 4 = 60-120, 5 = over 120 min; `NA` = no route |
| origin_snap_m | double | Distance from centroid to the road network (metres) |
| province_id, province, county_id, county | string | Administrative units |
| lon, lat | double | Origin (population-weighted centroid) |

### province_metrics (parquet with geometry, csv, geojson) and county_metrics

| Column | Description |
|---|---|
| province_id, province (county_id, county) | Unit id and name |
| population | Total population |
| population_reachable / population_unreachable | People with / without a road route to any hospital |
| mean_travel_time_min, median_travel_time_min | Population-weighted, reachable population |
| pop_within_15 ... pop_within_120 | People within each threshold |
| share_within_15 ... share_within_120 | Share of the *total* population within each threshold |
| n_hospitals, hospitals_per_100k | Mapped hospitals and rate |
| rank_coverage | Rank by 60-minute coverage (1 = best), provinces only |
| fill | Choropleth colour, GeoJSON only |

### national_metrics

Long format: `metric`, `value`, `unit`, `description`. Metrics: `total_population`, `population_reachable`, `population_unreachable`, `share_unreachable`, `mean_travel_time_min`, `median_travel_time_min`, `pop_within_*`, `share_within_*`, `p10`...`p99_travel_time_min`, `gini_travel_time`, `theil_travel_time`, `p90_p10_ratio`, `province_coverage_min`, `province_coverage_max`, `province_coverage_range`, `province_coverage_gini`, `n_hospitals`, `hospitals_per_100k`, `n_grid_cells`.

### province_travel_time_distribution

`province_id`, `province`, `bin_start`, `bin_end` (10-minute bins; `NA` = open-ended 180+), `population`.

### travel_time.geojson

Dissolved accessibility categories per province: `province_id`, `province`, `category_id` (6 = no road access), `label`, `lower`, `upper`, `color`, `population`, `cells`.

### metadata.csv

`dataset`, `provider`, `url`, `license`, `version`, `file`, `retrieved_at`, `bytes`, `md5`.

### qa_checks.csv

`check`, `status` (pass / warn / fail), `value`, `detail`.
