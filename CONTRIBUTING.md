# Contributing

Thanks for helping improve the Iran Health Accessibility Atlas. Issues and pull requests in English or Persian are welcome.

## Set up

```bash
git clone https://github.com/alirezach/iran-health-atlas.git
cd iran-health-atlas
Rscript -e 'install.packages("renv"); renv::activate(); renv::restore(prompt = FALSE)'
```

You need GDAL, GEOS and PROJ for `sf` and `terra`, and Quarto for the website.

## Where things live

- `R/`: pure functions (tested). Put logic here.
- `scripts/0X_*.R`: thin pipeline stages that read inputs, call `R/` helpers and write outputs.
- `_targets.R`: the dependency graph. New outputs need a target.
- `config.yml`: every tunable assumption. Do not hard-code speeds, thresholds or paths.
- `dashboard/`: Quarto pages; shared helpers in `dashboard/_common.R`; map logic in `assets/map.js`.

## Workflow

1. Open an issue describing the change (especially methodological ones).
2. Branch from `main`.
3. Add or update tests in `tests/testthat/` for any change in `R/`.
4. Run locally:
   ```r
   testthat::test_local()
   lintr::lint_dir(".")
   ```
   and, for pipeline changes, `targets::tar_make()` followed by a look at `data-processed/data-quality-report.html`.
5. Open a pull request. CI runs R CMD check, lintr and the tests.

## Style

- tidyverse style, 120-character lines, `pkg::fun()` calls inside `R/`.
- data.table for large tables, sf/terra for spatial data, all analytical outputs in EPSG:4326.
- Document methodological changes in `docs/methodology.md` and new columns in `docs/data-dictionary.md`.
- Colours must stay colour-blind safe (no red-green contrasts); add new palettes in `R/config.R`.

## Adding a routing engine

Implement `route_nearest_<engine>()` in `R/routing.R` with the contract documented at the top of that file, add it to `route_nearest()`, and add a test with the synthetic network in `tests/testthat/test-routing.R`.

## Data issues

Missing or wrong hospitals and roads are best fixed upstream in [OpenStreetMap](https://www.openstreetmap.org). The next monthly refresh picks them up.
