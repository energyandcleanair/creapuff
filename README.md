# creapuff
An R package to generate input for CALMET, CALPUFF and what not.

## Example
An example is available in [example_chile_baseline.R](example_chile_baseline.R), and will run on CALPUFF-2 VM.

## Generating PTEMARB files (time-varying point source emissions)

`generate_ptemarbs()` creates PTEMARB.DAT files for CALPUFF from two CSV inputs, and a config file:

### `sources.csv`

One row per emission source with stack parameters:

| Column | Description |
|---|---|
| `source_id` | Unique source identifier |
| `cluster_id` | Cluster grouping (one PTEMARB file per cluster) |
| `stack_height_m` | Stack height in meters |
| `stack_diameter_m` | Stack diameter in meters |
| `easting_m` | UTM easting in km |
| `northing_m` | UTM northing in km |
| `utm_zone` | UTM zone (e.g. `52N`) |
| `temp_k` | Exhaust temperature in Kelvin |

### `emissions.csv`

Hourly emission rates per source:

| Column | Description |
|---|---|
| `source_id` | Must match `source_id` in sources |
| `date` | Hourly timestamp (`YYYY-MM-DD HH:MM`) |
| `unit` | `kg/h` or `g/s` |
| `SO2`, `NO`, `NO2`, `PPM25`, ... | Emission rate per species |
| `vexit_m_s` | Stack exit velocity in m/s |


### Usage

```r
library(creapuff)

# Load template data shipped with the package
template_dir <- system.file("templates", "generate_ptemarbs", package = "creapuff")
sources   <- readr::read_csv(file.path(template_dir, "sources.csv"))
emissions <- readr::read_csv(file.path(template_dir, "emissions.csv"))

# Generate PTEMARB files
generate_ptemarbs(
  folder    = "output/ptemarb",
  sources   = sources,
  emissions = emissions,
  utc_zone  = "UTC+0900",
  species   = list(SO2 = 64, NO = 30, NO2 = 46, PPM25 = 1)
)
```

Template CSVs are in [`inst/templates/generate_ptemarbs/`](inst/templates/generate_ptemarbs/).

