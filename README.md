# modelVis

> Interactive diagnostics dashboards for stock assessment models

modelVis turns stock-assessment model output into interactive HTML
flexdashboard reports. Its design goal is to **decouple
visualisation from modelling**: every model package produces one
standardised intermediate object (the `mv` object), and modelVis
owns everything downstream of that.

```
raw model output
      |
      v   extract.<model>()      <-- written in the MODEL package
   mv object  (S3 list, class "mv")
      |
      +-----------------------------+
      |                             |
      v                             v
 mv_dashboard()              mv_plot_*() helpers
 (full HTML report)          (single plotly plots)
```

A new model needs only one `extract()` method that fills in the
components it can produce. If a component is `NULL`, its dashboard
section is silently skipped -- that is the entire extension
mechanism.

## Installation

```r
# install.packages("devtools")
devtools::install_github("LFR-code/modelVis")
```

Rendering dashboards and vignettes requires **pandoc** on the
`PATH` (RStudio bundles it; a standalone R session needs it
installed separately).

## Quick start

### Full dashboard from a model fit

`mv_dashboard()` calls `extract()` internally, so you pass the raw
fit object:

```r
library(modelVis)

# Write to an HTML file
mv_dashboard(
  fit         = model_reports,
  label       = "Base case 2025",
  output_file = "diagnostics.html"
)

# Or pass output_file = NULL to open an interactive viewer
mv_dashboard(fit = model_reports, label = "Base case")
```

Compare model runs by passing additional fits via `comp`:

```r
mv_dashboard(
  fit   = base_reports,
  label = "Base case",
  comp  = list("Low M" = lowM_reports, "High h" = highH_reports),
  output_file = "comparison.html"
)
```

MSE / closed-loop simulation output uses `mv_sim_dashboard()`.

### Single plots

Every `mv_plot_*()` function takes a standard data.frame and
returns a plotly object, usable independently of the dashboard:

```r
example_mv <- readRDS(system.file(
  "extdata", "example_mv.rds", package = "modelVis"
))

mv_plot_trend(
  df    = example_mv$spawning_biomass,
  ylab  = "Spawning biomass (kt)",
  title = "Spawning Biomass"
)
```

## The `mv` object

An `mv` object is an S3 list with a required `meta` element and
optional named data.frame components. Build it with `new_mv()`,
which validates the required metadata and column names.

### Required metadata

| Field         | Type      | Description       |
|---------------|-----------|-------------------|
| `model_type`  | character | Model class name  |
| `label`       | character | Run label         |
| `years`       | integer   | Assessment years  |
| `ages`        | integer   | Age classes       |
| `sex_names`   | character | Sex labels        |
| `fleet_names` | character | Fleet labels      |
| `area_names`  | character | Area labels       |

Useful optional flags read by the templates: `has_length_comps`,
`has_discards`, `has_mcmc`, and `fleet_type` (integer per fleet:
`1` = fishery, `0` = survey).

### Component conventions

- **Time-series** (`spawning_biomass`, `recruitment`,
  `harvest_rate`, ...): `year`, `est`; optional `lwr`/`upr`
  trigger confidence-interval ribbons.
- **Fit components** (`index_fits`, `catch_fits`,
  `discard_fits`): `year`, `obs`, `pred`; optional `fleet`,
  `residual`.
- **Composition fits** (`age_comp_fits`, `len_comp_fits`): a bin
  column (`age` or `length`), `year`, `obs`, `pred`, `fleet`;
  optional `sex`.
- **Selectivity** (`selectivity_at_age`,
  `selectivity_at_length`): bin column, `est`, `fleet`; optional
  `sex`.
- **Reference points**: a named list (`Fmsy`, `SBmsy`, `B0`,
  `R0`, ...).

A minimal object:

```r
mv <- new_mv(
  meta = list(
    model_type  = "demo",
    label       = "Minimal example",
    years       = 2000:2005,
    ages        = 1:5,
    sex_names   = "Combined",
    fleet_names = "survey1",
    area_names  = "All"
  ),
  spawning_biomass = data.frame(
    year = 2000:2005,
    est  = c(50, 48, 45, 43, 44, 46)
  )
)

print(mv)
summary(mv)
```

## Connecting a new model: writing an extractor

`extract()` is an S3 generic. You write
`extract.<class>(fit, label = "", ...)` **in your model package**
(not in modelVis), reshaping your model output into `new_mv()`
format:

```r
#' @export
extract.mymodel <- function(fit, label = "", ...) {
  if (!requireNamespace("modelVis", quietly = TRUE))
    stop("Package 'modelVis' is required.", call. = FALSE)

  meta <- list(
    model_type  = "mymodel",
    label       = label,
    years       = fit$years,
    ages        = fit$ages,
    sex_names   = "Combined",
    fleet_names = fit$fleet_names,
    area_names  = "All"
  )

  modelVis::new_mv(
    meta             = meta,
    spawning_biomass = data.frame(year = fit$years, est = fit$ssb),
    recruitment      = data.frame(year = fit$years, est = fit$rec)
  )
}
```

Registration:

- Add `#' @export` so roxygen generates `S3method(extract,
  mymodel)` in your NAMESPACE.
- List `modelVis` under **`Suggests:`** (not `Imports:`) and
  guard the body with `requireNamespace()` as above.
- Optionally register a structural fingerprint in
  `.detect_model_type()` (`R/extract.R`) so unclassed objects
  auto-detect.

The built-in `extract.ms3Blob()` (`R/extract_ms3.R`) is the most
complete worked example of array-to-data.frame reshaping, fleet
handling, and conditional components -- read it first.

See `vignette("modelvis-extractor")` for the full step-by-step
guide, and `HANDOVER.md` for project status and the in-progress
length-based model work.

## Tidy helpers for extractors

Helpers for converting model arrays into the long-format
data.frames the plot functions expect:

| Function           | Input        | Output            |
|--------------------|--------------|-------------------|
| `mv_tidy_ts()`     | named vector | year + est        |
| `mv_tidy_mat()`    | matrix       | row + col + est   |
| `mv_tidy_3d/4d/5d()` | array      | long data.frame   |
| `mv_quantile_ts()` | MCMC matrix  | year + est + CI   |
| `mv_ensure_ci()`   | data.frame   | adds lwr/upr      |
| `mv_year_seq()`    | fYear + nT   | integer vector    |

## Colour palettes

- `mv_colours(n)` -- general-purpose palette (interpolates beyond
  10 colours)
- `mv_fleet_colours(fleet_names)` -- named vector keyed to fleets
- `mv_sex_colours(sex_names)` -- Male = blue, Female = red,
  Combined = grey
- `mv_kobe_colours()` -- status-zone quadrant colours

## Documentation

- `vignette("modelvis-overview")` -- the `mv` object and pipeline
- `vignette("modelvis-plots")` -- the full plot gallery
- `vignette("modelvis-extractor")` -- writing an extractor
- `HANDOVER.md` -- project status, conventions, and the
  length-based model roadmap

## Supported models

- `ms3Blob` -- MS3 closed-loop simulation blobs (built in)
- `sableOpMod`, `sisca` -- registered in their own model
  packages; modelVis carries only their auto-detection
  fingerprints
- Length-based assessment output -- dashboard scaffolding exists
  (`len_comp_fits`, `selectivity_at_length`, `length_at_age`,
  `age_length_key`); extractor in progress (see `HANDOVER.md`)

## Development

```r
devtools::load_all()    # iterate without installing
devtools::document()    # regenerate NAMESPACE + man/ after
                        #   editing roxygen blocks
devtools::test()        # run the testthat suite
devtools::check()       # full R CMD check before release
```

Conventions for contributions:

- 80 characters per line in `.R`, `.Rmd`, and `.md`.
- Canadian English (visualise, colour, normalise).
- Always `drop = FALSE` when subsetting arrays; strip singleton
  dimensions explicitly afterwards.
- Use named arguments in function calls.
- Never hardcode gear/fleet indices; derive them from the model
  or its control file.
- `NAMESPACE` and `man/*.Rd` are roxygen-generated -- edit the
  roxygen blocks and run `devtools::document()`, never by hand.

## License

GPL (>= 3). Maintainer: Samuel Johnson
<sdnjohnson@landmarkfisheries.com>.
