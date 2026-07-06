# modelVis Developer Reference

**Maintainer:** Scarlett Wang
**Current version:** 0.1.7 (`DESCRIPTION`)
**Original handover:** Sam Johnson → Scarlett Wang, 2026-05-25

This document describes the architecture, current state, and
conventions for active modelVis development. It was originally a
handover document from Sam; it is now maintained as a living
developer reference.

For the canonical step-by-step extractor guide, see
`vignette("modelvis-extractor")` (source:
`vignettes/modelvis-extractor.Rmd`). This document adds context
that the vignette does not cover: multi-area models, MCMC
posteriors, density-dependent M, and the SISCAH reference
implementation.

---

## 1. What modelVis is

modelVis turns stock-assessment model output into interactive HTML
flexdashboard reports. Its whole design goal is to **decouple
visualisation from modelling**: every model package produces one
standardised intermediate object (the `mv` object), and modelVis
owns everything downstream of that.

Three layers, in order of data flow:

```
raw model output
      |
      v   extract.<model>()   <-- you write this, in the MODEL pkg
   mv object  (S3 list, class "mv")
      |
      +-----------------------+
      |                       |
      v                       v
 mv_dashboard()        mv_plot_*() helpers
 (full HTML report)    (single plotly plots)
```

The contract between layers is the `mv` object: a list with a
required `meta` element plus optional named data.frame components.
If a component is `NULL`, its dashboard section is silently
skipped. That is the entire extension mechanism -- a new model
only needs an `extract()` method that fills in the components it
can.

---

## 2. Repository map

```
modelVis/
  DESCRIPTION            pkg metadata; Imports/Suggests
  NAMESPACE              roxygen-generated; do not hand-edit
  NEWS.md                changelog
  R/
    extract.R            extract() generic + extract.default +
                         .detect_model_type() fingerprinting
    extract_ms3.R        REFERENCE extractor: MS3 sim blobs
    mv_object.R          new_mv() constructor, .validate_mv(),
                         print/summary methods
    vis_fit.R            mv_dashboard() entry point
    vis_sim.R            mv_sim_dashboard() entry point
    tidy_array.R         mv_tidy_* reshaping helpers
    plot_*.R             mv_plot_* plotly builders
    theme.R, utils.R     layout, colours, labels, mv_config()
    plot_helpers.R       shared plotting internals
  inst/rmd/
    vis_fit.Rmd          assessment dashboard master template
    vis_sim.Rmd          simulation dashboard master template
    child_*.Rmd          one navbar section each (see below)
    style.css, viewport.html
  inst/extdata/
    make_example_mv.R    builds the synthetic example
    example_mv.rds       synthetic mv used by vignettes/tests
  vignettes/
    modelvis-overview.Rmd    the mv object + pipeline
    modelvis-plots.Rmd       plot gallery
    modelvis-extractor.Rmd   how to write an extractor
  tests/testthat/        15 tests across 4 files (see sec. 7)
  man/                   roxygen-generated .Rd files
```

Master template `inst/rmd/vis_fit.Rmd` pulls in each child via a
knitr `child=` chunk, most gated on an `eval=` expression that
checks whether the relevant component or flag is present. To add
a dashboard section you add a child `.Rmd` and one chunk in the
master template.

---

## 3. Current state

**Working and wired up:**

- The `extract()` generic, `new_mv()` constructor, validation,
  print/summary.
- `extract.ms3Blob()` -- reference extractor for MS3 closed-loop
  simulation blobs (`R/extract_ms3.R`). Good example of
  array-to-data.frame reshaping and fleet-type handling.
- `mv_dashboard()` (assessment) and `mv_sim_dashboard()`
  (simulation) render end to end.
- All `mv_plot_*` helpers and tidy helpers are exported and
  tested.

**Registered model types:**

- `ms3Blob` -- built into this package (`extract_ms3.R`).
- `siscah` -- registered in the SISCAH package
  (`SISCAH/R/extract_mv.R`). This is the **primary active
  extractor** and the most complete reference for multi-area,
  multi-fleet, MCMC-posterior, and density-dependent-M models.
  Read this before writing a new extractor.
- `sableOpMod` and `sisca` -- registered in their own packages,
  not here. modelVis carries only their auto-detection
  fingerprints in `.detect_model_type()`.

**SISCAH-specific mv components (not in other extractors):**

| Component       | Description                                      |
|-----------------|--------------------------------------------------|
| `m_adult_ts`    | Age-2+ M time series; list with `series`, `Mbar`,|
|                 | `M0_p`. CI from `posts$M_iapt` if MCMC present. |
| `ddm_data`      | DDM scatter data: `obs` (depletion + M per year),|
|                 | `curve`, `curve_ci`, `params` (M_b, m1, sd, M0). |
| `meta$is_ddm`   | Logical; gates the Density-Dependent M tab.      |
| `meta$mode`     | `"OM"` vs `"MP"` controls some conditional tabs.|

**Length support (wired, exercised via SISCAH):**

| `mv` slot               | Rendered by               | Status    |
|-------------------------|---------------------------|-----------|
| `len_comp_fits`         | `child_len_comps.Rmd`     | working   |
| `len_comp_resids`       | `child_len_comps.Rmd`     | working   |
| `selectivity_at_length` | `child_selectivity.Rmd`   | working   |
| `length_at_age`         | `child_life_history.Rmd`  | working   |
| `age_length_key`        | `child_life_history.Rmd`  | working   |

**Combined-index (blended survey) convention:**

SISCAH can blend a surface + dive spawn survey into a single
combined index using `data$whichCombIdx_g` and `data$combI_pt`.
The extractor skips those individual fleet rows in data
availability, selectivity, and comp sections, and instead adds a
synthetic `"Spawn Survey"` row built from `combI_pt`. If your
model has a similar blended-index structure, follow the same
pattern in your extractor.

**Known open items (as of 2026-07-06):**

- Yearly length-fit panel heights: the `p$height <- h` +
  `config(responsive = FALSE)` fix is deployed but not yet
  verified on a regenerated dashboard by the user.
- Natural mortality and DDM scatter plots updated to match
  fitReport; user verification pending.
- Test suite has not been run since 0.1.3; fixtures may need
  updating for new `new_mv()` components.

---

## 4. How to write a dispatcher (extractor) for a new model

This is the core handover skill. Full detail is in
`vignette("modelvis-extractor")`; the essentials:

### 4.1 The method

`extract()` is an S3 generic (`R/extract.R`). You write
`extract.<class>(fit, label = "", ...)` that returns
`new_mv(meta = ..., <components>)`. The method lives **in your
model package**, not in modelVis.

```r
#' @export
extract.mymodel <- function(fit, label = "", ...) {
  if (!requireNamespace("modelVis", quietly = TRUE))
    stop("Package 'modelVis' is required.", call. = FALSE)

  # 1. dimensions + names (derive from the control file,
  #    never hardcode fleet/gear indices)
  # 2. build meta (required fields below)
  # 3. reshape arrays into the standard data.frames
  # 4. assemble with modelVis::new_mv()
  modelVis::new_mv(meta = meta, spawning_biomass = sb, ...)
}
```

### 4.2 Registration

- roxygen `#' @export` on `extract.mymodel` generates the
  `S3method(extract, mymodel)` line in your package NAMESPACE.
- List `modelVis` under **`Suggests:`** (not `Imports:`) so the
  model package does not hard-depend on the visualiser, and guard
  the body with `requireNamespace()` as above.

### 4.3 Auto-detection (optional)

If users hold an unclassed list, `extract.default()` calls
`.detect_model_type()` to fingerprint it and set the class. To
register a new fingerprint, add a structural check in
`R/extract.R` (this is the one place a new model touches modelVis
itself):

```r
.detect_model_type <- function(fit) {
  if (!is.list(fit)) return(NULL)
  if (!is.null(fit$repOpt) && !is.null(fit$data))
    return("sableOpMod")
  if (!is.null(fit$mymodel_field))      # <-- new
    return("mymodel")
  NULL
}
```

Keep fingerprints specific enough not to collide. If you skip
this, users just set `class(fit) <- "mymodel"` before calling
`extract()`.

### 4.4 Required `meta` fields

`new_mv()` enforces these (`mv_object.R`):

```
model_type, label, years, ages,
sex_names, fleet_names, area_names
```

Note `ages` is required even for a length-based model -- see
section 5 for how to handle that. Useful optional flags read by
the templates: `has_length_comps`, `has_discards`, `has_mcmc`,
`fleet_type` (integer per fleet: 1 = fishery, 0 = survey).

### 4.5 Component column conventions

Validated by `.validate_mv()`:

- **Time-series** (`spawning_biomass`, `recruitment`,
  `total_biomass`, `harvest_rate`, ...): `year`, `est`; optional
  `lwr`/`upr` trigger CI ribbons. Use `mv_ensure_ci()` to add
  `NA` CI columns when the model has no uncertainty.
- **Fit components** (`index_fits`, `catch_fits`,
  `discard_fits`): `year`, `obs`, `pred`; optional `fleet`,
  `residual`.

Conventions **not** enforced by the validator but expected by the
templates:

- **Composition fits** (`age_comp_fits`, `len_comp_fits`): bin
  column (`age` or `length`), `year`, `obs`, `pred`, `fleet`;
  optional `sex`. Normalise obs and pred to proportions.
- **Selectivity** (`selectivity_at_age`, `selectivity_at_length`):
  bin column, `est`, `fleet`; optional `sex`.

Because comps and selectivity are not validated, a wrong column
name fails silently at render time, not at `new_mv()`. Double-
check these by eye.

---

## 5. Length-based model: your starting task

The dashboard scaffolding for length output already exists
(section 3). Your job is the extractor that feeds it, plus
filling the gaps where the templates currently assume age.

### 5.1 Slots to populate

```r
# Length composition fits: one row per (fleet, sex, year, length)
len_comp_fits <- data.frame(
  length = ...,    # bin midpoint or lower edge (be consistent)
  year   = ...,
  obs    = ...,    # normalised proportion
  pred   = ...,    # normalised proportion
  fleet  = ...,
  sex    = ...     # optional; "Combined" if single-sex
)

# Length comp residuals: bubble plot in child_len_comps.Rmd
len_comp_resids <- data.frame(
  year = ..., length = ..., fleet = ...,
  est  = ...        # signed residual; size/colour maps to it
)

# Selectivity at length: curves by fleet
selectivity_at_length <- data.frame(
  length = ..., est = ..., fleet = ..., sex = ...  # sex optional
)

# Growth: child_life_history.Rmd
length_at_age <- data.frame(age = ..., est = ..., sex = ...)
age_length_key <- data.frame(          # P(length | age) heatmap
  age = ..., length = ..., est = ..., sex = ...
)
```

Set `meta$has_length_comps <- TRUE` so `vis_fit.Rmd` evaluates the
length-comp child.

### 5.2 Known gaps / decisions you will hit

These are the rough edges where the package currently assumes an
age-structured model. Expect to resolve them:

1. **`meta$ages` is mandatory** in `new_mv()`, but a purely
   length-structured model may not have an age dimension. Short
   term: pass the length-bin vector (or `1:nBins`) as `ages` so
   construction succeeds. Better: relax `new_mv()`/`.validate_mv()`
   to accept length-structured models and let `print`/`summary`
   report a length axis. Discuss with Sam before changing the
   constructor contract -- other extractors depend on it.

2. **`selectivity_surface` is age-only.** The surface plot in
   `child_selectivity.Rmd` is hardcoded to `ylab = "Age"`. For a
   time-varying length-selectivity surface you will need either a
   length-aware branch or a parallel `selectivity_surface_len`
   slot. The flat `selectivity_at_length` curve already works.

3. **No validation on comp/selectivity slots.** Misnamed columns
   fail at render, not construction. Consider extending
   `.validate_mv()` to cover `len_comp_fits` etc. -- low-risk,
   high-value, and gives you a fast feedback loop.

4. **Length bin convention.** Templates use the `length` column
   verbatim as the x-axis. Decide midpoint vs. lower edge once and
   apply it consistently across comps, selectivity, and the ALK.

### 5.3 Suggested order of work

1. Build a minimal `extract.<yourmodel>()` that fills only
   `meta`, `spawning_biomass`, `recruitment`, and `len_comp_fits`.
   Render with `mv_dashboard()` and confirm the length-comp tab
   appears.
2. Add `selectivity_at_length`, `length_at_age`, `age_length_key`.
3. Add residuals (`len_comp_resids`) and index/catch fits.
4. Add a fingerprint to `.detect_model_type()` and a test (sec 7).
5. Resolve the `ages` and `selectivity_surface` gaps with Sam.

---

## 6. House conventions (apply to all changes)

These are Sam's standing rules; please keep to them:

- 80 characters per line in `.R`, `.Rmd`, and `.md`.
- Canadian English everywhere (visualise, colour, normalise).
- Always `drop = FALSE` when subsetting arrays with `[`; strip
  singleton dims explicitly with `dim<-` afterwards. The MS3
  extractor follows this throughout -- copy that style.
- Always use **named arguments** in function calls
  (`apply(X = a, MARGIN = 2, FUN = sum)`).
- Never hardcode gear/fleet indices; derive them from the model
  or control file (MS3 reads `fleet_type` from `om$fleetType_f`).
- Plotting here is plotly, not base R, because output is
  interactive HTML. (Sam's "base plot" rule is for static report
  figures, not this package.)
- NAMESPACE and `man/*.Rd` are roxygen-generated -- edit roxygen
  blocks and run `devtools::document()`, never hand-edit.

---

## 7. Building, testing, rendering

```r
devtools::load_all()      # iterate without installing
devtools::document()      # regenerate NAMESPACE + man/ after
                          #   changing roxygen
devtools::test()          # run testthat suite
devtools::check()         # full R CMD check before release
```

Rendering a dashboard from a fit, without installing:

```r
devtools::load_all()
mv_dashboard(fit = your_fit, label = "Test",
             output_file = "out.html")
# output_file = NULL opens the interactive RStudio viewer
```

`pandoc` is required to knit the flexdashboard templates and the
vignettes (RStudio bundles it; a standalone session needs it on
`PATH`).

Current test suite (`tests/testthat/`, 15 tests / 4 files):

- `test-mv-object.R` -- constructor and validation (4)
- `test-tidy-helpers.R` -- the `mv_tidy_*` reshapers (7)
- `test-exports.R` -- exported-symbol surface (2)
- `test-rmd-templates.R` -- templates resolve and knit (2)
- `helper-find-dupes.R` -- shared test helper

When you add the length extractor, add a test mirroring the
extractor-vignette pattern: assert it returns an `mv`, that
`meta$model_type` is right, and that `len_comp_fits` has
`c("length", "year", "obs", "pred", "fleet")`. A small synthetic
fit (like `inst/extdata/make_example_mv.R`) is the cheapest
fixture.

---

## 8. Loose ends in the working tree

- `render_fwTest15.R` -- scratch render script in the package
  root; not part of the package, fine to delete once no longer
  needed.
- Test fixtures: the `testthat` suite uses `example_mv.rds` from
  `inst/extdata`. It does not cover SISCAH-specific components
  (`m_adult_ts`, `ddm_data`, combined-index data availability).
  Adding a small synthetic SISCAH-like fixture would let the
  suite catch regressions in those code paths.

---

## 9. Constructor contract

`new_mv()` and `.validate_mv()` define the interface that all
model packages depend on. The `ages`, `sex_names`, `fleet_names`,
and `area_names` fields in `meta` must remain present and typed
consistently -- `sableOpMod`, `sisca`, and `ms3Blob` extractors
all rely on them. Before changing the constructor or validator,
grep for `new_mv(` across all dependent packages and verify
compatibility. The `m_adult_ts` and `ddm_data` components are
SISCAH-specific and passed through `...` to `new_mv()`, so they
do not affect the validated contract for other models.
