# modelVis 0.1.9

## Lazy tab/page rendering

* **`mv_lazy_tabs()`** (internal, called automatically by
  `mv_dashboard()`/`mv_sim_dashboard()` after rendering): flexdashboard
  turns every top-level page and every `{.tabset}` Row into a Bootstrap
  tab-pane at runtime, but that's a pure CSS `display:none` toggle --
  `HTMLWidgets.staticRender()` still calls `Plotly.newPlot()` for every
  widget on every page/tab regardless of visibility, so a large
  dashboard fully renders everything before the user sees the first
  page.
* htmlwidgets' `find()` matches a widget's container `<div>` purely by
  CSS class (e.g. `.plotly`), independent of whether its paired JSON
  data script exists, and marks any found element
  `html-widget-static-bound` the moment it's found -- so hiding only
  the JSON script is a no-op; a later restore + re-render is skipped
  because the container was already marked bound on the first pass.
  The fix renames the widget's own class token (`plotly` ->
  `mv-lazy-plotly`) on every widget outside the default page/tab, so
  `find()` doesn't match it at all on the first pass.
* Rather than waiting for a tab to be shown, a small injected script
  restores and binds every deferred widget in the background, a few
  at a time via `setTimeout`, right after the default page finishes
  loading -- so non-default pages are usually already rendered by the
  time the user switches to them. `Plotly.newPlot()` on a
  `display:none` container measures 0x0 at creation time, which is
  harmless as long as something forces a resize once the container
  becomes visible; a periodic `window.dispatchEvent(new
  Event("resize"))` covers that via htmlwidgets' own resize listener.
* Implementation note: the balanced-`<div>` scanner used to find each
  page/tab's true boundary masks `<script>` bodies first, since plotly
  widget JSON or hand-written `<script>` chunks can contain
  `<div`/`</div>`-like substrings inside string values that would
  otherwise be miscounted as real markup. The mask regex needs the
  `(?s)` (DOTALL) modifier -- without it, multi-line `<script>` blocks
  silently fail to match, leaving their real tags to be double-counted
  by later scans.

## Index fits: Single line / Per quarter toggle

* **Trace-index offset bug**: `subplot()` silently allocates a
  placeholder trace for any panel built from an empty `plot_ly()` call
  (e.g. a stock with zero rows for a given fleet, rendered as a blank
  panel for column alignment) -- an "empty" panel still occupies one
  real trace slot in the merged widget. `child_index_fits.Rmd`'s
  toggle-index bookkeeping assumed such panels contributed zero
  traces, so every trace index computed for panels after one or more
  empty panels was off by the number of empty panels before it. Fixed
  by reporting `n_traces = max(trace_cursor, 1L)` per panel so the
  offset matches the real runtime layout.
* **Boolean-coercion bug**: the toggle's `visible` arrays were built
  by `unlist()`-ing a mix of logical vectors (real panels) and
  `integer(0)` placeholders (empty panels); `unlist()` promotes mixed
  logical/integer input to integer, turning `TRUE`/`FALSE` into
  `1`/`0`. Plotly's `visible` attribute strictly requires a real
  boolean (or `"legendonly"`) and silently ignores a bare number, so
  the affected traces never actually toggled. Fixed by using
  `logical(0)` as the placeholder instead.

# modelVis 0.1.8

## Index-fit hover/rendering fixes (driven by SISCAL dashboard work)

* **Composition plot hover**: `mv_plot_comp_grid()`/`mv_plot_comp_fit()`
  had a decorative marker trace with `hoverinfo = "skip"`, leaving only
  a thin, hard-to-hit line trace with a working tooltip. Fixed by
  moving the hovertemplate onto that marker trace (sized up slightly
  to be a real hit target) instead of adding a duplicate trace -- an
  earlier attempt at this fix via a separate invisible marker trace
  per panel roughly doubled dashboard file size across a full
  length-comp grid; reverted in favour of this cheaper approach.

* **Index fits hover**: same issue in `child_index_fits.Rmd`'s pred
  lines; fixed by switching those traces to `mode = "lines+markers"`
  with an invisible marker layer (no extra trace, no data
  duplication).

# modelVis 0.1.7

## SISCAH dashboard: population dynamics plots

* **Natural mortality**: new `m_adult_ts` mv component carries the
  age-2+ M time series with optional MCMC CI bounds, `Mbar` (temporal
  mean), and `M0_p` (base M per area). `child_mortality.Rmd` Natural
  Mortality tab now renders a salmon line + MCMC ribbon + grey-dashed
  Mbar and salmon-dotted M0 reference lines -- matching the
  `plotMt()` fitReport panel. Reference lines are plotted as traces
  so they appear in the legend (`"Age-2+ M"`, `"95% CI"`,
  `"Mbar"`, `"M0"`).

* **Density-dependent M scatter**: `ddm_data` extraction extended
  with `params` (M_b, m1, residual SD, M0 per area), `curve_ci`
  (MCMC quantile ribbon around the fitted DDM curve), and point CI
  merged into `obs`. `ddm_scatter` chunk rewritten to show: salmon
  CI ribbon around the fitted curve; year-gradient coloured points
  (red = oldest, blue = newest); vertical CI error bars; dashed
  reference lines at M0 and depletion = 1; top-right annotation box
  (M_b, m1, SD); colour legend entries for the first and last year.

* **Stock-recruitment**: `sr_params` now stores full per-area
  vectors (`reca`, `recb`, `h`, `B0`, `R0`) instead of scalars.
  `sr_plot` chunk rewritten with a `.sr_panel()` helper: per-area
  B-H curve, scatter coloured by area, B0/R0 reference lines, and
  a steepness annotation box (`h = X.XXX`) in the top-right corner.
  Multi-area models render as subplots.

## SISCAH dashboard: fits-to-data and data summary

* **Length comps yearly fits**: height is now computed per-fleet
  (`max(490L, 122L * n_row + 90L)` px) and applied via both
  `layout(height = h)` and `p$height <- h` so the htmlwidget
  container actually resizes (previous approach of setting only
  `layout(height=)` was ignored when `responsive = TRUE`). Switched
  to `config(responsive = FALSE)` so plotly.js honours the explicit
  pixel height.

* **Data availability**: individual combined-index fleets (surface
  survey, dive survey) are now suppressed from the fleet loop and
  replaced with a single `"Spawn Survey"` row built from
  `combI_pt`. This matches the fleet names visible elsewhere in the
  dashboard.

* **Selectivity**: spawn-survey fleets (combined-index gears with
  `whichCombIdx_g == 1`) are excluded from the selectivity-at-age
  table and plot.

## Hover popups

* **At-a-Glance**: CI ribbons on the SB panel, recruitment panel,
  harvest-rate panel, and bio overlay panel now have
  `hovertemplate` instead of `hoverinfo = "skip"`.
* **Status distributions**: crosshair CI traces on B/Bmsy and
  U/Umsy distributions now show hover tooltips.

## Model information

* **Density-dep M flag**: `meta$is_ddm` is exposed in the Model
  Information table (`child_data_summary.Rmd`).

## Child Rmd templates

* `child_mortality.Rmd` is now included in `vis_fit.Rmd` (it was
  missing entirely; the Mortality nav-menu page was never rendered).
* `child_comp_corr.Rmd` and `child_osa.Rmd` added.

# modelVis 0.1.6

## Index and toggle fixes

* Fixed fleet-index toggle so it works correctly for all fleets,
  not just the first.
* Growth transitions: per-quarter panel rows with scaled height;
  hover shows true probability via the `text` field (not raw
  array value).

## Quarterly index line toggle

* Quarterly observations can now be toggled on/off independently
  of the annual trend line in the index-fits panel.

## Index fit confidence intervals

* Lognormal 95% CI error bars added to index fits plot.

# modelVis 0.1.5

## Multi-stock column layout for Fits to Data

* **Index fits, catch fits, length comp fits**: stocks are now displayed as
  columns instead of stacking fleet/stock groups vertically. Length comp panels
  use n_stocks × n_sexes columns per fleet row. A blank placeholder panel is
  inserted for any (fleet × stock) combination with no data, preserving grid
  alignment.

* **Data Summary**: datatype rows appear in fixed order (Length comp on top,
  Catch on bottom); fleet dots within each row are jittered; legend shows each
  fleet name once across all stock panels; explicit y-axis range prevents Catch
  tick label from being clipped.

* **CSS fix**: `.chart-wrapper .chart-stage` overridden with
  `overflow: visible; height: auto` so `htmltools::tagList` panels beyond the
  first are no longer clipped by flexdashboard's default `overflow: hidden`.

## extract.R

* `extract.default()` gives a clear error for objects with an explicit but
  unregistered class rather than attempting auto-detection.
* `siscaL` recognised before `sableOpMod` in auto-detection (disambiguated by
  `lenBinMids_l` and `G_llxpq`).

## Life history / selectivity pages

* `vis_fit.Rmd` eval guards updated: Life History page renders when any of
  `weight_at_age`, `length_at_age`, `age_length_key`, or `maturity_at_length`
  is present; Selectivity page renders for `selectivity_at_length` too.
* Maturity at Length tab added to the Life History page.

# modelVis 0.1.4

## Documentation

* Added `README.md`, developed from the package vignettes:
  pipeline overview, installation, quick start, the `mv`
  object and component conventions, writing an extractor,
  tidy helpers, colour palettes, supported models, and
  development conventions
* Added `HANDOVER.md` documenting project status, the `mv`
  contract, how to write a dispatcher for a new model, and
  the roadmap for length-based assessment output
* Added `^HANDOVER\.md$` to `.Rbuildignore`

# modelVis 0.1.3

## Composition fit plot restyling

* Composition fits now use grey bars (observed) with
  coloured points + line (predicted), matching the
  SABplots.R `plotCompFitYrs` convention
* Applied to `mv_plot_comp_fit()`, `mv_plot_comp_grid()`,
  `mv_plot_comp_avg()`, and time-averaged grids in
  age/length comp child Rmd templates

## Prior NLP extraction

* New `prior_table` mv component extracted from
  sableOpMod (h_nlp, mort_nlp, Fnlp, rec_nlp,
  SelAnlp_f, SelBnlp_f, etc.)
* Prior NLP bar chart added to Optimisation tab

## Exported layout and config functions

* `mv_layout()`, `mv_config()`, `mv_kobe_colours()` now
  exported (were internal `.mv_*` functions)
* All `:::` calls in Rmd templates replaced with `::`
* DT dependency added for interactive parameter
  estimates table

## Tests

* testthat suite: duplicate chunk labels, no `:::`
  in templates, exported function coverage, tidy helper
  unit tests, mv object construction (49 tests)

## TODO for next session

* Test fixtures: create small .rds fixtures for
  testing — MLE-only fit, MCMC fit with posteriors,
  and eventually one per model type (sableOpMod,
  SISCA, ms3R). Needed for extractor smoke tests
  and MCMC diagnostic development.
* Mobile viewport: JS override added but needs
  testing on actual device
* Proportion female fits (low priority, few models
  use it)
* Full MCMC diagnostics: Rhat/ESS histograms,
  prior/posterior density comparisons (needs stanfit
  object in test fixture)
* Sample size (N) annotations on yearly comp fit panels
* `selectivity_at_length` extraction once `sel_lspft`
  is available in sableOpMod
* `mv_sim_dashboard()` for MSE/simulation output
* Verify Pearson residuals match SABplots.R conventions
* Retrospective visualisations: squid plots for SB,
  R, rec devs, releases (plotRetroSBt, plotRetroRt,
  plotRetroRecDev style). Needs retro run output
  as input — either a list of mv objects or a
  dedicated retro extractor.
* Batch/scenario comparison visualisations: overlay
  time series, ref points, selectivity across model
  runs (plotBatchMt, plotComparisonRefPtsU style).
  May use mv_plot_compare or need new functions.
* Consider GitHub Actions CI workflow

# modelVis 0.1.2

## Dashboard restructure

* Dashboard tabs now follow the sableOpMod fit report
  template (fitReportTemplate.Rmd) structure:
  - Data Summary with availability grid
  - At-a-Glance: 3-panel stack (SB + scaled indices,
    recruitment with R0 line, legal HR with catch bars
    and Umsy line) plus all-biomass overlay
  - Index Fits: single column with residual bars below
    each fleet, shared x-axis
  - Catch Fits: landings and releases in separate tabs,
    single column per commercial fleet
  - Recruitment: recruitment + standardised residuals
    (2-panel stack), S-R curve with Beverton-Holt overlay
  - Optimisation: NLL table, phase table, SD report,
    likelihood bar chart
* Removed standalone Biomass tab (replaced by
  At-a-Glance)
* Removed F-at-age surface tab

## Extractor changes

* Extractors now live in model packages (sableOpMod),
  not in modelVis
* New exported helpers: `mv_tidy_ts()`, `mv_tidy_mat()`,
  `mv_tidy_3d()`, `mv_tidy_4d()`, `mv_tidy_5d()`,
  `mv_quantile_ts()`, `mv_year_seq()`, `mv_ensure_ci()`,
  `mv_has_data()` for extractor authors
* `extract.default()` guard against infinite recursion
  when S3 method not registered

## New mv object components

* `data_availability` -- fleet x year x datatype
  presence flags
* `vulnerable_biomass` -- vulnB_pft for index overlays
* `landed_catch_total` -- total landed catch for catch
  bars
* `obs_error` -- estimated observation tau by fleet and
  data type
* `discard_comp_fits` -- discard proportions at age
* `sr_params` -- S-R curve parameters (reca, recb, h,
  B0, R0, sigmaR)
* `fit_diagnostics` -- phase table, gradient table,
  objective function
* `meta$fleet_type` -- 0 = survey, 1 = commercial

## Other changes

* Renamed `vis_fit()` to `mv_dashboard()`
* `mcmc_probs` default changed to `c(0.025, 0.5, 0.975)`
  -- median as risk-neutral point estimate
* Reference curves: two stacked panels (yield + SSB) vs
  legal harvest rate, ref point values annotated
* Kobe plot: trajectory connects chronologically
* Index fits: predicted = vulnB * q (not repOpt$I_pft
  pass-through)
* Age/length comps normalised to proportions
* Mobile-responsive CSS and `responsive = TRUE` in
  plotly config

# modelVis 0.1.0

* Initial package release
* Three-layer architecture: extractor, plotting, template
* 14 plotly plotting functions
* flexdashboard with 13 child Rmd documents
* S3 generic `extract()` with auto-detection
