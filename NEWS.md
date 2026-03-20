# modelVis 0.1.1

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
