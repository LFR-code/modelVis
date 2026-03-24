# make_example_mv.R
# Generate a synthetic mv object for use in modelVis
# vignettes. Run from the modelVis package root:
#   source("inst/extdata/make_example_mv.R")

library(modelVis)

set.seed(42)

years      <- 1990:2020
nT         <- length(years)
ages       <- 1:10
nA         <- length(ages)
fleetNames <- c("fishery1", "fishery2", "survey1")
nG         <- length(fleetNames)

# --- Spawning biomass ---
sb_true <- 50 * exp(
  -0.02 * seq_len(nT) +
    cumsum(rnorm(n = nT, mean = 0, sd = 0.05))
)
spawning_biomass <- data.frame(
  year = years,
  est  = sb_true,
  lwr  = sb_true * 0.8,
  upr  = sb_true * 1.2
)

# --- Recruitment ---
rec_true <- 10 * exp(
  rnorm(n = nT, mean = 0, sd = 0.3)
)
recruitment <- data.frame(
  year = years,
  est  = rec_true,
  lwr  = rec_true * 0.7,
  upr  = rec_true * 1.3
)

# --- Recruitment deviations ---
rec_devs <- data.frame(
  year = years,
  est  = rnorm(n = nT, mean = 0, sd = 0.3)
)

# --- Stock-recruit ---
stock_recruit <- data.frame(
  ssb         = sb_true,
  recruitment = rec_true,
  year        = years
)

sr_params <- list(
  reca   = 15.2,
  recb   = 0.025,
  h      = 0.74,
  B0     = 55,
  R0     = 10,
  sigmaR = 0.3
)

# --- Index fits ---
idx_rows <- list()
for (g in seq_len(nG)) {
  q_g    <- c(0.5, 0.3, 1.0)[g]
  pred_g <- q_g * sb_true *
    runif(n = nT, min = 0.9, max = 1.1)
  obs_g  <- pred_g *
    exp(rnorm(n = nT, mean = 0, sd = 0.15))
  # Surveys have missing years
  if (g == 3) obs_g[c(1:3, 10, 20)] <- NA
  residual_g <- ifelse(
    test = !is.na(obs_g) & obs_g > 0,
    yes  = log(obs_g) - log(pred_g),
    no   = NA_real_
  )
  idx_rows[[g]] <- data.frame(
    year     = years,
    obs      = obs_g,
    pred     = pred_g,
    fleet    = fleetNames[g],
    residual = residual_g
  )
}
index_fits <- do.call(what = rbind, args = idx_rows)

# --- Catch fits ---
catch_rows <- list()
for (g in 1:2) {
  pred_g <- sb_true * c(0.1, 0.05)[g]
  obs_g  <- pred_g *
    exp(rnorm(n = nT, mean = 0, sd = 0.05))
  catch_rows[[g]] <- data.frame(
    year  = years,
    obs   = obs_g,
    pred  = pred_g,
    fleet = fleetNames[g]
  )
}
catch_fits <- do.call(what = rbind, args = catch_rows)

# --- Age composition fits ---
sel_curves <- list(
  fishery1 = 1 / (1 + exp(-1.5 * (ages - 4))),
  fishery2 = 1 / (1 + exp(-2.0 * (ages - 3))),
  survey1  = 1 / (1 + exp(-1.0 * (ages - 2)))
)
age_rows <- list()
for (g in seq_len(nG)) {
  sel_g <- sel_curves[[g]]
  for (t in seq_len(nT)) {
    pred_prop <- sel_g / sum(sel_g)
    obs_prop  <- pred_prop *
      exp(rnorm(n = nA, mean = 0, sd = 0.2))
    obs_prop  <- obs_prop / sum(obs_prop)
    age_rows[[length(age_rows) + 1]] <- data.frame(
      age   = ages,
      sex   = "Combined",
      year  = years[t],
      obs   = obs_prop,
      pred  = pred_prop,
      fleet = fleetNames[g]
    )
  }
}
age_comp_fits <- do.call(what = rbind, args = age_rows)

# --- Selectivity at age ---
sel_rows <- list()
for (g in seq_len(nG))
  sel_rows[[g]] <- data.frame(
    age   = ages,
    sex   = "Combined",
    est   = sel_curves[[g]],
    fleet = fleetNames[g]
  )
selectivity_at_age <- do.call(
  what = rbind, args = sel_rows
)

# --- Legal harvest rate ---
legal_hr <- data.frame(
  year = years,
  est  = 0.15 + 0.005 * seq_len(nT) +
    rnorm(n = nT, mean = 0, sd = 0.01),
  lwr  = NA_real_,
  upr  = NA_real_
)

# --- Weight at age ---
weight_at_age <- data.frame(
  age = ages,
  sex = "Combined",
  est = 0.1 * ages^2.8
)

# --- Maturity at age ---
maturity_at_age <- data.frame(
  age = ages,
  est = 1 / (1 + exp(-2.5 * (ages - 3)))
)

# --- Reference points ---
ref_points <- list(
  Fmsy  = 0.18,
  SBmsy = 25,
  lUmsy = 0.16,
  lMSY  = 4.5,
  B0    = 55,
  R0    = 10
)

# --- Reference curves ---
u_seq <- seq(from = 0, to = 0.5, length.out = 100)
ref_curves <- data.frame(
  legalU  = u_seq,
  yield   = 4.5 * 4 * u_seq / 0.16 *
    (1 - u_seq / 0.5) / (1 + u_seq / 0.16),
  biomass = 55 * (1 - u_seq / 0.5)
)

# --- Likelihood table ---
likelihood_table <- data.frame(
  component = c(
    rep("Index", 3),
    rep("Age composition", 3)
  ),
  fleet = rep(fleetNames, 2),
  nll   = c(12.3, 15.1, 8.7, 45.2, 38.9, 22.1)
)

# --- Meta ---
meta <- list(
  model_type       = "example",
  label            = "Simulated example",
  years            = years,
  ages             = ages,
  sex_names        = "Combined",
  fleet_names      = fleetNames,
  area_names       = "All",
  has_length_comps = FALSE,
  has_discards     = FALSE,
  has_mcmc         = FALSE,
  fleet_type       = c(1L, 1L, 0L)
)

# --- Assemble ---
example_mv <- new_mv(
  meta               = meta,
  spawning_biomass    = spawning_biomass,
  recruitment        = recruitment,
  rec_devs           = rec_devs,
  stock_recruit      = stock_recruit,
  sr_params          = sr_params,
  index_fits         = index_fits,
  catch_fits         = catch_fits,
  age_comp_fits      = age_comp_fits,
  selectivity_at_age = selectivity_at_age,
  legal_hr           = legal_hr,
  weight_at_age      = weight_at_age,
  maturity_at_age    = maturity_at_age,
  ref_points         = ref_points,
  ref_curves         = ref_curves,
  likelihood_table   = likelihood_table
)

saveRDS(
  object = example_mv,
  file   = "inst/extdata/example_mv.rds"
)
message("Saved inst/extdata/example_mv.rds")
