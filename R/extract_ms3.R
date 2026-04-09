# extract_ms3.R
# S3 extractor for MS3 closed-loop simulation blobs.
# Converts replicate-indexed arrays into mv_sim objects
# with simulation envelopes (median + quantile CIs).

#' Extract standardised output from an MS3 simulation blob
#'
#' Converts a saved MS3 simulation blob (the \code{blob}
#' object from \code{runMS3()}) into a standardised
#' \code{"mv_sim"} object for visualisation with
#' \code{\link{mv_sim_dashboard}}.
#'
#' @param fit An MS3 blob list (class \code{"ms3Blob"} or
#'   auto-detected by fingerprint).
#' @param label Character label for this scenario
#'   (default: scenario/MP from control list).
#' @param probs Numeric vector of length 3: lower quantile,
#'   median, upper quantile. Default
#'   \code{c(0.025, 0.5, 0.975)}.
#' @param ... Additional arguments (ignored).
#' @return An object of class \code{c("mv_sim", "mv")}.
#' @export
extract.ms3Blob <- function(fit, label = NULL,
                            probs = c(0.025, 0.5, 0.975),
                            ...) {

  om      <- fit$om
  mp      <- fit$mp
  ctlList <- fit$ctlList
  # rp is a list of per-replicate rp objects; use first
  rp <- if (is.list(fit$rp) && length(fit$rp) > 0) {
    fit$rp[[1]]
  } else {
    fit$rp
  }

  # ---- Dimensions ----
  tMP   <- om$tMP
  nT    <- om$nT
  nS    <- om$nS
  nP    <- om$nP
  nF    <- om$nF
  nA    <- om$nA
  fYear <- ctlList$opMod$fYear
  pT    <- ctlList$opMod$pT
  years <- seq(from = fYear, length.out = nT)

  # Good replicates
  goodReps   <- fit$goodReps
  goodRepIdx <- which(goodReps)
  nGoodReps  <- length(goodRepIdx)

  # Ages
  ages <- seq(from = 0, length.out = nA)

  # Fleet names
  fleet_names <- if (!is.null(om$fleetNames)) {
    om$fleetNames
  } else {
    paste0("Fleet_", seq_len(nF))
  }

  # Fleet type
  fleet_type <- if (!is.null(om$fleetType_f)) {
    om$fleetType_f
  } else {
    rep(1L, nF)
  }

  # Species/stock names
  species <- if (!is.null(om$speciesNames)) {
    om$speciesNames
  } else {
    ctlList$opMod$species
  }
  stock <- if (!is.null(om$stockNames)) {
    om$stockNames
  } else {
    ctlList$opMod$stocks
  }

  # ---- Label ----
  if (is.null(label)) {
    label <- paste0(
      ctlList$ctl$scenarioName, " / ",
      ctlList$ctl$mpName
    )
  }

  # ---- Helper: envelope across good reps ----
  # x: vector of values across replicates for one
  #    time step (length = nGoodReps)
  .envelope <- function(arr_ispt, s = 1, p = 1) {
    # arr_ispt is [nReps x nS x nP x nT]
    mat <- arr_ispt[goodRepIdx, s, p, , drop = FALSE]
    dim(mat) <- c(nGoodReps, nT)
    data.frame(
      year = years,
      est  = apply(
        X = mat, MARGIN = 2, FUN = quantile,
        probs = probs[2], na.rm = TRUE
      ),
      lwr  = apply(
        X = mat, MARGIN = 2, FUN = quantile,
        probs = probs[1], na.rm = TRUE
      ),
      upr  = apply(
        X = mat, MARGIN = 2, FUN = quantile,
        probs = probs[3], na.rm = TRUE
      ),
      row.names = NULL
    )
  }

  # Fleet-specific envelope [nReps x nS x nP x nF x nT]
  .envelope_fleet <- function(arr_ispft, s = 1, p = 1) {
    dfs <- lapply(
      X = seq_len(nF),
      FUN = function(f) {
        mat <- arr_ispft[goodRepIdx, s, p, f, ,
                         drop = FALSE]
        dim(mat) <- c(nGoodReps, nT)
        data.frame(
          year  = years,
          fleet = fleet_names[f],
          est   = apply(
            X = mat, MARGIN = 2, FUN = quantile,
            probs = probs[2], na.rm = TRUE
          ),
          lwr   = apply(
            X = mat, MARGIN = 2, FUN = quantile,
            probs = probs[1], na.rm = TRUE
          ),
          upr   = apply(
            X = mat, MARGIN = 2, FUN = quantile,
            probs = probs[3], na.rm = TRUE
          ),
          row.names = NULL,
          stringsAsFactors = FALSE
        )
      }
    )
    do.call(what = rbind, args = dfs)
  }

  # Replicate traces [nReps x nS x nP x nT]
  .traces <- function(arr_ispt, s = 1, p = 1) {
    dfs <- lapply(
      X = goodRepIdx,
      FUN = function(i) {
        data.frame(
          year  = years,
          rep   = i,
          value = arr_ispt[i, s, p, ],
          row.names = NULL
        )
      }
    )
    do.call(what = rbind, args = dfs)
  }

  # ---- Envelopes ----
  spawning_biomass <- .envelope(arr_ispt = om$SB_ispt)
  total_biomass    <- .envelope(arr_ispt = om$B_ispt)
  recruitment      <- .envelope(arr_ispt = om$R_ispt)
  catch            <- .envelope(arr_ispt = om$C_ispt)

  # ---- Mean M (averaged over ages) ----
  nat_mort <- NULL
  if (!is.null(om$M_iaxspt)) {
    # Average M over ages per rep and year
    M_arr <- om$M_iaxspt[, , 1, 1, 1, , drop = FALSE]
    dim(M_arr) <- c(dim(om$M_iaxspt)[1:2], nT)
    # mean over ages -> [nReps x nT]
    M_it <- apply(X = M_arr, MARGIN = c(1, 3), FUN = mean)
    nat_mort <- data.frame(
      year = years,
      est  = apply(
        X = M_it[goodRepIdx, , drop = FALSE],
        MARGIN = 2, FUN = quantile,
        probs = probs[2], na.rm = TRUE
      ),
      lwr  = apply(
        X = M_it[goodRepIdx, , drop = FALSE],
        MARGIN = 2, FUN = quantile,
        probs = probs[1], na.rm = TRUE
      ),
      upr  = apply(
        X = M_it[goodRepIdx, , drop = FALSE],
        MARGIN = 2, FUN = quantile,
        probs = probs[3], na.rm = TRUE
      ),
      row.names = NULL
    )
  }

  # ---- Fleet-level envelopes ----
  catch_by_fleet   <- .envelope_fleet(
    arr_ispft = om$C_ispft
  )
  harvest_rate     <- .envelope_fleet(
    arr_ispft = om$F_ispft
  )

  # ---- Survey indices ----
  # I_ispft has dims [nReps, nS+1, nP+1, nF, nT]
  # Extract species 1, pop 1 only
  idx_dfs <- lapply(
    X = seq_len(nF),
    FUN = function(f) {
      mat <- fit$mp$data$I_ispft[goodRepIdx, 1, 1, f, ,
                                  drop = FALSE]
      dim(mat) <- c(nGoodReps, nT)
      data.frame(
        year  = years,
        fleet = fleet_names[f],
        est   = apply(
          X = mat, MARGIN = 2, FUN = quantile,
          probs = probs[2], na.rm = TRUE
        ),
        lwr   = apply(
          X = mat, MARGIN = 2, FUN = quantile,
          probs = probs[1], na.rm = TRUE
        ),
        upr   = apply(
          X = mat, MARGIN = 2, FUN = quantile,
          probs = probs[3], na.rm = TRUE
        ),
        row.names = NULL,
        stringsAsFactors = FALSE
      )
    }
  )
  indices <- do.call(what = rbind, args = idx_dfs)
  # Drop fleets with no data
  indices <- indices[!is.na(indices$est), , drop = FALSE]

  # ---- Survey data (state + index + q per fleet) ----
  # Each survey fleet gets a clean bundle for the dashboard.
  # Biomass-based surveys: state = vuln B
  # Numbers-based surveys: state = vuln N (summed over ages)
  #
  # condObj stores the NCAM fleet structure:
  #   Fleet 2 (RV) = biomass index (uses rvw)
  #   Fleet 3 (SN) = numbers index (power on N)
  numbers_fleets <- 3L  # SN is numbers-based

  .rep_traces <- function(arr_ispft, f) {
    dfs <- lapply(
      X = goodRepIdx,
      FUN = function(i) {
        data.frame(
          year  = years,
          rep   = i,
          value = arr_ispft[i, 1, 1, f, ],
          row.names = NULL,
          stringsAsFactors = FALSE
        )
      }
    )
    out <- do.call(what = rbind, args = dfs)
    out$value[out$value == 0] <- NA
    out
  }

  .rep_traces_vn <- function(f) {
    dfs <- lapply(
      X = goodRepIdx,
      FUN = function(i) {
        vn_af <- om$vN_iaxspft[i, , 1, 1, 1, f, ,
                                drop = FALSE]
        dim(vn_af) <- c(nA, nT)
        data.frame(
          year  = years,
          rep   = i,
          value = colSums(vn_af),
          row.names = NULL,
          stringsAsFactors = FALSE
        )
      }
    )
    out <- do.call(what = rbind, args = dfs)
    out$value[out$value == 0] <- NA
    out
  }

  survey_data <- list()
  surv_idx <- which(fleet_type == 0)
  for (f in surv_idx) {
    is_numbers <- f %in% numbers_fleets
    survey_data[[fleet_names[f]]] <- list(
      state = if (is_numbers) {
        .rep_traces_vn(f = f)
      } else {
        .rep_traces(arr_ispft = om$vB_ispft, f = f)
      },
      index = .rep_traces(
        arr_ispft = fit$mp$data$I_ispft, f = f
      ),
      q     = .rep_traces(
        arr_ispft = om$q_ispft, f = f
      ),
      label = if (is_numbers) "numbers" else "biomass"
    )
  }

  # Backward compat: keep envelope for overview
  vuln_biomass <- .envelope_fleet(
    arr_ispft = om$vB_ispft
  )
  vuln_biomass <- vuln_biomass[
    !is.na(vuln_biomass$est), , drop = FALSE
  ]
  vuln_biomass$est[vuln_biomass$est == 0] <- NA
  vuln_biomass$lwr[vuln_biomass$lwr == 0] <- NA
  vuln_biomass$upr[vuln_biomass$upr == 0] <- NA

  # Legacy traces (kept for existing consumers)
  q_traces <- lapply(
    X = seq_len(nF),
    FUN = function(f) {
      .rep_traces(arr_ispft = om$q_ispft, f = f)
    }
  )
  names(q_traces) <- fleet_names

  # ---- TAC envelope ----
  tac <- NULL
  if (!is.null(mp$hcr$TAC_ispt)) {
    tac <- .envelope(arr_ispt = mp$hcr$TAC_ispt)
  }

  # ---- Replicate traces ----
  traces <- list(
    spawning_biomass = .traces(arr_ispt = om$SB_ispt),
    catch            = .traces(arr_ispt = om$C_ispt),
    recruitment      = .traces(arr_ispt = om$R_ispt)
  )

  # ---- Stock-recruit data (all good reps) ----
  sr_dfs <- lapply(
    X = goodRepIdx,
    FUN = function(i) {
      # SSB at t, R at t+1
      ssb <- om$SB_ispt[i, 1, 1, ]
      rec <- om$R_ispt[i, 1, 1, ]
      nyr <- length(ssb) - 1
      data.frame(
        ssb         = ssb[seq_len(nyr)],
        recruitment = rec[seq_len(nyr) + 1L],
        year        = years[seq_len(nyr)],
        rep         = i,
        period      = ifelse(
          test = seq_len(nyr) < tMP,
          yes  = "Historical",
          no   = "Projection"
        ),
        row.names = NULL,
        stringsAsFactors = FALSE
      )
    }
  )
  stock_recruit <- do.call(what = rbind, args = sr_dfs)

  # ---- Reference points ----
  ref_points <- NULL
  if (!is.null(rp$FmsyRefPts)) {
    ref_points <- list(
      Bmsy  = rp$FmsyRefPts$Bmsy_sp[1, 1],
      Fmsy  = rp$FmsyRefPts$Fmsy_sp[1, 1],
      MSY   = rp$FmsyRefPts$MSY_sp[1, 1],
      B0    = if (!is.null(om$B0_sp)) {
        om$B0_sp[1, 1]
      } else {
        NA_real_
      },
      rec_a = if (!is.null(rp$rec.a_sp)) {
        rp$rec.a_sp[1, 1]
      } else {
        NA_real_
      },
      rec_b = if (!is.null(rp$rec.b_sp)) {
        rp$rec.b_sp[1, 1]
      } else {
        NA_real_
      }
    )
  }

  # ---- HCR parameters ----
  hcr_pars <- NULL
  if (!is.null(ctlList$mp$hcr)) {
    hcr_pars <- ctlList$mp$hcr
  }

  # ---- Conditioning validation (rep 1 vs condModel) ----
  cond_valid <- NULL
  condObj <- ctlList$opMod$condObj
  if (!is.null(condObj) && !is.null(condObj$rep)) {
    crep  <- condObj$rep
    cdata <- condObj$data
    histdx <- seq_len(tMP - 1)
    hist_yrs <- years[histdx]

    # NCAM reference quantities
    ncam_ssb <- crep$ssb[histdx]
    ncam_R   <- crep$recruitment[histdx]
    ncam_N   <- crep$N_matrix[, histdx, drop = FALSE]
    ncam_Ntot <- colSums(ncam_N)

    # MS3 rep-1 quantities
    ms3_ssb  <- om$SB_ispt[1, 1, 1, histdx]
    ms3_R    <- om$R_ispt[1, 1, 1, histdx]
    ms3_N_at <- om$N_iaxspt[1, , 1, 1, 1, histdx,
                             drop = FALSE]
    dim(ms3_N_at) <- c(nA, length(histdx))
    ms3_Ntot <- colSums(ms3_N_at)

    .re <- function(ms3, ncam) {
      (ms3 - ncam) / (abs(ncam) + 1e-10)
    }

    re_ssb  <- .re(ms3 = ms3_ssb,  ncam = ncam_ssb)
    re_R    <- .re(ms3 = ms3_R,    ncam = ncam_R)
    re_Ntot <- .re(ms3 = ms3_Ntot, ncam = ncam_Ntot)

    # Catch validation (if available)
    re_C <- NULL
    if (!is.null(crep$pred_catch_wt_matrix)) {
      ncam_C <- colSums(
        crep$pred_catch_wt_matrix[, histdx, drop = FALSE]
      )
      ms3_C <- om$C_ispt[1, 1, 1, histdx]
      re_C  <- .re(ms3 = ms3_C, ncam = ncam_C)
    }

    cond_valid <- list(
      years    = hist_yrs,
      re_ssb   = re_ssb,
      re_R     = re_R,
      re_N     = re_Ntot,
      re_C     = re_C,
      ncam_ssb = ncam_ssb,
      ms3_ssb  = ms3_ssb,
      ncam_R   = ncam_R,
      ms3_R    = ms3_R,
      ncam_N   = ncam_Ntot,
      ms3_N    = as.numeric(ms3_Ntot),
      ncam_C   = if (!is.null(re_C)) ncam_C else NULL,
      ms3_C    = if (!is.null(re_C)) ms3_C else NULL,
      max_re = data.frame(
        variable = c("SSB", "Recruitment", "Total_N",
                     if (!is.null(re_C)) "Catch"),
        max_abs_re = c(
          max(abs(re_ssb), na.rm = TRUE),
          max(abs(re_R), na.rm = TRUE),
          max(abs(re_Ntot), na.rm = TRUE),
          if (!is.null(re_C))
            max(abs(re_C), na.rm = TRUE)
        ),
        stringsAsFactors = FALSE
      )
    )
  }

  # ---- Build mv object ----
  mv <- new_mv(
    meta = list(
      model_type  = "ms3Blob",
      label       = label,
      years       = years,
      ages        = ages,
      sex_names   = "Combined",
      fleet_names = fleet_names,
      area_names  = if (!is.null(stock)) stock else "Area1",
      is_simulation = TRUE,
      fleet_type  = fleet_type,
      tMP         = tMP,
      fYear       = fYear,
      pT          = pT,
      n_reps      = nGoodReps,
      good_reps   = goodRepIdx,
      scenario    = ctlList$ctl$scenarioName,
      mp_name     = ctlList$ctl$mpName,
      species     = species,
      stock       = stock
    ),
    spawning_biomass = spawning_biomass,
    total_biomass    = total_biomass,
    recruitment      = recruitment,
    catch            = catch,
    nat_mort         = nat_mort,
    catch_by_fleet   = catch_by_fleet,
    harvest_rate     = harvest_rate,
    indices          = indices,
    survey_data      = survey_data,
    vuln_biomass     = vuln_biomass,
    q_traces         = q_traces,
    tac              = tac,
    traces           = traces,
    stock_recruit    = stock_recruit,
    ref_points       = ref_points,
    hcr_pars         = hcr_pars,
    cond_valid       = cond_valid
  )

  # Set simulation class
  class(mv) <- c("mv_sim", "mv")
  mv
}


#' @export
print.mv_sim <- function(x, ...) {
  cat("modelVis simulation object (class 'mv_sim')\n")
  cat("  Scenario   :", x$meta$scenario, "\n")
  cat("  MP         :", x$meta$mp_name, "\n")
  cat("  Species    :", x$meta$species, "\n")
  cat("  Stock      :", x$meta$stock, "\n")
  cat("  Years      :",
      min(x$meta$years), "-", max(x$meta$years), "\n")
  cat("  tMP        :", x$meta$tMP,
      "(", x$meta$fYear + x$meta$tMP - 1, ")\n")
  cat("  Replicates :", x$meta$n_reps, "\n")
  cat("  Fleets     :",
      paste(x$meta$fleet_names, collapse = ", "), "\n")
  comps <- setdiff(names(x), "meta")
  non_null <- comps[!vapply(
    X = x[comps], FUN = is.null, FUN.VALUE = logical(1)
  )]
  cat("  Components :", length(non_null), "populated\n")
  invisible(x)
}
