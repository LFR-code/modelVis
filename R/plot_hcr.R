# plot_hcr.R
# Harvest control rule diagnostic plots.

#' Plot AGC step-ramp HCR with per-replicate trajectory
#'
#' Draws the HCR shape (Frel vs biomass) and overlays
#' per-replicate estimated/realised biomass-F pairs
#' connected by lines for each year.
#'
#' @param hcr_pars List with elements: Bmsy, LRP, midCaut,
#'   USR, Ffloor, Fmid, Fusr, Fref (all scalars).
#' @param trajectory Data.frame with columns: rep, year,
#'   est_B, true_B, target_F, realised_HR.
#' @param good_reps Integer vector of replicate IDs.
#' @param title Plot title.
#' @return A plotly object.
#' @export
mv_plot_hcr <- function(hcr_pars,
                        trajectory = NULL,
                        good_reps  = NULL,
                        title = "Harvest Control Rule") {

  Bmsy    <- hcr_pars$Bmsy
  LRP     <- hcr_pars$LRP
  midCaut <- hcr_pars$midCaut
  USR     <- hcr_pars$USR
  Ffloor  <- hcr_pars$Ffloor
  Fmid    <- hcr_pars$Fmid
  Fusr    <- hcr_pars$Fusr
  Fref    <- hcr_pars$Fref

  # HCR breakpoints (biomass in kt)
  Blrp    <- LRP     * Bmsy / 1e3
  Bmid    <- midCaut * Bmsy / 1e3
  Busr    <- USR     * Bmsy / 1e3
  Bmsy_kt <- Bmsy / 1e3
  # Extend HCR line to cover all data points
  Bmax <- Bmsy_kt * 1.3
  if (!is.null(trajectory) &&
      is.data.frame(trajectory)) {
    data_max <- max(
      c(trajectory$est_B, trajectory$true_B),
      na.rm = TRUE
    ) / 1e3
    Bmax <- max(Bmax, data_max * 1.1)
  }

  # HCR line segments
  hcr_B <- c(0, Blrp - 0.01,
             Blrp, Bmid,
             Bmid, Busr,
             Busr, Bmsy_kt,
             Bmsy_kt, Bmax)
  hcr_F <- c(0, 0,
             Ffloor, Fmid,
             Fmid, Fusr,
             Fusr, Fref,
             Fref, Fref)

  p <- plot_ly()

  # HCR line (always visible)
  p <- add_lines(
    p = p,
    x    = hcr_B,
    y    = hcr_F,
    name = "HCR",
    line = list(color = "#1f77b4", width = 3),
    hovertemplate = paste0(
      "B: %{x:.0f} kt<br>",
      "Frel: %{y:.4f}<extra></extra>"
    )
  )

  # Per-replicate traces with slider
  if (!is.null(trajectory) &&
      is.data.frame(trajectory)) {

    reps <- if (!is.null(good_reps)) {
      good_reps
    } else {
      sort(unique(trajectory$rep))
    }

    # 3 traces per rep: target markers, realised markers,
    # connecting lines
    for (i in seq_along(reps)) {
      r <- reps[i]
      sub <- trajectory[trajectory$rep == r, ,
                         drop = FALSE]
      vis <- (i == 1)

      # Target (estimated B, target F)
      p <- add_markers(
        p = p,
        x       = sub$est_B / 1e3,
        y       = sub$target_F,
        name    = "Target (est. B)",
        visible = vis,
        marker  = list(
          color = "#ff7f0e", size = 8,
          symbol = "circle"
        ),
        text = sub$year,
        legendgroup = "target",
        showlegend  = (i == 1),
        hovertemplate = paste0(
          "Year %{text}<br>",
          "Est. B: %{x:.0f} kt<br>",
          "Target Frel: %{y:.4f}",
          "<extra></extra>"
        )
      )

      # Realised (true B, realised HR)
      p <- add_markers(
        p = p,
        x       = sub$true_B / 1e3,
        y       = sub$realised_HR,
        name    = "Realised (true SSB)",
        visible = vis,
        marker  = list(
          color = "#2ca02c", size = 6,
          symbol = "diamond"
        ),
        text = sub$year,
        legendgroup = "realised",
        showlegend  = (i == 1),
        hovertemplate = paste0(
          "Year %{text}<br>",
          "True SSB: %{x:.0f} kt<br>",
          "Realised C/SSB: %{y:.4f}",
          "<extra></extra>"
        )
      )

      # Connecting lines (est_B,targetF) to
      # (true_B, realised_HR) for each year
      conn_x <- as.numeric(rbind(
        sub$est_B / 1e3,
        sub$true_B / 1e3,
        NA
      ))
      conn_y <- as.numeric(rbind(
        sub$target_F,
        sub$realised_HR,
        NA
      ))
      p <- add_lines(
        p = p,
        x       = conn_x,
        y       = conn_y,
        name    = "Est. vs realised",
        visible = vis,
        line    = list(
          color = "rgba(150,150,150,0.5)",
          width = 1
        ),
        legendgroup = "connect",
        showlegend  = (i == 1),
        hoverinfo   = "skip"
      )
    }

    # Slider: HCR line (trace 0) always visible,
    # then 3 traces per rep
    n_per_rep <- 3L
    steps <- lapply(
      X = seq_along(reps),
      FUN = function(i) {
        vis <- rep(FALSE, length(reps) * n_per_rep)
        idx <- ((i - 1) * n_per_rep + 1):(i * n_per_rep)
        vis[idx] <- TRUE
        # Prepend TRUE for HCR line
        vis <- c(TRUE, vis)
        list(
          method = "restyle",
          args   = list(list(
            visible = as.list(vis)
          )),
          label  = as.character(reps[i])
        )
      }
    )
  }

  # Reference line shapes
  ref_lines <- list(
    list(x = Blrp, label = "LRP",
         col = "#d62728", dash = "dash"),
    list(x = Bmid, label = "0.6 Bmsy",
         col = "#ff7f0e", dash = "dot"),
    list(x = Busr, label = "USR",
         col = "#2ca02c", dash = "dash"),
    list(x = Bmsy_kt, label = "Bmsy",
         col = "#9467bd", dash = "dash")
  )

  ref_shapes <- list()
  ref_annot  <- list()
  for (rl in ref_lines) {
    ref_shapes[[length(ref_shapes) + 1]] <- list(
      type = "line",
      x0 = rl$x, x1 = rl$x,
      y0 = 0, y1 = 1, yref = "paper",
      line = list(
        color = rl$col, width = 1,
        dash = rl$dash
      )
    )
    ref_annot[[length(ref_annot) + 1]] <- list(
      x = rl$x, y = 1.05, yref = "paper",
      text = rl$label, showarrow = FALSE,
      font = list(size = 10, color = rl$col)
    )
  }

  lay <- .mv_layout()
  lay$xaxis$title <- "SSB (kt)"
  lay$yaxis$title <- "Harvest rate (Frel)"
  lay$title <- title
  lay$showlegend <- TRUE
  lay$shapes <- ref_shapes
  lay$annotations <- ref_annot

  if (!is.null(trajectory) && exists("steps")) {
    lay$sliders <- list(list(
      active = 0,
      currentvalue = list(
        prefix = "Replicate: ",
        font   = list(size = 12)
      ),
      pad   = list(t = 40),
      steps = steps
    ))
  }

  p <- do.call(
    what = layout,
    args = c(list(p = p), lay)
  )

  .mv_config(p = p)
}
