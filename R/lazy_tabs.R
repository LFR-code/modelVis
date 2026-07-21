# lazy_tabs.R
# Post-processes a rendered flexdashboard HTML file so that only the
# initially-visible page/tab has its plotly widgets bound on load.
# flexdashboard turns every top-level page and every {.tabset} Row into
# a Bootstrap tab-pane at runtime (see flexdashboard.js's
# layoutDashboardPage()/layoutTabset()), but that's a pure CSS
# display:none toggle -- htmlwidgets' HTMLWidgets.staticRender() still
# calls Plotly.newPlot() for every widget on the page regardless of
# visibility. For a dashboard with many pages/tabs this means every
# widget on every page renders before the user sees anything.
#
# Fix: for every widget NOT on the default (first) page/tab, change its
# `<script type="application/json" data-for="htmlwidget-...">` data
# block's type attribute so htmlwidgets' static render can't find it
# (and so skips binding that widget). This touches nothing but that one
# attribute -- no divs are restructured or removed, so flexdashboard's
# own layout code (which reads headers and row/column divs to compute
# sizes) sees completely normal structure. An earlier version of this
# fix wrapped each page's *entire* content in an inert tag, which broke
# flexdashboard.js's own page layout (it depends on finding those
# headers/divs) and corrupted tab navigation entirely -- don't
# reintroduce that.
#
# On first tab-show, a small injected script finds deferred script
# tags inside the shown pane, restores the real type, and calls
# HTMLWidgets.staticRender() again (idempotent, safe to call more than
# once -- it only binds widgets it hasn't already bound).

#' Blank out `<script>...</script>` bodies, preserving length/offsets
#'
#' Plotly widget JSON can contain literal `<div`/`</div>`-like
#' substrings inside string values (hover templates, annotation text),
#' which would otherwise be miscounted as real markup by the
#' balanced-div scanner below. Masking script bodies to spaces (same
#' length, so all other character offsets in the document are
#' unaffected) makes the div/close-div position scan see only real
#' DOM structure.
#'
#' @param html Character scalar, the full document.
#' @return `html` with every `<script>` tag's inner text replaced by
#'   spaces of the same length.
.mv_mask_scripts <- function(html) {
  # (?s) = DOTALL, so "." spans newlines -- some hand-written <script>
  # chunks in the templates are multi-line; without it this regex
  # silently fails to match them, leaving their real <div>/</div> tags
  # unmasked and miscounted as live markup.
  m <- gregexpr("(?s)<script\\b[^>]*>.*?</script>", html, perl = TRUE, ignore.case = TRUE)[[1L]]
  if (m[1L] == -1L) return(html)
  starts <- as.integer(m)
  ends   <- starts + attr(m, "match.length") - 1L

  pieces <- vector("character", length(starts) * 2L + 1L)
  cursor <- 1L
  for (i in seq_along(starts)) {
    pieces[(i - 1L) * 2L + 1L] <- substring(html, cursor, starts[i] - 1L)
    full        <- substring(html, starts[i], ends[i])
    tag_end     <- regexpr(">", full, fixed = TRUE)[1L]
    inner_start <- tag_end + 1L
    inner_end   <- nchar(full) - nchar("</script>")
    pieces[(i - 1L) * 2L + 2L] <- if (inner_end >= inner_start) {
      paste0(
        substring(full, 1L, tag_end),
        strrep(" ", inner_end - inner_start + 1L),
        substring(full, inner_end + 1L)
      )
    } else {
      full
    }
    cursor <- ends[i] + 1L
  }
  pieces[length(pieces)] <- substring(html, cursor)
  paste0(pieces, collapse = "")
}

#' Positions of every real `<div`/`</div>` in `html`
#'
#' @param html Character scalar.
#' @return List with integer vectors `open` and `close`.
.mv_div_positions <- function(html) {
  masked <- .mv_mask_scripts(html)
  list(
    open  = gregexpr("<div\\b", masked)[[1L]],
    close = gregexpr("</div>", masked)[[1L]]
  )
}

#' Find the position of a `<div>`'s matching `</div>`
#'
#' @param pos List as returned by [.mv_div_positions()].
#' @param start Position of the specific `<div` whose match to find;
#'   must be an element of `pos$open`.
#' @return Position of the matching `</div>` start.
.mv_match_close_div <- function(pos, start) {
  ev_pos  <- c(pos$open, pos$close)
  ev_type <- c(rep(TRUE, length(pos$open)), rep(FALSE, length(pos$close)))
  ord     <- order(ev_pos)
  ev_pos  <- ev_pos[ord]
  ev_type <- ev_type[ord]

  i0    <- which(ev_pos == start & ev_type)[1L]
  depth <- 1L
  i     <- i0 + 1L
  while (depth > 0L) {
    depth <- if (ev_type[i]) depth + 1L else depth - 1L
    i <- i + 1L
  }
  ev_pos[i - 1L]
}

#' Defer every widget within a character range from initial binding
#'
#' htmlwidgets' default `find()` for a binding named e.g. "plotly" is
#' `querySelectorAll(scope, ".plotly")` -- it matches by the
#' *container div's CSS class*, completely independent of whether a
#' paired JSON data script exists. The container gets marked
#' `html-widget-static-bound` the moment it's found, regardless of
#' whether valid data was available -- so hiding only the JSON script
#' (an earlier version of this function) does nothing: the div is
#' still found, still marked bound, and a later `staticRender()` call
#' skips it via that mark.
#'
#' The actual fix has to rename the widget-name class token itself
#' (e.g. "plotly" -> "mv-lazy-plotly") so `find()` doesn't match the
#' element at all on the first pass. The JSON script's `type` is also
#' renamed as defense in depth (harmless either way, but keeps the
#' data block from being mistaken for real JSON by anything else that
#' might scan for it).
#'
#' @param html Character scalar.
#' @param start,end Integer character positions bounding the range
#'   (inclusive) to search within.
#' @return The modified `html` string.
.mv_defer_widgets_in_range <- function(html, start, end) {
  span <- substring(html, start, end)
  span <- gsub(
    '(class="[^"]*\\b)plotly(\\b[^"]*"[^>]*>\\s*</div>\\s*<script type=")application/json(" data-for="htmlwidget-)',
    "\\1mv-lazy-plotly\\2mv-lazy-json\\3", span, perl = TRUE
  )
  paste0(substring(html, 1L, start - 1L), span, substring(html, end + 1L))
}

#' Make non-default flexdashboard pages/tabs lazy-render
#'
#' @param html_path Path to a rendered flexdashboard HTML file.
#' @return Invisibly, `html_path`.
#' @keywords internal
mv_lazy_tabs <- function(html_path) {
  html <- paste(readLines(html_path, warn = FALSE, encoding = "UTF-8"),
                collapse = "\n")

  # ---- Top-level pages: defer widgets in every page after the first ----
  page_re <- '<div id="[^"]*" class="section level1"[^>]*>'
  page_starts <- gregexpr(page_re, html)[[1L]]
  first_page_end <- nchar(html)

  if (length(page_starts) > 1L && page_starts[1L] != -1L) {
    pos <- .mv_div_positions(html)
    first_page_end <- .mv_match_close_div(pos, page_starts[1L])
    for (k in seq(2L, length(page_starts))) {
      page_end <- .mv_match_close_div(pos, page_starts[k])
      html <- .mv_defer_widgets_in_range(html, page_starts[k], page_end)
    }
  }

  # ---- Tabsets inside the default (first) page only ----
  # Other pages are already fully deferred above, so any tabset inside
  # them is already covered; only the first page's tabsets need this.
  head_html <- substring(html, 1L, first_page_end)

  tabset_re <- '<div id="[^"]*" class="section level[0-9] tabset"[^>]*>'
  tabset_starts <- gregexpr(tabset_re, head_html)[[1L]]
  if (tabset_starts[1L] != -1L) {
    pos_h <- .mv_div_positions(head_html)
    for (t in seq_along(tabset_starts)) {
      tabset_end <- .mv_match_close_div(pos_h, tabset_starts[t])
      tab_span   <- substring(head_html, tabset_starts[t], tabset_end + 6L)

      panel_re <- '<div id="[^"]*" class="section level[0-9]"[^>]*>'
      panel_starts <- gregexpr(panel_re, tab_span)[[1L]]
      if (length(panel_starts) > 1L && panel_starts[1L] != -1L) {
        pos_p <- .mv_div_positions(tab_span)
        for (p in seq(2L, length(panel_starts))) {
          panel_end <- .mv_match_close_div(pos_p, panel_starts[p])
          tab_span  <- .mv_defer_widgets_in_range(tab_span, panel_starts[p], panel_end)
        }
      }
      head_html <- paste0(
        substring(head_html, 1L, tabset_starts[t] - 1L),
        tab_span,
        substring(head_html, tabset_end + 7L)
      )
    }
  }
  html <- paste0(head_html, substring(html, first_page_end + 1L))

  # ---- Injected background-render script (once, end of body) ----
  # Deferring a widget until its tab is *shown* means the user pays
  # the render cost right when they click the tab, which is exactly
  # the wait we were trying to remove. Instead: as soon as the default
  # page has finished loading, bind every deferred widget in the
  # background a few at a time (small setTimeout steps, so the still-
  # visible first page stays responsive), regardless of whether its
  # tab is currently shown. HTMLWidgets.staticRender() itself doesn't
  # care about visibility -- Plotly.newPlot() runs fine on a
  # display:none container, it just measures 0x0 at creation time.
  # That''s harmless as long as we force a resize once the container
  # becomes visible: the plotly htmlwidgets binding registers a
  # resize() method, and HTMLWidgets'' own window-resize listener
  # re-measures every bound widget and calls it when dimensions
  # change. So a plain periodic `window.dispatchEvent(new
  # Event("resize"))` is enough to make every already-rendered-but-
  # hidden plot snap to the right size the moment its tab opens, with
  # no dependency on catching a specific bootstrap tab-show event
  # (which, per earlier attempts, doesn''t fire the way plain
  # addEventListener or $(document).on() expect here anyway).
  lazy_js <- '
<script>
(function() {
  function restoreOne(el) {
    el.classList.remove("mv-lazy-plotly");
    el.classList.add("plotly");
    var scr = el.nextElementSibling;
    if (scr && scr.tagName === "SCRIPT" && scr.getAttribute("type") === "mv-lazy-json") {
      scr.setAttribute("type", "application/json");
    }
  }
  function backgroundRender() {
    var queue = Array.prototype.slice.call(document.querySelectorAll(".mv-lazy-plotly"));
    if (queue.length === 0) return;
    var i = 0;
    function step() {
      var batch = queue.slice(i, i + 3);
      batch.forEach(restoreOne);
      i += batch.length;
      if (window.HTMLWidgets) HTMLWidgets.staticRender();
      if (i < queue.length) setTimeout(step, 30);
    }
    step();
  }
  if (document.readyState === "complete") {
    setTimeout(backgroundRender, 50);
  } else {
    window.addEventListener("load", function() { setTimeout(backgroundRender, 50); });
  }
  setInterval(function() { window.dispatchEvent(new Event("resize")); }, 500);
})();
</script>
'
  html <- sub("</body>", paste0(lazy_js, "</body>"), html, fixed = TRUE)

  writeLines(html, html_path, useBytes = TRUE)
  invisible(html_path)
}
