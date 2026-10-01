# Web condition diagnostic gaze plots (Days 1-3) — base R.
# These are useful for debugging, but not the main poster figures.
script_dir <- function(default) {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg)) return(dirname(normalizePath(sub("^--file=", "", file_arg[1]))))
  dirname(normalizePath(default))
}
ROOT <- normalizePath(file.path(script_dir("R/02_web_gaze_plots.R"), ".."))
FIG  <- file.path(ROOT, "archive", "diagnostic_web_plots")
dir.create(FIG, showWarnings = FALSE, recursive = TRUE)

web <- read.csv(file.path(ROOT, "output", "web_aoi_long.csv"), stringsAsFactors = FALSE)
typ <- read.csv(file.path(ROOT, "output", "web_dwell_by_type.csv"), stringsAsFactors = FALSE)

# ---- Plot 1: VAI per AOI, faceted by day ----
png(file.path(FIG, "fig1_vai_by_aoi.png"), width = 1400, height = 1600, res = 150)
par(mfrow = c(3, 1), mar = c(4, 14, 3, 2))
for (d in c("Day 1", "Day 2", "Day 3")) {
  sub <- web[web$day == d, ]
  sub <- sub[order(sub$vai_pct), ]
  barplot(
    sub$vai_pct,
    names.arg = sub$aoi,
    horiz = TRUE, las = 1,
    col = "#3b7dd8", border = NA,
    xlim = c(0, 100), xlab = "Visual Attention Index (%)",
    main = paste0(d, "  (N=", unique(sub$n_participants), ")"),
    cex.names = 0.85
  )
  abline(v = mean(sub$vai_pct), lty = 2, col = "darkred")
}
dev.off()

# ---- Plot 2: Dwell time (% of session) per AOI, faceted by day ----
png(file.path(FIG, "fig2_dwell_pct_by_aoi.png"), width = 1400, height = 1600, res = 150)
par(mfrow = c(3, 1), mar = c(4, 14, 3, 2))
for (d in c("Day 1", "Day 2", "Day 3")) {
  sub <- web[web$day == d, ]
  sub <- sub[order(sub$dwell_pct_of_session), ]
  barplot(
    sub$dwell_pct_of_session,
    names.arg = sub$aoi,
    horiz = TRUE, las = 1,
    col = "#3aa05a", border = NA,
    xlim = c(0, 50), xlab = "Dwell time (% of session)",
    main = paste0(d, "  —  session = ", round(unique(sub$session_len_s)/60, 1), " min"),
    cex.names = 0.85
  )
}
dev.off()

# ---- Plot 3: Dwell share by AOI type (aggregated across days) ----
png(file.path(FIG, "fig3_dwell_by_type.png"), width = 1100, height = 700, res = 150)
par(mar = c(4, 12, 3, 2))
typ <- typ[order(typ$pct_total_dwell), ]
barplot(
  typ$pct_total_dwell,
  names.arg = typ$aoi_type,
  horiz = TRUE, las = 1,
  col = "#d97b3b", border = NA,
  xlim = c(0, 40), xlab = "Share of total task-relevant gaze time (%)",
  main = "Where attention went: AOI type (Web, Days 1-3)",
  cex.names = 1
)
dev.off()

# ---- Plot 4: Time-to-first-fixation per AOI, faceted by day ----
png(file.path(FIG, "fig4_ttff_by_aoi.png"), width = 1400, height = 1600, res = 150)
par(mfrow = c(3, 1), mar = c(4, 14, 3, 2))
for (d in c("Day 1", "Day 2", "Day 3")) {
  sub <- web[web$day == d, ]
  sub <- sub[order(-sub$ttff_s), ]
  barplot(
    sub$ttff_s,
    names.arg = sub$aoi,
    horiz = TRUE, las = 1,
    col = "#7a4ec9", border = NA,
    xlab = "Average time to first fixation (s)",
    main = paste0(d, " — lower = student looked here earlier"),
    cex.names = 0.85
  )
}
dev.off()

cat("Plots saved to: ", FIG, "\n")
list.files(FIG)
