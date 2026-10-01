# ============================================================
# Web Condition Gaze Analysis — Days 1, 2, 3
# SciTrek Bioinformatics Eye-Tracking Pilot
# ============================================================
# Reads RealEye AOI-level exports for the immersive (Web) condition
# and computes descriptive engagement metrics for the panel.

script_dir <- function(default) {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg)) return(dirname(normalizePath(sub("^--file=", "", file_arg[1]))))
  dirname(normalizePath(default))
}
ROOT <- normalizePath(file.path(script_dir("R/01_web_gaze_analysis.R"), ".."))
OUT  <- file.path(ROOT, "output")
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

# ---------- helpers ----------
parse_pct <- function(x) as.numeric(sub("%", "", x))
parse_sec <- function(x) {
  x <- as.character(x)
  x[x == "N/A"] <- NA
  as.numeric(sub("s$", "", x))
}
parse_session_len <- function(rng) {
  nums <- regmatches(rng, gregexpr("[0-9]+\\.?[0-9]*", rng))[[1]]
  if (length(nums) >= 2) as.numeric(nums[2]) - as.numeric(nums[1]) else NA
}

load_day <- function(path, day_label, n_participants) {
  d <- read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  data.frame(
    day              = day_label,
    n_participants   = n_participants,
    aoi              = d$Name,
    vai_pct          = parse_pct(d$VAI),
    vai_hold_pct     = parse_pct(d$VAIHold),
    ttff_s           = parse_sec(d$AvgTTFF),
    rel_ttff_s       = parse_sec(d$AvgRelativeTTFF),
    fixations        = d$Fixations,
    gazes            = d$Gazes,
    gaze_time_s      = parse_sec(d$GazesAvgTimeSpent),
    fixation_time_s  = parse_sec(d$FixationsAvgTimeSpent),
    avg_fix_dur_s    = parse_sec(d$AvgFixationDuration),
    k_coefficient    = as.numeric(d$Kcoefficient),
    avg_revisits     = as.numeric(d$AvgRevisits),
    clicks           = as.numeric(d$Clicks),
    session_len_s    = parse_session_len(d$AOITimeRange[1]),
    stringsAsFactors = FALSE
  )
}

# ---------- load ----------
d1 <- load_day(file.path(ROOT, "web/day_1/web_day_1_realeye_aoi_metrics.csv"), "Day 1", 3)
d2 <- load_day(file.path(ROOT, "web/day_2/web_day_2_realeye_aoi_metrics.csv"), "Day 2", 2)
d3 <- load_day(file.path(ROOT, "web/day_3/web_day_3_realeye_aoi_metrics.csv"), "Day 3", 3)

web <- rbind(d1, d2, d3)
web$dwell_pct_of_session <- 100 * web$gaze_time_s / web$session_len_s

# ---------- per-day summary ----------
# NOTE: RealEye AOIs can overlap (parent AOIs contain child AOIs), so summing
# dwell across AOIs can exceed session length. We report per-AOI dwell-of-session
# (bounded 0-100%) and use the MAX single-AOI dwell as the cleanest "deepest
# engagement" anchor. We also report mean VAI across AOIs (well-defined).
per_day <- do.call(rbind, lapply(split(web, web$day), function(x) {
  data.frame(
    day                       = unique(x$day),
    n_participants            = unique(x$n_participants),
    session_len_min           = round(unique(x$session_len_s) / 60, 1),
    n_aois                    = nrow(x),
    mean_vai_pct              = round(mean(x$vai_pct, na.rm = TRUE), 1),
    mean_aoi_dwell_pct        = round(mean(x$dwell_pct_of_session, na.rm = TRUE), 1),
    median_aoi_dwell_pct      = round(median(x$dwell_pct_of_session, na.rm = TRUE), 1),
    mean_ttff_s               = round(mean(x$ttff_s, na.rm = TRUE), 1),
    total_fixations           = sum(x$fixations, na.rm = TRUE),
    top_aoi                   = x$aoi[which.max(x$gaze_time_s)],
    top_aoi_dwell_pct         = round(100 * max(x$gaze_time_s, na.rm = TRUE) / unique(x$session_len_s), 1),
    stringsAsFactors = FALSE
  )
}))

# ---------- AOI-type categorization (web condition) ----------
# Heuristic: name patterns -> instructional type
classify_aoi <- function(name) {
  n <- tolower(name)
  if (grepl("simulat|phet|case spark", n))           return("Interactive Simulation")
  if (grepl("video|watch|amoeba|explainer", n))      return("Video")
  if (grepl("exercise|activity|hypothesis|gene cards|patient cards", n)) return("Interactive Exercise")
  if (grepl("inquiry|discussion|wrap up|reflection", n)) return("Reflection / Discussion")
  if (grepl("objective|core concepts|daily plan|detection and diagnosis", n)) return("Static Text")
  "Other"
}
web$aoi_type <- sapply(web$aoi, classify_aoi)

by_type <- aggregate(
  cbind(gaze_time_s, vai_pct) ~ aoi_type,
  data = web, FUN = function(x) round(mean(x, na.rm = TRUE), 1)
)
type_totals <- aggregate(gaze_time_s ~ aoi_type, data = web, FUN = sum)
type_totals$pct_total_dwell <- round(100 * type_totals$gaze_time_s / sum(type_totals$gaze_time_s), 1)

# ---------- headline numbers ----------
overall_vai          <- round(mean(web$vai_pct, na.rm = TRUE), 1)
mean_aoi_dwell_pct   <- round(mean(web$dwell_pct_of_session, na.rm = TRUE), 1)
total_fixations      <- sum(web$fixations, na.rm = TRUE)
mean_ttff            <- round(mean(web$ttff_s, na.rm = TRUE), 1)
median_dwell_pct     <- round(median(web$dwell_pct_of_session, na.rm = TRUE), 1)

# ---------- write outputs ----------
write.csv(web,        file.path(OUT, "web_aoi_long.csv"),     row.names = FALSE)
write.csv(per_day,    file.path(OUT, "web_per_day_summary.csv"), row.names = FALSE)
write.csv(type_totals,file.path(OUT, "web_dwell_by_type.csv"), row.names = FALSE)

# ---------- console report ----------
cat("============================================================\n")
cat(" WEB CONDITION — DAYS 1-3 — SUMMARY\n")
cat("============================================================\n\n")
cat("HEADLINE METRICS (Days 1-3)\n")
cat("  Mean Visual Attention Index across AOIs:      ", overall_vai,     "%\n", sep = "")
cat("  Mean per-AOI dwell as % of session:           ", mean_aoi_dwell_pct, "%\n", sep = "")
cat("  Median per-AOI dwell as % of session:         ", median_dwell_pct,  "%\n", sep = "")
cat("  Mean Time-to-First-Fixation across AOIs:      ", mean_ttff, " s\n", sep = "")
cat("  Total fixations recorded across 3 days:       ", total_fixations, "\n\n", sep = "")
cat("  NOTE: RealEye AOIs can overlap (parent/child regions), so a simple\n")
cat("        'sum of dwell / session' can exceed 100%. We report bounded\n")
cat("        per-AOI metrics instead.\n\n")

cat("PER-DAY SUMMARY\n")
print(per_day, row.names = FALSE)
cat("\n")

cat("DWELL BY AOI TYPE (aggregated across days)\n")
print(type_totals[order(-type_totals$pct_total_dwell), ], row.names = FALSE)
cat("\n")

cat("TOP 5 AOIs BY DWELL TIME (across days)\n")
top5 <- web[order(-web$gaze_time_s), c("day","aoi","gaze_time_s","vai_pct","dwell_pct_of_session")][1:5, ]
top5$gaze_time_s <- round(top5$gaze_time_s, 1)
top5$dwell_pct_of_session <- round(top5$dwell_pct_of_session, 1)
print(top5, row.names = FALSE)
cat("\n")

cat("BOTTOM 5 AOIs BY DWELL TIME\n")
bot5 <- web[order(web$gaze_time_s), c("day","aoi","gaze_time_s","vai_pct","dwell_pct_of_session")][1:5, ]
bot5$gaze_time_s <- round(bot5$gaze_time_s, 1)
bot5$dwell_pct_of_session <- round(bot5$dwell_pct_of_session, 1)
print(bot5, row.names = FALSE)

cat("\nWritten:\n")
cat("  ", file.path(OUT, "web_aoi_long.csv"), "\n")
cat("  ", file.path(OUT, "web_per_day_summary.csv"), "\n")
cat("  ", file.path(OUT, "web_dwell_by_type.csv"), "\n")
