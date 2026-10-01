# ============================================================
# Unified Web + Document Gaze Analysis Pipeline
# SciTrek Bioinformatics — Immersive vs. Document-Based study
# ============================================================
# Loads both conditions into one long table, classifies AOIs into shared
# canonical categories, aggregates doc sub-AOIs to category level, and
# produces side-by-side comparison tables.

script_dir <- function(default) {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg)) return(dirname(normalizePath(sub("^--file=", "", file_arg[1]))))
  dirname(normalizePath(default))
}
ROOT <- normalizePath(file.path(script_dir("R/03_unified_pipeline.R"), ".."))
OUT  <- file.path(ROOT, "output")
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

# ---------- helpers ----------
parse_pct <- function(x) suppressWarnings(as.numeric(sub("%", "", x)))
parse_sec <- function(x) {
  x <- as.character(x); x[x == "N/A"] <- NA
  suppressWarnings(as.numeric(sub("s$", "", x)))
}
parse_range <- function(rng) {
  m <- regmatches(rng, gregexpr("[0-9]+\\.?[0-9]*", rng))
  do.call(rbind, lapply(m, function(r) {
    if (length(r) >= 2) c(as.numeric(r[1]), as.numeric(r[2])) else c(NA, NA)
  }))
}

# Strip a "Day N - " prefix; rows without it default to Day 1
parse_day <- function(name) {
  m <- regmatches(name, regexec("^Day\\s*([0-9])\\s*-\\s*", name))
  sapply(m, function(r) if (length(r) >= 2) paste0("Day ", r[2]) else "Day 1")
}
strip_day <- function(name) sub("^Day\\s*[0-9]\\s*-\\s*", "", name)

# Canonical category mapping (case-insensitive substring match).
# Order matters — earlier rules override later.
#  1. Reflection/discussion first (some include "exercise"/"activity")
#  2. Workbook → Exercise (overrides PhET match for "PhET Workbook")
#  3. Videos
#  4. Interactive simulations require explicit "simulation" or "phet"
#     (a doc page titled "p53 Gene & Cancer" is text, not a sim;
#      "p53 Gene & Cancer Simulation" is a sim)
#  5. Other interactive exercises
#  6. Static text (catches p53/cell-cycle text pages with no sim/video/ex marker)
classify_aoi <- function(name) {
  n <- tolower(name)
  if (grepl("inquiry|discussion|wrap up|reflection", n)) return("Reflection / Discussion")
  if (grepl("workbook", n)) return("Interactive Exercise")
  if (grepl("amoeba|watch learn|video|cancer detection explainer", n)) return("Video")
  if (grepl("phet|simulation", n)) return("Interactive Simulation")
  if (grepl("activity|exercise|gene cards|patient cards|gene detective|hypothesis activity|gene expression.*regulation|gene expression signals", n)) return("Interactive Exercise")
  if (grepl("objective|core concepts|daily plan|detection and diagnosis|case spark|p53 gene|what is cancer|cell cycle", n)) return("Static Text")
  "Other"
}

# ---------- loader ----------
load_csv <- function(path, condition, day_in_name = FALSE, default_day = "Day 1") {
  d <- read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  rng <- parse_range(d$AOITimeRange)
  if (day_in_name) {
    day <- parse_day(d$Name)
    aoi <- strip_day(d$Name)
  } else {
    day <- rep(default_day, nrow(d))
    aoi <- d$Name
  }
  data.frame(
    condition       = condition,
    day             = day,
    aoi_raw         = d$Name,
    aoi             = aoi,
    category        = sapply(aoi, classify_aoi),
    vai_pct         = parse_pct(d$VAI),
    vai_reach_pct   = parse_pct(d$VAIReach),
    vai_hold_pct    = parse_pct(d$VAIHold),
    ttff_s          = parse_sec(d$AvgTTFF),
    rel_ttff_s      = parse_sec(d$AvgRelativeTTFF),
    gaze_time_s     = parse_sec(d$GazesAvgTimeSpent),
    fixation_time_s = parse_sec(d$FixationsAvgTimeSpent),
    fixations       = suppressWarnings(as.numeric(d$Fixations)),
    gazes           = suppressWarnings(as.numeric(d$Gazes)),
    avg_fix_dur_s   = parse_sec(d$AvgFixationDuration),
    avg_ffd_s       = parse_sec(d$AvgFFD),
    k_coefficient   = suppressWarnings(as.numeric(d$Kcoefficient)),
    avg_revisits    = suppressWarnings(as.numeric(d$AvgRevisits)),
    clicks          = suppressWarnings(as.numeric(d$Clicks)),
    range_start_s   = rng[, 1],
    range_end_s     = rng[, 2],
    stringsAsFactors = FALSE
  )
}

# ---------- load all data ----------
w1 <- load_csv(file.path(ROOT, "web/day_1/web_day_1_realeye_aoi_metrics.csv"), "Web", default_day = "Day 1")
w2 <- load_csv(file.path(ROOT, "web/day_2/web_day_2_realeye_aoi_metrics.csv"), "Web", default_day = "Day 2")
w3 <- load_csv(file.path(ROOT, "web/day_3/web_day_3_realeye_aoi_metrics.csv"), "Web", default_day = "Day 3")
dc <- load_csv(file.path(ROOT, "doc/document_realeye_aoi_metrics.csv"),       "Document", day_in_name = TRUE)

all <- rbind(w1, w2, w3, dc)

# N participants per (condition, day) — inferred from web manually, doc from data
n_table <- data.frame(
  condition = c("Web","Web","Web","Document","Document","Document"),
  day       = c("Day 1","Day 2","Day 3","Day 1","Day 2","Day 3"),
  n         = c(3, 2, 3, 6, 6, 6)
)
all <- merge(all, n_table, by = c("condition","day"), all.x = TRUE)

# Session length per (condition, day) — max end-time
session_len <- aggregate(range_end_s ~ condition + day, data = all,
                         FUN = function(x) max(x, na.rm = TRUE))
names(session_len)[3] <- "session_len_s"
all <- merge(all, session_len, by = c("condition","day"), all.x = TRUE)

# ---------- save the unified long table ----------
write.csv(all, file.path(OUT, "unified_aoi_long.csv"), row.names = FALSE)

# ---------- category-level rollup per (condition, day) ----------
roll <- aggregate(
  cbind(gaze_time_s, fixation_time_s, fixations, gazes, avg_revisits, clicks) ~
    condition + day + category,
  data = all, FUN = function(x) sum(x, na.rm = TRUE)
)
# Means need separate aggregation (since aggregate with FUN=sum above)
mean_metrics <- aggregate(
  cbind(vai_pct, vai_hold_pct, avg_fix_dur_s, avg_ffd_s, k_coefficient) ~
    condition + day + category,
  data = all, FUN = function(x) mean(x, na.rm = TRUE)
)
min_ttff <- aggregate(ttff_s ~ condition + day + category, data = all,
                      FUN = function(x) min(x, na.rm = TRUE))
names(min_ttff)[4] <- "min_ttff_s"
n_sub <- aggregate(aoi ~ condition + day + category, data = all, FUN = length)
names(n_sub)[4] <- "n_sub_aois"

cat_day <- Reduce(function(a, b) merge(a, b, by = c("condition","day","category"), all = TRUE),
                  list(roll, mean_metrics, min_ttff, n_sub))

# Add session length and N
cat_day <- merge(cat_day, session_len, by = c("condition","day"), all.x = TRUE)
cat_day <- merge(cat_day, n_table,    by = c("condition","day"), all.x = TRUE)

# Two dwell metrics:
#  - dwell_pct_session: gaze_time / session_len (can exceed 100% due to overlapping sub-AOIs)
#  - share_of_measured: gaze_time / sum_of_all_category_gaze_time (within condition×day)
cat_day$dwell_pct_session <- 100 * cat_day$gaze_time_s / cat_day$session_len_s
tot_meas <- aggregate(gaze_time_s ~ condition + day, data = cat_day, FUN = function(x) sum(x, na.rm = TRUE))
names(tot_meas)[3] <- "total_measured_gaze_s"
cat_day <- merge(cat_day, tot_meas, by = c("condition","day"), all.x = TRUE)
cat_day$share_of_measured_pct <- 100 * cat_day$gaze_time_s / cat_day$total_measured_gaze_s

write.csv(cat_day, file.path(OUT, "category_day_summary.csv"), row.names = FALSE)

# ---------- overall by condition (collapsed across days) ----------
overall <- aggregate(
  cbind(gaze_time_s, fixations, clicks) ~ condition + category,
  data = cat_day, FUN = sum
)
tot_cond <- aggregate(gaze_time_s ~ condition, data = overall, FUN = sum)
names(tot_cond)[2] <- "cond_total_gaze_s"
overall <- merge(overall, tot_cond, by = "condition")
overall$share_pct <- round(100 * overall$gaze_time_s / overall$cond_total_gaze_s, 1)

# Pivot to wide for the headline comparison
wide <- reshape(overall[, c("condition","category","share_pct")],
                idvar = "category", timevar = "condition", direction = "wide")
names(wide) <- sub("share_pct\\.", "", names(wide))
wide$delta_immersive_minus_doc <- wide$Web - wide$Document

# Order categories for display
cat_order <- c("Interactive Simulation","Video","Interactive Exercise",
               "Reflection / Discussion","Static Text","Other")
wide <- wide[match(cat_order, wide$category), ]
wide <- wide[!is.na(wide$category), ]

# NOTE: this pre-dedup headline double-counts doc sub-AOIs. Use
# dedup_headline_dwell.csv (from script 04) for the panel.
# Output suppressed to avoid confusion.
# write.csv(wide, file.path(OUT, "headline_condition_comparison.csv"), row.names = FALSE)

# ---------- console report ----------
cat("============================================================\n")
cat(" UNIFIED PIPELINE — Web vs Document\n")
cat("============================================================\n\n")

cat("DATA INVENTORY\n")
inv <- aggregate(aoi ~ condition + day, data = all, FUN = length)
names(inv)[3] <- "n_aoi_rows"
inv <- merge(inv, n_table, by = c("condition","day"))
inv <- merge(inv, session_len, by = c("condition","day"))
inv$session_min <- round(inv$session_len_s / 60, 1)
print(inv[order(inv$condition, inv$day), c("condition","day","n","n_aoi_rows","session_min")], row.names = FALSE)
cat("\n")

cat("HEADLINE — Share of Measured Attention by Category (across all days)\n")
cat("--- numbers are % of total task-relevant gaze time in that condition ---\n")
hd <- wide
hd$Web      <- round(hd$Web, 1)
hd$Document <- round(hd$Document, 1)
hd$delta_immersive_minus_doc <- round(hd$delta_immersive_minus_doc, 1)
print(hd, row.names = FALSE)
cat("\n")

cat("INTERACTIVE-vs-STATIC summary\n")
interactive_cats <- c("Interactive Simulation", "Interactive Exercise", "Video")
static_cats      <- c("Static Text")
ix <- aggregate(share_pct ~ condition, data = overall[overall$category %in% interactive_cats, ], FUN = sum)
st <- aggregate(share_pct ~ condition, data = overall[overall$category %in% static_cats, ],      FUN = sum)
names(ix)[2] <- "interactive_pct"; names(st)[2] <- "static_pct"
ix_st <- merge(ix, st, by = "condition")
ix_st$interactive_pct <- round(ix_st$interactive_pct, 1)
ix_st$static_pct      <- round(ix_st$static_pct, 1)
print(ix_st, row.names = FALSE)
cat("\n")

cat("PER-DAY × CATEGORY (share_of_measured_pct)\n")
view <- cat_day[, c("condition","day","category","share_of_measured_pct","n_sub_aois")]
view$share_of_measured_pct <- round(view$share_of_measured_pct, 1)
view <- view[order(view$day, view$condition, -view$share_of_measured_pct), ]
print(view, row.names = FALSE)
cat("\n")

cat("Outputs written:\n")
cat("  ", file.path(OUT, "unified_aoi_long.csv"), "\n")
cat("  ", file.path(OUT, "category_day_summary.csv"), "\n")
