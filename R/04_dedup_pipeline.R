# ============================================================
# Deduplicated Unified Pipeline — Option 2 + C
# Collapses doc sub-AOIs to parent instructional elements,
# averages across duplicates, then aggregates to category.
# Produces BOTH dwell-share and VAI-based comparisons.
# ============================================================
script_dir <- function(default) {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg)) return(dirname(normalizePath(sub("^--file=", "", file_arg[1]))))
  dirname(normalizePath(default))
}
ROOT <- normalizePath(file.path(script_dir("R/04_dedup_pipeline.R"), ".."))
OUT  <- file.path(ROOT, "output")

# Reuse the unified long table from script 03
all <- read.csv(file.path(OUT, "unified_aoi_long.csv"), stringsAsFactors = FALSE)

# ---------- parent extraction ----------
# Rules (applied to AOI name that has already had the "Day N - " prefix stripped):
#   1. Split on " - " — keep first piece (the parent name)
#   2. Strip trailing space+digit(s) "2", "3", etc.
#   3. Normalize PHET / PhET / phet to "PhET" for matching
#   4. Trim whitespace
extract_parent <- function(name) {
  p <- strsplit(name, " - ", fixed = TRUE)[[1]][1]
  p <- sub("\\s+\\d+\\s*$", "", p)        # drop trailing "2", "3", "2 2"
  p <- gsub("^\\s+|\\s+$", "", p)
  # case-normalize PhET variants
  p <- gsub("(?i)phet", "PhET", p, perl = TRUE)
  # normalize "&" → "and" so "p53 Gene & Cancer" collapses with
  # "p53 Gene and Cancer" (RealEye exported both)
  p <- gsub("\\s*&\\s*", " and ", p)
  p <- gsub("\\s+", " ", p)
  p
}
all$parent <- sapply(all$aoi, extract_parent)

# ---------- dedup: one row per (condition, day, parent) ----------
# All numeric metrics averaged across duplicate sub-rows of the same parent.
num_cols <- c("vai_pct","vai_reach_pct","vai_hold_pct",
              "ttff_s","rel_ttff_s","gaze_time_s","fixation_time_s",
              "fixations","gazes","avg_fix_dur_s","avg_ffd_s",
              "k_coefficient","avg_revisits","clicks")

dedup <- aggregate(
  all[, num_cols],
  by = list(condition = all$condition, day = all$day,
            category = all$category, parent = all$parent),
  FUN = function(x) mean(x, na.rm = TRUE)
)

# Count of sub-AOIs per parent (transparency)
n_sub <- aggregate(aoi ~ condition + day + category + parent, data = all, FUN = length)
names(n_sub)[5] <- "n_sub_aois"
dedup <- merge(dedup, n_sub, by = c("condition","day","category","parent"))

# Reattach N participants and session length
n_table <- data.frame(
  condition = c("Web","Web","Web","Document","Document","Document"),
  day       = c("Day 1","Day 2","Day 3","Day 1","Day 2","Day 3"),
  n         = c(3, 2, 3, 6, 6, 6)
)
dedup <- merge(dedup, n_table, by = c("condition","day"), all.x = TRUE)

session_len <- aggregate(range_end_s ~ condition + day, data = all,
                         FUN = function(x) max(x, na.rm = TRUE))
names(session_len)[3] <- "session_len_s"
dedup <- merge(dedup, session_len, by = c("condition","day"), all.x = TRUE)

write.csv(dedup, file.path(OUT, "dedup_parent_long.csv"), row.names = FALSE)

# ---------- aggregate parents to category per (condition, day) ----------
# Dwell: SUM across parents within category (each parent is now one element)
# VAI:   MEAN across parents
cat_sum <- aggregate(
  cbind(gaze_time_s, fixation_time_s, fixations, gazes, avg_revisits, clicks) ~
    condition + day + category,
  data = dedup, FUN = function(x) sum(x, na.rm = TRUE)
)
cat_mean <- aggregate(
  cbind(vai_pct, vai_reach_pct, vai_hold_pct, ttff_s, avg_fix_dur_s, k_coefficient) ~
    condition + day + category,
  data = dedup, FUN = function(x) mean(x, na.rm = TRUE)
)
# TTFF / rel_TTFF are not meaningful for doc condition (depend on student
# scroll position, not on attention or interface design). Suppress for doc.
cat_mean$ttff_s[cat_mean$condition == "Document"] <- NA
dedup$ttff_s[dedup$condition == "Document"]      <- NA
dedup$rel_ttff_s[dedup$condition == "Document"]  <- NA
cat_day <- merge(cat_sum, cat_mean, by = c("condition","day","category"), all = TRUE)
cat_day <- merge(cat_day, session_len, by = c("condition","day"), all.x = TRUE)
cat_day <- merge(cat_day, n_table,    by = c("condition","day"), all.x = TRUE)

# share-of-measured within condition×day
tot <- aggregate(gaze_time_s ~ condition + day, data = cat_day,
                 FUN = function(x) sum(x, na.rm = TRUE))
names(tot)[3] <- "total_measured_gaze_s"
cat_day <- merge(cat_day, tot, by = c("condition","day"), all.x = TRUE)
cat_day$share_of_measured_pct <- 100 * cat_day$gaze_time_s / cat_day$total_measured_gaze_s
cat_day$dwell_pct_session     <- 100 * cat_day$gaze_time_s / cat_day$session_len_s

write.csv(cat_day, file.path(OUT, "dedup_category_day.csv"), row.names = FALSE)

# ---------- overall by condition × category ----------
overall_dwell <- aggregate(gaze_time_s ~ condition + category, data = cat_day, FUN = sum)
tot_cond <- aggregate(gaze_time_s ~ condition, data = overall_dwell, FUN = sum)
names(tot_cond)[2] <- "cond_total_gaze_s"
overall_dwell <- merge(overall_dwell, tot_cond, by = "condition")
overall_dwell$share_pct <- round(100 * overall_dwell$gaze_time_s / overall_dwell$cond_total_gaze_s, 1)

# Exclude "Other" from the headline VAI table — Doc had an "Other" category but
# Web did not, which makes the scalar mean across categories non-comparable.
overall_vai <- aggregate(vai_pct ~ condition + category,
                         data = cat_day[cat_day$category != "Other", ],
                         FUN = mean)
overall_vai$vai_pct <- round(overall_vai$vai_pct, 1)

# Wide pivots
wide_dwell <- reshape(overall_dwell[, c("condition","category","share_pct")],
                     idvar = "category", timevar = "condition", direction = "wide")
names(wide_dwell) <- sub("share_pct\\.", "", names(wide_dwell))
wide_dwell$delta_web_minus_doc <- wide_dwell$Web - wide_dwell$Document

wide_vai <- reshape(overall_vai, idvar = "category", timevar = "condition", direction = "wide")
names(wide_vai) <- sub("vai_pct\\.", "", names(wide_vai))
wide_vai$delta_web_minus_doc <- wide_vai$Web - wide_vai$Document

cat_order <- c("Interactive Simulation","Video","Interactive Exercise",
               "Reflection / Discussion","Static Text","Other")
wide_dwell <- wide_dwell[match(cat_order, wide_dwell$category), ]
wide_dwell <- wide_dwell[!is.na(wide_dwell$category), ]
wide_vai   <- wide_vai[match(cat_order, wide_vai$category), ]
wide_vai   <- wide_vai[!is.na(wide_vai$category), ]

write.csv(wide_dwell, file.path(OUT, "dedup_headline_dwell.csv"), row.names = FALSE)
write.csv(wide_vai,   file.path(OUT, "dedup_headline_vai.csv"),   row.names = FALSE)

# ---------- console report ----------
cat("============================================================\n")
cat(" DEDUPLICATED PIPELINE — Web vs Document\n")
cat(" (Option 2: doc sub-AOIs collapsed to parents, averaged.)\n")
cat("============================================================\n\n")

cat("DEDUP STATS\n")
ds <- aggregate(parent ~ condition + day, data = dedup, FUN = length)
names(ds)[3] <- "n_parents"
ds2 <- aggregate(aoi ~ condition + day, data = all, FUN = length)
names(ds2)[3] <- "n_raw_rows"
ds <- merge(ds, ds2, by = c("condition","day"))
ds$collapsed_from <- ds$n_raw_rows
ds$now <- ds$n_parents
print(ds[order(ds$condition, ds$day), c("condition","day","collapsed_from","now")], row.names = FALSE)
cat("\n")

cat("HEADLINE A — Share of Measured Attention by Category (DEDUPED, % of condition total)\n")
hd <- wide_dwell
hd$Web      <- round(hd$Web, 1)
hd$Document <- round(hd$Document, 1)
hd$delta_web_minus_doc <- round(hd$delta_web_minus_doc, 1)
print(hd, row.names = FALSE)
cat("\n")

cat("HEADLINE B — Mean Visual Attention Index (VAI) by Category (% of viewers attending)\n")
hv <- wide_vai
hv$Web      <- round(hv$Web, 1)
hv$Document <- round(hv$Document, 1)
hv$delta_web_minus_doc <- round(hv$delta_web_minus_doc, 1)
print(hv, row.names = FALSE)
cat("\n")

cat("INTERACTIVE vs STATIC (deduped)\n")
ix <- aggregate(share_pct ~ condition,
                data = overall_dwell[overall_dwell$category %in%
                  c("Interactive Simulation","Interactive Exercise","Video"), ],
                FUN = sum)
st <- aggregate(share_pct ~ condition,
                data = overall_dwell[overall_dwell$category == "Static Text", ],
                FUN = sum)
re <- aggregate(share_pct ~ condition,
                data = overall_dwell[overall_dwell$category == "Reflection / Discussion", ],
                FUN = sum)
names(ix)[2] <- "interactive_pct"
names(st)[2] <- "static_pct"
names(re)[2] <- "reflection_pct"
combo <- Reduce(function(a,b) merge(a,b,by="condition"), list(ix, st, re))
combo$interactive_pct <- round(combo$interactive_pct, 1)
combo$static_pct      <- round(combo$static_pct, 1)
combo$reflection_pct  <- round(combo$reflection_pct, 1)
print(combo, row.names = FALSE)
cat("\n")

cat("PER-DAY × CATEGORY (deduped share_of_measured_pct)\n")
view <- cat_day[, c("condition","day","category","share_of_measured_pct","vai_pct","ttff_s")]
view$share_of_measured_pct <- round(view$share_of_measured_pct, 1)
view$vai_pct  <- round(view$vai_pct, 1)
view$ttff_s   <- round(view$ttff_s, 1)
view <- view[order(view$day, view$condition, -view$share_of_measured_pct), ]
print(view, row.names = FALSE)
cat("\n")

cat("Outputs:\n")
cat("  ", file.path(OUT, "dedup_parent_long.csv"), "\n")
cat("  ", file.path(OUT, "dedup_category_day.csv"), "\n")
cat("  ", file.path(OUT, "dedup_headline_dwell.csv"), "\n")
cat("  ", file.path(OUT, "dedup_headline_vai.csv"), "\n")
