# ============================================================
# Efficiency & Engagement-Quality Analysis
# Pulls metrics we have but hadn't surfaced yet:
#   - Session duration per condition×day (time to complete)
#   - K-coefficient (focal vs ambient processing)
#   - Click rate (clicks per minute)
#   - AvgRevisits (re-engagement / confusion proxy)
#   - VAI Reach (coverage — did everyone see it?)
#   - TTFF on Web only (orientation speed; suppressed for Doc)
#   - Per-session quiz trajectory
# ============================================================
script_dir <- function(default) {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg)) return(dirname(normalizePath(sub("^--file=", "", file_arg[1]))))
  dirname(normalizePath(default))
}
ROOT <- normalizePath(file.path(script_dir("R/06_efficiency_analysis.R"), ".."))
OUT  <- file.path(ROOT, "output")

all   <- read.csv(file.path(OUT, "unified_aoi_long.csv"), stringsAsFactors = FALSE)
dedup <- read.csv(file.path(OUT, "dedup_parent_long.csv"), stringsAsFactors = FALSE)
ascond <- read.csv(file.path(OUT, "assessment_condition_means.csv"), stringsAsFactors = FALSE)

cat("============================================================\n")
cat(" EFFICIENCY & ENGAGEMENT-QUALITY ANALYSIS\n")
cat("============================================================\n\n")

# ---------- 1. Session duration: how long did each day take? ----------
cat("1) SESSION DURATION (minutes) — same curriculum, how fast did they finish?\n")
sess <- aggregate(range_end_s ~ condition + day, data = all,
                  FUN = function(x) max(x, na.rm = TRUE))
sess$minutes <- round(sess$range_end_s / 60, 1)
sess <- sess[order(sess$day, sess$condition), c("condition","day","minutes")]
print(sess, row.names = FALSE)
sess_wide <- reshape(sess, idvar = "day", timevar = "condition", direction = "wide")
names(sess_wide) <- sub("minutes\\.", "", names(sess_wide))
sess_wide$doc_minus_web_min <- round(sess_wide$Document - sess_wide$Web, 1)
cat("\nDoc took LONGER than Web by:\n")
print(sess_wide, row.names = FALSE)
cat("\n")

# ---------- 2. K-coefficient: focal vs ambient ----------
# K > 0 = focal/exploitative (deep processing on few items)
# K < 0 = ambient/exploratory (scanning many items)
cat("2) K-COEFFICIENT (focal=positive, ambient=negative)\n")
k_cond <- aggregate(k_coefficient ~ condition + category, data = dedup,
                    FUN = function(x) round(mean(x, na.rm = TRUE), 3))
k_wide <- reshape(k_cond, idvar = "category", timevar = "condition", direction = "wide")
names(k_wide) <- sub("k_coefficient\\.", "", names(k_wide))
print(k_wide, row.names = FALSE)
cat("(positive = more focal/deeper processing; negative = more ambient/scanning)\n\n")

# ---------- 3. Click rate (clicks per minute on interactive content) ----------
cat("3) CLICKS PER MINUTE on Interactive Sim + Exercise + Video\n")
int_cats <- c("Interactive Simulation","Interactive Exercise","Video")
clicks_cd <- aggregate(clicks ~ condition + day,
                       data = dedup[dedup$category %in% int_cats, ],
                       FUN = function(x) sum(x, na.rm = TRUE))
clicks_cd <- merge(clicks_cd, sess, by = c("condition","day"))
clicks_cd$clicks_per_min <- round(clicks_cd$clicks / clicks_cd$minutes, 1)
print(clicks_cd[, c("condition","day","clicks","minutes","clicks_per_min")], row.names = FALSE)
cat("\n")

# ---------- 4. AvgRevisits: did students keep coming back? ----------
cat("4) AVG REVISITS per AOI (return behavior)\n")
rev_cond <- aggregate(avg_revisits ~ condition + category, data = dedup,
                      FUN = function(x) round(mean(x, na.rm = TRUE), 1))
rev_wide <- reshape(rev_cond, idvar = "category", timevar = "condition", direction = "wide")
names(rev_wide) <- sub("avg_revisits\\.", "", names(rev_wide))
print(rev_wide, row.names = FALSE)
cat("(higher revisits = students came back more often)\n\n")

# ---------- 5. VAI Reach: did the AOI get seen by everyone? ----------
cat("5) VAI REACH (% of viewers who looked at the AOI) — averaged by category\n")
reach <- aggregate(vai_reach_pct ~ condition + category, data = dedup,
                   FUN = function(x) round(mean(x, na.rm = TRUE), 1))
reach_wide <- reshape(reach, idvar = "category", timevar = "condition", direction = "wide")
names(reach_wide) <- sub("vai_reach_pct\\.", "", names(reach_wide))
print(reach_wide, row.names = FALSE)
cat("\n")

# ---------- 6. TTFF on Web only (Doc suppressed - scroll dominates) ----------
cat("6) TTFF on Web only (sec to first fixation — lower = faster orientation)\n")
ttff_web <- aggregate(ttff_s ~ category + day,
                      data = dedup[dedup$condition == "Web", ],
                      FUN = function(x) round(mean(x, na.rm = TRUE), 1))
print(ttff_web[order(ttff_web$day, ttff_web$ttff_s), ], row.names = FALSE)
cat("\n")

# ---------- 7. Session quiz trajectory ----------
cat("7) SESSION QUIZ TRAJECTORY by condition (% correct)\n")
sess_q <- ascond[ascond$assessment %in% c("Session 1","Session 2","Session 3"), ]
print(sess_q[order(sess_q$assessment, sess_q$condition), ], row.names = FALSE)
cat("\n")

# ---------- 8. EFFICIENCY HEADLINE: time per learning ----------
cat("8) EFFICIENCY HEADLINE — Same post-test (100%/100%), how long?\n")
total_time <- aggregate(range_end_s ~ condition, data = all,
                        FUN = function(x) max(x, na.rm = TRUE))
total_time$total_min <- round(total_time$range_end_s / 60, 1)
# sum across days
day_sums <- aggregate(minutes ~ condition, data = sess, FUN = sum)
print(day_sums, row.names = FALSE)
cat("\n")

# ---------- 9. Fixation density (focused looking per minute) ----------
cat("9) FIXATIONS PER MINUTE on interactive content (engagement intensity)\n")
fix_cd <- aggregate(fixations ~ condition + day,
                    data = dedup[dedup$category %in% int_cats, ],
                    FUN = function(x) sum(x, na.rm = TRUE))
fix_cd <- merge(fix_cd, sess, by = c("condition","day"))
fix_cd$fix_per_min <- round(fix_cd$fixations / fix_cd$minutes, 0)
print(fix_cd[, c("condition","day","fixations","minutes","fix_per_min")], row.names = FALSE)
cat("\n")

# ---------- write summary CSV ----------
eff_summary <- data.frame(
  metric = c(
    "Total session time across 3 days (min)",
    "Mean K-coefficient on interactives (closer to 0 = more focal)",
    "Clicks per minute on interactives",
    "Mean revisits per AOI",
    "VAI Reach (% viewers reaching AOI)",
    "Fixations per minute on interactives"
  )
)
write.csv(eff_summary, file.path(OUT, "efficiency_metrics_index.csv"), row.names = FALSE)
write.csv(sess_wide,  file.path(OUT, "session_duration_by_day.csv"), row.names = FALSE)
write.csv(k_wide,     file.path(OUT, "k_coefficient_by_category.csv"), row.names = FALSE)
write.csv(rev_wide,   file.path(OUT, "revisits_by_category.csv"), row.names = FALSE)
write.csv(reach_wide, file.path(OUT, "vai_reach_by_category.csv"), row.names = FALSE)

cat("Outputs:\n")
cat("  ", file.path(OUT, "session_duration_by_day.csv"), "\n")
cat("  ", file.path(OUT, "k_coefficient_by_category.csv"), "\n")
cat("  ", file.path(OUT, "revisits_by_category.csv"), "\n")
cat("  ", file.path(OUT, "vai_reach_by_category.csv"), "\n")
