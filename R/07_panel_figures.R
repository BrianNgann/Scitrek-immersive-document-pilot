# ============================================================
# Panel figures — clean base-R plots for the Friday presentation.
# Each figure pairs with one headline stat in PANEL_STATS.md.
# ============================================================
script_dir <- function(default) {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg)) return(dirname(normalizePath(sub("^--file=", "", file_arg[1]))))
  dirname(normalizePath(default))
}
ROOT <- normalizePath(file.path(script_dir("R/07_panel_figures.R"), ".."))
OUT  <- file.path(ROOT, "output")
FIG  <- file.path(ROOT, "figures")
dir.create(FIG, showWarnings = FALSE, recursive = TRUE)

dwell  <- read.csv(file.path(OUT, "dedup_headline_dwell.csv"),     stringsAsFactors = FALSE)
vai    <- read.csv(file.path(OUT, "dedup_headline_vai.csv"),       stringsAsFactors = FALSE)
sess   <- read.csv(file.path(OUT, "session_duration_by_day.csv"),  stringsAsFactors = FALSE)
rev    <- read.csv(file.path(OUT, "revisits_by_category.csv"),     stringsAsFactors = FALSE)
reach  <- read.csv(file.path(OUT, "vai_reach_by_category.csv"),    stringsAsFactors = FALSE)
asmt   <- read.csv(file.path(OUT, "assessment_condition_means.csv"), stringsAsFactors = FALSE)

WEB <- "#1f6fb4"; DOC <- "#c0504d"

# --- FIG 1: Time savings (the headline) -----------------------
png(file.path(FIG, "fig1_time_savings.png"), width = 900, height = 600, res = 130)
par(mar = c(5, 5, 4, 2))
totals <- c(Web = sum(sess$Web), Document = sum(sess$Document))
bp <- barplot(totals, col = c(WEB, DOC), border = NA,
              ylim = c(0, max(totals) * 1.15),
              ylab = "Total session time across 3 days (minutes)",
              main = "Same learning outcome, 41% less time")
text(bp, totals + 4, labels = sprintf("%.0f min", totals), font = 2, cex = 1.2)
text(bp, totals / 2, labels = "Post-test: 100%", col = "white", font = 2, cex = 1.0)
mtext("Both conditions reached 100% on the post-module assessment",
      side = 3, line = 0.3, cex = 0.85, col = "grey30")
dev.off()

# --- FIG 2: Time savings per day ------------------------------
png(file.path(FIG, "fig2_time_by_day.png"), width = 900, height = 600, res = 130)
par(mar = c(5, 5, 4, 2))
mat <- t(as.matrix(sess[, c("Web","Document")]))
colnames(mat) <- sess$day
bp <- barplot(mat, beside = TRUE, col = c(WEB, DOC), border = NA,
              ylim = c(0, max(mat) * 1.15),
              ylab = "Session duration (minutes)",
              main = "Web finished faster on every day")
text(bp, mat + 1.5, labels = sprintf("%.1f", mat), cex = 0.85)
legend("topright", c("Web (Immersive)", "Document"),
       fill = c(WEB, DOC), border = NA, bty = "n")
dev.off()

# --- FIG 3: Attention share split by content type -------------
# Group categories: Passive (Sim+Video), Active text (Exercise+Static), Reflection
png(file.path(FIG, "fig3_attention_split.png"), width = 950, height = 600, res = 130)
par(mar = c(5, 5, 4, 2))
group <- function(cats) sum(dwell[dwell$category %in% cats, c("Web","Document")])
grouping <- list(
  "Passive-receptive\n(Sims + Videos)"     = c("Interactive Simulation","Video"),
  "Active text-based\n(Exercises + Static)" = c("Interactive Exercise","Static Text"),
  "Reflection /\nDiscussion"               = c("Reflection / Discussion")
)
mat <- sapply(grouping, function(cats) c(
  Web      = sum(dwell$Web[dwell$category %in% cats]),
  Document = sum(dwell$Document[dwell$category %in% cats])
))
bp <- barplot(mat, beside = TRUE, col = c(WEB, DOC), border = NA,
              ylim = c(0, max(mat) * 1.20),
              ylab = "% of measured attention",
              main = "Attention split: Web wins passive, Doc wins active text")
text(bp, mat + 1.5, labels = sprintf("%.1f%%", mat), cex = 0.85)
legend("topright", c("Web (Immersive)", "Document"),
       fill = c(WEB, DOC), border = NA, bty = "n")
dev.off()

# --- FIG 4: Revisits (re-engagement / confusion proxy) --------
png(file.path(FIG, "fig4_revisits.png"), width = 950, height = 600, res = 130)
par(mar = c(8, 5, 4, 2))
ord <- c("Interactive Exercise","Interactive Simulation","Video",
         "Reflection / Discussion","Static Text")
rev <- rev[match(ord, rev$category), ]
mat <- t(as.matrix(rev[, c("Web","Document")]))
colnames(mat) <- rev$category
bp <- barplot(mat, beside = TRUE, col = c(WEB, DOC), border = NA,
              ylab = "Average revisits per AOI",
              main = "Doc students kept circling back",
              las = 2, cex.names = 0.8,
              ylim = c(0, max(mat) * 1.25))
text(bp, mat + 5, labels = sprintf("%.0f", mat), cex = 0.75)
legend("topright", c("Web (Immersive)", "Document"),
       fill = c(WEB, DOC), border = NA, bty = "n")
dev.off()

# --- FIG 5: VAI by category (engagement quality) --------------
png(file.path(FIG, "fig5_vai_by_category.png"), width = 950, height = 600, res = 130)
par(mar = c(8, 5, 4, 2))
ord <- c("Interactive Simulation","Video","Interactive Exercise",
         "Reflection / Discussion","Static Text")
vai <- vai[match(ord, vai$category), ]
mat <- t(as.matrix(vai[, c("Web","Document")]))
colnames(mat) <- vai$category
bp <- barplot(mat, beside = TRUE, col = c(WEB, DOC), border = NA,
              ylab = "VAI (%)",
              main = "Two interfaces, two cognitive styles",
              las = 2, cex.names = 0.8,
              ylim = c(0, 100))
text(bp, mat + 2, labels = sprintf("%.0f", mat), cex = 0.8)
legend("topright", c("Web (Immersive)", "Document"),
       fill = c(WEB, DOC), border = NA, bty = "n", inset = c(0, 0.02))
mtext("Doc sustains attention per element; Web traverses faster — both reach the same outcome",
      side = 3, line = 0.2, cex = 0.7, col = "grey30")
dev.off()

# --- FIG 6: Assessment trajectory ------------------------------
png(file.path(FIG, "fig6_assessment_trajectory.png"), width = 950, height = 600, res = 130)
par(mar = c(5, 5, 4, 2))
ord <- c("Pre","Session 1","Session 2","Session 3","Post")
asmt$assessment <- factor(asmt$assessment, levels = ord)
asmt <- asmt[order(asmt$assessment, asmt$condition), ]
web_y <- asmt$pct[asmt$condition == "Web"]
doc_y <- asmt$pct[asmt$condition == "Document"]
plot(seq_along(ord), web_y, type = "b", col = WEB, pch = 19, lwd = 2.5,
     ylim = c(70, 105), xaxt = "n",
     xlab = "", ylab = "% correct",
     main = "Equivalent learning trajectory")
lines(seq_along(ord), doc_y, type = "b", col = DOC, pch = 17, lwd = 2.5)
axis(1, at = seq_along(ord), labels = ord)
# N annotations
web_n <- asmt$n[asmt$condition == "Web"]
doc_n <- asmt$n[asmt$condition == "Document"]
text(seq_along(ord), web_y + 1.5, sprintf("n=%d", web_n), col = WEB, cex = 0.7)
text(seq_along(ord), doc_y - 1.5, sprintf("n=%d", doc_n), col = DOC, cex = 0.7)
legend("bottomright", c("Web (Immersive)", "Document"),
       col = c(WEB, DOC), lwd = 2.5, pch = c(19, 17), bty = "n")
dev.off()

cat("Generated 6 figures in", FIG, "\n")
list.files(FIG, pattern = "\\.png$")
