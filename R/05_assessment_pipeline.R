# ============================================================
# Assessment Scoring Pipeline
# Scores Pre/Post + Session 1-3 quizzes, tags by condition,
# computes per-condition means + Pre→Post change for matched
# students, and produces combined attention + learning table.
# ============================================================
script_dir <- function(default) {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg)) return(dirname(normalizePath(sub("^--file=", "", file_arg[1]))))
  dirname(normalizePath(default))
}
ROOT <- normalizePath(file.path(script_dir("R/05_assessment_pipeline.R"), ".."))
ASMT <- file.path(ROOT, "assessments")
OUT  <- file.path(ROOT, "output")
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

# ---------- name normalization ----------
# Handles trailing whitespace, case variants, and middle initials.
# Optional private aliases can be supplied in assessments/name_aliases_private.csv
# with columns: alias, canonical. That file is intentionally gitignored.
normalize_name <- function(x) {
  n <- trimws(x)
  n <- gsub("\\s+", " ", n)
  # title-case each word
  n <- sapply(strsplit(n, " "), function(parts) {
    parts <- parts[nchar(parts) > 0]
    paste(toupper(substring(parts,1,1)), tolower(substring(parts,2)), sep="", collapse=" ")
  })
  # remove single-letter middle initials
  n <- gsub(" [A-Z] ", " ", n)
  n
}

apply_private_aliases <- function(x) {
  alias_path <- file.path(ASMT, "name_aliases_private.csv")
  if (!file.exists(alias_path)) return(x)
  aliases <- read.csv(alias_path, stringsAsFactors = FALSE)
  aliases$alias <- normalize_name(aliases$alias)
  aliases$canonical <- normalize_name(aliases$canonical)
  for (i in seq_len(nrow(aliases))) {
    x[x == aliases$alias[i]] <- aliases$canonical[i]
  }
  x
}

# ---------- load roster ----------
roster <- read.csv(file.path(ASMT, "condition_roster.csv"), stringsAsFactors = FALSE)
roster$name_canonical <- normalize_name(roster$name_canonical)
roster$name_canonical <- apply_private_aliases(roster$name_canonical)
roster$condition[roster$condition == ""] <- NA

# ---------- load assessments ----------
load_form <- function(path) {
  d <- read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  d$name_canonical <- normalize_name(d$Name)
  d$name_canonical <- apply_private_aliases(d$name_canonical)
  d
}
pre  <- load_form(file.path(ASMT, "Pre-Module Assessment (Responses) - Form Responses 1.csv"))
post <- load_form(file.path(ASMT, "Post-Module Assessment (Responses) - Form Responses 1.csv"))
s1   <- load_form(file.path(ASMT, "Session 1 Post Quiz (Responses) - Form Responses 1.csv"))
s2   <- load_form(file.path(ASMT, "Session 2 Post Quiz (Responses) - Form Responses 1.csv"))
s3   <- load_form(file.path(ASMT, "Session 3 Post Quiz (Responses) - Form Responses 1.csv"))

# ---------- expected answers ----------
# Pre: 5 Y/N items. Items 1,3,5 are well-defined testable scientific
# questions; items 2,4 are subjective/value-based (not testable).
# Correct = "Yes" for 1,3,5 and "No" for 2,4.
pre_cols     <- names(pre)[3:7]
pre_expected <- c("Yes","No","Yes","No","Yes")

# Post: same 5-item Y/N structure mirrored to cancer content.
post_cols     <- names(post)[3:7]
post_expected <- c("Yes","No","Yes","No","Yes")

# Session quizzes: single-correct MC. Hard-coded biological answer keys
# (derived from curriculum, not from majority response — too few
# respondents for "majority = correct" to be defensible).
s1_cols <- names(s1)[3:7]
s2_cols <- names(s2)[3:7]
s3_cols <- names(s3)[3:7]
s1_expected <- c(
  "Using information from a gene to produce a functional product like a protein",
  "Transcription",
  "Ribosome",
  "It blocks transcription and prevents a gene from being expressed",
  "RNA polymerase cannot access the gene to begin transcription"
)
s2_expected <- c(
  "Cancer cells grow and divide uncontrollably",
  "S phase",
  "It helps stop the cell cycle when DNA is damaged",
  "Tumor suppressor gene",
  "The cell will continue dividing even with damaged DNA"
)
s3_expected <- c(
  "The gene produces more RNA or protein than normal",
  "Their expression levels stay relatively constant across samples",
  "RNA sequencing (RNA-seq)",
  "The location and amount of a protein in tissue",
  "MYC may promote tumor growth, while TP53 may no longer stop cell division"
)

# ---------- scoring ----------
score_form <- function(df, cols, expected, label) {
  n_items <- length(cols)
  mat <- sapply(seq_along(cols), function(i) trimws(df[[cols[i]]]) == expected[i])
  if (!is.matrix(mat)) mat <- matrix(mat, ncol = n_items)
  pct <- 100 * rowSums(mat, na.rm = TRUE) / n_items
  data.frame(
    name_canonical = df$name_canonical,
    assessment = label,
    n_correct = rowSums(mat, na.rm = TRUE),
    n_items = n_items,
    pct = pct,
    stringsAsFactors = FALSE
  )
}

pre_sc  <- score_form(pre,  pre_cols,  pre_expected,  "Pre")
post_sc <- score_form(post, post_cols, post_expected, "Post")
s1_sc   <- score_form(s1,   s1_cols,   s1_expected,   "Session 1")
s2_sc   <- score_form(s2,   s2_cols,   s2_expected,   "Session 2")
s3_sc   <- score_form(s3,   s3_cols,   s3_expected,   "Session 3")

all_scores <- rbind(pre_sc, post_sc, s1_sc, s2_sc, s3_sc)
all_scores <- merge(all_scores, roster, by = "name_canonical", all.x = TRUE)
write.csv(all_scores, file.path(OUT, "assessment_scores_long.csv"), row.names = FALSE)

# ---------- per-condition means ----------
cond_means <- aggregate(pct ~ assessment + condition, data = all_scores, FUN = mean)
cond_n     <- aggregate(pct ~ assessment + condition, data = all_scores, FUN = length)
names(cond_n)[3] <- "n"
cond_means <- merge(cond_means, cond_n, by = c("assessment","condition"))
cond_means$pct <- round(cond_means$pct, 1)
write.csv(cond_means, file.path(OUT, "assessment_condition_means.csv"), row.names = FALSE)

# ---------- pre → post matched change ----------
matched <- merge(
  pre_sc[, c("name_canonical","pct")],
  post_sc[, c("name_canonical","pct")],
  by = "name_canonical", suffixes = c("_pre","_post")
)
matched <- merge(matched, roster, by = "name_canonical", all.x = TRUE)
matched$delta_pct <- matched$pct_post - matched$pct_pre
write.csv(matched, file.path(OUT, "assessment_prepost_matched.csv"), row.names = FALSE)

# ---------- combined attention + learning by condition ----------
dwell_path <- file.path(OUT, "dedup_headline_dwell.csv")
vai_path   <- file.path(OUT, "dedup_headline_vai.csv")
combined <- NULL
if (file.exists(dwell_path) && file.exists(vai_path)) {
  dwell <- read.csv(dwell_path, stringsAsFactors = FALSE)
  vai   <- read.csv(vai_path,   stringsAsFactors = FALSE)
  # interactive share by condition (Sim + Exercise + Video)
  int_rows <- dwell$category %in% c("Interactive Simulation","Interactive Exercise","Video")
  int_web  <- sum(dwell$Web[int_rows], na.rm = TRUE)
  int_doc  <- sum(dwell$Document[int_rows], na.rm = TRUE)
  vai_web  <- mean(vai$Web, na.rm = TRUE)
  vai_doc  <- mean(vai$Document, na.rm = TRUE)

  pick <- function(asmt, cond, col) {
    v <- cond_means[[col]][cond_means$assessment == asmt & cond_means$condition == cond]
    if (length(v) == 0) NA_real_ else v
  }
  combined <- data.frame(
    condition = c("Web","Document"),
    interactive_share_pct = round(c(int_web, int_doc), 1),
    mean_vai_pct          = round(c(vai_web, vai_doc), 1),
    pre_score_pct  = round(c(pick("Pre","Web","pct"),  pick("Pre","Document","pct")), 1),
    pre_n          = c(pick("Pre","Web","n"),  pick("Pre","Document","n")),
    post_score_pct = round(c(pick("Post","Web","pct"), pick("Post","Document","pct")), 1),
    post_n         = c(pick("Post","Web","n"), pick("Post","Document","n"))
  )
  write.csv(combined, file.path(OUT, "combined_attention_learning.csv"), row.names = FALSE)
}

# ---------- console report ----------
cat("============================================================\n")
cat(" ASSESSMENT PIPELINE\n")
cat("============================================================\n\n")

cat("ROSTER COVERAGE\n")
cov <- table(roster$condition, useNA = "always")
print(cov)
unassigned <- roster$name_canonical[is.na(roster$condition)]
if (length(unassigned)) {
  cat("\nUnassigned private roster records:", length(unassigned), "\n")
  cat("Edit assessments/condition_roster.csv locally; names are suppressed in console output.\n")
}
cat("\n")

cat("ANSWER KEYS USED\n")
cat("Pre  (Y/N, * = expected):", paste0(pre_expected, collapse=" / "), "\n")
cat("Post (Y/N, * = expected):", paste0(post_expected, collapse=" / "), "\n")
cat("Session 1 key (hard-coded):\n"); for (i in seq_along(s1_cols)) cat("  Q", i, ": ", s1_expected[i], "\n", sep="")
cat("Session 2 key (hard-coded):\n"); for (i in seq_along(s2_cols)) cat("  Q", i, ": ", s2_expected[i], "\n", sep="")
cat("Session 3 key (hard-coded):\n"); for (i in seq_along(s3_cols)) cat("  Q", i, ": ", s3_expected[i], "\n", sep="")
cat("\n")

cat("PER-CONDITION MEANS (% correct)\n")
print(cond_means[order(cond_means$assessment, cond_means$condition), ], row.names = FALSE)
cat("\n")

cat("PRE -> POST MATCHED CHANGE (aggregate; names suppressed)\n")
matched_summary <- aggregate(delta_pct ~ condition, data = matched,
                             FUN = function(x) round(mean(x, na.rm = TRUE), 1))
matched_n <- aggregate(delta_pct ~ condition, data = matched, FUN = length)
names(matched_n)[2] <- "n_matched"
matched_summary <- merge(matched_summary, matched_n, by = "condition")
print(matched_summary, row.names = FALSE)
cat("\n")

if (!is.null(combined)) {
  cat("COMBINED ATTENTION + LEARNING BY CONDITION\n")
  print(combined, row.names = FALSE)
  cat("\n")
}

cat("Outputs:\n")
cat("  ", file.path(OUT, "assessment_scores_long.csv"), "\n")
cat("  ", file.path(OUT, "assessment_condition_means.csv"), "\n")
cat("  ", file.path(OUT, "assessment_prepost_matched.csv"), "\n")
if (!is.null(combined)) cat("  ", file.path(OUT, "combined_attention_learning.csv"), "\n")
