# ============================================================
# Public release prep
# Copies only aggregate/de-identified outputs into data/processed/,
# applies minimum-cell suppression, and writes a validation report.
# ============================================================
script_dir <- function(default) {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg)) return(dirname(normalizePath(sub("^--file=", "", file_arg[1]))))
  dirname(normalizePath(default))
}
ROOT <- normalizePath(file.path(script_dir("R/08_prepare_public_release.R"), ".."))
OUT  <- file.path(ROOT, "output")
PUB  <- file.path(ROOT, "data", "processed")
dir.create(PUB, showWarnings = FALSE, recursive = TRUE)

MIN_CELL_N <- 5

public_files <- data.frame(
  file = c(
    "dedup_headline_dwell.csv",
    "dedup_headline_vai.csv",
    "dedup_category_day.csv",
    "combined_attention_learning.csv",
    "assessment_condition_means.csv",
    "session_duration_by_day.csv",
    "k_coefficient_by_category.csv",
    "revisits_by_category.csv",
    "vai_reach_by_category.csv"
  ),
  check_small_n = c(
    FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE
  ),
  stringsAsFactors = FALSE
)

missing <- character()
validation <- data.frame(
  file = character(),
  input_rows = integer(),
  output_rows = integer(),
  rows_removed_small_n = integer(),
  values_suppressed_small_n = integer(),
  pii_scan_hits = integer(),
  notes = character(),
  stringsAsFactors = FALSE
)

private_terms_path <- file.path(ROOT, "assessments", "sensitive_terms_private.csv")
private_terms <- character()
if (file.exists(private_terms_path)) {
  private_terms <- read.csv(private_terms_path, stringsAsFactors = FALSE)[[1]]
}

pii_patterns <- c("Timestamp", "name_canonical", "email", "@", private_terms)

public_doc_files <- c(
  "README.md",
  "CURRENT_FILES.md",
  "DATA_DICTIONARY.md",
  "PUBLISHING_CHECKLIST.md",
  "PRIVACY.md",
  "CITATION.cff",
  "LICENSE",
  "LICENSE-DATA.md"
)
doc_sensitive_patterns <- c("name_canonical", "Form Responses 1.csv", private_terms)

write_suppressed <- function(d, file, check_small_n) {
  input_rows <- nrow(d)
  rows_removed <- 0L
  values_suppressed <- 0L
  notes <- character()

  # Session counts are not distinct participants across days. Withhold Web
  # gaze summaries until the number of distinct contributors is verified.
  if ("Web" %in% names(d)) {
    cols <- intersect(c("Web", "delta_web_minus_doc", "doc_minus_web_min"), names(d))
    for (col in cols) {
      values_suppressed <- values_suppressed + sum(!is.na(d[[col]]))
      d[[col]] <- NA_real_
    }
    notes <- c(notes, "Withheld Web gaze and comparison values: eligible distinct-participant count unverified")
  }
  if (file == "combined_attention_learning.csv") {
    web_rows <- d$condition == "Web"
    for (col in c("interactive_share_pct", "mean_vai_pct")) {
      values_suppressed <- values_suppressed + sum(!is.na(d[[col]][web_rows]))
      d[[col]][web_rows] <- NA_real_
    }
    notes <- c(notes, "Withheld Web gaze fields; assessment fields use their own counts")
  }

  if (check_small_n && "n" %in% names(d)) {
    low_n <- !is.na(suppressWarnings(as.numeric(d$n))) &
      suppressWarnings(as.numeric(d$n)) < MIN_CELL_N
    rows_removed <- sum(low_n)
    d <- d[!low_n, , drop = FALSE]
    if (rows_removed > 0) {
      notes <- c(notes, paste0("Removed rows with n < ", MIN_CELL_N))
    }
  }

  n_cols <- grep("_n$", names(d), value = TRUE)
  for (n_col in n_cols) {
    n_values <- suppressWarnings(as.numeric(d[[n_col]]))
    low_n <- !is.na(n_values) & n_values < MIN_CELL_N
    if (!any(low_n)) next

    prefix <- sub("_n$", "", n_col)
    paired_cols <- grep(paste0("^", prefix, "_"), names(d), value = TRUE)
    paired_cols <- setdiff(paired_cols, n_col)
    suppress_cols <- c(n_col, paired_cols)

    for (col in suppress_cols) {
      d[[col]][low_n] <- NA
      values_suppressed <- values_suppressed + sum(low_n)
    }
    notes <- c(notes, paste0("Suppressed ", prefix, " values where ", n_col, " < ", MIN_CELL_N))
  }

  text <- tolower(paste(c(names(d), unlist(d, use.names = FALSE)), collapse = "\n"))
  pii_hits <- sum(vapply(pii_patterns, function(pattern) {
    grepl(tolower(pattern), text, fixed = TRUE)
  }, logical(1)))

  list(
    data = d,
    record = data.frame(
      file = file,
      input_rows = input_rows,
      output_rows = nrow(d),
      rows_removed_small_n = rows_removed,
      values_suppressed_small_n = values_suppressed,
      pii_scan_hits = pii_hits,
      notes = if (length(notes)) paste(unique(notes), collapse = "; ") else "OK",
      stringsAsFactors = FALSE
    )
  )
}

unlink(file.path(PUB, "*.csv"))
unlink(file.path(PUB, "release_validation_report.md"))

for (i in seq_len(nrow(public_files))) {
  file <- public_files$file[i]
  src <- file.path(OUT, file)
  dst <- file.path(PUB, file)
  if (!file.exists(src)) {
    missing <- c(missing, file)
    next
  }
  d <- read.csv(src, check.names = FALSE, stringsAsFactors = FALSE)
  result <- write_suppressed(d, file, public_files$check_small_n[i])
  write.csv(result$data, dst, row.names = FALSE, na = "")
  validation <- rbind(validation, result$record)
}

if (length(missing)) {
  stop("Missing expected aggregate files: ", paste(missing, collapse = ", "))
}

if (any(validation$pii_scan_hits > 0)) {
  bad <- validation$file[validation$pii_scan_hits > 0]
  stop("PII scan flagged public output(s): ", paste(bad, collapse = ", "))
}

doc_scan <- data.frame(
  file = character(),
  hits = integer(),
  stringsAsFactors = FALSE
)
for (file in public_doc_files) {
  path <- file.path(ROOT, file)
  if (!file.exists(path)) next
  text <- tolower(paste(readLines(path, warn = FALSE), collapse = "\n"))
  hits <- sum(vapply(doc_sensitive_patterns, function(pattern) {
    grepl(tolower(pattern), text, fixed = TRUE)
  }, logical(1)))
  doc_scan <- rbind(doc_scan, data.frame(file = file, hits = hits))
}
if (any(doc_scan$hits > 0)) {
  bad <- doc_scan$file[doc_scan$hits > 0]
  stop("Sensitive-term scan flagged public documentation: ", paste(bad, collapse = ", "))
}

report_path <- file.path(PUB, "release_validation_report.md")
report <- c(
  "# Public Release Validation Report",
  "",
  paste0("Generated by `R/08_prepare_public_release.R`."),
  "",
  paste0("Minimum public cell size: `n >= ", MIN_CELL_N, "`."),
  "",
  "Tables with explicit `n` columns have rows below this threshold removed.",
  "Tables with paired fields such as `post_score_pct` / `post_n` have those values blanked when the paired `*_n` is below the threshold.",
  "Public documentation is scanned for specific known student-name examples and private raw-file markers.",
  "Web gaze values and comparisons are withheld because day-level groups have fewer than five participants and pooled distinct-participant counts are unverified.",
  "Raw AOI exports and existing poster figures are excluded to prevent disclosure of suppressed values.",
  "",
  "## Results",
  "",
  "| File | Input rows | Output rows | Rows removed | Values suppressed | PII scan hits | Notes |",
  "|---|---:|---:|---:|---:|---:|---|",
  apply(validation, 1, function(row) {
    paste0(
      "| `", row[["file"]], "` | ",
      row[["input_rows"]], " | ",
      row[["output_rows"]], " | ",
      row[["rows_removed_small_n"]], " | ",
      row[["values_suppressed_small_n"]], " | ",
      row[["pii_scan_hits"]], " | ",
      row[["notes"]], " |"
    )
  }),
  "",
  "## Public Documentation Scan",
  "",
  "| File | Sensitive-term hits |",
  "|---|---:|",
  apply(doc_scan, 1, function(row) {
    paste0("| `", row[["file"]], "` | ", row[["hits"]], " |")
  })
)
writeLines(report, report_path)

cat("Public aggregate release written to:", PUB, "\n")
cat("Minimum public cell size:", MIN_CELL_N, "\n")
cat("Files:\n")
cat(paste0("  - ", public_files$file, "\n"), sep = "")
cat("Validation report:\n")
cat("  - ", report_path, "\n", sep = "")
