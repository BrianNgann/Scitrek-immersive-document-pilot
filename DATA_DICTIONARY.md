# Data Dictionary

Public-release tables are generated into `data/processed/` by `R/08_prepare_public_release.R`.

For this release, all `Web` gaze columns and corresponding differences are blanked, including session durations. Web gaze fields in the combined table are also blanked. Daily Web gaze groups contain fewer than five participants; distinct contributor counts for pooled gaze metrics remain unverified. Assessment counts are evaluated independently. Raw AOI exports and existing poster figures are excluded.

The release script applies a minimum public cell-size rule of `n >= 5`. Rows with a plain `n` column below that threshold are removed. Paired count/metric fields such as `post_score_pct` / `post_n` are blanked when the paired count is below the threshold. The script also writes `release_validation_report.md`.

## Public Tables

| File | Description | Privacy level |
|---|---|---|
| `dedup_headline_dwell.csv` | Overall share of measured gaze time by content category and condition. | Aggregate |
| `dedup_headline_vai.csv` | Mean Visual Attention Index by content category and condition. | Aggregate |
| `dedup_category_day.csv` | Day-by-day aggregate attention metrics by condition and content category, with rows below `n=5` removed. | Aggregate, small-cell suppressed |
| `combined_attention_learning.csv` | Condition-level combined attention and assessment summary, with post-test fields blanked when `post_n < 5`. | Aggregate, small-cell suppressed |
| `assessment_condition_means.csv` | Mean assessment score by condition and assessment, with rows below `n=5` removed. | Aggregate, small-cell suppressed |
| `session_duration_by_day.csv` | Session duration by day and condition. | Aggregate |
| `k_coefficient_by_category.csv` | Focal vs. ambient processing index by category and condition. | Aggregate |
| `revisits_by_category.csv` | Average revisits by category and condition. | Aggregate |
| `vai_reach_by_category.csv` | Mean percentage of viewers reaching each category by condition. | Aggregate |

## Common Columns

| Column | Meaning |
|---|---|
| `condition` | Interface condition: `Web` or `Document`. |
| `day` | Curriculum day: `Day 1`, `Day 2`, or `Day 3`. |
| `category` | Canonical instructional category assigned from AOI labels. |
| `Document` | Metric value for the document-based condition. |
| `Web` | Metric value for the immersive web condition. |
| `delta_web_minus_doc` | `Web - Document` for the row's metric. |
| `n` | Number of participants represented in the aggregate row. |
| `gaze_time_s` | Seconds of gaze time on the category or AOI. |
| `fixation_time_s` | Seconds spent in fixations. |
| `fixations` | Count of fixations. |
| `gazes` | Count of gazes exported by RealEye. |
| `avg_revisits` | Average return visits to an AOI or category. |
| `clicks` | Click count. |
| `vai_pct` | Visual Attention Index percentage. |
| `vai_reach_pct` | Percentage of viewers who looked at the AOI/category. |
| `vai_hold_pct` | VAI hold component percentage. |
| `ttff_s` | Time to first fixation in seconds. Suppressed as `NA` for document rows where scroll position makes it non-comparable. |
| `avg_fix_dur_s` | Average fixation duration in seconds. |
| `k_coefficient` | Focal/ambient processing index; higher values indicate more focal looking. |
| `session_len_s` | Session length in seconds. |
| `share_of_measured_pct` | Percent of measured task-relevant gaze time within a condition/day. |
| `dwell_pct_session` | Percent of session time represented by category dwell time. |

## Provenance And Processing

Source eye-tracking files are restricted RealEye AOI metric exports stored locally in `doc/` and `web/`; they are excluded from this release. Scripts in `R/` parse their metrics, map AOIs to shared instructional categories, and collapse document sub-AOIs before producing category summaries.

Assessment outputs are derived from private Google Forms exports stored locally under `assessments/`. Those raw files contain student names, timestamps, and form responses and are not part of the public dataset.

Participant counts in the gaze analysis come from study session records encoded in the analysis scripts: Web Day 1 `n=3`, Web Day 2 `n=2`, Web Day 3 `n=3`, and Document Days 1-3 `n=6`. Public day-level outputs remove rows where `n < 5`.

Missing values are written as blank CSV fields in public release files. Blank fields may indicate either an unavailable/non-comparable metric, such as document-condition TTFF, or a value suppressed by the minimum-cell-size rule. The validation report distinguishes suppression actions.

## Privacy And Suppression Policy

This project involves student/human-subject educational data. The public release excludes direct identifiers and private response data:

- student names,
- timestamps,
- free-text assessment responses,
- individual-level assessment scores,
- private name-alias files,
- internal notes that name individual students.

The public release uses a conservative minimum cell size of `n >= 5`. Rows or paired values below that threshold are removed or blanked. This reduces re-identification risk from small cells, but it does not replace formal IRB, FERPA, institutional, consent, or data-use review.

## Non-Public Inputs

The raw assessment exports and individual-level assessment outputs contain student names, timestamps, free-text responses, and small-n records. They are intentionally excluded from the public release by `.gitignore`.
