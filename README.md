# Immersive vs. Document-Based Virtual Learning Pilot

Analysis code and aggregate summaries for a pilot comparing immersive web and document-based delivery of the SciTrek bioinformatics curriculum.

This is Brian Ngan's portfolio fork of the [team's public release](https://github.com/Anteater10/Scitrek-immersive-document-pilot). Brian is listed among the project authors in `CITATION.cff`. This fork contains the same aggregate-only release, with the original team and data attribution preserved.

## Public Release

The public dataset is in `data/processed/`. Code uses base R. Code is licensed under MIT; aggregate data are licensed under CC BY 4.0.

Assessment rows require at least five participants. Small post-test cells are blanked. Web gaze values and corresponding comparison values are withheld: the daily gaze groups contain fewer than five participants, and distinct participant counts across pooled sessions have not been verified. Assessment counts are separate from gaze counts.

Raw AOI exports, assessment responses, participant identifiers, internal notes, intermediate outputs, and existing poster figures are excluded. Those figures were generated from unsuppressed outputs and can disclose withheld values.

See `DATA_DICTIONARY.md`, `PRIVACY.md`, and `data/processed/release_validation_report.md`.

## Reproducibility

Public users can reuse the released tables. Full regeneration requires restricted inputs that are not included. Approved researchers with those inputs can run:

```sh
Rscript R/03_unified_pipeline.R
Rscript R/04_dedup_pipeline.R
Rscript R/05_assessment_pipeline.R
Rscript R/06_efficiency_analysis.R
Rscript R/08_prepare_public_release.R
```

Scripts 01, 02, and 07 are local analysis/figure tools. Their unsuppressed outputs must remain private.

## Citation

Use `CITATION.cff` to cite this prepared dataset. The project contributor confirmed that the study owner approved public sharing of the aggregate release. No raw participant data are included in this fork.
