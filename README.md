# Dissertation_2026

This repository is organised to keep the core materials for the dissertation in one place while supporting a reproducible workflow.

## Repository structure

```text
chapters/      # Quarto chapter source files (.qmd)
assets/css/    # CSS styles used by Quarto documents
scripts/R/     # R scripts that generate dissertation outputs
outputs/       # Generated outputs (kept out of version control by default)
```

## Recommended usage

- Store each dissertation chapter as a separate `.qmd` file in `/home/runner/work/Dissertation_2026/Dissertation_2026/chapters`.
- Save any dissertation CSS files in `/home/runner/work/Dissertation_2026/Dissertation_2026/assets/css`.
- Save R scripts that generate tables, figures, or derived data in `/home/runner/work/Dissertation_2026/Dissertation_2026/scripts/R`.
- Put generated outputs in `/home/runner/work/Dissertation_2026/Dissertation_2026/outputs`.

To help keep chapter drafts secure, keep the repository private for unpublished work and avoid committing rendered output files unless you explicitly want to publish them.
