# Report folder

This folder is designed to be a sibling of `output/` and `src/` inside `extension-adh2013`.

## Render the final report

From the project root:

```bash
quarto render report/final-report.qmd
```

The QMD references figures and tables directly from `../output/...`, so do not move the `report/` folder inside `output/`.

## Render the replication-code PDF

From the project root or from `report/`:

```r
source("report/make_replication_code_pdf.R")
```

This regenerates `report/replication-code.qmd` and renders `report/replication-code.pdf` if Quarto is on PATH.

## LaTeX note

Rendering either PDF requires a LaTeX distribution. TinyTeX is usually easiest from R:

```r
install.packages("tinytex")
tinytex::install_tinytex()
```

## Teammate files

The teammate prose was converted into section fragments with minimal changes. Yuxiao's two submitted table images were extracted into `report/assets/` because they were embedded in the original DOCX and not part of the generated `output/` folder.
