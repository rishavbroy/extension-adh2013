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

## PDF rendering notes

Both `final-report.qmd` and `replication-code.qmd` use `pdf-engine: xelatex`.
The replication-code appendix intentionally uses `fvextra` for line-wrapped verbatim source code.
If rendering fails with `fvextra.sty not found`, install the LaTeX package before rerendering, for example:

```r
install.packages("tinytex")
tinytex::install_tinytex()
tinytex::tlmgr_install("fvextra")
```

or from a shell after TinyTeX/TeX Live is on PATH:

```bash
tlmgr install fvextra
```

Seeing Quarto list a Julia engine in the render metadata does not mean the report is using Julia.
The report chunks are R/knitr chunks; no Python virtual environment is required for these PDFs.
