# Generate report/replication-code.qmd and render it to PDF.
# Run from the project root or from report/.

if (!requireNamespace("fs", quietly = TRUE)) stop("Package 'fs' is required.")
if (!requireNamespace("glue", quietly = TRUE)) stop("Package 'glue' is required.")
if (!requireNamespace("rprojroot", quietly = TRUE)) stop("Package 'rprojroot' is required.")

library(fs)
library(glue)

project_root <- rprojroot::find_root(rprojroot::has_dir("src"))

ensure_code_pdf_tex_available <- function() {
  # Keep fvextra, but give the user a clearer setup path than a raw LaTeX failure.
  if (!nzchar(Sys.which("kpsewhich"))) return(invisible(FALSE))
  has_fvextra <- suppressWarnings(system2("kpsewhich", "fvextra.sty", stdout = TRUE, stderr = FALSE))
  if (length(has_fvextra) && nzchar(has_fvextra[1])) return(invisible(TRUE))

  message("LaTeX package fvextra.sty was not found.")
  if (requireNamespace("tinytex", quietly = TRUE)) {
    message("Attempting tinytex::tlmgr_install('fvextra')...")
    try(tinytex::tlmgr_install("fvextra"), silent = TRUE)
    has_fvextra <- suppressWarnings(system2("kpsewhich", "fvextra.sty", stdout = TRUE, stderr = FALSE))
    if (length(has_fvextra) && nzchar(has_fvextra[1])) return(invisible(TRUE))
  }
  message("Install fvextra manually, for example: quarto install tinytex; Rscript -e \"tinytex::tlmgr_install('fvextra')\"")
  invisible(FALSE)
}

ensure_code_pdf_tex_available()
src_dir <- file.path(project_root, "src")
out_dir <- file.path(project_root, "report")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
qmd_path <- file.path(out_dir, "replication-code.qmd")

source_files <- fs::dir_ls(src_dir, recurse = FALSE, type = "file", regexp = "\\.(R|r|csv|json|md|qmd|ya?ml)$")
preferred_order <- c(
  "00_config.R", "01_helpers.R", "02_build_analysis_data.R", "03_estimate_event_study.R",
  "04_export_outputs.R", "05_export_diagnostic_maps.R", "06_sixth_extensions.R",
  "run_pipeline.R", "county_bridge_exceptions.csv"
)
source_files <- source_files[order(match(fs::path_file(source_files), preferred_order, nomatch = 999), fs::path_file(source_files))]
rel_for_latex <- function(path) gsub("\\\\", "/", fs::path_rel(path, start = out_dir))

header <- c(
  "---",
  'title: "Replication Code Appendix"',
  'subtitle: "ADH China Shock Political Event Study Extension"',
  'author: "Rishav Roy"',
  "format:",
  "  pdf:",
  "    documentclass: article",
  "    pdf-engine: xelatex",
  "    geometry:",
  "      - margin=0.65in",
  "    fontsize: 9pt",
  "    toc: true",
  "    number-sections: true",
  "    colorlinks: true",
  "    keep-tex: true",
  "    include-in-header:",
  "      text: |",
  "        \\usepackage{fvextra}",
  "        \\usepackage{xcolor}",
  "execute:",
  "  echo: false",
  "  warning: false",
  "  message: false",
  "---",
  "",
  "This appendix prints the replication source files used to construct the final results. Files are included directly from `../src/` at render time.",
  ""
)

body <- unlist(lapply(source_files, function(path) {
  nm <- fs::path_file(path)
  rel <- rel_for_latex(path)
  c(
    "\\newpage", "",
    glue("# `{nm}`"), "",
    "```{=latex}",
    glue("\\VerbatimInput[breaklines=true,breakanywhere=true,fontsize=\\scriptsize,numbers=left,frame=single,tabsize=2]{{{rel}}}"),
    "```", ""
  )
}))

writeLines(c(header, body), qmd_path)
message("Wrote ", qmd_path)

if (nzchar(Sys.which("quarto"))) {
  old <- setwd(out_dir)
  on.exit(setwd(old), add = TRUE)
  system2("quarto", c("render", basename(qmd_path)))
} else {
  message("Quarto not found on PATH. Render manually with: quarto render report/replication-code.qmd")
}
