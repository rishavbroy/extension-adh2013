# src/make_replication_code_pdf.R
# Generate a Quarto document that prints all replication source files.

library(fs)
library(glue)
library(stringr)

project_root <- rprojroot::find_root(rprojroot::has_file("src/run_pipeline.R"))
src_dir <- file.path(project_root, "src")
out_dir <- file.path(project_root, "report")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

qmd_path <- file.path(out_dir, "replication-code.qmd")

source_files <- dir_ls(
  src_dir,
  recurse = FALSE,
  type = "file",
  regexp = "\\.(R|r|csv|json|md|qmd|yml|yaml)$"
)

# Put pipeline files first, then auxiliary data/config files.
preferred_order <- c(
  "00_config.R",
  "01_helpers.R",
  "02_build_analysis_data.R",
  "03_estimate_event_study.R",
  "04_export_outputs.R",
  "05_export_diagnostic_maps.R",
  "06_sixth_extensions.R",
  "run_pipeline.R"
)

source_files <- source_files[order(
  match(path_file(source_files), preferred_order, nomatch = 999),
  path_file(source_files)
)]

rel_for_latex <- function(path) {
  # QMD lives in report/, so paths are relative to report/.
  rel <- fs::path_rel(path, start = out_dir)
  # LaTeX on Windows is happier with forward slashes.
  rel <- gsub("\\\\", "/", rel)
  rel
}

escape_md_inline_code <- function(x) {
  # File names here should not contain backticks, but be defensive.
  gsub("`", "\\\\`", x)
}

header <- c(
  "---",
  'title: "Replication Code Appendix"',
  'subtitle: "ADH China Shock Political Event Study Extension"',
  'author: "Rishav Roy"',
  "format:",
  "  pdf:",
  "    documentclass: article",
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
  "        \\DefineVerbatimEnvironment{CodeFile}{Verbatim}{%",
  "          breaklines=true,",
  "          breakanywhere=true,",
  "          breaksymbolleft={},",
  "          breaksymbolright={},",
  "          fontsize=\\scriptsize,",
  "          numbers=left,",
  "          numbersep=5pt,",
  "          frame=single,",
  "          framerule=0.2pt,",
  "          framesep=2mm,",
  "          tabsize=2,",
  "          commandchars=\\\\\\{\\}",
  "        }",
  "execute:",
  "  echo: false",
  "  warning: false",
  "  message: false",
  "---",
  "",
  "This appendix prints the replication source files used to construct the final results.",
  "",
  "The files are included directly from `src/` at render time.",
  ""
)

body <- unlist(lapply(source_files, function(path) {
  nm <- path_file(path)
  rel <- rel_for_latex(path)

  c(
    glue("\\newpage"),
    "",
    glue("# `{escape_md_inline_code(nm)}`"),
    "",
    "```{=latex}",
    glue("\\VerbatimInput[breaklines=true,breakanywhere=true,fontsize=\\scriptsize,numbers=left,frame=single,tabsize=2]{{{rel}}}"),
    "```",
    ""
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