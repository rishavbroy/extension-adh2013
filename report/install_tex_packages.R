# Install the small set of LaTeX packages used by the report and code appendix.
# Run from the project root with: source("report/install_tex_packages.R")

if (!requireNamespace("tinytex", quietly = TRUE)) {
  install.packages("tinytex")
}

if (!tinytex::is_tinytex()) {
  message("TinyTeX does not appear to be the active TeX distribution. Installing TinyTeX...")
  tinytex::install_tinytex()
}

pkgs <- c(
  "fvextra",   # line-wrapped verbatim source-code appendix
  "xcolor",
  "booktabs",
  "caption",
  "float"
)

message("Installing/checking TeX packages: ", paste(pkgs, collapse = ", "))
tinytex::tlmgr_install(pkgs)

missing <- vapply(pkgs, function(pkg) {
  sty <- paste0(pkg, ".sty")
  !nzchar(Sys.which("kpsewhich")) || length(system2("kpsewhich", sty, stdout = TRUE, stderr = FALSE)) == 0
}, logical(1))

if (any(missing)) {
  warning("Some TeX packages may still be missing: ", paste(names(missing)[missing], collapse = ", "))
} else {
  message("All requested TeX packages appear available.")
}
