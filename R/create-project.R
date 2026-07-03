#' Scaffold a new Stem analysis project
#'
#' Creates a standard Stem project directory structure, a minimal `README.md`
#' and, optionally, a `.gitignore` file. This function backs the RStudio
#' "New Project" template defined in
#' `inst/rstudio/templates/project/create_project.dcf`, but can also be called
#' directly.
#'
#' @param path Path to the project directory to create.
#' @param createGitignore If `TRUE` (default), writes a `.gitignore` with sensible
#'   defaults for R projects. Also supplied by the RStudio template wizard.
#' @param ... Additional named arguments collected from the RStudio template
#'   wizard. Currently ignored.
#'
#' @return Invisibly returns `path`. Called for its side effect of creating the
#'   project directory structure on disk.
#' @export
#'
#' @examples \dontrun{
#' create_project(file.path(tempdir(), "my-analysis"))
#' }
create_project <- function(path, createGitignore = TRUE, ...) {

  # Create the project root, then the baseline folder structure.
  dir.create(path, recursive = TRUE, showWarnings = FALSE)

  dir_names <- c(
    "01_data-input",
    "02_data-processed",
    "03_data-output",
    "04_scripts",
    "05_documentation",
    "06_figures",
    "07_models"
  )
  for (dir_name in dir_names) {
    dir.create(file.path(path, dir_name), showWarnings = FALSE)
  }

  # Minimal README so the RStudio template's `OpenFiles` target exists.
  writeLines(paste0("# ", basename(path)), file.path(path, "README.md"))

  if (isTRUE(createGitignore)) {
    git_ignores <- c(
      ".Rhistory",
      ".Rapp.history",
      ".RData",
      ".Ruserdata",
      ".Rproj.user/",
      ".Renviron"
    )
    writeLines(git_ignores, file.path(path, ".gitignore"))
  }

  invisible(path)
}
