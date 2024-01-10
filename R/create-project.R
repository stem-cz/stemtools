#' This package will create a function called create_project()
#'
#' It's callback is at: inst/rstudio/templates/project/create_project.dcf
#' @param path Path to the project
#' @param ... Additional arguments, used to collect user input
#'
#' @export

create_project <-
  function(path, ...) {

    # Create the project path given the name chosen by the user:
    dir.create(path, recursive = TRUE, showWarnings = FALSE)

    # Change the working directory to the recently created folder:
    # setwd(file.path(getwd(), path))

    # Baseline folder structure
    dir_names <- c("01-data-input", "02-data-processed", "03-data-output", "04-scripts", "05-documentation", "06-figures", "07-models")
    dir_names <- file.path(path, dir_names)
    for (dir_name in dir_names) {
      dir.create(dir_name)
    }

    # Collect the list of inputs in a list to be called later:
    dots <- list(...)

    # In the project template we've added 2 choices for the user:
    # * One allows them to select if the project will have a .gitignore file
    # * The other will initiate a renv project.

    # Check .gitignore argument
    if(dots[["createGitignore"]]) {
      git_ignores <-
        c(
          '.Rhistory',
          '.Rapp.history',
          '.RData',
          '.Ruserdata',
          '.Rproj.user/',
          '.Renviron',
          'renv/*',
          '!renv/activate.R'
        )

      #file.create(file.path(path, ".gitignore"))
      writeLines(paste(git_ignores, sep = '\n'), file.path(path, '.gitignore'))
    }

    if(dots[["useRenv"]]) {
      renv::init(path)
    }

  }
