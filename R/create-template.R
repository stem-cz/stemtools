#' This package will create a function called create_template()
#'
#' It's callback is at: inst/rstudio/templates/project/create_template.dcf
#'
#' @export

create_template <-
  function(path, ...) {

    # Create the project path given the name chosen by the user:
    dir.create(path, recursive = TRUE, showWarnings = FALSE)

    # Change the working directory to the recently created folder:
    setwd(file.path(getwd(), path))

    # Collect the list of inputs in a list to be called later:
    dots <- list(...)

    # Create all relevant folders
    dir_names <- c("data-input", "data-processed", "data-output", "scripts",
                   "figures", "models", "documentation")

    for (dir_name in dir_names) {
      dir.create(dir_name, showWarnings = FALSE)
    }

    # In the project template we've added 2 choices for the user:
    # * One allows them to select if the project will have a .gitignore file

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
          "!renv/activate.R"
        )

      writeLines(paste(git_ignores, sep = '\n'), '.gitignore')
    }

  }
