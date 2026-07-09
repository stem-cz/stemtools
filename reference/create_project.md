# Scaffold a new Stem analysis project

Creates a standard Stem project directory structure, a minimal
`README.md` and, optionally, a `.gitignore` file. This function backs
the RStudio "New Project" template defined in
`inst/rstudio/templates/project/create_project.dcf`, but can also be
called directly.

## Usage

``` r
create_project(path, createGitignore = TRUE, ...)
```

## Arguments

- path:

  Path to the project directory to create.

- createGitignore:

  If `TRUE` (default), writes a `.gitignore` with sensible defaults for
  R projects. Also supplied by the RStudio template wizard.

- ...:

  Additional named arguments collected from the RStudio template wizard.
  Currently ignored.

## Value

Invisibly returns `path`. Called for its side effect of creating the
project directory structure on disk.

## Examples

``` r
if (FALSE) { # \dontrun{
create_project(file.path(tempdir(), "my-analysis"))
} # }
```
