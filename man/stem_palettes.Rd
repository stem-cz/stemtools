% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/color-palettes.R
\name{stem_palettes}
\alias{stem_palettes}
\title{Select a Color Palette}
\usage{
stem_palettes(palette = "modern", n = NA)
}
\arguments{
\item{palette}{Name of a palette.}

\item{n}{Number of colors to extract. If \code{NA}, extracts all of them.}
}
\value{
A vector of color hexcodes
}
\description{
Returns a vector of colors forming a cohesive palette. Current palettes include:
\itemize{
\item Nominal: \code{gruvbox},
\item Diverging: \code{gruvbox_div}, \code{modern}, \code{bluered_light}
}
}
\examples{
stem_palettes(palette = "gruvbox")
stem_palettes(palette = "modern", n = 3)
}
