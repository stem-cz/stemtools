% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots.R
\name{stem_plot_multiselect}
\alias{stem_plot_multiselect}
\title{Plot a set of multiselection choice items}
\usage{
stem_plot_multiselect(
  data,
  items,
  group = NULL,
  weight = NULL,
  item_reverse = FALSE,
  infreq_order = TRUE,
  backround_fill = "grey",
  backround_alpha = 1,
  ...
)
}
\arguments{
\item{data}{Dataframe including item (and group) variables}

\item{items}{Plotted items. Can be selected using Tidyselect's selection helpers.}

\item{group}{Optional; plotted grouping variable}

\item{weight}{Optional; survey weights}

\item{item_reverse}{Optional; should item order be reversed?}

\item{infreq_order}{Optional; should items be ordered in order of their frequency?}

\item{backround_fill}{Color of backround bars. Default is \code{grey}.}

\item{backround_alpha}{Alpha of backround bars. Default is \code{1}.}

\item{...}{Other arguments passed to \code{\link[=stem_plot]{stem_plot()}}}
}
\value{
A ggplot2 object with custom attribute "stem_plot"
}
\description{
Plot a set of multiselection choice items
}
\examples{
\dontrun{
stem_plot_multiselect(trust, items = dplyr::starts_with("biggest"))
}
}
