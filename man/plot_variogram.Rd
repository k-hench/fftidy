% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ctmm_functions.R
\name{plot_variogram}
\alias{plot_variogram}
\title{modified plot.variogram from {ctmm}}
\usage{
plot_variogram(
  x,
  CTMM = NULL,
  level = 0.95,
  units = TRUE,
  fraction = 0.5,
  col = "black",
  col.CTMM = "red",
  xlim = NULL,
  ylim = NULL,
  ext = NULL,
  ...
)
}
\description{
The function was altered from ctmm:::plot.variogram()
}
\details{
It now returns the internally computed SVF (by using plot_svf - the altered version of ctmm:::plot.svf())
}
