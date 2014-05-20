\name{find_figs}
\alias{find_figs}
\title{Find location of figures or tables using latex auxfile}
\usage{
find_figs(auxfile, ftype = c("Fig", "Table"))
}
\arguments{
  \item{auxfile}{Path to aux file produced by latex (inc
  LyX)}

  \item{ftype}{Whether to look for figures or tables}
}
\value{
Named integer page number sequence for figures/tables
}
\description{
Find location of figures or tables using latex auxfile
}
\author{
jefferis
}

