\name{linked_from_lyx}
\alias{linked_from_lyx}
\title{Return all the files linked from a LyX file}
\usage{
  linked_from_lyx(x, AbsolutePaths = TRUE, mustWork = NA)
}
\arguments{
  \item{x}{Path to LyX file}

  \item{AbsolutePaths}{Return abolute path to linked files}

  \item{mustWork}{If true, check that linked files actually
  exist}
}
\description{
  Return all the files linked from a LyX file
}
\details{
  by default make paths absolute using normalizePath
}
\examples{
\dontrun{linked_from_lyx('/GD/LMBD/Papers/2012fruLHNs/lyx/main.lyx')}
linked_from_lyx(system.file('lyx','test.lyx',package='paperutils'))
}
\author{
  jefferis
}

