\name{current_lyx_tempfile}
\alias{current_lyx_tempfile}
\title{Full path to most recent temporary pdf previewed by lyx}
\usage{
  current_lyx_tempfile(ftype = c("pdf", "aux", "dir", "tex", "lof"),
    lyxfile = NULL, tmproot = NULL)
}
\arguments{
  \item{ftype}{Type of file (or 'dir' for encolsing
  directory)}

  \item{lyxfile}{Optional stem or full name of lyx file}

  \item{tmproot}{Optional path to temporary directory
  containing LyX tempdir}
}
\value{
  Path to current lyx temporary directory /pdf
}
\description{
  Can also return e.g. aux file or containing directory.
}
\details{
  Uses the modification date of the tex file in the
  directory to choose lyx dir in case of ambiguity => last
  previewed file.
}
\examples{
\dontrun{
## current lyx directory
current_lyx_tempfile()
## make sure the file is main.lyx
current_lyx_tempfile(lyxfile='main.lyx')
}
}
\author{
  jefferis
}

