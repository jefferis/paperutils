\name{gscompress}
\alias{gscompress}
\title{Compress a pdf using ghostscript}
\usage{
  gscompress(pdfin, pdfout, firstpage, lastpage,
    pdflevel = "1.5", Force = TRUE)
}
\arguments{
  \item{pdfin,pdfout}{Input and Output pdfs}

  \item{firstpage}{Start page (when omitted = 1)}

  \item{lastpage}{Last page (when omitted = last page in
  pdf)}

  \item{pdflevel}{Minium compatible pdf version of output
  file}

  \item{Force}{When FALSE, only update file for newer
  input}
}
\value{
  character vector with path to pdfout
}
\description{
  When pdfout is missing it defaults to <input>_gso.pdf
}
\details{
  This seems to work well for pdflatex output
}
\author{
  jefferis
}

