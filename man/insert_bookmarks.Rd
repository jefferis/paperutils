\name{extract_bookmarks}
\alias{extract_bookmarks}
\alias{insert_bookmarks}
\title{Insert and Extract bookmarks from a pdf}
\usage{
  extract_bookmarks(pdfin, bookmarks)

  insert_bookmarks(pdfin, bookmarks, pdfout = pdfin)
}
\arguments{
  \item{pdfin,pdfout}{Input and Output pdfs}

  \item{bookmarks}{Path to a bookmarks file in pdf format}
}
\value{
  location of pdftk bookmark file
  location of modified pdf
}
\description{
  Insert and Extract bookmarks from a pdf
}
\author{
  jefferis
}
\seealso{
  \code{\link{make_bookmark_text}}
}

