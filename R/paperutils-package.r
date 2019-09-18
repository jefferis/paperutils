#' paperutils: PDF manipulation utilities (wrapping pdftk and ghostscript)
#' 
#' @section External dependencies:
#'   
#'   See the \href{https://github.com/jefferis/paperutils}{README}.
#'   
#' @section Package options:
#'   
#'   There are three package options at present:
#'   
#'   \itemize{
#'   
#'   \item paperutils.pdftk sets the path to the pdftk command line tool used by
#'   \code{\link{extract_pdf}} and friends.
#'   
#'   \item paperutils.convert can be used to set the path to the ImageMagick 
#'   convert function used by \code{\link{convert_pptx_pdfs}} and friends.
#'   
#'   \item paperutils.bibtool sets thep path to the bibtool command line tool 
#'   used by \code{\link{bibdesk_clean}} and friends.
#'   
#'   }
#'   
#' @seealso \code{\link{convert_pptx_pdfs}}, \code{\link{bibdesk_clean}},
#'   \code{\link{extract_pdf}}
#' @name paperutils
#' @docType package
#' @examples 
#' # Show state of elmr package options
#' options()[grep('^paperutils\\.', names(options()))]
NULL

dr_paperutils <- function() {
  if (!nzchar(bibtool_path(mustWork = F))) {
    usethis::ui_todo("Please install {ui_field('bibtool')} from https://ctan.org/pkg/bibtool")
  } else {
    usethis::ui_done('bibtool is installed')
  }
  if (!nzchar(biber(mustWork = F))) {
    usethis::ui_todo(
      paste(
        "Please install {ui_field('biber')} from https://ctan.org/pkg/biber",
        "(or via your favourite TeX distribution)"
      )
    )
  } else {
    usethis::ui_done('biber is installed')
  }
  
  if (!nzchar(Sys.which('gs'))) {
    usethis::ui_todo(
      paste(
        "Please install {ui_field('ghostscript')} (gs) via your favourite",
        "TeX distribution (e.g. mactex)"
      )
    )
  } else {
    usethis::ui_done('ghostscript (gs) is installed')
  }
  
  if (!nzchar(pdftotextbin(mustWork = F))) {
    usethis::ui_todo(
      paste(
        "Please install {ui_field('pdftotext')}",
        "eg from https://www.xpdfreader.com/"
      )
    )
  } else {
    usethis::ui_done('pdftotext is installed')
  }
  
}