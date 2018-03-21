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
#'   \item paperutils.pdftk sets the path to the pdftk commandline tool used by
#'   \code{\link{extract_pdf}} and friends.
#'   
#'   \item paperutils.convert can be used to set the path to the ImageMagick 
#'   convert function used by \code{\link{convert_pptx_pdfs}} and friends.
#'   
#'   \item paperutils.bibtool sets thep path to the bibtool commandline tool 
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
