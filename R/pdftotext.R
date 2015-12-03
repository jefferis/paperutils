#' Return absolute path to pdftotext binary
pdftotextbin<-function(){
  path=getOption('paperutils.pdftotext',Sys.which('pdftotext')[[1]])
  if(nchar(path)==0){
    stop("Cannot locate pdftotext Make sure that it is in your path or set", 
         " options(paperutils.pdftotext='/path/to/pdftotext')")
  }
  if(is.null(getOption('paperutils.pdftotext'))) 
    options(paperutils.pdftotext=path)
  path
}

#' Invoke pdftotext tool to extrat text from a PDF document
#' 
#' @param x Path to pdf
#' @param outfile Optional output file (defaults to <pdfstem.txt>)
#' @param first Optional page to start conversion
#' @param last Optional page to finish conversion
#' @param layout,table,raw pdftotext options (all default \code{FALSE})
#' @param ... additional arguments passed to \code{readLines} when
#'   \code{outfile=FALSE}
#' @details Run pdftotext -help for details
#' @return the return value of the system call or, if outfile has the signalling
#'   value of \code{FALSE}, the contents of the text file as a character vector.
#' @export
pdftotext<-function(x, outfile=NULL, first=NULL, last=NULL, layout=FALSE, 
                    table=FALSE, raw=FALSE, ...) {
  cmd=paste(pdftotextbin(), 
            ifelse(layout, "-layout",""),
            ifelse(raw, "-raw",""),
            ifelse(table, "-table",""))
  if(!is.null(first)) cmd=paste(cmd, first)
  if(!is.null(last)) cmd=paste(cmd, last)
  cmd=paste(cmd, shQuote(x))
  if(!is.null(outfile)) {
    if(is.logical(outfile) && !outfile){
      # use tempfile
      outfile=tempfile(fileext = '.txt')
      on.exit(unlink(outfile))
      system(paste(cmd, shQuote(outfile)))
      return(readLines(outfile, ...))
    }
    cmd=paste(cmd, shQuote(outfile))
  }
  system(cmd)
}
