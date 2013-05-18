#' @rdname insert_bookmarks
#' @return location of pdftk bookmark file
#' @export
extract_bookmarks<-function(pdfin,bookmarks){
  if(missing(bookmarks)) bookmarks=sub("\\.pdf$",".info",basename(pdfin))
  cmd=sprintf("%s %s  dump_data > %s",pdftk(),shQuote(pdfin),shQuote(bookmarks))
  RunCmdForNewerInput(cmd,pdfin,bookmarks)
  bookmarks
}

#' Insert and Extract bookmarks from a pdf
#' @param pdfin,pdfout Input and Output pdfs
#' @param bookmarks Path to a bookmarks file in pdf format
#' @return location of modified pdf
#' @author jefferis
#' @export
#' @seealso \code{\link{make_bookmark_text}}
insert_bookmarks<-function(pdfin,bookmarks,pdfout=pdfin){
  if(pdfin==pdfout){
    real_out=pdfout
    pdfout=tempfile(basename(pdfout),tmpdir=dirname(pdfout),fileext='.pdf')
    on.exit(file.rename(pdfout,real_out))
  }
  cmd=sprintf("%s %s  update_info %s output %s",pdftk(),shQuote(pdfin),
      shQuote(bookmarks),shQuote(pdfout))
  RunCmdForNewerInput(cmd,c(pdfin,bookmarks),pdfout)
  pdfout
}

#' Make a correctly formatted block of text defining a bookmark in pdftk format
#' 
#' @param title The title of the bookmark
#' @param level The level of the bookmark (1 is the highest)
#' @param page Absolute page that this refers to
#' @return String that can be written to bookmarks file
#' @author jefferis
#' @export
#' @seealso \code{\link{extract_bookmarks}}
#' @examples
#' make_bookmark_text('Figure 1',3,20)
make_bookmark_text<-function(title,level,page){
  sprintf("BookmarkBegin\nBookmarkTitle: %s\nBookmarkLevel: %d\nBookmarkPageNumber: %d",
      title,level,page)
}