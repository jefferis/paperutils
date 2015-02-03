#' Return absolute path to pdftk binary
pdftk<-function(){
  path=getOption('paperutils.pdftk',Sys.which('pdftk')[[1]])
  if(nchar(path)==0){
    # pdftk is usually here on a mac
    if(!file.exists(path<-"/opt/pdflabs/pdftk/bin/pdftk"))
      stop("Cannot locate pdftk. Make sure that it is in your path or set", 
           " options(paperutils.pdftk='/path/to/pdftk')")
  }
  if(is.null(getOption('paperutils.pdftk'))) options(paperutils.pdftk=path)
  path
}

#' Return number of pages in pdf
#' 
#' @param pdf Path to input pdf file
#' @return number of pages
#' @author jefferis
#' @export
numpages<-function(pdf){
  t=system(paste(pdftk(),shQuote(pdf),"dump_data output -"),intern=TRUE)
  tt=grep('NumberOfPages',t,value=TRUE)
  return(as.integer(sub("NumberOfPages:[ ]*(\\d+)","\\1",tt,perl=TRUE)))
}

#' Extract pages from a pdf to new pdf(s)
#' 
#' @param pdfin,pdfout Path to input and output pdf
#' @param pages Integer vector or list. If named, the names specify output files
#' @param prefix a prefix to add to the start of all output pdf file names
#' @param DryRun Just say what would happen (when TRUE)
#' @param gscomp Compress with ghostscript
#' @param bookmarks Bookmarks info file (pdftk format) that can be used to add
#'   bookmarks to the output pdf.
#' @author jefferis
#' @export
#' @seealso \code{\link{gscompress}}
#' @examples
#' \dontrun{
#' aux='/path/to/myfile.aux'
#' pdffile='/path/to/myfile.pdf'
#' figurepages=findfigs(aux)
#' extractpdf(pdffile,figurepages,prefix='LastAuthor_',gscomp=TRUE)
#' }
extractpdf<-function(pdfin,pages,pdfout,prefix=NULL,DryRun=F,gscomp=FALSE,bookmarks=NULL){
  if(length(pages)==0) return(NULL)
  if(missing(pdfout)) {
    # outfiles should be names of pages vector
    if(length(names(pages))!=length(pages)) stop("specify outfile explicitly or as names of pages vector")
    pdfout=names(pages)
    if(!is.null(prefix)) pdfout=paste(prefix,pdfout,sep="")
  }
  if(length(pdfout)>1){
    if(length(pdfout)!=length(pages)) stop("must supply one outfile for each page")
    names(pages)=pdfout
    for(n in pdfout){
      extractpdf(pdfin,unlist(pages[n]),n,DryRun=DryRun,gscomp=gscomp)
    }
    return(length(pdfout))
  }
  cmd=paste(pdftk(),shQuote(pdfin),"cat",paste(pages,collapse=" "),"output",shQuote(pdfout))
  if(DryRun) {
    cat(cmd,"\n")
  }
  else {
    rval=RunCmdForNewerInput(cmd,pdfin,pdfout)
    if(rval){
      if(!is.null(bookmarks)){
        # add back bookmarks
        insert_bookmarks(pdfout,bookmarks,pdfout)
      }
      if(gscomp) gscompress(pdfout,pdfout)
    }
  }
}
