#' Return absolute path to pdftk binary
pdftk<-function(){
  path=getOption('pdftk',system('which pdftk',intern=TRUE))
  if(length(path)==0){
    stop("Cannot locate pdftk. Make sure that it is in your path or set options(pdftk='/path/to/pdftk')")
  }
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
#' @param pdfin Path to input pdf
#' @param pages Integer vector. If named, the names specify output files
#' @param outfile 
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
extractpdf<-function(pdfin,pages,outfile,prefix=NULL,DryRun=F,gscomp=FALSE,bookmarks=NULL){
  if(length(pages)==0) return(NULL)
  if(missing(outfile)) {
    # outfiles should be names of pages vector
    if(length(names(pages))!=length(pages)) stop("specify outfile explicitly or as names of pages vector")
    outfile=names(pages)
    if(!is.null(prefix)) outfile=paste(prefix,outfile,sep="")
  }
  if(length(outfile)>1){
    if(length(outfile)!=length(pages)) stop("must supply one outfile for each page")
    names(pages)=outfile
    for(n in outfile){
      extractpdf(pdfin,pages[n],n,DryRun=DryRun,gscomp=gscomp)
    }
    return(length(outfile))
  }
  cmd=paste(pdftk(),shQuote(pdfin),"cat",paste(pages,collapse=" "),"output",shQuote(outfile))
  if(DryRun) {
    cat(cmd,"\n")
  }
  else {
    rval=RunCmdForNewerInput(cmd,pdfin,outfile)
    if(rval){
      if(!is.null(bookmarks)){
        # add back bookmarks
        insert_bookmarks(outfile,bookmarks,outfile)
      }
      if(gscomp) gscompress(outfile,outfile)
    }
  }
}