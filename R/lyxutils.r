#' Return all the files linked from a LyX file
#'
#' @details by default make paths absolute using normalizePath
#' @param x Path to LyX file
#' @inheritParams linked_from_ai
#' @examples
#' \dontrun{linked_from_lyx('/GD/LMBD/Papers/2012fruLHNs/lyx/main.lyx')}
#' linked_from_lyx(system.file('lyx','test.lyx',package='pdfutils'))
#' @author jefferis
#' @export
#' @family lyx,linked_from
linked_from_lyx<-function(x,AbsolutePaths=TRUE,mustWork=NA){
  ll=readLines(x)
  lf=grep('filename',value=TRUE,ll,useBytes=TRUE)
  ff=sub("^\\s*filename\\s+(.*)","\\1",lf)
  if(AbsolutePaths){
    owd=setwd(dirname(x))
    on.exit(setwd(owd))
    ff=normalizePath(ff,mustWork=mustWork)
  }
  ff
}

#' Full path to most recent temporary pdf previewed by lyx
#' 
#' Can also return e.g. aux file or containing directory.
#' @details Uses the modification date of the tex file in the directory to
#'   choose lyx dir in case of ambiguity => last previewed file.
#' @param ftype Type of file (or 'dir' for encolsing directory)
#' @param lyxfile Optional stem or full name of lyx file
#' @param tmproot Optional path to temporary directory containing LyX tempdir
#' @return Path to current lyx temporary directory /pdf
#' @author jefferis
#' @export
#' @family lyx
#' @examples
#' \dontrun{
#' ## current lyx directory
#' current_lyx_tempfile()
#' ## make sure the file is main.lyx
#' current_lyx_tempfile(lyxfile='main.lyx')
#' }
current_lyx_tempfile<-function(ftype=c('pdf','aux','dir','tex','lof'),lyxfile=NULL,tmproot=NULL){
  ftype=match.arg(ftype)
  # Two up from R's temp directory is place to start looking
  if(is.null(tmproot)) tmproot=dirname(dirname(tempdir()))
  patt="\\.tex$"
  if(!is.null(lyxfile)){
    # be more specific if we can
    patt=paste(sub("\\.(lyx|pdf)","",lyxfile),patt,sep='')
  }
  cand_files<-dir(tmproot,pattern=patt,full.names=TRUE,recursive=TRUE)
  # if(length(cand_files)==0) return(NA)
  if(length(cand_files)>1){
    # sort by mtime
    df=file.info(cand_files)
    cand_files=rownames(df[order(df$mtime,decreasing=TRUE)[1],])
  }
  res <- if(ftype=='dir') dirname(cand_files)
      else sub('tex$',ftype,cand_files)
  if(!length(res) || !file.exists(res)) stop("Unable to find requested lyx temp file. Please preview in LyX")
  res
}
