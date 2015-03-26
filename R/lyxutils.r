#' Return all the files linked from a LyX file
#'
#' @details by default make paths absolute using normalizePath
#' @param x Path to LyX file
#' @inheritParams linked_from_ai
#' @examples
#' \dontrun{
#' linked_from_lyx('/GD/LMBD/Papers/2012fruLHNs/lyx/main.lyx')
#' linked_from_lyx(system.file('tests','testthat','testdata','lyx','test.lyx',package='paperutils'))
#' }
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
#' # current lyx pdf
#' current_lyx_tempfile()
#' # current lyx directory
#' current_lyx_tempfile('pdf')
#' # make sure the file is main.lyx
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

#' Export a LyX file to another format (by default xhtml)
#' 
#' @param x The input file
#' @param outfile Path to output file (defaults to <infilestem>.<format>)
#' @param format The LyX shortname for the format (see details)
#' @details Look in Tools->Preferences->File Handling->File Formats->Short Name 
#' to see which parameter (which differs from the format name in the
#' File->Export menu) should be passed as format.
#' @export
#' @family lyx
lyxexport <-function(x, outfile=NULL, format="xhtml") {
  if(is.null(outfile)) {
    outfile=paste(tools::file_path_sans_ext(x),sep=".", format)
  }
  rval=lyx(c("-batch","--export-to", format, shQuote(outfile), shQuote(x)))
  if(rval!=0) stop("error running lyxexport")
  invisible(outfile)
}

# Find lyx binary
lyxbin<-function() {
  lyx=if(Sys.info()[['sysname']] == "Darwin") {
    "/Applications/LyX.app/Contents/MacOS/lyx"
  } else {
    Sys.which('lyx')[[1]]
  }
  if(!nzchar(lyx) || !file.exists(lyx)) stop("Can't find lyx binary!")
  lyx
}

# Call lyx command line tool
lyx<-function(args, ...) {
  system2(lyxbin(), args, ...)
}


#' Convert a LyXHTML file to an html file that can be opened by Word
#' 
#' LyXHTML is currently (LyX 2.1 series) the highestq quality html export 
#' available by default. However, it cannot be opened by word immediately
#' because it interposes a small xml type header before the html content. This
#' function removes that header and renames the file with a .html extension by
#' default.
#' @param infile path to xhtml file
#' @param outfile path to output html file (defaults to <infilestem>.<html>)
#' @family lyx
#' @export
#' @importFrom XML xmlParse saveXML xmlChildren
#' @examples 
#' \dontrun{
#' lyxfile=system.file('tests/testthat/testdata/lyx/test.lyx', package='paperutils')
#' lyxhtml=tempfile(pattern = basename(lyxfile), fileext = '.xhtml')
#' lyxexport(lyxfile, outfile=lyxhtml)
#' lyxhtml2html(lyxhtml)
#' }
lyxhtml2html<-function(infile, outfile=NULL){
  if(is.null(outfile)) 
    outfile=paste0(tools::file_path_sans_ext(infile), ".html")
  x=XML::xmlParse(infile)
  # cut off the header 
  saveXML(xmlChildren(x)[[2]], file = outfile)
}
