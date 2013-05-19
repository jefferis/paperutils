# utility functions for Adobe Illustrator files

#' List all linked files from given illustrator file (ai or pdf)
#' 
#' @param x Path to Illustrator file (ai,pdf)
#' @param AbsolutePaths Return abolute path to linked files
#' @param mustWork If true, check that linked files actually exist
#' @return Character vector of paths to linked files
#' @author jefferis
#' @export
#' @seealso ailinkedfiles
#' @family linked_from
#' @examples
#' lfs=linked_from_ai(system.file('lyx','composite_fig.pdf',package='paperutils'))
#' stopifnot(length(lfs)==2)
linked_from_ai<-function(x,AbsolutePaths=TRUE,mustWork=NA){
  ail=system.file('exec','ailinkedfiles.pl',package='paperutils',mustWork = TRUE)
  cmd=paste(ail,shQuote(x))
  rval=system(cmd,intern=TRUE)
  # now get rid of comments
  tc=textConnection(grep("^#",invert=T,value=T,rval))
  on.exit(close(tc))
  # and use scan to dequote
  ff=scan(tc,what='',quiet=TRUE)
  if(AbsolutePaths){
    owd=setwd(dirname(x))
    on.exit(setwd(owd),add=TRUE)
    ff=normalizePath(ff,mustWork=mustWork)
  }
  ff
}

#' pure R implementation of ailinkedfiles.pl
#' 
#' This is much slower than perl version for large files
#' This appears to be due to speed of readLines
#' @inheritParams linked_from_ai
#' @return Character vector of paths to linked files
#' @author jefferis
#' @export
#' @family linked_from
ailinkedfiles<-function(x){
  zz<-readLines(x)
  i=grep('%%DocumentFiles',zz,fixed=T,useBytes=T)
  if(!length(i)) return(character(0))
  ff=zz[i]
  while(length(grep('^%%+',l<-zz[i<-i+1],perl=T))) ff<-c(ff,l)
  sub("^%%(DocumentFiles:|\\+)","",ff)
}
