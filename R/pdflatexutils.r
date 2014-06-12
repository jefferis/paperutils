# functions to handle parsing of pdflatex files

#' Find location of figures or tables using latex auxfile
#' 
#' @param auxfile Path to aux file produced by latex (inc LyX)
#' @param ftype Whether to look for figures or tables
#' @return Named integer page number sequence for figures/tables
#' @author jefferis
#' @export
find_figs<-function(auxfile,ftype=c("Fig","Table")){
  ftype=match.arg(ftype)
  auxentry=ifelse(ftype=="Fig","lof","lot")
  t=readLines(auxfile)
  tt=t[grep(paste("{",auxentry,"}",sep=""),t,perl=TRUE)]
  figurefiles=sub(".*numberline {(\\d+)}.*}{\\d+}{.*",paste(ftype,"\\1.pdf",sep=""),tt,perl=TRUE)
  figurepages=as.integer(sub(".*numberline {\\d+}.*}{(\\d+)}{.*","\\1",tt,perl=TRUE))
  names(figurepages)=figurefiles
  na.omit(figurepages)
}

#' Find location of supplemental information (SI) using latex auxfile
#' 
#' This can be confused by entries in table of contents etc, in which case the
#' explicit parameter lastRegularPage can be passed.
#' @param auxfile Path to aux file produced by latex (inc LyX)
#' @param numpages Number of pages in pdf (assuming this is where SI stops)
#' @param lastRegularPage Last page before SI
#' @return Integer page number sequence for SI
#' @author jefferis
#' @export
find_supplemental<-function(auxfile,numpages,lastRegularPage){
  if(missing(lastRegularPage)){
    t=readLines(auxfile)
    tt=t[grep(paste("{(Supporting|Supplementa(l|ry)) Information}",sep=""),t,perl=TRUE,ignore.case=TRUE)]
    supplementalStart=as.integer(sub(".*{section}.*}{(\\d+)}{.*","\\1",tt,perl=TRUE))
    if(length(supplementalStart)==0) stop("Unable to locate SI")
    else if(length(supplementalStart)>1)
      supplementalStart=supplementalStart[length(supplementalStart)]
  } else {
    supplementalStart=lastRegularPage+1
  }
  seq(from=supplementalStart,to=numpages)
}
