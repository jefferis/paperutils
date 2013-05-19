#' Compress a pdf using ghostscript
#' 
#' When pdfout is missing it defaults to <input>_gso.pdf
#' 
#' This seems to work well for pdflatex output
#' @param pdfin,pdfout Input and Output pdfs
#' @param firstpage,lastpage Page range (defaults to firstpage = 1, lastpage = last in pdf)
#' @param pdflevel Minium compatible pdf version of output file
#' @param Force When FALSE, only update file for newer input
#' @return character vector with path to pdfout
#' @author jefferis
#' @export
gscompress<-function(pdfin,pdfout,firstpage,lastpage,pdflevel="1.5",Force=TRUE){
  if(is.numeric(pdflevel)) pdflevel=as.character(pdflevel)
  if(missing(pdfout)) pdfout=sub("\\.pdf$","_gso.pdf",pdfin)
  
  if(pdfin==pdfout){
    real_out=pdfout
    pdfout=tempfile(basename(pdfout),tmpdir=dirname(pdfout),fileext='.pdf')
    on.exit(file.rename(pdfout,real_out))
  }
  
  basic_cmd=sprintf(paste('gs -sDEVICE=pdfwrite -dCompatibilityLevel=%s -dNOPAUSE',
          ' -dQUIET -dBATCH -dAutoRotatePages="/None"'),pdflevel)
  if(!missing(firstpage))
    basic_cmd=paste(basic_cmd,' -dFirstPage=',firstpage,sep="")
  if(!missing(lastpage))
    basic_cmd=paste(basic_cmd,' -dLastPage=',lastpage,sep="")
  
  cmd=paste(basic_cmd,sprintf('-sOutputFile=%s %s',shQuote(pdfout),shQuote(pdfin)))
  cat(cmd,"\n")
  RunCmdForNewerInput(cmd,pdfin,pdfout)
  return(pdfout)
}
