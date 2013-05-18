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
