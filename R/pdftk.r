#' Return absolute path to pdftk binary
pdftk<-function(){
  path=getOption('pdftk',system('which pdftk',intern=TRUE))
  if(length(path)==0){
    stop("Cannot locate pdftk. Make sure that it is in your path or set options(pdftk='/path/to/pdftk')")
  }
  path
}
