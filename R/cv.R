# Utilities related to creating a cv

#' Run bibtool command line utility
#' @param infile,outfile Input and Output files
#' @param ... Additional character arguments passed straight to bibtool 
#'   executable
#' @export
#' @examples
#' \dontrun{
#' # remove the Annote field
#' bibtool('in.bib', outfile='in-cleaned.bib', 'delete.field Annote')
#' }
bibtool<-function(infile, ..., outfile=NULL) {
  args=c(..., paste("-i", shQuote(path.expand(infile))))
  if(!is.null(outfile)) {
    args=c(args, paste("-o", shQuote(path.expand(outfile))))
  }
  system2(bibtool_path(), args=args)
}

# Return absolute path to bibtool binary
bibtool_path<-function(){
  path=getOption('bibtool',system('which bibtool',intern=TRUE))
  if(length(path)==0){
    stop("Cannot locate bibtool Make sure that it is in your path or set options(bibtool='/path/to/bibtool')")
  }
  if(is.null(getOption('bibtool'))) options(bibtool=path)
  path
}
