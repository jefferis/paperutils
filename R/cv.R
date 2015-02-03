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
bibtool<-function(infile, ..., cmds=NULL, outfile=NULL) {
  args=c(..., paste("-i", shQuote(path.expand(infile))))
  if(!is.null(outfile)) {
    args=c(args, paste("-o", shQuote(path.expand(outfile))))
  }
  if(!is.null(cmds)) {
    args=c(cmds,"--", shQuote(paste(cmds, collapse=" ")))
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

#' Fetch citation counts for references with google scholar ids 
#' @importFrom scholar get_publications
#' @import RefManageR
#' @examples
#' \dontrun{
#' add_scholar_cites_to_bib("cuXoCA8AAAAJ", 'mypubs.bib')
#' }
add_scholar_cites_to_bib<-function(author_id, bibin, bibout=NULL, clean=TRUE) {
  if(is.null(bibout))
    bibout=file.path(paste0(tools::file_path_sans_ext(bibin), "_scholarcites.bib"))
  
  if(!inherits(try(bibtool_path()), 'try-error')) {
    # clean up annote field which can kill bibtex parser
    tmp=tempfile(pattern = basename(bibin), fileext = '.bib')
    on.exit(unlink(tmp))
    bibtool(bibin, outfile=tmp, 'delete.field Annote')
    bibin=tmp
  }
  r=ReadBib(bibin)
  df=get_publications(author_id)
  
  for(i in seq_along(r)){
    gsid=r[[i]]$googlescholarid
    if(!is.null(gsid)) {
      ml=match(r[[i]]$googlescholarid, df$id)
      if(!is.na(ml)) {
        r[[i]]$citationnum=df[ml,'cites']
      }
    }
  }
  WriteBib(r, bibout)
}
