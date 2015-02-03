# Utilities related to creating a cv

#' Run bibtool command line utility
#' @param infile,outfile Input and Output files
#' @param cmds Commands defining operations to be carried out by bibtool on the
#'   bibtex file.
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

# Clean up a bib file produced by BibDesk
bibdesk_clean<-function(bibin, bibout=NULL) {
  if(!inherits(try(bibtool_path()), 'try-error')) {
    # clean up annote field which can kill bibtex parser
    if(is.null(bibout)) 
      bibout=tempfile(pattern = basename(bibin), fileext = '.bib')
    rsc_file=system.file("bibtool","bibdesk-clean.rsc", package = 'paperutils', 
                         mustWork = TRUE)
    bibtool(bibin, outfile=bibout, args=paste("-r", shQuote(rsc_file)))
    nat.utils::touch(bibout, reference=bibin)
    bibout
  } else NA_character_
}

#' Fetch citation counts for references with google scholar ids
#' @param author_id The google scholar author id
#' @param bibin,bibout The input and output bibtex files. \code{bibout} defaults
#'   to \code{<bibin_stem>_scholarcites.bib}
#' @param clean Whether to remove difficult fields / clean up input file
#' @param Force Whether to insist on updating the output file (see \code{\link[nat.utils]{RunCmdForNewerInput}})
#' @importFrom scholar get_publications
#' @import RefManageR
#' @seealso \code{\link[nat.utils]{RunCmdForNewerInput}}
#' @examples
#' \dontrun{
#' add_scholar_cites_to_bib("cuXoCA8AAAAJ", 'mypubs.bib')
#' }
#' @export
add_scholar_cites_to_bib<-function(author_id, bibin, bibout=NULL, clean=TRUE,
                                   Force=FALSE) {
  bibin=path.expand(bibin)
  if(is.null(bibout))
    bibout=file.path(paste0(tools::file_path_sans_ext(bibin), "_scholarcites.bib"))
  
  tmp=bibdesk_clean(bibin)
  if(!is.na(tmp)){
    bibin=tmp
    on.exit(unlink(tmp))
  }
  
  update_required=nat.utils::RunCmdForNewerInput(NULL, infiles=bibin, outfiles = bibout, Force=Force)
  if(!update_required) return(invisible(NA_character_))
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
  invisible(bibout)
}
