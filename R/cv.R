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
bibtool_path<-function(mustWork=TRUE){
  path=getOption('bibtool', Sys.which('bibtool')[[1]])
  if(nzchar(path)){
    if(is.null(getOption('paperutils.bibtool'))) options(paperutils.bibtool=path)
  } else {
    if(mustWork)
      stop("Cannot locate bibtool! Make sure that it is in your path or set",
           " options(paperutils.bibtool='/path/to/bibtool')")
  }
  path
}

#' Clean up a bibtex file produced by BibDesk
#' 
#' Uses the command line \bold{bibtool} program with a custom resource file to 
#' remove over-long fields that can cause problems for bibtex parsers. These include the annote and b
#' 
#' @param bibin Path to input file
#' @param bibout Path to output file. The defacult value of \code{NULL} will 
#'   generate a temporary file.
#' @return A character vector containing the path to the output file or 
#'   NA_character_ when conversion fails
#' @export
#' @examples
#' rsc_file=system.file("bibtool","bibdesk-clean.rsc", package = 'paperutils')
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

#' Update citation counts in bib file for references with google scholar ids
#' 
#' This is a rather specific function designed for my (Greg's) CV, based on 
#' initial work by James Manton. The idea is that the bib file (probably 
#' produced by BibDesk) contains a list of publications with google ids 
#' specified in the bibtex field \code{googlescholarid}; at present these must 
#' all have one single google scholar author (denoted by \code{author_id}) as a 
#' co-author.
#' 
#' The function first cleans up the input bib file by removing long, irrelevant 
#' fields. It then fetches the publication list from google scholar for the 
#' specified \code{author_id} using \code{\link[scholar]{get_publications}} ( 
#' which returns a data.frame including the sholar publication ids and citation 
#' counts). This information is then merge with the bibtex file and new/updated 
#' citation counts are placed in the bibtex field \code{citationnum}.
#' 
#' By default the output is written to a new bibtex file called 
#' \code{<bibin_stem>_scholarcites.bib}. Note that this process is \emph{lossy} 
#' since some fields are dropped and therefore it is \bold{not} recommended to 
#' overwrite the original input file.
#' 
#' @param author_id The google scholar author id
#' @param bibin,bibout The input and output bibtex files. \code{bibout} defaults
#'   to \code{<bibin_stem>_scholarcites.bib}
#' @param clean Whether to remove difficult fields / clean up input file
#' @param Force Whether to insist on updating the output file (see 
#'   \code{\link[nat.utils]{RunCmdForNewerInput}})
#'   
#' @importFrom scholar get_publications
#' @import RefManageR
#'   
#' @seealso \code{\link[nat.utils]{RunCmdForNewerInput}}, 
#'   \code{\link[scholar]{get_publications}}
#'   
#' @examples
#' \dontrun{
#' add_scholar_cites_to_bib("cuXoCA8AAAAJ", 'mypubs.bib')
#' 
#' ## a sample rmarkdown chunk:
#' # nb the block should have options like:
#' # ```r bibstuff, echo=FALSE, results="hide", message=FALSE, warning=FALSE```
#' # to avoid potentially distracting messages.
#' library(paperutils)
#' add_scholar_cites_to_bib("cuXoCA8AAAAJ", "~/cv/JefferisPublications.bib")
#' # produces "~/cv/JefferisPublications_scholarcites.bib"
#' 
#' library(scholar)
#' gs_prof=get_profile("cuXoCA8AAAAJ")
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
