# functions relating to bibliographies

#' Return full path to the biber (biblatex) cache or delete it to fix problems
#' 
#' \code{biber_cache} returns the path to the cache
#' 
#' @details Biblatex is a flexible alternative to bibtex for processing bibliographies. 
#' The most commonly used tool for processing biblatex is biber. Unfortunately 
#' biber has a bug that frequently results in failure to produce a bibliography.
#' see
#' \url{http://tex.stackexchange.com/questions/140814/biblatex-biber-fails-with-a-strange-error-about-missing-recode-data-xml-file}
#' for discussion.
#' @return \code{biber_cache} returns a character vector 
#' @family bib
#' @export
biber_cache<-function(){
  system2(biber(), "--cache", stdout = TRUE)
}

#' @rdname biber_cache
#' @description \code{remove_biber_cache} deletes cache to fix bib problems.
#' @return \code{remove_biber_cache} returns 0 for success, 1 for failure, invisibly.
#' @export
#' @seealso \code{\link{unlink}}
remove_biber_cache<-function() {
  unlink(biber_cache(), recursive = TRUE)
}

biber<-function(mustWork=TRUE){
  biber=Sys.which("biber")[[1]]
  if(!nzchar(biber)) stop("Cannot find biber in path!")
  biber
}

read_bib_comments <- function(x) {
  ll=readLines(x)
  starts=grep("^@comment", ll)
  if(!length(starts)) return(list())
  stops=grep("^}", ll)
  if(length(stops)!=length(starts))
    stop("Unable to match up comment start and end points!")
  res=mapply(function(x,y) ll[x:y], x=starts, y=stops, SIMPLIFY=FALSE)
  nn=sub("@comment\\{(.*)\\{", "\\1",ll[starts])
  names(res)=nn
  res
}


#' Read a BibDesk bib file after cleaning it to remove problem fields
#'
#' @param x Path to a BibDesk bib file
#'
#' @return See \code{\link[RefManageR]{ReadBib}}
#' @export
#'
#' @examples
#' \dontrun{
#' r=read_bibdesk_bib("~/Greg/ProfessionalAdmin/cv_lyx/JefferisPublications.bib")
#' citekeys=unname(names(r))
#' citekeys
#' }
#' @importFrom RefManageR ReadBib
read_bibdesk_bib <- function(x) {
  tf=tempfile(fileext = '.bib')
  on.exit(unlink(tf))
  bibdesk_clean(x, bibout = tf)
  ReadBib(tf)
}
