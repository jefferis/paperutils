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
biber_cache<-function(){
  system2(biber(), "--cache", stdout = TRUE)
}

#' @rdname biber_cache
#' @description \code{remove_biber_cache} deletes cache to fix bib problems.
#' @return \code{remove_biber_cache} returns a logical indicating success.
remove_biber_cache<-function() {
  unlink(biber_cache(), recursive = TRUE)
}

biber<-function(mustWork=TRUE){
  biber=Sys.which("biber")[[1]]
  if(!nzchar(biber)) stop("Cannot find biber in path!")
}
