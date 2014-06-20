#' List all linked files from given Scribus file (sla)

#' @param x path to Scribus file.
#' @return Character vector of paths to linked files.
#' @export
linked_from_sla <- function(x) {
  # Read in Scribus file
  lines <- readLines(path.expand(x))
  linked_lines <- lines[grepl("PFILE=", lines)]
  links <- regexpr("PFILE=\".*?\"", linked_lines)
  
  # Extract PFILE key-value pairs
  link_pfiles <- rep(NA, length(links))
  for(i in 1:length(links)) {
    link_pfiles[i] <- substr(linked_lines[i], links[i], links[i]+attr(links, "match.length")[i]-1)
  }
  
  # Remove empty values
  link_pfiles <- link_pfiles[link_pfiles != "PFILE=\"\"" ]
  
  # Clean-up values to give paths
  linked_files <- sub("PFILE=\"", "", link_pfiles)
  linked_files <- sub("\"", "", linked_files)
  linked_files
}


#' Re-link all linked files in Scribus file in relative manner
#' 
#' @param x path to Scribus file.
#' @param writeout logical indicating whether to write out fixed Scribus file or
#'   character vector specifying path for file.
#' @export
fix_sla_links <- function(x, writeout=FALSE) {
  if(is.logical(writeout)) writeout=ifelse(writeout, x, '')
  
  lines <- readLines(path.expand(x))
  links <- linked_from_sla(x)
  strip_indices <- regexpr(dirname(x), links)
  patched_links <- links
  patched_links <- substr(patched_links, strip_indices + attr(strip_indices, "match.length") + 1, 100000)
  for(i in 1:length(links)) {
    lines <- gsub(links[i], patched_links[i], lines)
  }
  if(nzchar(writeout)){
    writeLines(lines, writeout)
    invisible(patched_links)
  } else patched_links
}
