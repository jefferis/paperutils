#' List all linked files from given Scribus file (sla)

#' @param x path to Scribus file.
#' @return Character vector of paths to linked files.
sla_linked_files <- function(x) {
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