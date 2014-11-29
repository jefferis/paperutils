# Functions to help with pptx format files

#' Unzip a PowerPoint pptx to a temporary directory
unzip_pptx<-function(x, exdir=tempfile(pattern = basename(x))) {
  
  unzip(x, exdir = exdir)
  exdir
}

#' Zip up a temporary directory into a PowerPoint pptx file
#' 
#' @details Note that the temporary directory itself does not form part of the 
#'   resultant zip folder, which only includes the 3 folders underneath it
#'   (_rels, docProps, ppt)
#' @importFrom nat.utils abs2rel
zip_pptx_dir<-function(x, pptx, action=c("freshen", "update", "error"), files=NULL) {
  ext=tools::file_ext(pptx)
  if(ext=="") {
    pptx=paste0(pptx, ".pptx")
    message("Adding pptx extension to output file")
    ext="pptx"
  } else if(ext!="pptx") {
    stop("Invalid extension for output file!")
  }
  if(file.exists(pptx)){
    action=match.arg(action)
    if(action=='error')
      stop("Output file: ", pptx, " already exists! ",
           "Delete or choose another value for action")
    zipflags=paste0(formals(zip)$flags, substr(action,1,1))
  } else zipflags=formals(zip)$flags
  
  # make sure that pptx is absolute since we will change dir later
  outdir=tools::file_path_as_absolute(dirname(pptx))
  pptx=file.path(outdir, basename(pptx))
  
  owd=setwd(x)
  on.exit(setwd(owd))
  
  ff=if(!is.null(files)) {
    # nothing to do
    if(!length(files)) return(pptx)
    # check if specified files exist
    if(!all(file.exists(files))) stop("Some files do not exist")
    # convert these to paths relative to
    abs2rel(normalizePath(files))
  } else {
    ff=dir(recursive = T, all.files = T)
    Filter(function(f) basename(f)!=".DS_Store", ff) 
  }
  
  if(zip(zipfile = pptx, files = ff, flags = zipflags)!=0)
    stop("error making pptx zip file", pptx)
  pptx
}

convert_pptx_pdfs<-function(x, outpptx=NULL, pngres=300, ...) {
  if(file_test("-f", x)) {
    inpptx=x
    if(!is.pptx(x)) stop("This doesn't look like a pptx file!")
    if(is.null(outpptx))
      outpptx=paste0(tools::file_path_sans_ext(x),"_fixed.pptx")
    # make a copy of input and freshen that
    if(!file.copy(x, outpptx))
      stop("Unable to copy input file! Are you trying to overwrite? Bad idea!")
    x=unzip_pptx(x)
    on.exit(unlink(x, recursive = T))
  } else if(!file_test("-d", x)) {
    stop("x does not look a pptx file or an epxanded directory")
  }
  
  ## process all the pdfs using convert
  # first find them
  pdfs=dir(x, pattern = "^image.*.pdf$", full.names = T, recursive = T)
  message("There are ", length(pdfs), " pdfs")
  if(!length(pdfs))
    return(NULL)
  
  # now construct png paths
  # pngs should be called image(n+1).png
  pdfstems=tools::file_path_sans_ext(pdfs)
  n=as.integer(sub("^image([0-9]+)\\.pdf$", "\\1", basename(pdfs)))
  if(any(is.na(n))) stop("Unable to parse pdf names")
  pngs=file.path(dirname(pdfs), paste0("image", n+1, ".png"))
  if(!all(file.exists(pngs)))
    stop("PowerPoint should have already made pngs for all pdfs when you saved, but I can't find these!")
  
  # now actually convert
  successes=mapply(pdf2png, pdfs, pngs, res=pngres)
  if(!all(successes)) stop("Failed to convert some pdfs to png!")
  
  if(!is.null(outpptx))
    zip_pptx_dir(x, outpptx, files = pngs, ...)
}

is.pptx<-function(x, Verbose=TRUE) {
  if(!file_test("-f", x)) {
    if(Verbose) message(x, " is not a file!")
    return(FALSE)
  }
  ziplist=try(unzip(x, list = T), silent = T)
  if(inherits(ziplist, "try-error") || nrow(ziplist)<1) {
    if(Verbose) message("Unable to read from pptx zip file!")
    return(FALSE)
  }
  ziplist[1,'Name']=="[Content_Types].xml"
}

pdf2png<-function(pdf, png, res=300) {
  cmd=paste("convert -density",res, shQuote(pdf), shQuote(png))
  system(cmd)==0
}

# Utility function to check if two zip files are equal by content
# files can come in different and order and be compressed in a different way
all.equal.zip<-function(x, y) {
  normalised_zi<-function(f) {
    zi=zipinfo(f)
    # NB drop rownames after reordering
    as.matrix(zi[order(zi$Name),c("Name","CRC.32")], rownames.force=F)
  }
  
  all.equal(normalised_zi(x), normalised_zi(y))
}
