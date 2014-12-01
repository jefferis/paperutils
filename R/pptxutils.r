# Functions to help with pptx format files

#' Unzip a PowerPoint pptx to a temporary directory
#' @param x Path to PowwerPoint pptx file
#' @param exdir Path to temporary diectory 
#' @return \code{exdir}
#' @export
unzip_pptx<-function(x, exdir=tempfile(pattern = basename(x))) {
  
  unzip(x, exdir = exdir)
  exdir
}

#' Zip up a temporary directory into a PowerPoint pptx file
#' 
#' @param x Path to directory containing expanded PowerPoint data
#' @param pptx Path to new output pptx
#' @param action What to do if \code{pptx} points to an existing file
#' @param files Optional character vector of files to freshen (when
#'   \code{action='freshen'})
#' @details Note that the temporary directory itself does not form part of the 
#'   resultant zip folder, which only includes the 3 folders underneath it 
#'   (_rels, docProps, ppt)
#' @importFrom nat.utils abs2rel
#' @export
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

#' Convert all the pdfs in PowerPoint presentation into higher res pngs
#' 
#' PowerPoint 2010 et al will convert any pdfs that have been dropped onto 
#' slides into bitmap pngs. Unfortuantely they do this at 72 dpi, which is 
#' typically much too low resolution. This function makes higher resolution 
#' versions of the pngs. Note that it will not upgrade pngs that already meet 
#' the requested resolution. If this means that no images at all are upgraded 
#' then the output file will be identical to the input.
#' 
#' @details This function depends on having the 
#'   \href{http://www.imagemagick.org/}{ImageMagick} \bold{convert} function in 
#'   the path.
#' @param x A PowerPoint pptx file or an expanded directory.
#' @param outpptx Path to output pptx file (default "inputfilestem_fixed.pptx")
#' @param pngres The resolution of the new png file
#' @param ... Additional arguments passed to \code{zip_pptx_dir}
#' @export
#' @seealso \code{\link{zip_pptx_dir}}
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
  pngs=character()
  if(length(pdfs)) {
    # now construct png paths if there are some pdfs
    # pngs should be called image(n+1).png
    pdfstems=tools::file_path_sans_ext(pdfs)
    n=as.integer(sub("^image([0-9]+)\\.pdf$", "\\1", basename(pdfs)))
    if(any(is.na(n))) stop("Unable to parse pdf names")
    pngs=file.path(dirname(pdfs), paste0("image", n+1, ".png"))
    if(!all(file.exists(pngs)))
      stop("PowerPoint should have already made pngs for all pdfs when you saved, but I can't find these!")
    
    # now actually convert
    pdf2pngwcheck<-function(pdf, png, res, ...) {
      if(pngres(png)<res){
        pdf2png(pdf=pdf, png=png, res=res, ...)
      } else NA
    }
    successes=mapply(pdf2pngwcheck, pdfs, pngs, res=pngres)
    # we only want to update the pngs we processed
    # nb which omits NAs
    pngs=pngs[which(successes)]
    if(!all(na.omit(successes))) stop("Failed to convert some pdfs to png!")
  }
  
  if(!is.null(outpptx))
    zip_pptx_dir(x, outpptx, files = pngs, ...)
}

#' Test if file is in pptx format
#' @inheritParams unzip_pptx
#' @param Verbose Whether to provide a user message on error.
#' @export
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

#' Convert pdf file to a png using imagemagick convert tool
#' 
#' @param pdf Path to input pdf
#' @param png Path to output png
#' @param res Output resolution in dpi
#' @return Logical value indicating success (or failure) of operation
#' @export
pdf2png<-function(pdf, png, res=300) {
  cmd=paste(convert(T), "-density",res, shQuote(pdf), shQuote(png))
  system(cmd)==0
}

# Utility function to check if two zip files are equal by content
# files can come in different and order and be compressed in a different way
all.equal.zip<-function(x, y, strict=FALSE) {
  normalised_zi<-function(f) {
    zi=nat.utils::zipinfo(f)
    # NB drop rownames after reordering
    as.matrix(zi[order(zi$Name),c("Name","CRC.32")], rownames.force=F)
  }
  if(strict) all.equal(nat.utils::zipinfo(x), nat.utils::zipinfo(y))
  else all.equal(normalised_zi(x), normalised_zi(y))
}

#' Return (horizontal) resolution in dots per inch of a png file 
#' @param x Path to a png file
#' @return numeric vector giving resolution in dots per inch
pngres<-function(x) {
  cmd=paste(convert(T),'-print "%x" ', shQuote(x), ' null:')
  x=system(cmd, intern = T)
  scan(text=x, n = 1, quiet = T)*2.54
}

convert<-function(mustWork=FALSE) {
  if(is.null(w<-getOption("paperutils.convert"))){
    w=Sys.which("convert")[[1]]
    # check this looks like ImageMagick convert
    if(!nzchar(w) || !length(grep("ImageMagick", system(w, intern=T)[1:4], fixed = T)))
      w=""
    options(paperutils.convert=w)
  }
  if(mustWork && !nzchar(w))
    stop("Cannot find ImageMagick convert command!",
         " See ?paperutils for how to specify location manually!")
  return(w)
}
