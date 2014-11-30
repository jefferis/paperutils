context("Test handling of PowerPoint files")

pptx="testdata/pptx/test_pdf.pptx"
pptx.nopdf="testdata/pptx/test_nopdf.pptx"

test_that("is.pptx works", {
  expect_true(is.pptx(pptx))
  tf=tempfile(fileext = '.pptx')
  expect_false(is.pptx(tf, Verbose=F))
  writeLines("Not a pptx file", tf)
  expect_false(is.pptx(tf, Verbose=F))
})

test_that("unzip_pptx works", {
  td=tempfile('pptxout')
  expect_equal(unzip_pptx(pptx, td), td)
  ff=dir(td, recursive = T, all.files = T)
  zd=unzip(pptx, list = T)
  expect_true(all(zd[,'Name']%in%ff))
  
  expect_equal(pngres(file.path(td, 'ppt', 'media', 'image2.png')), 72, tol=1e-3)
})

test_that("zip_pptx_dir works", {
  td=unzip_pptx(pptx)
  pptx2=tempfile(basename(pptx), fileext = '.pptx')
  expect_is(zip_pptx_dir(td, pptx2), "character")
  
  expect_true(all.equal.zip(pptx, pptx2))
  pptx3=tempfile(basename(pptx), fileext = '.pptx')
  file.copy(pptx, pptx3)
  zip_pptx_dir(unzip_pptx(pptx), pptx3, files=character())
  expect_true(all.equal.zip(pptx, pptx3))
  
  all_files=unzip(pptx3, list = T)$Name
  zip_pptx_dir(unzip_pptx(pptx), pptx3, files=all_files)
  expect_true(all.equal.zip(pptx, pptx3))
  
  expect_error(zip_pptx_dir(unzip_pptx(pptx), pptx3, action='error'))
})

test_that("convert_pptx_pdfs works", {
  # should be identical when no pdf involved
  tf=tempfile(fileext = '.pptx')
  expect_is(pptx.nopdf.conv<-convert_pptx_pdfs(pptx.nopdf, tf), "character")
  expect_true(all.equal.zip(pptx.nopdf, pptx.nopdf.conv))
  
  if(isTRUE(nzchar(convert()))){
    tf2=tempfile(fileext = '.pptx')
    expect_is(pptx.conv<-convert_pptx_pdfs(pptx, tf2), "character")
    expect_equal(all.equal.zip(pptx.conv, pptx), "1 string mismatch")
    
    # should be identical when res only set to 
    tf3=tempfile(fileext = '.pptx')
    expect_is(tf3<-convert_pptx_pdfs(pptx, tf3, pngres=72), "character")
    expect_true(all.equal.zip(tf3, pptx, strict=TRUE))
    # check stricy=TRUE works
    expect_true(all.equal.zip(pptx, pptx, strict=TRUE))
  }
})
