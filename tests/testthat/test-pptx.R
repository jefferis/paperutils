context("Test handling of PowerPoint files")

pptx="testdata/pptx/test_pdf.pptx"

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
})
