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
