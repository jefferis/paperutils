context("Test handling of PowerPoint files")

pptx="testdata/pptx/test_pdf.pptx"

test_that("is.pptx works", {
  expect_true(is.pptx(pptx))
  tf=tempfile(fileext = '.pptx')
  expect_false(is.pptx(tf, Verbose=F))
  writeLines("Not a pptx file", tf)
  expect_false(is.pptx(tf, Verbose=F))
})
