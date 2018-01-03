context("Test handling of pdflatex pdfs")

test_that("Find figures pages from pdf", {
  lyxtmpdir=normalizePath(file.path('testdata','lyx_tmpdir'))
  expect_warning(figs<-find_figs(file.path(lyxtmpdir,'test.aux')),
                 regexp='NAs introduced',
                 info='warning due to omitted SI figure')
                
  baseline=structure(3:4, .Names = c("Fig1.pdf", "Fig2.pdf"))
  expect_equivalent(figs, baseline)
})
