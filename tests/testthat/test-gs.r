context("Test Ghostscript")

test_that("Find ghostscript can compress pdf", {
  aipdf=file.path('testdata','lyx','composite_fig.pdf')
  expect_output(out<-gscompress(aipdf),'pdfwrite',
                info='Call ghostscript to compress pdf')
  on.exit(unlink(out))
  baseline_out=sub('\\.pdf','_gso.pdf',normalizePath(aipdf))
  expect_equal(out,baseline_out,
               info='Correct path to default ouput file')
  
  expect_true(file.exists(out),
               info='Output file has been made')
  orig_size=file.info(aipdf)$size
  new_size=file.info(out)$size
  expect_true(new_size<orig_size,
              info='New size is less than original size after compression')
  
})
