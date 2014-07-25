context("Test Ghostscript")

test_that("ghostscript can compress pdf", {
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

test_that('ghostscript can safely compress a pdf to same output',{
  # setup
  td=tempfile()
  #on.exit(unlink(td, recursive = T))
  dir.create(td)
  testpdf=tempfile(fileext = '.pdf', tmpdir = td)  
  aipdf=file.path('testdata','lyx','composite_fig.pdf')
  file.copy(aipdf,testpdf)
  
  gsopts="-sDocumentUUID=F9CFEB6D9AEF9F6B23236FED72C041AA -sInstanceUUID=F9CFEB6D9AEF9F6B23236FED72C041AA" 
  # compress to separate file
  expect_output(test1<-gscompress(testpdf, gsopts = gsopts),'pdfwrite')
  test1.md5=tools::md5sum(test1)
  
  # compress to same file
  expect_output(test2<-gscompress(testpdf,testpdf, gsopts = gsopts),'pdfwrite')
  expect_equal(normalizePath(test2),normalizePath(testpdf))
  test2.md5=tools::md5sum(test2)
  
  # gs add XMP metadata including UUID and timestamp so this doesn't work
  #expect_equal(test1.md5, test2.md5)
  expect_equal(file.info(test1)$size, file.info(test2)$size)
})
