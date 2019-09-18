context("Test handling of bib files")

test_that("Read a BibDesk bib file", {
  skip_if_not(nzchar(bibtool_path(mustWork = F)))
  f='testdata/bib/bibdesk.bib'
  expect_is(read_bibdesk_bib(f), "BibEntry")
})
