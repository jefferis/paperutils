context("cv")

if(nzchar(bibtool_path(mustWork=FALSE))){
test_that("clean bibtex file", {
  bibin=system.file("tests/testthat/testdata/bib/bibdesk.bib", package='paperutils')
  expect_is(bibout<-bibdesk_clean(bibin), 'character')
  # modulo line endings I guess
  expect_equivalent(tools::md5sum(bibout), "5451e1662921cddffd7cbc264ce8f587")
})
}
