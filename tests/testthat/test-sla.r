context("Test handling of Scribus files")

test_that("finding linked files in Scribus file works", {
  linked <- linked_from_sla("testdata/scribus/testdoc.sla")
  expect_equal(linked, "../lyx/fig1.pdf")
})
