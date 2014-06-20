context("Test handling of Scribus files")

test_that("finding linked files in Scribus file works", {
  linked <- sla_linked_files("testdata/scribus/testdoc.sla")
  expect_equal(linked, "../lyx/fig1.pdf")
})
