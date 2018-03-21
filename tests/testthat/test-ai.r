context("Test handling of Adobe Illustrator files")

test_that("Find linked files in AI PDF", {
  linked<-linked_from_ai("testdata/lyx/composite_fig.pdf",mustWork=F)
  baseline=c("/GD/dev/R/paperutils/inst/lyx/fig2.pdf",
             "/GD/dev/R/paperutils/inst/lyx/fig1.pdf")
  expect_that(linked, equals(baseline))
  
  expect_that(ailinkedfiles("testdata/lyx/composite_fig.pdf"), equals(baseline))
})
