context("Test handling of LyX files")

test_that("Find files in current LyX directory", {
  lyxtmpdir=file.path(getwd(),'testdata','lyx_tmpdir')

  expect_equal(current_lyx_tempfile('dir',tmproot=lyxtmpdir),
               lyxtmpdir,
               info='lyx temporary directory')

  expect_equal(current_lyx_tempfile('pdf',tmproot=lyxtmpdir),
               file.path(lyxtmpdir,'test.pdf'),
               info='lyx temporary pdf')

  expect_equal(current_lyx_tempfile('aux',tmproot=lyxtmpdir),
               file.path(lyxtmpdir,'test.aux'),
               info='lyx temporary aux file')
  
})

test_that("Find files linked from LyX file", {
  lyxfile=file.path(getwd(),'testdata','lyx','test.lyx')
  linked_figs<-linked_from_lyx(lyxfile,AbsolutePaths=FALSE)
  baseline=c("fig1.pdf", "fig2.pdf", "composite_fig.pdf")
  expect_equal(length(linked_figs), 
              length(baseline),
              info='Correct number of linked files')
  expect_equal(linked_figs, baseline,
              info='Correct relative path to linked files')
  
  path_to_lyxdir=normalizePath(file.path(getwd(),'testdata','lyx'))
  linked_figs_abs=linked_from_lyx(lyxfile,AbsolutePaths=TRUE)
  baseline_abs=file.path(path_to_lyxdir,baseline)
  expect_equal(linked_figs_abs, baseline_abs,
              info='Correct absolute path to linked files')
})
