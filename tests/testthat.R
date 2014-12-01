library(testthat)

if(.Platform$OS.type=="unix") {
  test_check("paperutils")
} else {
  message("Skipping tests on non-unix platform! Patches for Windows welcome.")
}
