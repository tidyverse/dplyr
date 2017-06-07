print("Sourcing helper")
if (any(grepl("testthat::test_dir", sapply(sys.calls(), as.character)))) {
  print(sys.calls())
  print("Enabling gctorture")
  gctorture2(9)
}
