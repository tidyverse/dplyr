print("Sourcing helper")
if (any(grepl("testthat::test_dir", sapply(sys.calls(), as.character)))) {
  print(sys.calls())
  print("Enabling gctorture")
  bindrcpp:::init_logging("VERB")
  gctorture2(33)
}
