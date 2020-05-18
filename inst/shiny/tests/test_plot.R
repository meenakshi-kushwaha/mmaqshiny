app <- ShinyDriver$new("../", loadTimeout = 1e+05)
app$snapshotInit("test_plot")

app$setInputs(tabs1 = "2", timeout_=100000)
app$snapshot()
