app <- ShinyDriver$new("../", loadTimeout = 1e+05)
app$snapshotInit("test_joined_file")

app$snapshot()
app$setInputs(tabs1 = "3", timeout_ = 10000)
app$snapshot()
