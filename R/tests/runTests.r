#!/usr/bin/env Rscript

library(RUnit)

test.suite <- defineTestSuite("unit-tests",
                              dirs = file.path("tests"),
                              testFileRegexp = ".*-tests\\.R")

test.result <- runTestSuite(test.suite)

printTextProtocol(test.result)
