# install packages if needed
list.of.packages <- c('RUnit', 'dplyr', 'readr', 'XML')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) install.packages(new.packages)
# load libraries
lapply(list.of.packages, require, character.only=TRUE)  # load libs

# testthat reference:
# http://www.johndcook.com/blog/2013/06/12/example-of-unit-testing-r-code-with-testthat/
# library('testthat')
# source('DataManager.R')
# test_dir('tests', reporter = 'Summary')

# reference:
# http://www.johnmyleswhite.com/notebook/2010/08/17/unit-testing-in-r-the-bare-minimum/

# change testFileRegex so it's more like the testthat convention
# test all tests
# test.suite <- defineTestSuite('Tranalyzer tests',
#                               dirs = file.path('tests'),
#                               testFileRegexp = '^(test).+\\.R',
#                               testFuncRegexp = '^(test.)')

# run tests that make YQL calls for quote data from finance.yahoo:
test.suite <- defineTestSuite('Tranalyzer tests',
                              dirs = file.path('tests'),
                              testFileRegexp = '^(test).+(Yql).+\\.R',
                              testFuncRegexp = '^(test.)')

# test only techinical indicators
# test.suite <- defineTestSuite('Tranalyzer tests',
#                               dirs = file.path('tests'),
#                               testFileRegexp = '^(testTechInd).+\\.R',
#                               testFuncRegexp = '^(test.)')

test.result <- runTestSuite(test.suite)

printTextProtocol(test.result)