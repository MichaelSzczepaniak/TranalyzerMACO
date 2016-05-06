# install packages if needed
list.of.packages <- c('RUnit', 'dplyr', 'readr') # c('testthat', 'dplyr', 'readr')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) install.packages(new.packages)
# load libraries
libs <- c('RUnit', 'dplyr', 'readr') # c('testthat', 'dplyr', 'readr')
lapply(libs, require, character.only=TRUE)  # load libs

# reference:
# http://www.johndcook.com/blog/2013/06/12/example-of-unit-testing-r-code-with-testthat/
# library('testthat')
# source('DataManager.R')
# test_dir('tests', reporter = 'Summary')

# reference:
# http://www.johnmyleswhite.com/notebook/2010/08/17/unit-testing-in-r-the-bare-minimum/
library('RUnit')
source('DataManager.R')
# change testFileRegex so it's more like the testthat convention
test.suite <- defineTestSuite('DataManager.R tests',
                              dirs = file.path('tests'),
                              testFileRegexp = '^(test).+\\.R')

test.result <- runTestSuite(test.suite)

printTextProtocol(test.result)