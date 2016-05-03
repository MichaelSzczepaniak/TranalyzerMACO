# install packages if needed
list.of.packages <- c('testthat', 'dplyr', 'readr') # c('RUnit', 'dplyr', 'readr')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) install.packages(new.packages)
# load libraries
libs <- c('testthat', 'dplyr', 'readr') # c('RUnit', 'dplyr', 'readr')
lapply(libs, require, character.only=TRUE)  # load libs

# reference:
# http://www.johnmyleswhite.com/notebook/2010/08/17/unit-testing-in-r-the-bare-minimum/
library('testthat')
source('DataManager.R')
test_dir('tests', reporter = 'Summary')

# library('RUnit')
# source('DataManager.R')
# 
# test.suite <- defineTestSuite('DataManager.R tests',
#                               dirs = file.path('tests'),
#                               testFileRegexp = '^\\d+\\.R')
# 
# test.result <- runTestSuite(test.suite)
# 
# printTextProtocol(test.result)