library(testthat)

source('data.R')

# Since we're using static data immediately uploaded here, we can test the data quality
# during development. If we were to let the user upload test data of their own, we'd
# move some of this code and error handling into the code itself.

test_that('df.patients() has good data', {
  expect_equal(nrow(df.patients()), 452)
  expect_equal(colnames(df.patients()), c('STUDYID', 'USUBJID', 'country', 'id', 'AGE', 'SEX', 'RACE', 'ACTARM', 'ACTARMCD', 'BMRKR1', 'BMRKR2'))
  
  expect_true(all(!is.na(df.patients()$country)))
  expect_true(all(!is.na(df.labtests()$BMRKR2))) # Verify that the conversion to a factor went well
  
  expect_true(all(!is.na(df.patients()$id)))
  expect_equal(length(unique(df.patients()$id)), nrow(df.patients())) # Check that the id's are correct
})


test_that('df.labtests() has good data', {
  expect_equal(nrow(df.labtests()), 9492)
  expect_equal(colnames(df.labtests()), c('USUBJID', 'LBTESTCD', 'LBTEST', 'LBCAT', 'AVAL', 'AVALU', 'AVISIT', 'day'))
  
  expect_true(all(!is.na(df.labtests()$day)))
})


