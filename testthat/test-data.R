library(testthat)

source('data.R')

# Since we're using static data immediately uploaded here, we can test the data quality
# during development. If we were to let the user upload test data of their own, we'd
# move some of this code and error handling into the code itself.

test_that('data_patients() has good data', {
  expect_equal(nrow(data_patients()), 452)
  expect_equal(colnames(data_patients()), c('STUDYID', 'USUBJID', 'country', 'id', 'AGE', 'SEX', 'RACE', 'ACTARM', 'ACTARMCD', 'BMRKR1', 'BMRKR2', 'screening_ALT', 'screening_CRP', 'screening_IGA'))
  
  expect_true(all(!is.na(data_patients()$country)))
  expect_true(all(!is.na(data_patients()$BMRKR2))) # Verify that the conversion to a factor went well
  
  expect_true(all(!is.na(data_patients()$id)))
  expect_equal(length(unique(data_patients()$id)), nrow(data_patients())) # Check that the id's are correct
})


test_that('data_labtests() has good data', {
  expect_equal(nrow(data_labtests()), 8136)
  expect_equal(colnames(data_labtests()), c('USUBJID', 'LBTESTCD', 'LBTEST', 'LBCAT', 'AVAL', 'AVALU', 'AVISIT', 'day'))
  
  expect_true(all(!is.na(data_labtests()$day)))
})


