library(tidyverse)

# Publish cleaned data.
#
# The reason to keep the public data fetching (df.XXX()) as functions is that it
# generalizes well. In the future, if we were to let the user upload data files
# of their own, use a database or another API, we can easily swap implementations
# of this -- including with regular refreshes on top of cached/memoized implementations.
#
# I don't have a strong intuition on how important/well known the original column
# names are. I assume that these researchers live and breathe by them, so I will
# keep them as is. Column names that I generate myself will be lowercase, which
# makes for an implicit but clear separation between data original to the data
# in capital letters, and generated / added ones in lowercase.

df.patients <- function() {
  data_patients_read() %>%
    inner_join(data_labvalues_read() %>%
                 group_by(USUBJID) %>%
                 summarize(BMRKR1 = first(BMRKR1), BMRKR2 = first(BMRKR2)),
               by = 'USUBJID')
}


df.labtests <- function() {
  data_labvalues_read() %>%
    select(USUBJID, LBTESTCD, LBTEST, LBCAT, AVAL, AVALU, AVISIT, day)
}


data_patients_read <- function() {
  df.tmp <- suppressMessages(read_tsv('data/Random_PatientLevelInfo_2020.tsv')) %>%
    mutate(SEX = recode(SEX, UNDIFFERENTIATED = 'U')) %>%
    separate(USUBJID, into = c(NA, 'country', NA, NA, 'id'), remove = FALSE) %>%
    mutate(id = as.integer(id))
  
  df.tmp
}


data_labvalues_read <- function() {
  df.tmp <- suppressMessages(read_tsv('data/Random_LabValuesInfo_2020.tsv')) %>%
    mutate(BMRKR2 = ordered(BMRKR2, levels = c('LOW', 'MEDIUM', 'HIGH'))) %>%
    mutate(day = case_when(AVISIT == 'SCREENING' ~ -99,
                           AVISIT == 'BASELINE' ~ 1,
                           TRUE ~ str_extract(AVISIT, 'DAY [0-9]+') %>%
                             str_remove_all('[^0-9]') %>%
                             as.numeric()))
  
  df.tmp
}