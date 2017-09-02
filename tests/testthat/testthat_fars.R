library(testthat)
library(fars)

## save path
l_getwd <- getwd()

## path with test files
setwd("../../data/")

## tests for function fars_read

#### positive tests
l_df <- fars_read("accident_2015.csv.bz2")
l_names_fars <- names(l_df)

test_that( "class tbl_df    :", { expect_equal(class(l_df)[1], "tbl_df") } )
test_that( "class tbl       :", { expect_equal(class(l_df)[2], "tbl") } )
test_that( "class data.frame:", { expect_equal(class(l_df)[3], "data.frame") } )

test_that( "field MONTH:   ", { expect_true( any(grepl("MONTH", l_names_fars))) } )
test_that( "field YEAR:    ", { expect_true( any(grepl("YEAR", l_names_fars))) } )
test_that( "field STATE:   ", { expect_true( any(grepl("STATE", l_names_fars))) } )
test_that( "field LATITUDE:", { expect_true( any(grepl("LATITUDE", l_names_fars))) } )
test_that( "field LATITUDE:", { expect_true( any(grepl("LATITUDE", l_names_fars))) } )

## negative tests
test_that( "error in fars_read:", {
  expect_that( fars_read("accident_2099.csv.bz2"), throws_error() ) } )


## tests for function fars_summarize_years

#### positive tests
l_df_summ <- fars_summarize_years( years=c(2015) )
l_names_fars_summ <- names(l_df_summ)

test_that( "class tbl_df    :", { expect_equal(class(l_df_summ)[1], "tbl_df") } )
test_that( "class tbl       :", { expect_equal(class(l_df_summ)[2], "tbl") } )
test_that( "class data.frame:", { expect_equal(class(l_df_summ)[3], "data.frame") } )

test_that( "field MONTH:   ", { expect_true( any(grepl("MONTH", l_names_fars_summ))) } )
test_that( "field 2015:    ", { expect_true( any(grepl("2015", l_names_fars_summ))) } )

#### negative tests
## test_that( "warning in fars_summ:", {
##   expect_that( fars_summarize_years( years=c(2099) ), gives_warning() ) } )


## tests for function fars_map_state
#### positive tests
test_that( "plot done:", { expect_equal( fars_map_state( 01, 2015 ), NULL ) } )

#### negative tests
test_that( "error in fars_map_state:", {
  expect_that( fars_map_state( 99, 2015 ), throws_error() ) } )
test_that( "error in fars_map_state:", {
  expect_that( fars_map_state( 01, 2099 ), throws_error() ) } )


## back to original path
setwd( l_getwd )
