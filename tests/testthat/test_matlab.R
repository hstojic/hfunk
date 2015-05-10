# ----------------------------------------------------------------------
# Unit tests
# ----------------------------------------------------------------------

# unit tests for the functions in other.R

#source("../../R/matlab.R")
#library(assertthat)
#library(testthat)


# ----
# rmat function
# ----

context("rmat function")


test_that("rmat - runif 2x3", {

    # basic results
    res <- rmat(runif, 2, 3)

    expect_that( dim(res), is_identical_to(c(2,3)) )
    expect_that( res, is_a("numeric") )
    expect_that( all(res >= 0 && res <= 1), is_true() )
})

