# ----------------------------------------------------------------------
# Unit tests
# ----------------------------------------------------------------------

# unit tests for the functions in stats.R

#source("../../R/stats.R")
#library(assertthat)
#library(testthat)


# -------------------------------------------------------------------------
# Tlag function
# -------------------------------------------------------------------------

context("tlag function")


test_that("tlag - default, numeric", {

    x <- 1:4
    d <- 1
    res <- tlag(x, d=d, na.rm=FALSE)

    expect_that( sum(is.na(res)), equals(d) )
    expect_that( min(res, na.rm=TRUE), equals(min(x)) )
    expect_that( max(res, na.rm=TRUE), equals(max(x)-d) )
    expect_that( res, is_a("integer") )
    expect_that( length(res), is_identical_to(length(x)) )
})



test_that("tlag - default, character", {

    x <- LETTERS[1:4]
    d <- 1
    res <- tlag(x, d=d, na.rm=FALSE)

    expect_that( sum(is.na(res)), equals(d) )
    expect_that( res, is_a("character") )
    expect_that( length(res), is_identical_to(length(x)) )
})


# -------------------------------------------------------------------------
# Rescale function
# -------------------------------------------------------------------------

context("rescale function")


test_that("rescale - default, to 0,1", {

    x <- 10:20
    res <- rescale(x)

    expect_that( min(res), equals(0) )
    expect_that( max(res), equals(1) )
    expect_that( res, is_a("numeric") )
    expect_that( length(res), is_identical_to(length(x)) )
})


test_that("rescale - mixed numbers, to 20,100", {

    x <- -10:10
    res <- rescale(x, 20, 100)

    expect_that( min(res), equals(20) )
    expect_that( max(res), equals(100) )
    expect_that( res, is_a("numeric") )
    expect_that( length(res), is_identical_to(length(x)) )
})


test_that("rescale - negative numbers, to 20,100", {

    x <- -20:-10
    res <- rescale(x, 20, 100)

    expect_that( min(res), equals(20) )
    expect_that( max(res), equals(100) )
    expect_that( res, is_a("numeric") )
    expect_that( length(res), is_identical_to(length(x)) )
})


test_that("rescale - negative numbers, to mixed interval", {

    x <- -20:-10
    res <- rescale(x, -1, 1)

    expect_that( min(res), equals(-1) )
    expect_that( max(res), equals(1) )
    expect_that( res, is_a("numeric") )
    expect_that( length(res), is_identical_to(length(x)) )
})


test_that("rescale - positive numbers, to negative interval", {

    x <- 10:20
    res <- rescale(x, -100, -1)

    expect_that( min(res), equals(-100) )
    expect_that( max(res), equals(-1) )
    expect_that( res, is_a("numeric") )
    expect_that( length(res), is_identical_to(length(x)) )
})


test_that("rescale - positive numbers, to mixed interval", {

    x <- 10:20
    res <- rescale(x, -10, 10)

    expect_that( min(res), equals(-10) )
    expect_that( max(res), equals(10) )
    expect_that( res, is_a("numeric") )
    expect_that( length(res), is_identical_to(length(x)) )
})


test_that("rescale - mixed numbers, to mixed interval", {

    x <- -10:20
    res <- rescale(x, -10, 10)

    expect_that( min(res), equals(-10) )
    expect_that( max(res), equals(10) )
    expect_that( res, is_a("numeric") )
    expect_that( length(res), is_identical_to(length(x)) )
})


test_that("rescale - non-default lower and upper", {

    x <- 0:10
    res <- rescale(x, 20, 100)

    expect_that( min(res), equals(20) )
    expect_that( max(res), equals(100) )
    expect_that( res, is_a("numeric") )
    expect_that( length(res), is_identical_to(length(x)) )
})


test_that("rescale - dealing with NAs", {

    # basic results
    x <- c(10:20, NA)
    resWna <- rescale(x)

    expect_that( min(resWna, na.rm=TRUE), equals(0) )
    expect_that( max(resWna, na.rm=TRUE), equals(1) )
    expect_that( resWna, is_a("numeric") )
    expect_that( length(resWna), is_identical_to(length(x)) )
    expect_that( any(is.na(resWna)), is_true() )

    resWOna <- rescale(x, na.rm = TRUE)

    expect_that( min(resWOna), equals(0) )
    expect_that( max(resWOna), equals(1) )
    expect_that( resWOna, is_a("numeric") )
    expect_that( length(resWOna), equals(length(x)-1) )
    expect_that( any(is.na(resWOna)), is_false() )
})


test_that("rescale - inputs that should throw error", {

    # some inputs that should fail
    expect_error( rescale() )
    expect_error( rescale(1:4,lower=NA) )
    expect_error( rescale(1:4,na.rm=NA) )
    expect_error( rescale(list(1,2,2)) )
    expect_error( rescale(matrix(1,2,2)) )
    expect_error( rescale(data.frame(1,2,2)) )
})



# -------------------------------------------------------------------------
# mod function
# -------------------------------------------------------------------------

context("mod function")


test_that("mod w/o freq - numeric vector, no ties", {

    # basic results
    x <- c(1,1,1,2,2)
    res <- mod(x)

    expect_that( res, is_identical_to(1) )
    expect_that( res, is_a("numeric") )
    expect_that( length(res), is_identical_to(1L) )
})


test_that("mod w/o freq - numeric vector, no ties, with NA's", {

    # should return 1 nevertheless
    x <- c(1,1,1,2,2,NA,NA,NA,NA)
    res <- mod(x)

    expect_that( res, is_identical_to(1) )
    expect_that( res, is_a("numeric") )
    expect_that( length(res), is_identical_to(1L) )
})


test_that("mod w/o freq - numeric vector, with ties", {

    # basic results
    x <- c(1,1,2,2)
    res <- mod(x)

    expect_that( res, is_identical_to(c(1,2)) )
    expect_that( res, is_a("numeric") )
    expect_that( length(res), is_identical_to(2L) )
})

test_that("mod w freq - numeric vector, with ties", {

    # basic results
    x <- c(1,1,2,2)
    res <- mod(x, freq = TRUE)

    expect_that( res$mod, is_identical_to(c(1,2)) )
    expect_that( res, is_a("list") )
    expect_that( res$mod, is_a("numeric") )
    expect_that( res$freq, is_a("integer") )
    expect_that( length(res$mod), is_identical_to(2L) )
    expect_that( length(res$freq), is_identical_to(1L) )
})

test_that("mod - inputs that should throw error", {

    # some inputs that should fail
    expect_error( mod() )
    expect_error( mod(c(1,2,2), NA) )
    expect_error( mod(list(1,2,2)) )
    expect_error( mod(matrix(1,2,2)) )
    expect_error( mod(data.frame(1,2,2)) )
})


