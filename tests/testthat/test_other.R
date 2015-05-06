# ----------------------------------------------------------------------
# Unit tests
# ----------------------------------------------------------------------

# unit tests for the functions in other.R

#source("../../R/other.R")
#library(assertthat)
#library(testthat)


# ----
# mod function
# ----

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



# ----
# %!in% operator
# ----

context("%!in% operator")


test_that("%!in% operator - numeric vectors", {

    # basic results
    x <- 1:4
    y <- 3:6
    res <- !(x %in% y)
    resNew <- x %!in% y

    expect_that( resNew, is_identical_to(res) )
    expect_that( resNew, is_a("logical") )
    expect_that( length(resNew), is_identical_to(length(x)) )
    expect_that( resNew, is_identical_to( c(TRUE, TRUE, FALSE, FALSE) ) )
})


test_that("%!in% operator - character vectors", {

    # basic results
    x <- c("a", "b")
    y <- c("c", "b", "d")
    res <- !(x %in% y)
    resNew <- x %!in% y

    expect_that( resNew, is_identical_to(res) )
    expect_that( resNew, is_a("logical") )
    expect_that( length(resNew), is_identical_to(length(x)) )
    expect_that( resNew, is_identical_to( c(TRUE, FALSE) ) )
})


test_that("%!in% operator - logical vectors", {

    # basic results
    x <- c(TRUE, FALSE)
    y <- c(FALSE, FALSE, FALSE)
    res <- !(x %in% y)
    resNew <- x %!in% y

    expect_that( resNew, is_identical_to(res) )
    expect_that( resNew, is_a("logical") )
    expect_that( length(resNew), is_identical_to(length(x)) )
    expect_that( resNew, is_identical_to( c(TRUE, FALSE) ) )
})
