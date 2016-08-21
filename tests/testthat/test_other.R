# ----------------------------------------------------------------------
# Unit tests
# ----------------------------------------------------------------------

# unit tests for the functions in other.R

#source("../../R/other.R")
#library(assertthat)
#library(testthat)


# ----
# equal function
# ----

context("equal function")


test_that("equal - test for equality of elements in a vector", {

    x1 <- c(1,2,3)
    r1 <- FALSE
    res1 <- equal(x1)
    expect_that( res1, is_identical_to(r1) )
    expect_that( res1, is_a("logical") )
    expect_that( length(res1), is_identical_to(1L) )

    x2 <- c(1,NA,1)
    expect_error( equal(x2) )

    x3 <- c(1,1,1)
    r3 <- TRUE
    res3 <- equal(x3)
    expect_that( res3, is_identical_to(r3) )
    expect_that( res3, is_a("logical") )
    expect_that( length(res3), is_identical_to(1L) )

    x4 <- c(1,1,1.01)
    r4 <- TRUE
    res4 <- equal(x4, 0.1)
    expect_that( res4, is_identical_to(r4) )
    expect_that( res4, is_a("logical") )
    expect_that( length(res4), is_identical_to(1L) )
})


# ----
# streval function
# ----

context("streval function")


test_that("streval - simple addition", {

    # basic results
    x <- "1+2"
    res <- streval(x)
    check <- eval(parse(text=x))

    expect_that( res, is_identical_to(check) )
    expect_that( res, is_a("numeric") )
    expect_that( length(res), is_identical_to(1L) )
})



# ----
# %nin% operator
# ----

context("%nin% operator")


test_that("%nin% operator - numeric vectors", {

    # basic results
    x <- 1:4
    y <- 3:6
    res <- !(x %in% y)
    resNew <- x %nin% y

    expect_that( resNew, is_identical_to(res) )
    expect_that( resNew, is_a("logical") )
    expect_that( length(resNew), is_identical_to(length(x)) )
    expect_that( resNew, is_identical_to( c(TRUE, TRUE, FALSE, FALSE) ) )
})


test_that("%nin% operator - character vectors", {

    # basic results
    x <- c("a", "b")
    y <- c("c", "b", "d")
    res <- !(x %in% y)
    resNew <- x %nin% y

    expect_that( resNew, is_identical_to(res) )
    expect_that( resNew, is_a("logical") )
    expect_that( length(resNew), is_identical_to(length(x)) )
    expect_that( resNew, is_identical_to( c(TRUE, FALSE) ) )
})


test_that("%nin% operator - logical vectors", {

    # basic results
    x <- c(TRUE, FALSE)
    y <- c(FALSE, FALSE, FALSE)
    res <- !(x %in% y)
    resNew <- x %nin% y

    expect_that( resNew, is_identical_to(res) )
    expect_that( resNew, is_a("logical") )
    expect_that( length(resNew), is_identical_to(length(x)) )
    expect_that( resNew, is_identical_to( c(TRUE, FALSE) ) )
})
