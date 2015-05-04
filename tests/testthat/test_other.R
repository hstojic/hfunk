# ----------------------------------------------------------------------
# Unit tests
# ----------------------------------------------------------------------

# unit tests for the functions in other.R

#source("../../R/other.R")
#library(assertthat)
#library(testthat)


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
