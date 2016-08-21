# ----------------------------------------------------------------------
# Unit tests
# ----------------------------------------------------------------------

# unit tests for the functions in math.R

#source("../../R/math.R")
#library(assertthat)
#library(testthat)


# ----
# vecSum function
# ----

context("vecSum function")


test_that("vecSum - adding two vectors while ignoring NA's", {

    # create some test items
    x1 <- c(1,0,3)
    y1 <- c(1,2,3)
    r1 <- c(2,2,6)
    res1 <- vecSum(x1,y1)
    expect_that( res1, is_identical_to(r1) )
    expect_that( res1, is_a("numeric") )
    expect_that( length(res1), is_identical_to(3L) )

    x2 <- c(1,0,NA)
    y2 <- c(1,2,NA)
    r2 <- c(2,2,NA)
    res2 <- vecSum(x2,y2)
    expect_that( res2, is_identical_to(r2) )
    expect_that( res2, is_a("numeric") )
    expect_that( length(res2), is_identical_to(3L) )

    x3 <- c(NA,0,NA)
    y3 <- c(NA,2,NA)
    r3 <- c(NA,2,NA)
    res3 <- vecSum(x3,y3)
    expect_that( res3, is_identical_to(r3) )
    expect_that( res3, is_a("numeric") )
    expect_that( length(res3), is_identical_to(3L) )

    x4 <- c(1,0,NA)
    y4 <- c(1,NA, 1)
    r4 <- c(2,0,1)
    res4 <- vecSum(x4,y4)
    expect_that( res4, is_identical_to(r4) )
    expect_that( res4, is_a("numeric") )
    expect_that( length(res4), is_identical_to(3L) )

    x5 <- c(1,0,NA)
    y5 <- c(1,2, 1)
    r5 <- c(2,2,1)
    res5 <- vecSum(x5,y5)
    expect_that( res5, is_identical_to(r5) )
    expect_that( res5, is_a("numeric") )
    expect_that( length(res5), is_identical_to(3L) )
})


# -------------------------------------------------------------------------
# sqrtmat function
# -------------------------------------------------------------------------

context("sqrtmat function")


test_that("sqrtmat - default", {
	
	mat <- matrix(c(3,2,1,5,7,6,4,3,2), 3, 3)
    res <- sqrtmat(mat)

    expect_that(all(res$sqrt > 0), is_true() )
    # expect_that( min(res, na.rm=TRUE), equals(min(x)) )
    # expect_that( max(res, na.rm=TRUE), equals(max(x)-d) )
    # expect_that( res, is_a("integer") )
    # expect_that( length(res), is_identical_to(length(x)) )
})

test_that("sqrtmat - inputs that should fail", {
	
	mat <- matrix(runif(9), 3, 3)
	mat[1,1] <- NA
	expect_error( sqrtmat(mat) )
	
   
    # expect_that( sum(is.na(res)), equals(d) )
    # expect_that( min(res, na.rm=TRUE), equals(min(x)) )
    # expect_that( max(res, na.rm=TRUE), equals(max(x)-d) )
    # expect_that( res, is_a("integer") )
    # expect_that( length(res), is_identical_to(length(x)) )
})
