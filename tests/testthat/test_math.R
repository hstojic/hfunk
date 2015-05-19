# ----------------------------------------------------------------------
# Unit tests
# ----------------------------------------------------------------------

# unit tests for the functions in math.R

#source("../../R/math.R")
#library(assertthat)
#library(testthat)


# -------------------------------------------------------------------------
# sqrtmat function
# -------------------------------------------------------------------------

context("sqrtmat function")


test_that("sqrtmat - default", {
	
	mat <- matrix(c(3,2,1,5,7,6,4,3,2), 3, 3)
    res <- sqrtmat(mat)

    expect_that(res$sqrt > 0, is_true() )
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
