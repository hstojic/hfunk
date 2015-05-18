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
	
	mat <- matrix(runif(9), 3, 3)
    mat_eig <- eigen(mat)
	mat_sqrt <- mat_eig$vectors %*% diag(sqrt(mat_eig$values)) %*% solve(mat_eig$vectors)

    # expect_that( sum(is.na(res)), equals(d) )
    # expect_that( min(res, na.rm=TRUE), equals(min(x)) )
    # expect_that( max(res, na.rm=TRUE), equals(max(x)-d) )
    # expect_that( res, is_a("integer") )
    # expect_that( length(res), is_identical_to(length(x)) )
})

