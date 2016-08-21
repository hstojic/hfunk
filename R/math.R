# ----------------------------------------------------------------------
# Adding two vectors. 
# ----------------------------------------------------------------------
#' Adding two vectors with special treatment of NA's. 
#' 
#' Addition of two vectors that will ignore NA's if one of the two numbers is NA, but will return NA if both are NA.
#'
#' @param x A numeric vector.
#' @param y A numeric vector.
#' @return A numeric vector of the same length as \code{x} and \code{y}.
#' @export
#' @examples
#' # create some vectors
#' x <- c(1,0,NA)
#' y <- c(1,NA,1)
#' z <- c(1,2,NA)
#'
#' # first addition will ignore NA's while second will produce NA
#' vecSum(x, y)
#' vecSum(x, z)

vecSum <- function(x, y) {

    stopifnot(length(x) == length(y), is.numeric(x), is.numeric(y))
    xna <- which(is.na(x))
    yna <- which(is.na(y))

    if (length(xna) > 0 && length(yna) > 0 && all(xna == yna)) {
        res <- x + y
    } else {
        x[xna] <- 0
        y[yna] <- 0
        res <- x + y
    }
    return(res)
}


# ----------------------------------------------------------------------
# matrix square roots. 
# ----------------------------------------------------------------------
#' Verify whether a matrix is a triangular matrix. 
#' 
#' @param L A numeric matrix.
#' @param method Function that extracts either a lower or upper triangular part of the matrix.
#' @param tol Tolerance for determining the differences between the elements of the matrix, required for real numbers.
#' @return A logical value, whether L is a triangular matrix or not.
#' @export
#' @examples
#' # create a non-triangular matrix
#' mat <- matrix(runif(9), 3, 3)
#' 
#' # is it a triangular matrix?
#' is.tri(mat)

is.tri <- function(L, method = lower.tri, tol = 1e-8) {
    
    # extract the triangular part
    triPart <- L[method(L)]

    # verify if all elements are close to zero 
    if (abs(max(triPart) - min(triPart)) < tol) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}


# ----------------------------------------------------------------------
# matrix square roots. 
# ----------------------------------------------------------------------
#' Computes square root of a matrix. 
#' 
#' A function for computing the square roots of a quadratic matrix using the Denman-Beavers algorithm. note that convergence is not guaranteed, even for matrices that do have square roots.
#'
#' @param mat A numeric matrix where numbers need to be rescaled.
#' @param maxit Number of iterations of the algorithm, by deafult set to 50.
#' @param tolerance Tolerance for the convergence, by deafult set to NA.
#' @param verbose Logical, set to TRUE if you used tolerance criterion and you wish to see the messages about convergence, by deafult set to FALSE.
#' @return A list with square root and inverse square root of \code{mat}.
#' @import assertthat
#' @export
#' @examples
#' # create a matrix
#' mat <- matrix(runif(9), 3, 3)
#' 
#' # taking a square root
#' sqrtmat(mat)


sqrtmat <- function(mat, maxit = 50, tolerance = NA, verbose = FALSE) {
    # basic check of the input
    not_empty(mat)
    assert_that(all(!is.na(mat)))
    assert_that(nrow(mat) == ncol(mat))
    assert_that(is.scalar(maxit))
    assert_that(is.matrix(mat))
    assert_that(is.numeric(mat))
    assert_that(is.logical(verbose))
    if (!is.na(tolerance)) assert_that(is.scalar(tolerance) && tolerance > 0)
    
    # the algorithm
    z <- diag(rep(1, nrow(mat)))
    for (niter in 1:maxit) {
        temp <- 0.5*(mat + solve(z))
        z <- 0.5*(z + solve(mat))

        # check for the convergence
        if(!is.na(tolerance)) {
            convergence <- all.equal(mat, temp, tolerance)
            if(convergence) {
                mat <- temp
                if (verbose) print("Converged!")
                break
            }
        }

        # assignment, if convergence criterion not met
        mat <- temp      
    }
    
    # report the mean relative difference if not converged
    if (!is.na(tolerance) && !convergence && verbose) cat("Not converged: ", convergence, "\n")
    return(list(sqrt = mat, 
                sqrtinv = z))
}



