# ----------------------------------------------------------------------
# matrix square roots. 
# ----------------------------------------------------------------------
#' Computes square root of a matrix. 
#' 
#' A function for rescaling a vector of integers or real numbers to an arbitrary interval. Element in the original vector with the minimum value will be transformed to a number deteremined by  \code{lower} argument (zero by default), while the element with the maximum value will be transformed to  a number deteremined by \code{upper} argument (one by default). All other elements will be rescaled proportionally.
#'
#' @param x A numeric vector where numbers need to be rescaled.
#' @param lower Desired minimum value, by deafult set to 0.
#' @param upper Desired maximum value, by deafult set to 1.
#' @param na.rm A logical that indicates whether NA elements should be removed from the output or not. By default set to FALSE. Note that, if it is set to FALSE, the rescale will still remove NA's to do the computations, but it will simply leave the NA elements in the resulting vector.
#' @return A numeric vector with rescaled elements from \code{x}.
#' @import assertthat
#' @export
#' @examples
#' # create some numeric vectors
#' x <- 1:10
#' 
#' # min element will be transformed to 0 and max element to 1
#' rescale(x)
#'
#' # arbitrary lower and upper value
#' rescale(x, 20, 100)


sqrtmat <- function(mat, maxit = 50) {
    # basic check of the input
    not_empty(x)
    assert_that(is.number(lower))
    assert_that(is.number(upper))
    assert_that(is.logical(na.rm))
    assert_that(  # a numeric vector
        is.numeric(x),
        !is.matrix(x),
        !is.list(x),
        !is.data.frame(x)
    )
    stopifnot(nrow(mat) == ncol(mat))
    niter <- 0
    y <- mat
    z <- diag(rep(1, nrow(mat)))
    for (niter in 1:maxit) {
      y.temp <- 0.5*(y + solve(z))
      z <- 0.5*(z + solve(y))
      y <- y.temp
    }
    return(list(sqrt = y, 
                sqrt.inv = z))
}



