# ----------------------------------------------------------------------
# Rescaling a vector of data. 
# ----------------------------------------------------------------------
#' Rescaling a numerical vector. 
#' 
#' A function for rescaling a vector of integers or real numbers to (0,1) range. Element in the original vector with the minimum value will be transformed to 0, while the element with the maximum value will be transformed to 1. All other elements will be rescaled proportionally.
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
#' # ties, with and without reporting frequency
#' rescale(x, 1, 10)


rescale <- function(x, lower=0, upper=1, na.rm = FALSE) { 
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

    # extract a list of nonNA elements in x 
    if (na.rm) x <- x[!is.na(x)]

    # function
    rescaled <- (x - min(x, na.rm=TRUE)) / 
                (max(x,na.rm=TRUE) - min(x, na.rm=TRUE))
    if (lower != 0 && upper != 1) {
        rescaled <- lower + (upper - lower)*rescaled
    }
    return(rescaled)
}



# ----------------------------------------------------------------------
# Lag function. 
# ----------------------------------------------------------------------
#' Creates a lagged variable of a vector. 
#' 
#' Creates a lagged variable of a numerical vector without requiring it to be of time series class.
#'
#' @param x A numeric vector that needs to be lagged.
#' @return A numeric vector of the same length as \code{x}.
#' @import assertthat
#' @export
#' @examples
#' # create some numeric vectors
#' x <- c(1,1,1,2,2)
#' y <- c(1,1,2,2)
#' 
#' # no ties, with and without reporting frequency
#' mod(x)
#' mod(x, freq = TRUE)
#'
#' # ties, with and without reporting frequency
#' mod(y)
#' mod(y, freq = TRUE)

tslag <- function(x, d = 1)
  {
    x <- as.vector(x)
    n <- length(x)
    c(rep(NA,d),x)[1:n]
  }



# ----------------------------------------------------------------------
# Mod function. 
# ----------------------------------------------------------------------
#' Computes a mod statistic. 
#' 
#' A function for finding the most frequent element in a vector and optionally its frequency. The intended use is not for estimating mod statistic, but for finding a mod in sequences where elements repeat themselves often. Hence, it should not be used with vectors of real numbers, as such elements are likely to be unique. Note that the function is not capable of breaking ties randomly.
#'
#' @param x A numeric, logical or a character vector where the mod needs to be found.
#' @param freq A logical that indicates whether frequency of elements in the mod should be returned as well. By default set to FALSE.
#' @param ties A string that indicates how the ties should be broken. Possible values are: "unbroken" in which case ties are not broken and all of the tied elements are returned in the output (in this case, if \code{freq} is set to TRUE, frequency output will be a scalar that indicates frequency of all tied elements), and "first" where only the first element is returned. By default set to "unbroken".
#' @param na.rm A logical that indicates whether NA elements should be removed or not. By default set to TRUE.
#' @return If \code{freq} set to FALSE, the output is a vector of the same class as \code{x} with a single element, indicating the mod of the vector provided in \code{x}. If \code{freq} set to FALSE, the output is a list with first element being the mod and second element being the frequency of the mod.
#' @import assertthat
#' @export
#' @examples
#' # create some numeric vectors
#' x <- c(1,1,1,2,2)
#' y <- c(1,1,2,2)
#' 
#' # no ties, with and without reporting frequency
#' mod(x)
#' mod(x, freq = TRUE)
#'
#' # ties, with and without reporting frequency
#' mod(y)
#' mod(y, freq = TRUE)

mod <- function(x, freq = FALSE, ties = "unbroken", na.rm = TRUE) {
    # basic checks of the inputs
    not_empty(x)
    assert_that(is.logical(freq))
    assert_that(is.logical(na.rm))
    assert_that(is.character(ties))
    assert_that(  # a num/char/logical vector
        !is.matrix(x),
        !is.list(x),
        !is.data.frame(x)
    )
    
    # extract a list of unique elements in x 
    if (na.rm) x <- x[!is.na(x)]
    ux <- unique(x)
    tab <- tabulate(match(x, ux))

    # find which is the most frequent, depending on type of breaking ties
    if (ties == "unbroken") {
        mod <- ux[tab == max(tab)]
    } else if (ties == "first") {
        mod <- ux[which.max(tab)]
    }
    
    # returning the mod, if freq=TRUE then also with number of elements in mod
    if (freq) {
        modFreq <- max(tab)
        return(list(mod = mod, freq = modFreq))
    } else {
        return(mod)
    }
}

