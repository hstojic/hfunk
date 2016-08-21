# ----------------------------------------------------------------------
# Index of a max element
# ----------------------------------------------------------------------
#' Get the position of a max element, resolving the ties in a reliable way. 
#' 
#' If all elements are the same or if there is more than one max element, max element is chosen randomly. At the moment it cannot deal with NA's, an error will be issued.
#'
#' @param x A numeric vector.
#' @param tol A scalar, small value, required to test the equality of real numbers.
#' @return An integer denoting the position of a max element in vector \code{x}.
#' @export
#' @examples
#' # create some numeric vectors
#' x <- rep(1, 10)
#' y <- 1:10
#' z <- c(0, rep(1, 10))
#'
#' # get the index of max element
#' set.seed(1234)
#' idxMax(x)  # repeating will give different results
#' idxMax(y)
#' idxMax(z)  # repeating will give different results

idxMax <- function(x, tol = 1e-16) {
    stopifnot(is.numeric(x), is.numeric(tol), all(!is.na(x)))
    if (all(is.infinite(x)) || abs(max(x) - min(x)) < tol) {
        idx <- sample(1:length(x), 1)
    } else {
        idx <- which(x == max(x))
        if (length(idx) > 1) {
            idx <- sample(idx, 1) 
        }
    }
    return(idx)
}


# ----------------------------------------------------------------------
# Testing whether all elements in a vector are equal. 
# ----------------------------------------------------------------------
#' Testing whether all elements in a vector are equal. 
#' 
#' Tests whether all the elements in a numeric vector are equal. A special care is needed with such operations because we might be working with real numbers and this requires stating some threshold for declaring elements to be the same. If any of the elements are NA, the function will not work and the result will be an error. See also http://stackoverflow.com/questions/4752275/test-for-equality-among-all-elements-of-a-single-vector
#'
#' @param x A numeric vector.
#' @param tol A scalar, small value, required to test the equality of real numbers.
#' @return A logical value, TRUE, FALSE or NA.
#' @export
#' @examples
#' # create some vectors
#' x <- c(1,2,3)
#' y <- c(1,NA,1)
#' z <- c(1,1,1)
#' w <- c(1,1,1.01)
#'
#' # these produce logical values
#' equal(x)
#' equal(z)
#' equal(w, 0.1)
#' 
#' # running this will produce an error
#' # equal(y)

equal <- function(x, tol = 1e-16) {
    stopifnot(is.numeric(x), is.numeric(tol), all(!is.na(x)))
    abs(max(x) - min(x)) < tol
}


# ----------------------------------------------------------------------
# A shortcut for evaluating a string. 
# ----------------------------------------------------------------------
#' A shortcut for evaluating an expression in a string format. 
#' 
#' To evaluate an expression such as "1+1", we need to apply two function iteratively. streaval() function is a simple wrapper around eval() and parse() functions.
#'
#' @param x A character vector with a single element that contains an expression.
#' @return Result of an evaluation of the expression contained in the string.
#' @seealso \code{\link{eval}}, \code{\link{parse}}
#' @import assertthat
#' @export
#' @examples
#' # create some expression
#' x <- "1+2"
#'
#' # when evaluated it should produce 3
#' streval(x)
#'
#' # streval() function has the same output as:
#' eval(parse(text=x))

streval <- function(x) {return(eval(parse(text=x)))}


# ----------------------------------------------------------------------
# A shortcut for negation of the %in% operator. 
# ----------------------------------------------------------------------
#' A shortcut for negation of the \%in\% operator. 
#' 
#' Using a negation of \%in\% operator in the standard format produces less readable code. More natural way of using negation in this context would be to prepend 'in' with standard negation symbol, '!', which then results in using shorter and more readable 'x \%!in\% y' instead of '!(x \%in\% y)'. However, since R does not allow using '!' in function names, we use a special keyword 'nin' instead of '!in'.
#'
#' @param x A data structure whose elements will be checked whether they are also present in data structure \code{y}.
#' @param y A data structure that will be checked whether elements from \code{x} are present in it.
#' @return A logical vector of equal length as \code{x}, with element(s) not being present in \code{y} as TRUE. If \code{x} was a matrix, then the output corresponds to \code{x} converted to a vector.
#' @export
#' @examples
#' # create some numeric vectors
#' x <- 1:4
#' y <- 3:6
#'
#' # standard form
#' results <- !(x %in% y)
#'
#' # new abbreviated format
#' resultsNew <- x %nin% y
#' all(results == resultsNew)

'%nin%' <- function(x, y) !('%in%'(x, y))


# ----------------------------------------------------------------------
# Change type of multiple variables
# ----------------------------------------------------------------------
#' Change classes of multiple variables/columns in a data frame in one go. 
#' 
#' Changing classes of many variables in the data frame simultaneously. The function also checks if one is converting a factor to numeric variable and it will make a correct conversion without loss of digits.
#'
#' @param df A data frame.
#' @param new A character vector which specifies desired class of each
#' variable in \code{df}.
#' @return The data frame \code{df} where each column has a class as specified 
#' in \code{new}.
#' @seealso \code{\link{as.numeric}}, \code{\link{as.character}}, \code{\link{as.factor}} which this function wraps
#' @import assertthat
#' @export
#' @examples
#' # check the classes of variables in mtcars dataset
#' sapply(mtcars, class)
#' 
#' # change them to something else
#' changeClass(mtcars, c("fac", rep("char",10)))


changeClass <- function(df, new) {
    # assert that input is of appropriate format
    assert_that(is.data.frame(df), dim(df)[2]==length(new), is.character(new))
    if (!all(new %in% c("num", "char", "fac", "e"))) stop("One or more of new classes is not defined appropriately as either 'num', 'char', 'fac' or 'e'.")

    for(i in 1:length(new)) { 
        if (new[i] == "num") { 
            if(is.factor(df[,i])) {
                df[,i] <- as.numeric(levels(df[,i]))[df[,i]]
            } else {
                df[,i] <- as.numeric(df[,i])
            }
        } else if (new[i] == "char") {
            df[,i] <- as.character(df[,i])
        } else if (new[i] == "fac") {
            df[,i] <- as.factor(df[,i])
        } else {
            df[,i] <- df[,i]
        }      
    }
    return(df)
}

