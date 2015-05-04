# ----------------------------------------------------------------------
# A shortcut for negation of the %in% operator. 
# ----------------------------------------------------------------------
#' A shortcut for negation of the %in% operator. 
#' 
#' Using a negation of %in% operator in the standard format produces less readable code. More natural way of using negation in this context would be to prepend 'in' with standard negation symbol, '!', which then results in using shorter and more readable 'x %!in% y' instead of '!(x %in% y)'.
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
#' resultsNew <- x %!in% y
#' all(results == resultsNew)

'%!in%' <- function(x, y) !('%in%'(x, y))


# ----------------------------------------------------------------------
# Change class of multiple variables
# ----------------------------------------------------------------------
#' Change class of multiple variables in one go. 
#' 
#' Changing classes of many variables in the data frame simultaneously. The function also checks if one is converting a factor to numeric variable and it will make a correct
#' conversion without loss of digits.
#'
#' @param df A data frame.
#' @param newClass A character vector which specifies desired class of each
#' variable in \code{df}.
#' @return The data frame \code{df} where each column has a class as specified 
#' in \code{newClass}.
#' @seealso \code{\link{as.numeric}}, \code{\link{as.character}}, \code{\link{as.factor}} which this function wraps
#' @export
#' @examples
#' # check the classes of variables in mtcars dataset
#' sapply(mtcars, class)
#' # change them to something else
#' changeClass(mtcars, c("fac", rep("char",10)))


changeClass <- function(df, newClass) {
    # assert that input is of appropriate format
    assert_that(is.data.frame(df), dim(df)[2]==length(newClass), is.character(newClass))
    if (!all(newClass %in% c("num", "char", "fac"))) stop("One or more of new classes is not defined appropriately as either 'num', 'char' or 'fac'.")

    for(i in 1:length(newClass)) { 
        if (newClass[i] == "num") { 
            if(is.factor(df[,i])) {
                df[,i] <- as.numeric(levels(df[,i]))[df[,i]]
            } else {
                df[,i] <- as.numeric(df[,i])
            }
        } else if (newClass[i] == "char") {
            df[,i] <- as.character(df[,i])
        } else if (newClass[i] == "fac") {
            df[,i] <- as.factor(df[,i])
        }       
    }
    return(df)
}

