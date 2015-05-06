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
#' @return If \code{freq} set to FALSE, the output is a vector of the same class as \code{x} with a single element, indicating the mod of the vector provided in \code{x}. If \code{freq} set to FALSE, the output is a list with first element being the mod and second element being the frequency of the mod.
#' @param na.rm A logical that indicates whether NA elements should be removed or not. By default set to TRUE.
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
#' @import assertthat
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

