# ----------------------------------------------------------------------
# An improved listing of objects in the working memory. 
# ----------------------------------------------------------------------
#' A nicely formatted listing of objects in the R working memory. 
#' 
#' An improved listing of the objects available in R memory. Built-in function ls() does not provide information such as how much memory an object takes, which might be important if one is working with large objects. The function relies on internal function .ls_objects().
#'
#' @param n A scalar indicating how many objects to print in the output. By default 20 objects will be printed in decreasing order according to the memory they require.
#' @return A data frame with objects as rows and details about them in five variables: type, size, nicely formatted size, number of rows and number of columns in the object.
#' @seealso \code{\link{ls}}
#' @import assertthat
#' @export
#' @examples
#' # create some objects
#' x <- 1:4
#' y <- runif(10e6)
#'
#' # inspect the objects in the memory and their details
#' lsos()


lsos <- function(..., n = 20) {
    # basic assertions
    assert_that(is.scalar(n))
    # output
    .ls_objects(..., order_by = "Size", decreasing = TRUE, head = TRUE, n = n)
}


# ----
# internal function for lsos()
# ----

# taken from:
# http://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session
.ls_objects <- function (pos = 1, pattern, order_by,
                        decreasing = FALSE, head = FALSE, n = 5) {
    
    napply <- function(names, fn) sapply(names, function(x)
                                         fn(get(x, pos = pos)))
    names <- ls(pos = pos, pattern = pattern)
    obj_class <- napply(names, function(x) as.character(class(x))[1])
    obj_mode <- napply(names, mode)
    obj_type <- ifelse(is.na(obj_class), obj_mode, obj_class)
    obj_prettysize <- napply(names, function(x) {
                           capture.output(print(object.size(x), units = "auto")) })
    obj_size <- napply(names, object.size)
    obj_dim <- t(napply(names, function(x)
                        as.numeric(dim(x))[1:2]))
    vec <- is.na(obj_dim)[, 1] & (obj_type != "function")
    obj_dim[vec, 1] <- napply(names, length)[vec]
    out <- data.frame(obj_type, obj_size, obj_prettysize, obj_dim)
    names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
    if (!missing(order_by))
        out <- out[order(out[[order_by]], decreasing=decreasing), ]
    if (head)
        out <- head(out, n)
    return(out)
}
