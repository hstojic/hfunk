# ----------------------------------------------------------------------
# Rep for matrices
# ----------------------------------------------------------------------
#' Replicate matrices.  
#' 
#' A simple wrapper for \code{\link{matrix}} that emulates repmat from Matlab. The function behaves similar to \code{matrix} function when a single scalar is specified - it multiplies that scalar to fill out a matrix specified by nrow and ncol arguments. The repmat function takes the whole matrix and treats it as if it were a scalar, and replicates it as many times as needed to fill out a new matrix specified by \code{nrow} and \code{ncol} arguments.
#'
#' @param mat A matrix.
#' @param nrow A scalar that determines how many times \code{mat} will be replicated and added as new rows, each of the size of \code{mat}. Default value is 1.
#' @param ncol A scalar that determines how many times \code{mat} will be replicated and added as new columns, each of the size of \code{mat}. Default value is 1.
#' @return A matrix where \code{mat} is copied \code{ncol} x \code{nrow} times and added as new columns as specified in \code{ncol} and new rows as specified in \code{nrow}.
#' @seealso \code{\link{matrix}}

#'
#' @export
#' @examples
#' mat <- matrix(c(1,2,3,4), 2,2)
#' repmat(mat, 2, 3)

repmat = function(mat, nrow=1, ncol=1) {
    if (class(mat) != "matrix") stop("Your input is not a matrix.")
    if (!is.numeric(nrow) | !is.numeric(ncol)) stop("nrow and ncol argument has to be numeric")
    if (length(nrow)>1 | nrow<1 | length(ncol)>1 | ncol<1) stop("nrow and ncol argument has to be a positive scalar.")
    m = dim(mat)[1]
    n = dim(mat)[2]
    newmat <- matrix(t(matrix(mat,m,n*ncol)),m*nrow,n*ncol,byrow=T)
    return(newmat)
} 

# ----------------------------------------------------------------------
# Rep for vectors
# ----------------------------------------------------------------------
#' A function that replicates a whole vector. 
#' 
#' A simple wrapper for \code{\link{rep}} that takes a vector and creates a matrix by replicating it certain number of times either as rows or as columns.
#'
#' @param vec A vector of any mode except list.
#' @param nrep A strictly positive scalar that determines how many times \code{vec} will be replicated and added as new rows or new columns, as indicated by \code{dim} argument.
#' @param dim A scalar with value of either 1 or 2. Value 1 will copy vector \code{vec} as rows, while value 2 will copy it as columns.
#' @return A matrix with \code{length(vec)} rows and \code{nrep} columns if \code{dim==2} and vice versa if \code{dim==1}.
#' @seealso \code{\link{rep}}
#'
#' @export
#' @examples
#' vec <- c(1,2,3,4)
#' repvec(vec, 4, 2)
#'
#' vec <- c("character", "vector")
#' repvec(vec, 3, 1)
#'
#' vec <- c(TRUE, FALSE, FALSE)
#' repvec(vec, 3, 2)

repvec<-function(vec, nrep, dim){
    if (!is.vector(vec)) stop("Your input is not a vector.")
    if (is.list(vec)) stop("List inputs are not allowed.")
    if ( !(dim %in% c(1,2)) ) stop("dim argument has to be either 1 or 2.")
    if (!is.numeric(nrep) ) stop("nrep argument has to be numeric.")
    if (length(nrep)>1 | nrep<1) stop("nrep argument has to be a positive scalar.")

    if (dim==1) {
        mat <- matrix(rep(vec, each=nrep), ncol=nrep, byrow=TRUE)
    } else if (dim==2) {
        mat <- matrix(rep(vec, each=nrep), nrow=nrep)
    }
    return(mat)
}


