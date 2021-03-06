% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/other.R
\name{changeClass}
\alias{changeClass}
\title{Change classes of multiple variables/columns in a data frame in one go.}
\usage{
changeClass(df, new)
}
\arguments{
\item{df}{A data frame.}

\item{new}{A character vector which specifies desired class of each
variable in \code{df}.}
}
\value{
The data frame \code{df} where each column has a class as specified 
in \code{new}.
}
\description{
Changing classes of many variables in the data frame simultaneously. The function also checks if one is converting a factor to numeric variable and it will make a correct conversion without loss of digits.
}
\examples{
# check the classes of variables in mtcars dataset
sapply(mtcars, class)

# change them to something else
changeClass(mtcars, c("fac", rep("char",10)))
}
\seealso{
\code{\link{as.numeric}}, \code{\link{as.character}}, \code{\link{as.factor}} which this function wraps
}

