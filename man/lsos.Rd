% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/workspace.R
\name{lsos}
\alias{lsos}
\title{A nicely formatted listing of objects in the R working memory.}
\usage{
lsos(n = 20)
}
\arguments{
\item{n}{A scalar indicating how many objects to print in the output. By default 20 objects will be printed in decreasing order according to the memory they require.}
}
\value{
A data frame with objects as rows and details about them in five variables: type, size, nicely formatted size, number of rows and number of columns in the object.
}
\description{
An improved listing of the objects available in R memory. Built-in function ls() does not provide information such as how much memory an object takes, which might be important if one is working with large objects. The function relies on internal function .ls_objects().
}
\examples{
# create some objects
x <- 1:4
y <- runif(10e6)

# inspect the objects in the memory and their details
lsos()
}
\seealso{
\code{\link{ls}}
}

