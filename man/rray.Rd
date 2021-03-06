% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rray.R
\name{rray}
\alias{rray}
\title{Build a rray object}
\usage{
rray(x = numeric(0), dim = NULL, dim_names = NULL)
}
\arguments{
\item{x}{A numeric vector, matrix, or array to convert to an rray. It
is brodcast based on \code{dim} to other dimensions if requested.}

\item{dim}{An integer vector describing the dimensions of the rray. If \code{NULL},
the dimensions are taken from the existing object using \code{\link[vctrs:vec_dim]{vctrs::vec_dim()}}.}

\item{dim_names}{A list. For no names, \code{NULL}, in which case a list of
empty characters will be constructed. Otherwise the list must
be the same length as the total number of dimensions
(i.e \code{length(c(size, shape))}). Each element of the list much be either
a character vector the same size as the corresponding dimension, or
\code{character(0)} for no names for that dimension.}
}
\description{
Constructor for building rray objects. Existing vectors, matrices, and
arrays can be used to build the rray, and they can be broadcast up to
a different dimension using \code{dim}.
}
\details{
rray objects are never reduced to vectors when subsetting using \code{[} (i.e.
dimensions are never dropped).
}
\examples{

# 3 rows
rray(c(1,2,3), dim = c(3))

# 3 rows, 4 cols
rray(c(1,2,3), dim = c(3, 4))

# 3x2x4 array
rray(1, dim = c(3, 2, 4))

# from a matrix
mat <- matrix(c(1, 2, 3, 4), ncol = 2)
rray(mat)

# from a matrix, with broadcasting
rray(mat, dim = c(2, 2, 3))

# from an array, with broadcasting
arr <- array(1, c(1, 2, 2))
rray(arr, c(3, 2, 2))

# with row names
rray(c(1, 2, 3), c(3, 2), dim_names = list(c("x", "y", "z"), character()))

}
