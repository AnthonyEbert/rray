% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dim.R
\name{common-dim}
\alias{common-dim}
\alias{rray_dims_common}
\alias{rray_dim_common}
\title{Find common dimensions}
\usage{
rray_dims_common(...)

rray_dim_common(...)
}
\arguments{
\item{...}{Objects to find common dimension/dimensionality for.}
}
\description{
\itemize{
\item \code{rray_dims_common()} finds the common dimensionality
among a set of objects.
\item \code{rray_dim_common()} finds the common size of each dimension
of a set of objects.
}
}
\details{
\code{rray_dims_common()} essentially takes the maximum dimensionality of the
inputs. This is the "common" dimensionality. Below is an example of
comparing a 4x5 matrix and a 4x1x2 array to get the common dimensionality.\preformatted{(4, 5,  ) <- dimensionality of 2, but a 1 is implicit to match dimensionality
(4, 1, 2) <- dimensionality of 3
---------
3         <- resulting dimensionality is 3
}

\code{rray_dim_common()} finds the common dimensionality,
makes any implicit dimensions explicit, then recycles the size of each
dimension to a common size. With the above example, with implicit \code{1}
is made to be explicit, and then the dimensions are compared:\preformatted{(4, 5, 1) <- implicit 1 is made to be explicit, then recycled to 2
(4, 1, 2) <- the 1 in the second dimension here is recycled to 5
---------
(4, 5, 2) <- resulting common dim
}

The resulting dimensions from \code{rray_dim_common()} are the dimensions that
are used in broadcasted arithmetic operations.
}
\examples{

x_1_by_4 <- mtrx(1, 2, 3, 4)
x_5_by_1 <- mtrx(1:5)

# these are both 2D
rray_dims_common(x_1_by_4, x_5_by_1)

# recycle rows: 1 VS 4 = 4
# recycle cols: 5 VS 1 = 5
rray_dim_common(x_1_by_4, x_5_by_1)

x_5_by_1_by_3 <- rray(1, c(5, 1, 3))

# recycle rows:    1 VS 4 = 4
# recycle cols:    5 VS 1 = 5
# recycle 3rd dim: 1 VS 3 = 3
# (here, 3rd dim of 1 for the matrix is implicit)
rray_dim_common(x_1_by_4, x_5_by_1_by_3)

}
