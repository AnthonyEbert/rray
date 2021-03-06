#' Create a new mtrx
#'
#' Low level constructor for mtrx objects
#'
#' @inheritParams new_rray
#' @param shape A single integer specifying the number of columns in the mtrx.
#'
#' @examples
#'
#' mtrx_ex <- new_mtrx(1:10, size = 5L, shape = 2L, col_names = c("a", "b"))
#'
#' mtrx_ex
#'
#' @export
new_mtrx <- function(.data = numeric(0),
                     size = 0L,
                     shape = 1L,
                     dim_names = NULL,
                     ...,
                     subclass = character(0)) {

  # mtrx can only have 2 columns
  stopifnot(vec_size(shape) == 1L)
  stopifnot(vec_size(dim_names) <= 2L)

  new_rray(
    .data = .data,
    size = size,
    shape = shape,
    dim_names = dim_names,
    ...,
    subclass = c(subclass, "vctrs_mtrx")
  )

}



#' Build a mtrx object
#'
#' Helpful constructor for building mtrx objects. Pass in named vectors,
#' where each vector represents a single column in the mtrx. Length `1`
#' vectors are recycled.
#'
#' mtrx objects are never reduced to vectors when subsetting using `[` (i.e.
#' dimensions are never dropped).
#'
#' @param ... Vector that represent the columns of the mtrx. These must be
#' the same length, or can be length `1` in which case the vector will be
#' recycled. Vectors are combined with `vec_c()` which performs an implicit
#' cast to enforce a common type. Vectors can optionally be named, which
#' results in a named matrix. Otherwise, default names are generated.
#'
#' @param row_names A character vector of row names for the resulting mtrx.
#'
#' @examples
#'
#' mtrx(1:10)
#'
#' a_mtrx <- mtrx(a = 1:5, b = 6:10)
#'
#' a_mtrx
#'
#' # first column
#' a_mtrx[,1]
#' a_mtrx[,"a"]
#'
#' # first row
#' a_mtrx[1,]
#'
#' # single element
#' a_mtrx[1, 1]
#'
#' # multiple columns
#' a_mtrx[, c("a", "b")]
#'
#' @export
mtrx <- function(..., row_names = character()) {

  .dots <- dots_list(...)

  # for prototype (calculate n_cols after this so you
  # always have at least 1 col)
  if (is_empty(.dots)) {
    .dots <- list(numeric(0))
  }

  n_cols <- length(.dots)

  is_fully_named <- rlang::is_named(.dots)
  nms <- names2(.dots)

  if (any(nms != "")) {
    if (!is_fully_named) {
      abort("All inputs must be named, or none can be.")
    }
  }

  if (is_fully_named) {
    col_names <- nms
  } else {
    col_names <- character()
  }

  .dots <- unname(.dots)
  common_size <- vec_size_common(!!! .dots)

  mtrx_lst <- vec_recycle_common(!!! .dots, .size = common_size)
  mtrx_vec <- vec_c(!!! mtrx_lst)

  new_mtrx(
    .data = mtrx_vec,
    size = common_size,
    shape = n_cols,
    dim_names = list(row_names, col_names)
  )

}

# This is the only thing that would require the tibble dependency, so let's
# remove it for now and reevaluate later. Maybe we can add tibble::frame_matrix()
# as a native function here.

# #' Row-wise mtrx creation
# #'
# #' `mrtrx()` is the equivalent of [tibble::tribble()], but for mtrx objects.
# #' It allows for easy row-wise creation of mtrx objects, which is especially
# #' helpful for small mtrices where readability is key.
# #'
# #' @inheritParams mtrx
# #' @param ... Arguments specifying the structure of a mtrx. Column names should
# #' be formulas, and may only appear before the data.
# #'
# #' @examples
# #'
# #' mrtrx(
# #'   ~col1, ~col2,
# #'   1,     3,
# #'   5,     2
# #' )
# #'
# #' @export
# mrtrx <- function(..., row_names = character()) {
#   mat <- tibble::frame_matrix(...)
#   dim_names(mat) <- list(row_names, colnames(mat))
#   as_mtrx(mat)
# }
