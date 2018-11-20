#' Elementwise equality
#'
#' `rray_equal()` provides elementwise equality that is also broadcastable.
#'
#' @param x,y Array-like objects, where at least one is an rray.
#'
#' @examples
#'
#' x <- rray(1:5)
#'
#' rray_equal(x, 4)
#'
#' # Broadcastable
#' y <- matrix(1:10, ncol = 2)
#' rray_equal(x, y)
#'
#' # Another broadcastable example,
#' # both dimensions are broadcasted
#' # Only the diagonals are TRUE
#' rray_equal(x, t(x))
#'
#' @export
rray_equal <- function(x, y) {
  rray_binary_op_base("equal", x, y)
}
