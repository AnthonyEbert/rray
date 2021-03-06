context("test-broadcast")

test_that("can broadcast dimension of 1 to N", {
  x <- as_rray(array(1, dim = c(1, 2)))
  expect_equal(vec_dim(rray_broadcast(x, c(5, 2))), c(5, 2))
})

test_that("broadcasting keeps original class (with vec_restore())", {
  x <- as_rray(array(1, dim = c(1, 2)))
  res <- rray_broadcast(x, c(1, 2))
  expect_is(res, "vctrs_rray")

  y <- as_mtrx(x)
})

test_that("can broadcast up to a new dimension", {
  x <- as_rray(array(1, dim = c(1, 2)))
  new_dim <- 2
  res <- rray_broadcast(x, c(1, 2, new_dim))
  expect_equal(vec_dim(rray_broadcast(x, c(1, 2, new_dim))), c(1, 2, new_dim))
})
