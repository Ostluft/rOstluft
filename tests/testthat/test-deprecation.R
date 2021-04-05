test_that("deprecation works", {
  df1 <- tibble::tibble(x = as.factor(c("a", "b", "c")))
  df2 <- tibble::tibble(x = as.factor(c("d", "e", "c")))

  expect_warning(
    bind_rows_with_factor_columns(df1, df2),
    "was deprecated"
  )
})
