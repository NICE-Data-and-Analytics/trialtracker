testthat::test_that("clean_utf8 coerces to character and produces valid UTF-8", {
  x <- factor("hello")
  y <- clean_utf8(x)

  testthat::expect_type(y, "character")
  testthat::expect_true(all(nzchar(y)))

  # This is the robust check: the string is valid in UTF-8
  testthat::expect_false(any(is.na(iconv(y, from = "UTF-8", to = "UTF-8"))))

  # Optional: ensure it can be re-encoded (won't error, won't create NAs)
  y2 <- iconv(y, from = "", to = "UTF-8")
  testthat::expect_false(any(is.na(y2)))
})

testthat::test_that("fast_trunc truncates and adds ellipsis", {
  x <- paste(rep("a", 200), collapse = "")
  y <- fast_trunc(x, n = 10L)
  testthat::expect_true(nchar(y) <= 13) # 10 + ellipsis(3)
  testthat::expect_match(y, "...$", perl = TRUE)
})

testthat::test_that("truncate_str truncates and adds ellipsis", {
  x <- paste(rep("b", 50), collapse = "")
  y <- truncate_str(x, n = 20L)
  testthat::expect_true(nchar(y) <= 23)
  testthat::expect_match(y, "...$", perl = TRUE)
})

testthat::test_that("clean_text_pubmed returns non-character unchanged and strips control/format chars", {
  testthat::expect_identical(clean_text_pubmed(1:3), 1:3)

  x <- "hi\u200Bthere" # zero-width space (Cf)
  y <- clean_text_pubmed(x)
  testthat::expect_false(grepl("\u200B", y, fixed = TRUE))
})

testthat::test_that("coerce_pm_date converts numeric-like days to Date and non-numeric to NA", {
  x <- c("0", "1", "not", "", NA)
  d <- coerce_pm_date(x)
  testthat::expect_s3_class(d, "Date")
  testthat::expect_identical(d[[1]], as.Date("1970-01-01"))
  testthat::expect_identical(d[[2]], as.Date("1970-01-02"))
  testthat::expect_true(is.na(d[[3]]))
  testthat::expect_true(is.na(d[[4]]))
  testthat::expect_true(is.na(d[[5]]))
})
