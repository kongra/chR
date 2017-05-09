# Copyright (c) Konrad Grzanek
# Created: 2017-05-09

context("chR")

test_that("ch works", {
  expect_equal (ch(is.character, "a"), "a")
  expect_error (ch(is.character,  1L))
  expect_true  (ch(is.character, "a", asPred = TRUE))
  expect_false (ch(is.character,  1L, asPred = TRUE))
})
