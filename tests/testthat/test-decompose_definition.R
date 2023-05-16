test_that("Check that definitions are decomposed", {
  definition <-
    "1.4.1.2 1.1.1.399 2.8.3.12 4.2.1.167 7.2.4.5 1.3.1.109 (2.8.3.1,2.8.3.8)"
  actual <- decompose_definition(definition)
  expected <- c(
    "1.4.1.2", " ", "1.1.1.399", " ", "2.8.3.12", " ", "4.2.1.167", " ",
    "7.2.4.5", " ", "1.3.1.109", " ", "(", "2.8.3.1", ",", "2.8.3.8", ")"
  )
  expect_equal(actual, expected)
})
