context("getCustomerData")

test_that("Validate deriviated table by getCustomerData()", {
  tbl <- getCustomerData()

  var <- sapply(tbl, class)
  num_col <- ncol(tbl)
  num_row <- nrow(tbl)

  expect_equal(names(tbl), c("id",
                             "customer_name",
                             "industry_name",
                             "account_manager_name")
  )
  expect_equal(var, structure(c("integer",
                                "character",
                                "character",
                                "character"),
                              .Names = c("id",
                                         "customer_name",
                                         "industry_name",
                                         "account_manager_name")
                              )
  )
  expect_equal(num_col, 4)
  expect_gte(num_row, 368)
})
