context("getRepoData")

test_that("Validate deriviated table by getRepoData()", {
  tbl <- getRepoData()

  var <- sapply(tbl, class)
  num_col <- ncol(tbl)
  num_row <- nrow(tbl)
  expect_equal(names(tbl), c("id", "desc", "desc_long", "pay_term",
                             "pay_method", "bill_type_id", "project_management", "learning",
                             "travel", "knowledge", "cust_id", "estimate_day_plan", "estimate_revenue_sales",
                             "bus_unit_id", "perc_strat", "perc_anal", "perc_appdev", "perc_env",
                             "perc_train", "status_id", "key_contact", "vat_no", "is_journyx",
                             "creation_date", "notes", "est_expenses_cap", "go_live_date",
                             "drawdown_method", "requestor_name", "billing_type_name", "status_name",
                             "customer_name", "industry_name", "account_manager_name", "bus_unit_name"
  ))
  expect_equal(var, structure(c("integer", "character", "character",
                                "integer", "character", "integer", "character", "character",
                                "character", "character", "integer", "character", "integer",
                                "integer", "character", "character", "character", "character",
                                "character", "integer", "character", "character", "character",
                                "Date", "character", "integer", "Date", "character", "character",
                                "character", "character", "character", "character", "character",
                                "character"), .Names = c("id", "desc", "desc_long", "pay_term",
                                                         "pay_method", "bill_type_id", "project_management", "learning",
                                                         "travel", "knowledge", "cust_id", "estimate_day_plan", "estimate_revenue_sales",
                                                         "bus_unit_id", "perc_strat", "perc_anal", "perc_appdev", "perc_env",
                                                         "perc_train", "status_id", "key_contact", "vat_no", "is_journyx",
                                                         "creation_date", "notes", "est_expenses_cap", "go_live_date",
                                                         "drawdown_method", "requestor_name", "billing_type_name", "status_name",
                                                         "customer_name", "industry_name", "account_manager_name", "bus_unit_name"
                                )))

  expect_equal(num_col, 35)
  expect_gte(num_row, 185)

})

