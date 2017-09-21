context("original tbls")

#' Convert an id column from character to integer
#' When a column contains NA, the type of which is character instead of integer
#'
#' @param tbl table that contains NA and NaN values
#' @param vars_id column that represents an id
dropNAstring <- function(tbl, vars_id){

  for(j in vars_id){
    tbl[, j] <- as.integer(tbl[, j])
  }

  return(tbl)

}

test_that("Validate that all tables exist in DB", {
  all_tbl <- listTable()
  expect_equal(all_tbl, c("billing_type", "business_unit",
                          "customer", "employee",
                          "industry", "invoice",
                          "job", "log",
                          "payment_method", "po",
                          "project_role", "role",
                          "role_rates", "status")
  )
})

test_that("Validate table: billing_type", {
  tbl <- getTable("billing_type")

  var <- sapply(tbl, class)
  dim <- dim(tbl)

  expect_equal(var,
               structure(c("integer", "character"), .Names = c("id", "name")
               )
  )
  expect_equal(dim, c(11L, 2L))

})


test_that("Validate table: business_unit", {
  tbl <- getTable("business_unit")

  var <- sapply(tbl, class)
  dim <- dim(tbl)

  expect_equal(var, structure(c("integer", "character"),
                     .Names = c("id", "name")
    )
  )
  expect_equal(dim, c(2L, 2L))

})

test_that("Validate table: customer", {
  tbl <- getTable("customer") %>%
    dropNAstring(vars = c("acc_mgr", "ind_id"))

  var <- sapply(tbl, class)
  num_col <- ncol(tbl)
  num_row <- nrow(tbl)

  expect_equal(var, structure(c("integer", "character", "integer", "integer"),
                    .Names = c("id", "name", "acc_mgr", "ind_id")))
  expect_equal(num_col, 4)
  expect_gte(num_row, 368)

})


test_that("Validate table: employee", {
  tbl <- getTable("employee")

  var <- sapply(tbl, class)
  num_col <- ncol(tbl)
  num_row <- nrow(tbl)

  expect_equal(var,
               structure(c("integer", "character", "character", "integer", "integer", "integer"),
                .Names = c("id", "name", "emp_no", "role_id", "contractor", "inactive")))
  expect_equal(num_col, 6)
  expect_gte(num_row, 46)

})

test_that("Validate table: job", {
  tbl <- getTable("job")

  var <- sapply(tbl, class)
  num_col <- ncol(tbl)
  num_row <- nrow(tbl)

  expect_equal(var, structure(c("integer", "character", "character",
                                "integer", "character", "integer",
                                "integer", "character", "character",
                                "character", "character", "integer",
                                "character", "integer", "integer",
                                "character", "character", "character",
                                "character", "character", "integer",
                                "character", "character", "character",
                                "character", "character", "integer",
                                "character", "character"),
                              .Names = c("id", "desc", "desc_long", "pay_term", "pay_method",
                                "requestor", "bill_type_id", "project_management", "learning",
                                "travel", "knowledge", "cust_id", "estimate_day_plan", "estimate_revenue_sales",
                                "bus_unit_id", "perc_strat", "perc_anal", "perc_appdev", "perc_env",
                                "perc_train", "status_id", "key_contact", "vat_no", "is_journyx",
                                "creation_date", "notes", "est_expenses_cap", "go_live_date",
                                "drawdown_method")))

  expect_equal(num_col, 29)
  expect_gte(num_row, 185)

})

test_that("Validate table: invoice", {
  tbl <- getTable("invoice")

  var <- sapply(tbl, class)
  num_col <- ncol(tbl)
  num_row <- nrow(tbl)

  expect_equal(var, structure(c("integer", "integer", "integer",
                                "character", "character", "character", "integer", "character",
                                "character", "numeric", "integer", "character", "integer", "character",
                                "character", "numeric", "integer", "integer", "character", "integer",
                                "numeric", "character", "character"),
                              .Names = c("id", "job_id",
                                "cust_id", "issued_by", "office", "xcharge", "myob", "client_po",
                                "link", "net_amount", "vat_perc", "currency", "pass_through",
                                "issue_date", "pay_date", "curr_ex_rt", "pub_cust_id", "is_credit",
                                "parent_invoice_id", "pay_method_id", "amount_paid", "ack_by",
                                "notes")))
  expect_equal(num_col, 23)
  expect_gte(num_row, 733)

})



test_that("Validate table: status", {
  tbl <- getTable("status")

  var <- sapply(tbl, class)
  dim <- dim(tbl)

  expect_equal(var,
               structure(c("integer", "character"), .Names = c("id", "name")
               )
  )
  expect_equal(dim, c(6L, 2L))

})

