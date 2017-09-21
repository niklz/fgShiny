context("getInvoices")

test_that("Validate deriviated table by getInvoices()", {

  tbl <- getInvoices()

  var <- sapply(tbl, class)
  num_col <- ncol(tbl)
  num_row <- nrow(tbl)

  expect_equal(var, structure(c("integer", "integer", "character",
                                "Date", "character", "numeric", "integer", "character", "character",
                                "integer", "Date", "numeric", "character", "numeric", "integer",
                                "character", "character", "character", "numeric", "character",
                                "numeric", "character", "character", "numeric", "character",
                                "numeric", "character", "character", "character", "integer",
                                "character", "character"), .Names = c("id", "job_id", "office",
                                                                      "issue_date", "client_po", "net_amount", "vat_perc", "link",
                                                                      "currency", "pass_through", "pay_date", "curr_ex_rt", "payment_methods",
                                                                      "amount_paid", "is_credit", "parent_invoice_id", "notes", "net_amount2",
                                                                      "vat", "pass_through2", "tol", "alt_currency", "invoice_number",
                                                                      "net_in_original_curr", "net_amount3", "net_alt_curr", "Customer",
                                                                      "bus_unit_name", "desc", "pay_term", "pay_method", "overdue")))

  expect_equal(num_col, 32)
  expect_gte(num_row, 733)

})

test_that("Validate Range of vat_perc is beyond [0, 100]", {
  tbl <- getInvoices()

  expect_true(range(tbl$vat_perc)[1] >= 0)
  expect_true(range(tbl$vat_perc)[2] <= 100)
})
