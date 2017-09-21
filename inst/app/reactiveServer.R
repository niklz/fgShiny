# 1. Create a reactive data source, which works by periodically
# polling a non-reactive data source.
pollEmployee <- reactivePoll(1000, session,
                             # This function checks if any update to the employee table by returning...
                             checkFunc = function() {
                               #dbGetQuery(db(), "SELECT * FROM employee") # Ideally a timestamp field
                               file.info(dbfile)$mtime
                               # dbfile <- file.path('C:', 'Projects', 'Kraken', 'kraken',"kraken2.db") is defined as environment var
                             },
                             # This function returns the employee view
                             valueFunc = function() {
                               getEmployeeTable()
                             }
)

pollCustomer <- reactivePoll(1000, session,
                             # This function checks if any update to the customer table by returning...
                             checkFunc = function() {
                               file.info(dbfile)$mtime
                             },
                             # This function returns the customer view
                             valueFunc = function() {
                               getCustomerData()
                             }
)

pollJob <- reactivePoll(1000, session,
                        # This function checks if any update to the job table by returning...
                        checkFunc = function() {
                          file.info(dbfile)$mtime
                        },
                        # This function returns the job view
                        valueFunc = function() {
                          getRepoData()
                        }
)

pollPO <- reactivePoll(1000, session,
                        # This function checks if any update to the job table by returning...
                        checkFunc = function() {
                          file.info(dbfile)$mtime
                        },
                        # This function returns the job view
                        valueFunc = function() {
                          getPO()
                        }
)

pollInvoice <- reactivePoll(1000, session,
                        # This function checks if any update to the invoice table by returning...
                        checkFunc = function() {
                          file.info(dbfile)$mtime
                        },
                        # This function returns the invoice view
                        valueFunc = function() {
                          getInvoices()
                        }
)

pollSkillsMatrix <- reactivePoll(1000, session,
                                # This function checks if any update to the invoice table by returning...
                                checkFunc = function() {
                                  file.info(dbfile)$mtime
                                },
                                # This function returns the invoice view
                                valueFunc = function() {
                                  getSkillsMatrix()
                                }
)
# ============================================================
# 2. Create reactive expressions for individule variables.
# They are used in dropdowns across multiple widgets.
employeeName <- reactive({pollEmployee()$name %>% sort()})
customerName <- reactive({pollCustomer()$customer_name %>% sort()})
jobRef <- reactive({pollJob()$id})

# ============================================================
# 3. Define reactiveValues to be used on each screen
myReactive <- reactiveValues(
  index_job = -1,
  selectedJobId = NULL,

  index_invoice = -1,
  selectedInvoiceId = NULL,

  index_cust = -1,
  selectedCustId = NULL,

  index_emp = -1,
  selectedEmpId = NULL
)
