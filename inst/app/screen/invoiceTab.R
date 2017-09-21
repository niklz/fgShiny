invoiceUI <- function(prefix = "addInvoice_") {

  column(12,
         fluidRow(
           column(4,
                  if(prefix == "edtInvoice_") {
                    list(
                         textInput("selectedInvoiceId", label="ID"),
                         textInput("selectedinvoiceNumber", label="Invoice Number")
                    )
                  },

                  selectInput(
                    paste0(prefix, "job_id"),
                    label = "Job Reference",
                    choices = c(Choose='')

                  ),
                  textInput(
                    paste0(prefix, "desc"),
                    label = "Job Description"
                  ),
                  textInput(
                    paste0(prefix, "cust"),
                    label = "Customer"
                  ),
                  dateInput(
                    paste0(prefix, "issue_date"),
                    label = "Issue Date",
                    value = lubridate::today()
                  ),
                  uiOutput(paste0(prefix, "uiselectClientPO")),
                  checkboxInput(paste0(prefix, "pass_through"), label = "Pass Through"),
                  checkboxInput(paste0(prefix, "is_credit"), label= "Credit Invoice"),
                  if (prefix == "addInvoice_") {
                    conditionalPanel("input.addInvoice_is_credit == true",
                                     uiOutput("addInvoice_parent_ui")
                    ) 
                  } else {
                    conditionalPanel("input.edtInvoice_is_credit == true",
                                     uiOutput("edtInvoice_parent_ui")
                    )
                  },                  
                  textareaInput(
                    paste0(prefix, "notes"),
                    label = "Notes",
                    value = "",
                    rows = 4
                  ),
                  # Custom function actionButton
                  div(style="display:inline-block", goButton(paste0(prefix, "Go"), 'Confirm', btn.style = "primary")),
                  shinyBS::bsTooltip(paste0(prefix, "Go"), "You must select a job number to save the result.",
                            "right", trigger = "hover", options = list(container = "body")),
                  div(style = "display:inline-block", actionButton(paste0(prefix, "Cancel"), 'Cancel')),
                  shinyBS::bsTooltip(
                    paste0(prefix, "Cancel"),
                    "Click Cancel to reset the fields.",
                    "right",
                    trigger = "hover",
                    options = list(container = "body")
                  )

           ),

            column(4,
              selectInput(
                paste0(prefix, "currency"),
                label = "Currency",
                choices = getCurrencies(),
                selected = "GBP"),
              numericInput(paste0(prefix, "net_in_original_curr"), label= "Net amount in original Currency", value = 0, min = 0, max = 1000000000),
              numericInput(paste0(prefix, "curr_ex_rt"), label= "Currency Exchange Rate", value = 1, min = 0, max = 1000000000),
              numericInput(paste0(prefix, "vat_perc"), label = "VAT(%)", value = 20, min = 0, max = 100),
		          if (prefix == "edtInvoice_") {
				         list(
                   dateInput(
                     paste0(prefix, "pay_date"),
                     label = "Payment Date",
                     value = NA),
                   numericInput(paste0(prefix, "amount_paid"),
                     label = "Amount Paid", value = 0, min = 0, max = 1000000000),
                   selectInput(
                     paste0(prefix, "payment_methods"),
                     label = "Payment Method",
                     choices =  c(Choose='', paymentTbl$name)
                   )
                 )
				      }
          ),

          column(4,
                 if (prefix == "addInvoice_") {
                   list(
                     uiOutput("totalAmountADD"),
                     uiOutput("leftToPayADD"),
                     br(),
                     actionButton("addBut", "View Invoices"),
                     bsModal("addPopup", "Invoicing for selected Job Number", "addBut", size = "large",
                             tableOutput("addInvoiceForJob"))
                   )
                 },
                 if (prefix == "edtInvoice_") {
                   list(
                     uiOutput("totalAmountEDT"),
                     uiOutput("leftToPayEDT"),
                     br(),
                     actionButton("edtBut", "View Invoices"),
                     bsModal("edtPopup", "Invoicing for selected Job Number", "edtBut", size = "large",
                             tableOutput("edtInvoiceForJob"))
                   )
                 }

          )
       )
  )
}


fluidRow(
  column(width = 12,
    tabBox(width = NULL, side = "left", id="invoice_tab",
      tabPanel("Invoice View",
               fluidRow(
                 column(9, helpText("Dear user, please select a row before going to the edit page.")),
                 column(3, downloadButton("invoice_csv", "Download CSV"), align = 'right'),
                 column(12, hr()),
                 column(12, 
                        div(style = 'overflow-x: scroll',
                            DT::dataTableOutput("InvoiceTable")
                        )                        
                 )
               )
      ),
      tabPanel("New Invoice",
               div(id = "addInvoice_page",
                   fluidRow(
                     invoiceUI(prefix = "addInvoice_")
                   )
               )
      ),
	    tabPanel("Edit Invoice",
	             div(id = "edtInvoice_page",
	                 fluidRow(
	                   invoiceUI(prefix = "edtInvoice_")
	                 )
	             ) %>% shinyjs::hidden()
	    )
	  )
  )
)
