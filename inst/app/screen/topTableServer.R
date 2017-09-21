# Disabling this text input because it should not be changed
shinyjs::disable("selectedInvoiceId")
shinyjs::disable("selectedinvoiceNumber")
shinyjs::disable("addInvoice_desc")
shinyjs::disable("addInvoice_cust")
shinyjs::disable("edtInvoice_desc")
shinyjs::disable("edtInvoice_cust")

invoice_format <- reactive({
  # Get rid of id for the view but id still exists under the hood!
  pollInvoice() %>%
    select(invoice_number, Customer, bus_unit_name, job_id, desc,
           is_credit, issue_date, client_po, net_amount3, alt_currency,
           net_alt_curr, vat_perc, vat, tol, pass_through2,
           overdue, pay_date, pay_term, notes) %>%
    mutate(vat = formatC(vat, digits=2, format= "f", big.mark= ",", drop0trailing=TRUE)) %>%
    mutate(tol = formatC(tol, digits=2, format= "f", big.mark= ",", drop0trailing=TRUE)) %>%
    mutate(net_alt_curr = formatC(net_alt_curr, digits=2, format= "f", big.mark= ",", drop0trailing=TRUE)) %>%
    mutate(net_alt_curr = sub("^ *(NA|NaN)*", "", net_alt_curr)) %>%
    mutate(job_id = as.character(job_id)) %>% # convert to characterto to allow searching
    mutate(is_credit = ifelse(is_credit==1, "Yes", "No")) %>%
    mutate(invoice_number = createLink(invoice_number))
    
})

output$InvoiceTable <- renderDataTable({
  invoice_format() %>%
    datatable(colnames = c("Invoice Number", "Customer", "Business Unit", "Job Reference",
                           "Job Description", "Is Credit", "Date Issued", "Client PO/Authority Code",
                           "Net Amount (£)", "Alternative Currency", "Net in Alternative Currency", "VAT Percentage",
                           "VAT", "Total Amount (inc. VAT)", "Pass Through", "Overdue",
                           "Payment Date", "Payment Term", "Notes"),
              selection = "single", 
              extensions = 'Buttons', 
              rownames = FALSE, 
              escape = FALSE,
              filter = list(position = "top", clear = FALSE),
              #options = list(lengthMenu = 10,
              #               dom = 'Bfrtip',
              #               buttons = list(list(extend="csv", filename="Invoices_view"),
              #                               "copy")
              #)
              options = list(lengthMenu = c(5, 10, 100), dom = 'lftip')
    )
}, server = TRUE) # server processing to speed up the view

InvoiceProxy = dataTableProxy("InvoiceTable")

# Download the filtered data. Not using the download buttons because server = TRUE. 
output$invoice_csv = downloadHandler('Invoices_view_server.csv', content = function(file) {
  rows = input$InvoiceTable_rows_all
  write.csv(invoice_format()[rows, ], file, row.names = FALSE)
}) 

# Observe widgets that have reactive selected/value updates
observe({
  updateSelectInput(session, "addInvoice_job_id", choices = c(Choose="", jobRef() ))
})

observe({
  updateSelectInput(session, "addInvoice_vat_perc", selected = ifelse(input$addInvoice_currency == "GBP", 20, 0))
})

# Show edit widgets only when a row in the view is clicked!
observe({
  toggle(condition = input$InvoiceTable_rows_selected, id = "edtInvoice_page") # id is the id of tabPanel
})

# When a row has been selected, populate some fields so that they can be edited
observeEvent(input$InvoiceTable_rows_selected, {
  myReactive$index_invoice <- input$InvoiceTable_rows_selected
  selectedRow <- pollInvoice()[myReactive$index_invoice,]

  #updateTabsetPanel(session, inputId = 'invoice_tab', selected = 'Edit Invoice') # selected is title of tabPanel
  updateTextInput(session, "selectedInvoiceId", value = selectedRow$id)
  updateTextInput(session, "selectedinvoiceNumber", value = selectedRow$invoice_number)
  updateSelectInput(session, "edtInvoice_job_id",
                    selected = selectedRow$job_id, choices = c(Choose="", jobRef()))
  updateSelectInput(session, "edtInvoice_issue_date", selected = selectedRow$issue_date)
  updateSelectInput(session, "edtInvoice_net_amount3", selected = selectedRow$net_amount3)
  updateSelectInput(session, "edtInvoice_vat_perc", selected = selectedRow$vat_perc)
  #updateTextInput(session, "edtInvoice_link", value = selectedRow$link)
  updateSelectInput(session, "edtInvoice_currency", selected = selectedRow$currency)
  updateDateInput(session, "edtInvoice_pay_date", value = selectedRow$pay_date)
  updateSelectInput(session, "edtInvoice_curr_ex_rt", selected = selectedRow$curr_ex_rt)
  updateSelectInput(session, "edtInvoice_net_in_original_curr", selected = selectedRow$net_in_original_curr)
  updateCheckboxInput(session, "edtInvoice_pass_through", value=ifelse(selectedRow$pass_through == 1, TRUE, FALSE))
  updateSelectInput(session, "edtInvoice_payment_methods", selected = selectedRow$payment_methods)
  updateSelectInput(session, "edtInvoice_amount_paid", selected = selectedRow$amount_paid)
  updateCheckboxInput(session, "edtInvoice_is_credit", value = ifelse(selectedRow$is_credit == 1, TRUE, FALSE))
  updateTextInput(session, "edtInvoice_notes", value = selectedRow$notes)
})


# ============================================================
# Job description and customer shown as information with Real-time reactive effect
updateJobValue1 <- reactive({
  out <- pollJob() %>%
    filter(id == input$addInvoice_job_id) %>%
    select(desc)

  if(nrow(out) > 0) as.character(out)
  else ""
})

updateJobValue2 <- reactive({
  out <- pollJob() %>%
    filter(id == input$addInvoice_job_id) %>%
    select(customer_name)

  if(nrow(out) > 0) as.character(out)
  else ""
})

updateJobValue3 <- reactive({
  out <- pollJob() %>%
    filter(id == input$edtInvoice_job_id) %>%
    select(desc)

  if(nrow(out) > 0) as.character(out)
  else ""
})

updateJobValue4 <- reactive({
  out <- pollJob() %>%
    filter(id == input$edtInvoice_job_id) %>%
    select(customer_name)

  if(nrow(out) > 0) as.character(out)
  else ""
})

# Observe changes in the input value of job id, and change job description and customer on event.
observeEvent(input$addInvoice_job_id, {
  updateTextInput(session, "addInvoice_desc", value = updateJobValue1())
  updateTextInput(session, "addInvoice_cust", value = updateJobValue2())
})

# Observe changes in the input value of job id, and change job description and customer on event.
observeEvent(input$edtInvoice_job_id, {
  updateTextInput(session, "edtInvoice_desc", value = updateJobValue3())
  updateTextInput(session, "edtInvoice_cust", value = updateJobValue4())
})

# ============================================================
# Client PO
edtInvoice_availablePO <- reactive({
  # Available client PO choices for edit invoice
  res <- pollPO() %>% filter(job_id==input$edtInvoice_job_id)
  unique(res$name)
})

addInvoice_availablePO <- reactive({
  # Available client PO choices for new invoice
  res <- pollPO() %>% filter(job_id==input$addInvoice_job_id)
  unique(res$name)
})

thePO <- reactive({
  res <- pollInvoice() %>% filter(id==input$selectedInvoiceId)
  res$client_po
})

output$edtInvoice_uiselectClientPO <- renderUI({
  validate(
    need(expr=input$edtInvoice_job_id, message = paste("TO SHOW CLIENT PURCHASE ORDERS PLEASE SELECT A ROW"))
  )
  # Choices are limited to that specific job number, and select that correspording po
  selectInput("edtClientPO",
    label = "Client PO",
    choices = c(Choose="", edtInvoice_availablePO()),
    selected = thePO()
  )
})

output$addInvoice_uiselectClientPO <- renderUI({
  # Choices are limited to that specific job number
  selectInput("addClientPO",
    label = "Client PO",
    choices = c(Choose="", addInvoice_availablePO())
  )
})

# ============================================================
# Parent invoice
parentINV1 <- reactive({
  res <- pollInvoice() %>% filter(job_id==input$addInvoice_job_id, is_credit==0)
  res$invoice_number
})

parentINV2 <- reactive({
  res <- pollInvoice() %>% filter(job_id==input$edtInvoice_job_id, is_credit==0)
  res$invoice_number
})

theParentINV <- reactive({
  res <- pollInvoice() %>% filter(id==input$selectedInvoiceId)
  res$parent_invoice_id
})

output$addInvoice_parent_ui <- renderUI({
  selectInput("addInvoice_ParentINV",
              label = "Parent Invoice",
              choices = c(Choose="", parentINV1())
  )
})

output$edtInvoice_parent_ui <- renderUI({
  selectInput("edtInvoice_ParentINV",
              label = "Parent Invoice",
              choices = c(Choose="", parentINV2()),
              selected = theParentINV()
  )
})


# ============================================================
# Break down of payment for selected job
otherINVEDT <- reactive({

  inv <- pollInvoice() %>%
    filter(job_id==input$edtInvoice_job_id) %>% #, id!=input$selectedInvoiceId
    select(invoice_number, issue_date, client_po, amount_paid, is_credit) %>%
    mutate(issue_date = format(issue_date, "%Y-%m-%d")) %>%
    mutate(amount_paid = ifelse(is_credit==1, amount_paid * (-1), amount_paid)) %>%
    mutate(is_credit = ifelse(is_credit==1, "Yes", "No"))

  po <- pollPO() %>%
    filter(job_id==input$edtInvoice_job_id) %>%
    select(name, amount)

  invoicePO <- left_join(inv, po, by=c("client_po"="name")) %>%
    select(`Invoice Number`=invoice_number, `Issue Date`=issue_date, `Is credit?`=is_credit,
           `Purchase Order`=client_po, `Purchase Order`=client_po, `Total PO Amount`=amount,
           `Amount Paid`= amount_paid)

  invoicePO <- if(nrow(invoicePO) >= 1) {invoicePO}
})
otherINVADD <- reactive({

  inv <- pollInvoice() %>%
    filter(job_id==input$addInvoice_job_id) %>%
    select(invoice_number, issue_date, client_po, amount_paid, is_credit) %>%
    mutate(issue_date = format(issue_date, "%Y-%m-%d")) %>%
    mutate(amount_paid = ifelse(is_credit==1, amount_paid * (-1), amount_paid)) %>%
    mutate(is_credit = ifelse(is_credit==1, "Yes", "No"))

  po <- pollPO() %>%
    filter(job_id==input$addInvoice_job_id) %>%
    select(name, amount)

  invoicePO <- left_join(inv, po, by=c("client_po"="name")) %>%
    select(`Invoice Number`=invoice_number, `Issue Date`=issue_date, `Is credit?`=is_credit,
           `Purchase Order`=client_po, `Purchase Order`=client_po, `Total PO Amount`=amount,
           `Amount Paid`= amount_paid)

  invoicePO <- if(nrow(invoicePO) >= 1) {invoicePO}
})

output$edtInvoiceForJob <- renderTable({
  validate(need(expr=input$selectedInvoiceId, message = paste("TO SHOW THE OTHER INVOICES PLEASE SELECT A ROW")))
  otherINVEDT()
})

output$addInvoiceForJob <- renderTable({
  validate(need(expr=input$addInvoice_job_id, message = paste("TO SHOW THE OTHER INVOICES PLEASE SELECT A JOB")))
  otherINVADD()
})

jobAmtEDT <- reactive({

  jobAmt <- pollJob() %>%
    filter(id==input$edtInvoice_job_id) %>%
    select(estimate_revenue_sales)
  jobAmt[1,]
})

jobAmtADD <- reactive({

  jobAmt <- pollJob() %>%
    filter(id==input$addInvoice_job_id) %>%
    select(estimate_revenue_sales)
  jobAmt[1,]
})

output$totalAmountEDT <- renderUI({
  strong(paste("Total Job Budget (GBP) : ", 
               jobAmtEDT()))
})

output$totalAmountADD <- renderUI({
  strong(paste("Total Job Budget (GBP) : ", 
               jobAmtADD()))
})

output$leftToPayEDT <- renderUI({
  leftToPay <- as.numeric(jobAmtEDT()) - sum(otherINVEDT()[, "Amount Paid"], na.rm = TRUE)
  strong(paste("Left to pay : ", leftToPay))
})

output$leftToPayADD <- renderUI({
  leftToPay <- as.numeric(jobAmtADD()) - sum(otherINVADD()[, "Amount Paid"], na.rm = TRUE)
  strong(paste("Left to pay : ", leftToPay))
})


# ============================================================
# Click "Confirme" button -> update DB
# Add to invoice table
observeEvent(input$addInvoice_Go, {
  validate(need(expr=input$addInvoice_job_id, message = paste("TO ADD INVOICE PLEASE SELECT A JOB NUMBER")))

  newId <- addInvoice(job_id = input$addInvoice_job_id,
             issue_date = ifelse(is.null(input$addInvoice_issue_date),
                                 "",
                                 format(as.Date(input$addInvoice_issue_date), "%d/%m/%Y")),
             issued_by = 17,
             client_po = input$addClientPO,
             net_amount = input$addInvoice_net_in_original_curr * input$addInvoice_curr_ex_rt,
             vat_perc = input$addInvoice_vat_perc,
             currency = input$addInvoice_currency,
             curr_ex_rt = input$addInvoice_curr_ex_rt,
             pass_through = ifelse(input$addInvoice_pass_through, 1, 0),
             is_credit= ifelse(input$addInvoice_is_credit, 1, 0),
             parent_invoice_id = ifelse(input$addInvoice_is_credit, input$addInvoice_ParentINV, ""),
             notes = input$addInvoice_notes
  )

  # Click "Confirm" button -> message, reset & go to view
  shinyjs::info(paste0("New invoice: MGB/EXT/", newId, " added. Thank you!"))
  shinyjs::reset("addInvoice_page")
  updateTabsetPanel(session,
                    inputId = "invoice_tab",
                    selected = "Invoice View")
})

# Edit invoice table
observeEvent(input$edtInvoice_Go, {
  validate(need(input$selectedInvoiceId, "TO UPDATE TABLES PLEASE SELECT A ROW"))

  edtInvoice(id = input$selectedInvoiceId,
             job_id = input$edtInvoice_job_id,
             issue_date = ifelse(is.null(input$edtInvoice_issue_date),
                                 "",
                                 format(as.Date(input$edtInvoice_issue_date), "%d/%m/%Y")),
             issued_by = 17,
             client_po = input$edtClientPO,
             vat_perc = ifelse(is.na(input$edtInvoice_vat_perc), 0, input$edtInvoice_vat_perc),
             currency = input$edtInvoice_currency, 
             curr_ex_rt = input$edtInvoice_curr_ex_rt, # Cannot convert to blank
             net_amount = (ifelse(is.na(input$edtInvoice_net_in_original_curr),
                                  0,
                                  input$edtInvoice_net_in_original_curr)) * input$edtInvoice_curr_ex_rt,
             pass_through = ifelse(input$edtInvoice_pass_through, 1, 0),
             is_credit = ifelse(input$edtInvoice_is_credit, 1, 0),
             parent_invoice_id = ifelse(input$edtInvoice_is_credit, input$edtInvoice_ParentINV, ""),
             notes = input$edtInvoice_notes,
             
             pay_date = ifelse(is.null(input$edtInvoice_pay_date),
                               "",
                               format(as.Date(input$edtInvoice_pay_date), "%d/%m/%Y")),
             pay_method_id = ifelse(input$edtInvoice_payment_methods=="",
                                    "",
                                    paymentTbl$id[paymentTbl$name==input$edtInvoice_payment_methods]),
             amount_paid = ifelse(is.na(input$edtInvoice_amount_paid), 0, input$edtInvoice_amount_paid)

  )

  # Click "Confirm" button -> message, reset & go to view
  shinyjs::info("Invoice updated. Thank you!")
  shinyjs::reset("edtInvoice_page")
  updateTabsetPanel(session,
                    inputId = "invoice_tab",
                    selected = "Invoice View")
})

# ============================================================
# Click "Cancel" button -> reset data
# Cancel added
observeEvent(input$addInvoice_Cancel, {

  shinyjs::reset("addInvoice_page")
  updateTabsetPanel(session,
                    inputId = "invoice_tab",
                    selected = "Invoice View")
})

# Cancel edited
observeEvent(input$edtInvoice_Cancel, {

  shinyjs::reset("edtInvoice_page")
  updateTabsetPanel(session,
                    inputId = "invoice_tab",
                    selected = "Invoice View")
  InvoiceProxy %>% selectRows(NULL)
})

# ============================================================
# Definie compulsory variables
# Job ref is compulsory (add)
observe({
  if (is.null(input$addInvoice_job_id) || input$addInvoice_job_id == "") {
    shinyjs::disable("addInvoice_Go")
  } else {
    shinyjs::enable("addInvoice_Go")
  }
})

# Job ref is compulsory (edit)
observe({
  if (is.null(input$edtInvoice_job_id) || input$edtInvoice_job_id == "") {
    shinyjs::disable("edtInvoice_Go")
  } else {
    shinyjs::enable("edtInvoice_Go")
  }
})
