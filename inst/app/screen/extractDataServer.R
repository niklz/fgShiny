# Disabling this text input because it should not be changed
shinyjs::disable("selectedCustId")

output$CustomerTable <- renderDataTable({
  # Get rid of id for the view but id still exists under the hood!
  pollCustomer()[-1] %>%
    datatable(selection = "single",
              options = list(lengthMenu = c(5, 10, 100), dom = 'lftip'),
              rownames = FALSE,
              colnames = c("Customer", "Industry", "Account Manager"),
              filter=list(position = "top", clear = FALSE)
    )
}, server = FALSE)

customerProxy <- dataTableProxy("CustomerTable")

# Observe widgets that has reactive selected/value, mainly used for new invoice
observe({
  updateSelectInput(session,
                    "CustomerAddAM",
                    choices = c(Choose = "", employeeName()))
})

# Show edit widgets only when a row in the view is clicked!
observe({
  toggle(condition = input$CustomerTable_rows_selected,
         id = "edtCustomer_page") # id is the id of tabPanel
})

# ============================================================
# When a row has been selected, populate some fields so that they can be edited
observeEvent(input$CustomerTable_rows_selected, {
  myReactive$index_cust <- input$CustomerTable_rows_selected
  selectedRow <- pollCustomer()[myReactive$index_cust,]

  #updateTabsetPanel(session, inputId = 'customer_tab', selected = 'Edit Customer') # selected is title of tabPanel
  updateTextInput(session, "selectedCustId", value = selectedRow$id)
  updateTextInput(session, "CustomerCustomerForEdit", value = selectedRow$customer_name)
  updateSelectInput(session, "CustomerIndustryForEdit", selected = selectedRow$industry_name)
  updateSelectInput(session, "CustomerAMForEdit",
                    choices = c(Choose='',  employeeName() ),
                    selected = selectedRow$account_manager_name)
})

# ============================================================
# Click "Confirme" button -> updat DB
# Add a new customer
observeEvent(input$addCustomerConfirm, {

  # check if the cusomter name already exists in the database
  if(checkCustomer(input$CustomerAddCustomer)){

    shinyjs::info("Customer name already exists.  Please enter a unique name.")

  } else {

    # Update the internal database
    newId <- createCustomer(
      cusName = input$CustomerAddCustomer,
      indId = industryTbl$id[industryTbl$name==input$CustomerAddIndustry],
      accntMngrId = pollEmployee()$id[pollEmployee()$name==input$CustomerAddAM]
    )

    # Click "Confirm" button -> message, reset, go to view
    shinyjs::info(paste("New customer:", newId, "added. Thank you!"))
    shinyjs::reset("addCustomer_page")
    updateTabsetPanel(session,
                      inputId = "customer_tab", # inputId: id of tabBox
                      selected = "Customer View") # selected: title of tabPanel
  }
})


# Edit information of an existing customer
observeEvent(input$edtCustomerConfirm, {
  validate(need(input$selectedCustId, "TO UPDATE TABLES PLEASE SELECT A ROW"))

  editCustomer(
    cusId = input$selectedCustId,
    cusName = input$CustomerCustomerForEdit,
    indId = industryTbl$id[industryTbl$name == input$CustomerIndustryForEdit],
    accntMngrId = pollEmployee()$id[pollEmployee()$name == input$CustomerAMForEdit]
  )

  # Click "Confirm" button -> message, reset & go to view
  shinyjs::info("Customer updated. Thank you!")
  shinyjs::reset("edtCustomer_page")
  updateTabsetPanel(session,
                    inputId = "customer_tab", # inputId: id of tabBox
                    selected = "Customer View") # selected: title of tabPanel

})

# ============================================================
# Click "Cancel" button -> go back to View & reset data
# What to do when the cancel button is pressed (add)
observeEvent(input$addCustomerCancel, {
  shinyjs::reset("addCustomer_page")
  updateTabsetPanel(session,
                    inputId = "customer_tab",
                    selected = "Customer View")
})

# What to do when the cancel button is pressed (edit)
observeEvent(input$edtCustomerCancel, {
  shinyjs::reset("edtCustomer_page")
  updateTabsetPanel(session,
                    inputId = "customer_tab",
                    selected = "Customer View")
  customerProxy %>% selectRows(NULL)
})



# ============================================================
# Definie compulsory variables
# Customer name is compulsory (add)
observe({
  if (is.null(input$CustomerAddCustomer) || input$CustomerAddCustomer == "") {
    shinyjs::disable("addCustomerConfirm")
  } else {
    shinyjs::enable("addCustomerConfirm")
  }
})

# Customer name is compulsory (edit)
observe({
  if (is.null(input$CustomerCustomerForEdit) || input$CustomerCustomerForEdit == "") {
    shinyjs::disable("edtCustomerConfirm")
  } else {
    shinyjs::enable("edtCustomerConfirm")
  }
})


