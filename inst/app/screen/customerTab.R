fluidRow(column(
  width = 12,

  tabBox(
    width = NULL,
    side = "left",
    id = "customer_tab",
    tabPanel(
      title = "Customer View",
      helpText("Dear user, please select a row before going to the edit page."),
      hr(),
      div(style = 'overflow-x: scroll', DT::dataTableOutput("CustomerTable"))
    ),

    tabPanel(
      title = "Add Customer",
      div(
        id = "addCustomer_page",
        textInput("CustomerAddCustomer", label = "Customer"),
        selectInput(
          "CustomerAddIndustry",
          "Industry",
          choices = c(Choose = '', industryTbl$name)
        ),
        selectInput("CustomerAddAM", "Account Manager", choices = c(Choose =
                                                                      '')),
        goButton("addCustomerConfirm", "Confirm", btn.style = "primary"),
        div(style = "display:inline-block", actionButton("addCustomerCancel", 'Cancel')),
        shinyBS::bsTooltip(
          "addCustomerCancel",
          "Click Cancel to reset the fields.",
          "right",
          trigger = "hover",
          options = list(container = "body")
        )
      )
    ),
    tabPanel(
      title = "Edit Customer",
      div(
        id = "edtCustomer_page",
        textInput("selectedCustId", label = "ID"),
        textInput("CustomerCustomerForEdit", "Customer"),
        selectInput(
          "CustomerIndustryForEdit",
          "Industry",
          choices = c(Choose = '', industryTbl$name)
        ),
        selectInput("CustomerAMForEdit", "Account Manager", choices = c(Choose =
                                                                          '')),
        goButton("edtCustomerConfirm", "Confirm", btn.style = "primary"),

        div(style = "display:inline-block", actionButton("edtCustomerCancel", 'Cancel')),
        shinyBS::bsTooltip(
          "edtCustomerCancel",
          "Click Cancel to reset the fields.",
          "right",
          trigger = "hover",
          options = list(container = "body")
        )
      ) %>% shinyjs::hidden()
    )
  )
))
