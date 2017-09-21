fluidRow(column(
  width = 12,
  tabBox(
    width = NULL,
    side = "left",
    id = "job_tab",
    tabPanel(
      "Job View",
      fluidRow(
        column(9, helpText("Dear user, please select a row before going to the edit page :)")),
        column(3, downloadButton("job_csv", "Download CSV"), align = 'right'),
        column(12, hr()),
        column(12, 
               div(style = 'overflow-x: scroll',
                   DT::dataTableOutput("JobTable")
               )                        
        )
      )
    ),
    tabPanel("New Job",
             div(
               id = "addJob_page",
               fluidRow(column(12,
                               fluidRow(
                                 column(
                                   3,
                                   h4('Overview'),
                                   hr(),
                                   textInput('addJobDesc', label =
                                               'Job Description', value = ''),
                                   uiOutput("uiaddCust"),
                                   selectInput(
                                     "addJobStatus",
                                     label = "Status",
                                     choices = c("", statusTbl$name),
                                     # c("Pre-sales", "Live", "Closing", "Closed", "Lost", "Legals")
                                     selected = "Pre-sales"
                                   ),
                                   radioButtons(
                                     "addJobBusUnit",
                                     label = "Business Unit",
                                     choices = c("Pharma", "Data Science", "Central"),
                                     inline = TRUE
                                   ),
                                   textInput('addKeyContact', label =
                                               'Customer Sales Contact', value = ''),
                                   selectInput(
                                     "addProjectType",
                                     label = "Primary Project Type",
                                     choices = c(Choose= "", c("Strategy", "Analysis", "App Dev", "Environments", "Training"))
                                   ),
                                   selectInput(
                                     "addRequestor",
                                     label = "Requestor",
                                     choices = c(Choose = "")
                                   ),
                                   dateInput("addGoLiveDate",
                                             label = "Go Live Date",
                                             value = lubridate::today()),
                                   textareaInput(
                                     id = "addNotes",
                                     label = "Notes",
                                     value = "",
                                     rows = 4
                                   )
                                 ),
                                 column(
                                   3,
                                   h4('Financial'),
                                   hr(),
                                   numericInput(
                                     "addJobPayTerm",
                                     "Payment Terms(days)",
                                     0,
                                     min = 0,
                                     max = 365
                                   ),
                                   checkboxInput("addJob_end_of_month",
                                                 label = "From end of month?"),
                                   selectInput(
                                     "addJobBillType",
                                     label = "Billing Type",
                                     choices = c("Choose" =
                                                   "", billingTbl$name)
                                   ),
                                   strong("Billable Percentages (%)"),
                                   sliderInput(
                                     "addProject_management",
                                     label = h5("Project Management"),
                                     min = 0,
                                     max = 100,
                                     value = 100
                                   ),
                                   sliderInput(
                                     "addTravel",
                                     label = h5(p("Travel")),
                                     min = 0,
                                     max = 100,
                                     value = 100
                                   ),
                                   sliderInput(
                                     "addLearning",
                                     label = h5("Learning"),
                                     min = 0,
                                     max = 100,
                                     value = 100
                                   ),
                                   sliderInput(
                                     "addKnowledge",
                                     label = h5("Knowledge"),
                                     min = 0,
                                     max = 100,
                                     value = 100
                                   ),
                                   numericInput(
                                     "addJobEstimateDay",
                                     "Budget (days)",
                                     0,
                                     min = 0,
                                     max = 100000000
                                   ),
                                   numericInput(
                                     "addJobEstimateRev",
                                     "Budget (£)",
                                     0,
                                     min = 0,
                                     max = 100000000
                                   ),
                                   radioButtons(
                                     "addJobDrawdownMethod",
                                     "Budget Drawdown Method",
                                     choices = c("Cost", "Time"),
                                     inline = TRUE
                                   ),
                                   numericInput(
                                     "addJobEstExpensesCap",
                                     "Estimated Capped Expenses (£)",
                                     0,
                                     min = 0,
                                     max = 100000000
                                   ),
                                   fluidRow(
                                     column(width = 6,
                                            textInput(
                                              "addJobPO",
                                              value = "",
                                              label = "New Purchase Order",
                                              placeholder = "Leave it if no PO to be added"
                                              )
                                            ),
                                     column(width = 6,
                                            numericInput("addJobPOAmt",
                                                         value = NA,
                                                         label = "New PO Amount")
                                   ))

                                 ),
                                 column(
                                   width = 6,
                                   h4("Personnel"),
                                   hr(),
                                   fluidRow(column(12, uiOutput("newAddRoleBox"))),
                                   fluidRow(column(3, 'Role'),
                                            column(3, 'Rate'),
                                            column(4, 'Employee')),
                                   fluidRow(column(12, uiOutput("newPersonnelUI")))

                                 )
                               ))),
               fluidRow(column(
                 12,
                 # Custom function actionButton
                 div(style = "display:inline-block", goButton('addJobGo', 'Confirm', btn.style = "primary")),
                 div(style = "display:inline-block", actionButton("addJobCancel", 'Cancel')),
                 shinyBS::bsTooltip(
                   "addJobCancel",
                   "Click Cancel to reset the fields.",
                   "right",
                   trigger = "hover",
                   options = list(container = "body")
                 )
               ))
             )),
    tabPanel(
      "Edit Job",
      div(id = "edtJob_page",
          fluidRow(column(
            12,
            fluidRow(
              column(
                3,
                h4('Overview'),
                hr(),
                textInput("selectedJobId", label =
                            "Job Reference"),
                textInput('edtJobDesc', label =
                            'Job Description'), 
                #uiOutput("uiedtDesc"),
                uiOutput("uiedtCust"),
                selectInput(
                  "edtJobStatus",
                  label = "Status",
                  choices = c(Choose = "", statusTbl$name),
                  # c("Pre-sales", "Live", "Closing", "Closed", "Lost", "Legals")
                  selected = "Pre-sales"
                ),
                radioButtons(
                  "edtJobBusUnit",
                  label = "Business Unit",
                  choices = c("Pharma", "Data Science", "Central"),
                  inline = TRUE
                ),
                textInput('edtKeyContact', label =
                            'Customer Sales Contact', value = ''
                ),
                selectInput(
                  "edtProjectType",
                  label = "Primary Project Type",
                  choices = c(Choose = "", "Strategy", "Analysis", "App Dev", "Environments", "Training")
                ),
                selectInput(
                  "edtRequestor",
                  label = "Requestor",
                  choices = c(Choose = "")
                ),
                dateInput("edtGoLiveDate",
                          label = "Go Live Date",
                          value = NA),
                textareaInput(
                  id = "edtNotes",
                  label = "Notes",
                  value = "",
                  rows = 4
                )
              ),
              column(
                3,
                h4('Financial'),
                hr(),
                numericInput(
                  "edtJobPayTerm",
                  "Payment Terms (days)",
                  0,
                  min = 0,
                  max = 365
                ),
                checkboxInput("edtJob_end_of_month", label = "From end of month?"),
                selectInput(
                  "edtJobBillType",
                  label = "Billing Type",
                  choices = c(Choose =
                                "", billingTbl$name)
                ),
                strong("Billable Percentages (%)"),
                sliderInput(
                  "edtProject_management",
                  label = h5("Project Management"),
                  min = 0,
                  max = 100,
                  #width = '50%',
                  value = ''
                ),
                sliderInput(
                  "edtTravel",
                  label = h5("Travel"),
                  min = 0,
                  max = 100,
                  #width = '50%',
                  value = ''
                ),
                sliderInput(
                  "edtLearning",
                  label = h5("Learning"),
                  min = 0,
                  max = 100,
                  #width = '50%',
                  value = ''
                ),
                sliderInput(
                  "edtKnowledge",
                  label = h5("Knowledge"),
                  min = 0,
                  max = 100,
                  value = ''
                ),
                numericInput(
                  "edtJobEstimateDay",
                  "Budget (days)",
                  0,
                  min = 0,
                  max = 100000000
                ),
                numericInput(
                  "edtJobEstimateRev",
                  "Budget (?)",
                  value = 0,
                  min = 0,
                  max = 100000000
                ),
                radioButtons(
                  "edtJobDrawdownMethod",
                  "Budget Drawdown Method",
                  choices = c("Cost", "Time"),
                  inline = TRUE
                ),
                numericInput(
                  "edtJobEstExpensesCap",
                  "Estimate Capped Expenses (?)",
                  "",
                  min = 0,
                  max = 100000000
                ),
                uiOutput("poTbl"),
                fluidRow(column(
                  width = 6,
                  textInput(
                    "edtJobPO",
                    value = "",
                    label = "New Purchase Order",
                    placeholder = "Leave it blank if no PO to be added"
                  )
                ),
                column(
                  width = 6,
                  numericInput("edtJobPOAmt",
                               value = NA,
                               label = "New PO Amount")
                ))
              ),
              column(
                width = 6,
                fluidRow(column(
                  12,
                  h4("Personnel"),
                  hr(),
                  uiOutput("edtAddRoleBox")
                )),
                fluidRow(
                  column(3, 'Role'),
                  column(3, 'Rate'),
                  column(4, 'Employee'),
                  column(2, '')
                ),
                fluidRow(column(
                  12,
                  shinyBS::bsAlert("alert"),
                  uiOutput("edtPersonnelUI")
                ))
              )
            )
          )),
          fluidRow(
            column(
              12,
              # Custom function actionButton
              div(style = "display:inline-block", goButton('edtJobGo', 'Confirm', btn.style = "primary")),
              div(style = "display:inline-block", actionButton("edtJobCancel", 'Cancel')),
              shinyBS::bsTooltip(
                "edtJobCancel",
                "Click Cancel to reset the fields.",
                "right",
                trigger = "hover",
                options = list(container = "body")
              )
            )
          )) %>% shinyjs::hidden()
    )
  )

))
