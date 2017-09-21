fluidRow(
  column(width = 12,
         tabBox(width = "500px", side = "left", id="employee_tab",
                tabPanel("Employee View",
                         helpText("Dear user, please select a row before going to the edit page."),
                         hr(),
                         div(style = 'overflow-x: scroll', DT::dataTableOutput("employeeTbl"))),


                tabPanel("Add Employee",
                         div(id = "addEmployee_page", # id specified explicitly for reset
                             textInput("addName", label = "Employee Name"),
                             selectInput('addJobRole', 'JobRole', c(Choose='', roleTbl$name)),
                             checkboxInput("addContractor", label = "Contractor", value = FALSE),
                             goButton("addEmployee", label = "Confirm", btn.style = "primary"),
                             div(style = "display:inline-block", actionButton("addEmployeeCancel", 'Cancel')),
                             shinyBS::bsTooltip(
                               "addEmployeeCancel",
                               "Click Cancel to reset the fields.",
                               "right",
                               trigger = "hover",
                               options = list(container = "body")
                             )
                         )
                ),
                tabPanel(title = "Edit Employee",
                         div(id = "edtEmployee_page",
                             textInput("selectedEmpId", label="ID"),
                             textInput("edtName", label = "Employee Name"),
                             selectInput('edtJobRole', 'JobRole', c(Choose='', roleTbl$name)),
                             checkboxInput("edtContractor", label = "Contractor", value = FALSE),
                             checkboxInput("edtActiveStatus", label = "Active", value = TRUE),
                             goButton("updateEmployee", label = "Confirm", btn.style = "primary"),
                             div(style = "display:inline-block", actionButton("edtEmployeeCancel", 'Cancel')),
                             shinyBS::bsTooltip(
                               "edtEmployeeCancel",
                               "Click Cancel to reset the fields.",
                               "right",
                               trigger = "hover",
                               options = list(container = "body")
                             )
                         ) %>% shinyjs::hidden()
                ),
                tabPanel(title = "Employee Skills",
                         uiOutput("skillsUI")),
                tabPanel("Skills Matrix",
                         hr(),
                         div(style = 'overflow-x: scroll',
                             DT::dataTableOutput("skillsMatrixTbl")))
         )
  )
)
