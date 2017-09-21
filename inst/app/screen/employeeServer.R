# Disabling this text input because it should not be changed
shinyjs::disable("selectedEmpId")

output$employeeTbl <- DT::renderDataTable({
  # Get rid of id for the view but id still exists under the hood!
  pollEmployee()[-1] %>%
    datatable(selection = "single",
              options = list(lengthMenu = c(5, 10, 100), dom = 'lftip'),
              rownames = FALSE,
              colnames = c('Employee', 'Job Role', 'Contractor', 'Active'),
              filter = list(position = "top", clear = FALSE)
    )
}, server = FALSE)

employeeProxy = dataTableProxy("employeeTbl")

# SKILLS MATRIX TABLE


output$skillsMatrixTbl <- DT::renderDataTable({
  # Get rid of id for the view but id still exists under the hood!
  pollSkillsMatrix()[] %>%
  datatable(selection = "single",
            extensions = 'FixedHeader',
            options = list(paging = FALSE, searching = FALSE, dom = 'lftip', fixedHeader = TRUE),
            rownames = FALSE

              #colnames = c('Employee', 'Job Role', 'Contractor', 'Active'),
              #filter = list(position = "top", clear = FALSE)


    )
  
}, server = FALSE, scrollX = TRUE)

output$skillsTbl <- DT::renderDataTable({

  pollSkillsMatrix()[, 1] %>%
    datatable(selection = "single",
              extensions = 'FixedHeader',
              options = list(paging = FALSE,
                             searching = FALSE,
                             dom = 'lftip',
                             scrollY = TRUE,
                             fixedHeader = TRUE),

              rownames = FALSE
    )

  
  
}, server = FALSE)

employeeProxy = dataTableProxy("employeeTbl")




# Show edit widgets only when a row in the view is clicked!
observe({
  shinyjs::toggle(condition = input$employeeTbl_rows_selected, id = "edtEmployee_page") # id is the id of tabPanel
})

# ============================================================
# When a row has been selected, populate some fields so that they can be edited
observeEvent(input$employeeTbl_rows_selected, {
  myReactive$index_emp <- input$employeeTbl_rows_selected
  selectedRow <- pollEmployee()[myReactive$index_emp, ]

  #updateTabsetPanel(session, inputId = 'employee_tab', selected = 'Edit Employee') # selected is title of tabPanel
  updateTextInput(session, "selectedEmpId", value = selectedRow$id)
  updateTextInput(session, "edtName", value = selectedRow$name)
  updateSelectInput(session, "edtJobRole", selected = selectedRow$job_role)
  updateCheckboxInput(session, 'edtContractor', value = ifelse(selectedRow$contractor=="Yes", TRUE, FALSE))
  updateCheckboxInput(session, 'edtActiveStatus', value = ifelse(selectedRow$active=="Yes", TRUE, FALSE))
  
  loadSkillsForm(name = selectedRow$name, session = session)
})

# ============================================================
# Click "Confirm" button -> updat DB
# Add a new employee
observeEvent(input$addEmployee, {

  # add new employee in DB
  newId <- addEmployee(
    name = input$addName,
    jobRoleId = roleTbl$id[roleTbl$name==input$addJobRole],
    contractor = ifelse(input$addContractor, 1, 0)
  )

  # Click "Confirm" button -> message, reset, go to view
  shinyjs::info(paste("New employee:", newId, "added. Thank you!"))
  shinyjs::reset("addEmployee_page")
  updateTabsetPanel(session,
                    inputId = "employee_tab",
                    selected = "Employee View")
})

# Edit information of an existing employee
observeEvent(input$updateEmployee, {

  # Edit employee in DB
	edtEmployee(
	  name = input$edtName,
	  jobRoleId = roleTbl$id[roleTbl$name==input$edtJobRole],
	  contractor = ifelse(input$edtContractor, 1, 0),
		inactiveStatus = ifelse(input$edtActiveStatus, 0, 1),
		id = input$selectedEmpId
	)

  # Click "Confirm" button -> message, reset & go to view
  shinyjs::info("Employee updated. Thank you!")
  shinyjs::reset("edtEmployee_page")
  updateTabsetPanel(session,
                    inputId = "employee_tab",
                    selected = "Employee View")
})


# ============================================================
# Click "Cancel" button -> go back to View & reset data
# Cancel added
observeEvent(input$addEmployeeCancel,{
  shinyjs::reset("addEmployee_page")
  updateTabsetPanel(session,
                    inputId = "employee_tab",
                    selected = "Employee View")
})

# Cancel edited
observeEvent(input$edtEmployeeCancel,{
  shinyjs::reset("edtEmployee_page")
  updateTabsetPanel(session,
                    inputId = "employee_tab",
                    selected = "Employee View")
  employeeProxy %>% selectRows(NULL)
})

# ============================================================
# Definie compulsory variables
# First name is compulsory (add)
observe({
  if (is.null(input$addName) || input$addName == "") {
    shinyjs::disable("addEmployee")
  } else {
    shinyjs::enable("addEmployee")
  }
})

# First name is compulsory (edit)
observe({
  if (is.null(input$edtName) || input$edtName == "") {
    shinyjs::disable("updateEmployee")
  } else {
    shinyjs::enable("updateEmployee")
  }
})


# UI code for skills form

output$skillsUI <- renderUI({
div(id = "employeeSkills",
    fluidRow(tags$head(tags$style(type="text/css", "textarea { max-width: 100%; min-width: 150px;}")),
             div(id = "form", 
                 tags$head(tags$style(
                 type="text/css",
                 "#scoreText{ word-wrap: break-word; margin-bottom:25px }"
                            )),
                
                 column(8, 
                        helpText("To load skills data select a row on the Employee View panel"),
                        lapply(skills, function(x) {create_skill(x)})
                        ),
                 
                 column(4, (fixedPanel(div(id = "helpText", htmlOutput("scoreText")),
                                       
                                        actionButton("saveSkill",
                                                     label = "Save"),
                              width = '300px')) 
                            )
                        ))
           )
  })
