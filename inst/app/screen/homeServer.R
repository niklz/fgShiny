# Disabling this text input because it should not be changed
shinyjs::disable("selectedJobId")

# Define pre-load search - job status is one of pre-sales, legal, live, and closing by default
status_pre_load <- list(
  NULL, NULL,
  list(search = "[\"Pre-sales\",\"Live\",\"Closing\",\"Legals\"]")
)

# Format the table before rendering for DT because of the download button
job_format <- reactive({
  pollJob() %>%
    mutate(
      estimate_revenue_sales2 = formatC(
        estimate_revenue_sales,
        format = "f",
        big.mark = ",",
        drop0trailing = TRUE
      )
    ) %>%
    mutate(id = as.character(id)) %>% # convert to character to allow searching
    mutate(estimate_hour_plan = as.integer(estimate_day_plan) * 7.5) %>%
    mutate(status_name = as.factor(status_name)) %>%
    mutate(
      estimate_hour_plan = formatC(
        estimate_hour_plan,
        format = "f",
        big.mark = ",",
        drop0trailing = TRUE
      )
    ) %>%
    select(bus_unit_name, id, status_name, creation_date, go_live_date, 
           customer_name, desc, estimate_day_plan, estimate_hour_plan, estimate_revenue_sales2, 
           account_manager_name, requestor_name, key_contact
    )
})

output$JobTable <- renderDataTable({

  job_format() %>%
    datatable(
      colnames = c("Business Unit", "Job Reference", "Status", "Date of Creation",
                   "Go Live Date", "Customer", "Description", "Budget (days)", "Budget (hours)",
                   "Budget (£)", "Account Manager", "Requestor", "Customer Sales Contact"),
      selection = "single",
      extensions = 'Buttons', rownames = FALSE,
      filter = list(position = "top", clear = FALSE),
      #options = list(lengthMenu = 15,
      #               dom = 'Bfrtip',
      #               searchCols = status_pre_load,
      #               buttons = list(list(extend="csv", filename="jobs_view"),
      #                              "copy")
      #)
      options = list(lengthMenu = c(5, 10, 100), dom = 'lftip', searchCols = status_pre_load)
    )
}, server = TRUE)

JobProxy = dataTableProxy("JobTable")

# download the filtered formated data
output$job_csv = downloadHandler('Jobs_view_server.csv', content = function(file) {
  rows = input$JobTable_rows_all
  write.csv(job_format()[rows, ], file, row.names = FALSE)
}) 

# ============================================================
# Customer
theCustomer <- reactive({

  pollJob() %>%
    filter(id==input$selectedJobId) %>%
    select(customer_name)

})

output$uiedtCust <- renderUI({
  selectInput("edtJobCustomer",
              label = "Customer",
              choices = c(Choose="", customerName() ),
              selected = theCustomer())
})

output$uiedtDesc <- renderUI({
  selectedRow <- pollJob()[input$JobTable_rows_selected,]
  textInput('edtJobDesc', 
            'Job Description',
            value = selectedRow$desc) 
})

output$uiaddCust <- renderUI({
  selectInput("addJobCustomer",
              label = "Customer",
              choices = c(Choose="",  customerName() ))
})

# ============================================================
# Client PO
# List alive POs in reactive text.
# Select POs corresponding to a job number which must be unique.
po <- reactive({pollPO() %>%
    filter(job_id == input$selectedJobId) %>%
    distinct(name, .keep_all = TRUE)
})

output$poTbl <- renderUI({
  if(nrow(po())!=0) {

    mapply(name = row.names(po()),
           value = po()[, "name"],
           amount = po()[, "amount"],
           SIMPLIFY = FALSE, FUN = function(name, value, amount){
             fluidRow(
               column(width=6,
                 textInput(inputId = name, label = paste0("Purchase Order #", name), value = value)),
               column(width=6, 
                 numericInput(inputId = paste0(name, "amt"), label= paste0("PO #", name, " ", " Amount"), value = amount)
               )
             )
    })
  }
})

# Observe widgets that has reactive selected/value, mainly used for new invoice
observe({

  customer <- pollCustomer() %>%
    subset(customer_name == input$addJobCustomer)
  selectedAccountMan <- ifelse(is.na(customer$account_manager_name),
                               "Nicola Carley",
                               customer$account_manager_name)

  updateSelectInput(session, "addRequestor",
                    choices = c(Choose="", employeeName()  ),
                    selected = selectedAccountMan)

})

# Show edit widgets only when a row in the view is clicked!
observe({
  toggle(condition = input$JobTable_rows_selected, id = "edtJob_page") # id is the id of tabPanel
})


# desc can't be picked up if not in reactive function
updateJobDesc <- reactive({
  out <- pollJob() %>%
    filter(id == input$selectedJobId) %>%
    select(desc)
  
  if(nrow(out) > 0) as.character(out)
  else ""
})

# When a row has been selected, populate some fields so that they can be edited
observeEvent(input$JobTable_rows_selected, {

  myReactive$index_job <- input$JobTable_rows_selected
  selectedRow <- pollJob()[input$JobTable_rows_selected,] # pick up from original data
                                                          #myReactive$index_job

  #updateTabsetPanel(session, inputId = 'job_tab', selected = 'Edit Job') # selected is title of tabPanel
  updateTextInput(session, "selectedJobId", value = selectedRow$id)
  updateSelectInput(session, "edtJRoles", selected = getCurrentRoles(selectedRow$id))
  updateRadioButtons(session, "edtJobBusUnit", selected = selectedRow$bus_unit_name)
  updateSelectInput(session, "edtJobCustomer", choices = c(Choose="", customerName()), selected = selectedRow$customer_name)
  updateTextInput(session, "edtJobDesc", value = selectedRow$desc)
  updateSelectInput(session, "edtRequestor", choices = c(Choose="", employeeName()), selected = selectedRow$requestor_name) # getEmployeeTable()$name
  updateSelectInput(session, "edtJobPayTerm", selected = selectedRow$`pay_term`)
  updateSelectInput(session, "edtJobBillType", selected = selectedRow$`billing_type_name`)
  updateSelectInput(session, "edtJobEstimateDay", selected = selectedRow$`estimate_day_plan`)
  updateSelectInput(session, "edtJobEstimateRev", selected = selectedRow$`estimate_revenue_sales`)
  updateSelectInput(session, "edtJobEstExpensesCap", selected = selectedRow$`est_expenses_cap`)
  updateRadioButtons(session, "edtJobEstExpensesType", selected = selectedRow$`drawdown_method`)
  updateSelectInput(session, "edtJobStatus", selected = selectedRow$status_name)
  updateTextInput(session, "edtKeyContact", value = selectedRow$key_contact)

  updateTextInput(session, "edtNotes", value = selectedRow$notes)

  updateDateInput(session, "edtCreationDate", value = selectedRow$creation_date)
  updateDateInput(session, "edtGoLiveDate", value = selectedRow$go_live_date)

  updateCheckboxInput(session, "edtJob_end_of_month", value = ifelse(selectedRow$pay_method == "End_of_month", TRUE, FALSE))

  updateSelectInput(session, "edtProjectType", selected=if(selectedRow$perc_strat==100){
    "Strategy"
  } else if(selectedRow$perc_anal==100){
    "Analysis"
  } else if (selectedRow$perc_appdev==100){
    "App Dev"
  } else if (selectedRow$perc_env==100){
    "Environments"
  } else if (selectedRow$perc_train==100){
    "Training"
  })


  updateSliderInput(session, "edtProject_management", value = selectedRow$project_management)
  updateSliderInput(session, "edtTravel", value = selectedRow$travel)
  updateSliderInput(session, "edtLearning", value = selectedRow$learning)
  updateSliderInput(session, "edtKnowledge", value = selectedRow$knowledge)
  #updateSelectInput(session, "edtJobCurrency", value = selectedRow$currency)
  #updateNumericInput ##FUNCTIONALITY FOR EXCHANGE RATE and CURRENCY. I THINK BEST TO LEAVE BLANK WHEN EDITING AS NOT SURE WHAT VALUE TO PRE-LOAD.

  personTbl <- getProjectRole(selectedRow$id)
  persTbl$personUIData <- persTbl$personDBData <- personTbl

  # get the rates for all roles which exist for this project
  roles <- unique(personTbl[,c("job_id", "role_id")])
  roleRates <- getRoleRates(selectedRow$id)
  roleRates$role_id <- as.numeric(roleRates$role_id)
  roleTbl <- left_join(roles, roleRates, by=c("role_id"="role_id"))
  roleTbl <- roleTbl[,c("role_id", "rate")]
  roleTbl$id <- seq_len(nrow(roleTbl))

  if(nrow(roleTbl)>0){
    roleTbl$job_id <- selectedRow$id
  }

  persTbl$roleUIData <- persTbl$roleDBData <- roleTbl

  # triggers rebuild of personnel table
  persTbl$recordState <- persTbl$recordState + 1

})


################################################################################
# PERSONNEL TABLE - REACTIVE VALUES                                            #
################################################################################

# reactive values for data in the edit tab
persTbl <- reactiveValues(
  roleUIData = list(),
  roleDBData = list(),
  personUIData = list(),
  personDBData = list(),
  recordState = 1,
  dataSame = TRUE
)

# reactive values for data in the new tab
persTblNew <- reactiveValues(
  roleNew = list(),
  personNew = list(),
  recordState = 1
)

################################################################################
# PERSONNEL TABLE - UI COMPONENTS                                              #
################################################################################

# UI stuff for "add role" button in 'edit job' view
output$edtAddRoleBox <- renderUI({

  fluidRow(
    column(
      8,
      selectInput(
        "edtJRoles",
        label = "Job Roles",
        choices = c(Choose="",
                    setdiff(getRoles(),unlist(lapply(persTbl$roleUIData$role_id, getDescFromId))))
      )
    ),
    column(
      4,
      align = 'right',
      goButton(
        paste0("edtAddRole"), 
        label = "Add Role",
        btn.style = "primary"
      ),
      style="margin-top: 25px;"
    )
  )
})

# UI stuff for "add role" button in 'new job' view
output$newAddRoleBox <- renderUI({

  fluidRow(
    column(
      8,

      selectInput(
        "edtJRolesNew",
        label = "Job Roles",
        choices = c(Choose="",
                    setdiff(getRoles(),unlist(lapply(persTblNew$roleNew$role_id, getDescFromId))))
      )
    ),
    column(
      4,
      align = 'right',
      goButton(
        paste0("edtAddRoleNew"), 
        label = "Add Role",
        btn.style = "primary"
      ),
      style="margin-top: 25px;"
    )
  )

})

# UI code for the personnel table - edit view
output$edtPersonnelUI <- renderUI({
  if ((getProjectRole(jobid = input$selectedJobId)[, "emp_id"] %>% anyDuplicated()) > 0) {
    createAlert(session, "alert", "exampleAlert", title="Oops", dismiss = FALSE, style = "danger",
                content=" You selected the same employee for multiple roles!", append=TRUE)
  } else {
    closeAlert(session, "exampleAlert")
  }

  # depends on this to check for updates
  persTbl$recordState

  # get the data without triggering a UI rebuild
  roleData <- isolate(persTbl$roleUIData)
  personData <- isolate(persTbl$personUIData)

  # get the unique roles from the data
  uniqueRoles <- roleData$role_id

  # make the rows for the personnel table
  myRows <- lapply(seq_along(uniqueRoles), function(i){
    role <- getDescFromId(uniqueRoles[i])
    people <- personData[personData$role_id==uniqueRoles[i],]$emp_id
    people <- unlist(lapply(people, getPersonName))
    rate <- roleData$rate[roleData$role_id == uniqueRoles[i]] # get rate from the table
    edtCreateRow(roleData$id[i], role, rate, people)
  })

  # output these
  # column(12, do.call(fluidRow, myRows))
  column(12, lapply(myRows, fluidRow))

})


# create_row function to track widgets and set up observers - edit job view
edtCreateRow <- (function(){

  inited <- 0

  function(wid, role, rate, people){

    w <- fluidRow(

      # Role name
      column(3, p(role), style = "margin-top: 15px;"),

      # Role rate selector
      column(
        3,
        numericInput(
          paste0("edt_", wid, "_rate"),
          "",
          value = rate,
          min = 0,
          max = 10000
        ),
        style = "margin-top: 0px;"
      ),

      # Employee selector
      column(
        4,
        selectInput(
          paste0("edt_" , wid, "_name"),
          label = NULL,
          selected = people,
          choices = sort(employeeName()),
          multiple = TRUE,
          selectize = TRUE
        ),
        style = "margin-top: 20px;"
      ),
      column(
        2,
        align = 'right',
        goButton(
          paste0("del_", wid, "_role"), 
          label = "",
          icon = icon("trash")
        ),
        style = "margin-top: 20px;"
      )
    )

    if(wid > inited){

      # observer for the rate to update the UI Table
      observeEvent(input[[paste0("edt_", wid, "_rate")]],{
        roleId <- persTbl$roleUIData$role_id[persTbl$roleUIData$id == wid]

        if(!is.null(input[[paste0("edt_", wid, "_rate")]])){

          persTbl$roleUIData$rate[persTbl$roleUIData$role_id==roleId] <- input[[paste0("edt_", wid, "_rate")]]

        }
      })

      # observer for the name
      observeEvent(input[[paste0("edt_", wid, "_name")]],{

        # get the role ID from the wid
        roleId <- persTbl$roleUIData$role_id[persTbl$roleUIData$id == wid]

        # get the person table
        data <- isolate(persTbl$personUIData)

        newTbl <- data[data$role_id != roleId,]

        # if the box is not null
        if(length(input[[paste0("edt_", wid, "_name")]]) > 0){

        # get the IDs for all people associated with this role
        emps <- unlist(lapply(input[[paste0("edt_", wid, "_name")]], function(i){
          getPersonId(i)
        }))



        # make rows for these roles
        newRows <- data.frame(job_id = rep(as.integer(input$selectedJobId), length(emps)),
                              emp_id = emps,
                              role_id = rep(roleId, length(emps)))

        newTbl <- rbind(newTbl, newRows)

       }

        # Set UI data to these values
        persTbl$personUIData <- newTbl

      }, ignoreNULL=FALSE)

           # oberver for delete
      observeEvent(input[[paste0("del_", wid, "_role")]], {

        roleId <- persTbl$roleUIData$role_id[persTbl$roleUIData$id == wid]

        persTbl$roleUIData <- persTbl$roleUIData[persTbl$roleUIData$role_id!=roleId, ,drop = FALSE]

        persTbl$personUIData <- persTbl$personUIData[persTbl$personUIData$role_id != roleId, ]

        persTbl$recordState <- persTbl$recordState + 1
      })

      inited <<- wid

    }

    w
  }

})()


# UI code for the personnel table - add job view
output$newPersonnelUI <- renderUI({

  # depends on this to check for updates
  persTblNew$recordState

  # get the data without triggering a UI rebuild
  roleData <- isolate(persTblNew$roleNew)
  personData <- isolate(persTblNew$personNew)

  # get the unique roles from the data
  uniqueRoles <- roleData$role_id

  # make the rows for the personnel table
  myRows <- lapply(seq_along(uniqueRoles), function(i){
    role <- getDescFromId(uniqueRoles[i]) # get job name from role_id
    people <- personData[personData$role_id==uniqueRoles[i],]$emp_id # get people associated with that role
    people <- unlist(lapply(people, getPersonName)) # get names from IDs
    rate <- roleData$rate[roleData$role_id == uniqueRoles[i]] # get rate from the table
    createRowNew(roleData$id[i], role , rate, people)
  })

  # output these
  column(12, do.call(fluidRow, myRows))

})



# create_row function to track widgets and set up observers - new job view
createRowNew <- (function(){

  inited <- 0

  function(wid, role, rate, people){

      w <- fluidRow(

      # Role name
      column(3, p(role), style = "margin-top: 15px;"),

      # Role rate selector
      column(3,
             numericInput(
               paste0("new_", wid, "_rate"),
               "",
               value = rate,
               min = 0,
               max = 10000
             ),
             style = "margin-top: 0px;"),

      # Employee selector
      column(
        4,
        selectInput(
          paste0("new_" , wid, "_name"),
          label = NULL,
          selected = people,
          choices = sort(employeeName()),
          multiple = TRUE,
          selectize = TRUE
        ),
        style = "margin-top: 20px;"
      ),
      column(
        2,
        align = 'right',
        goButton(
          paste0("newdel_", wid),
          label = "",
          icon = icon("trash")
        ),
        style = "margin-top: 20px;"
      )
    )

    if(wid > inited){

      # observer for the rate to update the UI Table
      observeEvent(input[[paste0("new_", wid, "_rate")]],{

        persTblNew$roleNew[persTblNew$roleNew$id==wid,]$rate <- input[[paste0("new_", wid, "_rate")]]

      })

      # observer for the name
      observeEvent(input[[paste0("new_", wid, "_name")]],{

        # get the name of the widget from the UI
        inputNames <- paste0("new_", wid, "_name") # which personnel

        # get the roleID for this widget
        roleId <- persTblNew$roleNew$role_id[persTblNew$roleNew$id == wid]

        # get the person table
        data <- isolate(persTblNew$personNew)

        # get data without that role
        newTbl <- data[data$role_id != roleId,]

        # if the box isn't empty...
        if(length(input[[inputNames]]) > 0){

          # for each name, get the ID
          emps <- unlist(lapply(input[[inputNames]], function(i){
            getPersonId(i)
          }))

          # make a load of new rows with each person and role
          newRows <- data.frame(emp_id = emps,
                                role_id = rep(roleId, length(emps)))

          # bind the old table to the new one
          newTbl <- rbind(newTbl, newRows)

        }

        # Set UI data to these values
        persTblNew$personNew <- newTbl

      }, ignoreNULL=FALSE)

      # oberver for delete
      observeEvent(input[[paste0("newdel_", wid)]], {

        roleId <- persTblNew$roleNew$role_id[persTblNew$roleNew$id == wid]

        # remove the chosen role from the table
        persTblNew$roleNew <- persTblNew$roleNew[persTblNew$roleNew$role_id != roleId, ]

        # remove all people associate with that role
        persTblNew$personNew <- persTblNew$personNew[persTblNew$personNew$role_id != roleId, ]

        # trigger UI rebuild to remove that row
        persTblNew$recordState <- persTblNew$recordState + 1

      })
      inited <<- wid

    }

    w
  }

})()



################################################################################
# PERSONNEL TABLE - OBSERVERS                                                  #
################################################################################

# if we select the "new job" tab, initialise the table for the UI data
observeEvent(input$job_tab,{
  if(input$job_tab=="New Job"){
    persTblNew$roleNew <- data.frame(role_id=numeric(0), rate=numeric(0), id = numeric(0))
    persTblNew$personNew <- data.frame(emp_id=character(0), role_id=numeric(0))
  }
})

# observer for if we click to add a new role (edit job screen)
observeEvent(input$edtAddRole,{

  # only run if there's any roles left to select
  if(input$edtJRoles!=""){

    shinyjs::hide("edtAddRole")

    # create a new widget ID
    newid <- if(nrow(persTbl$roleUIData) ==0){
      1
    } else {
      max(as.numeric(persTbl$roleUIData$id)) + 1
    }
    # blank row for our new role
    newRow <- data.frame(
      job_id = input$selectedJobId,
      role_id = getIdFromRole(input$edtJRoles),
      rate = getRate(input$edtJRoles, input$selectedJobId),
      id = newid
    )

    # if there's already data loaded, bind it to the blank row
    if(nrow(persTbl$roleUIData)>0){
      # add a new row to the role table
      persTbl$roleUIData <- rbind(
        persTbl$roleUIData,
        newRow
      )
      # if not, just set the uidata to the blank row
    } else {
      persTbl$roleUIData <- newRow
    }

    # trigger UI rebuild
    persTbl$recordState <- persTbl$recordState + 1

    ## test if these are the same
    #persTbl$dataSame <- identical(persTbl$roleUIData, persTbl$roleDBData)
    shinyjs::delay(50, shinyjs::show("edtAddRole"))
  }

})


# observer for if we click to add a new role
observeEvent(input$edtAddRoleNew,{

  # only run if there's any roles left to select
  if(input$edtJRolesNew!=""){

    shinyjs::hide("edtAddRoleNew")

    # create a new widget ID
    newid <- if(nrow(persTblNew$roleNew) == 0){
      1
    } else {
      max(as.numeric(persTblNew$roleNew$id)) + 1
    }

    # blank row for our new role
    newRow <- data.frame(
      role_id = getIdFromRole(input$edtJRolesNew),
      rate = getRate(input$edtJRolesNew, input$selectedJobId), # as above
      id = newid
    )

    # if there's already data loaded, bind it to the blank row
    if(nrow(persTblNew$roleNew)>0){

      # add a new row to the role table
      persTblNew$roleNew <- rbind(
        persTblNew$roleNew,
        newRow
      )
      # if not, just set the uidata to the blank row
    } else {

      persTblNew$roleNew <- newRow
    }

    # trigger UI rebuild
    persTblNew$recordState <- persTblNew$recordState + 1

    shinyjs::delay(50, shinyjs::show("edtAddRoleNew"))
  }

})


# ============================================================
# Click "Confirm" button -> update DB
# Edit job and po tables
observeEvent(input$edtJobGo, {
  validate(need(input$selectedJobId, "TO UPDATE TABLES PLEASE SELECT A ROW"))

  # Update the internal database job
  editJob(id = input$selectedJobId,
          desc = input$edtJobDesc,
          cust_id = pollCustomer()$id[pollCustomer()$customer_name==input$edtJobCustomer],
          status_id = ifelse(input$edtJobStatus=="",
                             "",
                             statusTbl$id[statusTbl$name==input$edtJobStatus]),
          bus_unit_id = bus_unitTbl$id[bus_unitTbl$name==input$edtJobBusUnit],
          project_management = input$edtProject_management,
          travel = input$edtTravel,
          learning =input$edtLearning,
          knowledge =input$edtKnowledge,
          requestor = ifelse(input$edtRequestor=="",
                      "",
                      pollEmployee()$id[pollEmployee()$name==input$edtRequestor]),
          key_contact = input$edtKeyContact,
          notes = input$edtNotes,
          perc_strat = ifelse(input$edtProjectType == "Strategy", 100, 0),
          perc_anal = ifelse(input$edtProjectType == "Analysis", 100, 0),
          perc_train =ifelse(input$edtProjectType == "Training", 100, 0),
          perc_env = ifelse(input$edtProjectType == "Environments", 100, 0),
          perc_appdev = ifelse(input$edtProjectType == "App Dev", 100, 0),
          pay_method= ifelse(input$edtJob_end_of_month, "End_of_month", "Net"),
          estimate_day_plan= ifelse(is.na(input$edtJobEstimateDay), 0, input$edtJobEstimateDay),
          estimate_revenue_sales= ifelse(is.na(input$edtJobEstimateRev), 0, input$edtJobEstimateRev),
          est_expenses_cap= ifelse(is.na(input$edtJobEstExpensesCap), 0, input$edtJobEstExpensesCap),
          drawdown_method= input$edtJobDrawdownMethod,
          pay_term= input$edtJobPayTerm,
          bill_type_id = ifelse(input$edtJobBillType=="",
                                "",
                                billingTbl$id[billingTbl$name==input$edtJobBillType]),
          go_live_date = ifelse(is.null(input$edtGoLiveDate),
                            "",
                            format(as.Date(input$edtGoLiveDate), "%d/%m/%Y"))
  )

  # Update the internal database personnel
  editRoles(uiRoleTbl = persTbl$roleUIData, jobid = input$selectedJobId)
  editPersonnel(uiPersonTbl = persTbl$personUIData, dbPersonTbl = persTbl$personDBData)

  persTbl$roleDBData <- persTbl$roleUIData
  persTbl$personDBData <- persTbl$personUIData
  persTbl$dataSame = TRUE


  # Update the internal database po - edit existing and add new
  for(idx in row.names(po())) {
    edtPO(id=po()[idx, "id"], job_id=input$selectedJobId, name=input[[idx]], amount = input[[paste0(idx, "amt")]])
  }

  addPO(tab="edit", name=input$edtJobPO, job_id=input$selectedJobId, amount=input$edtJobPOAmt)

  # Click "Confirm" button -> message, reset & go to view
  shinyjs::info("Job updated. Thank you!")
  resetEdtJobPage()
  updateTabsetPanel(session,
                    inputId = "job_tab",
                    selected = "Job View")

})

# Add for job and po tables
observeEvent(input$addJobGo, {

  validate(need(expr=input$addJobDesc, message = paste("TO ADD A JOB PLEASE WRITE A DESCRIPTION"))%>%
             need(expr=input$addJobCustomer, message= paste("TO ADD A JOB PLEASE SELECT A CUSTOMER")))

  # update the internal database job
  jobId <- createJob(desc=input$addJobDesc,
                     cust_id=pollCustomer()$id[pollCustomer()$customer_name==input$addJobCustomer],
                     status_id=ifelse((length(statusTbl$id[statusTbl$name==input$addJobStatus]))>=1,statusTbl$id[statusTbl$name==input$addJobStatus], "NA"),
                     requestor = ifelse((length(pollEmployee()$id[pollEmployee()$name==input$addRequestor]))>=1,pollEmployee()$id[pollEmployee()$name==input$addRequestor], "NA"),
                     bus_unit_id = bus_unitTbl$id[bus_unitTbl$name==input$addJobBusUnit],
                     project_management = input$addProject_management,
                     travel=input$addTravel,
                     learning=input$addLearning,
                     knowledge=input$addKnowledge,
                     key_contact = input$addKeyContact,
                     notes = input$addNotes,
                     perc_strat = ifelse(input$addProjectType == "Strategy", 100, 0),
                     perc_anal = ifelse(input$addProjectType == "Analysis", 100, 0),
                     perc_train =ifelse(input$addProjectType == "Training", 100, 0),
                     perc_env = ifelse(input$addProjectType == "Environments", 100, 0),
                     perc_appdev = ifelse(input$addProjectType == "App Dev", 100, 0),
                     pay_method = ifelse(input$addJob_end_of_month, "End_of_month", "Net"),
                     estimate_day_plan= input$addJobEstimateDay,
                     estimate_revenue_sales= input$addJobEstimateRev,
                     est_expenses_cap= input$addJobEstExpensesCap,
                     drawdown_method = input$addJobDrawdownMethod,
                     pay_term= input$addJobPayTerm,
                     bill_type_id=ifelse((length(billingTbl$id[billingTbl$name==input$addJobBillType]))>=1, billingTbl$id[billingTbl$name==input$addJobBillType], "NA"),
                     creation_date = format(as.Date(lubridate::today()), "%d/%m/%Y"),
                     go_live_date=ifelse(is.null(input$addGoLiveDate),
                                         "",
                                         format(as.Date(input$addGoLiveDate), "%d/%m/%Y"))
  )

  # now we've generated the new jobId, we want to add it to our tables and save them
  if(nrow(persTblNew$roleNew)>0){
    persTblNew$roleNew$job_id <- jobId
    saveNewRole(persTblNew$roleNew)
  }

  if(nrow(persTblNew$personNew)>0){
    persTblNew$personNew$job_id <- jobId
    saveNewPersonnel(persTblNew$personNew)
  }

  # update the internal database po
  addPO(tab="add", name=input$addJobPO, amount= input$addJobPOAmt)

  # Click "Confirm" button -> message, reset & go to view
  shinyjs::info(paste("New job:", jobId, "added. Thank you!"))
  resetNewJobPage()
  updateTabsetPanel(session,
                    inputId = "job_tab",
                    selected = "Job View")
})



# ============================================================
# Click "Cancel" button -> go back to View & reset data
# Cancel edited
observeEvent(input$edtJobCancel,{

  resetEdtJobPage()
  updateTabsetPanel(session,
                    inputId = "job_tab",
                    selected = "Job View")
  JobProxy %>% selectRows(NULL)

})


# Cancel added
observeEvent(input$addJobCancel,{

  resetNewJobPage()
  updateTabsetPanel(session,
                    inputId = "job_tab",
                    selected = "Job View")

})

# ============================================================
# Job description and customer are compulsory (edit)
observe({
  if (is.null(input$edtJobDesc) || input$edtJobDesc == "" || is.null(input$edtJobCustomer) || input$edtJobCustomer == "") {
    shinyjs::disable("edtJobGo")
  } else {
    shinyjs::enable("edtJobGo")
  }
})

# Job description and customer are compulsory (add)
observe({
  if (is.null(input$addJobDesc) || input$addJobDesc == "" || is.null(input$addJobCustomer) || input$addJobCustomer == "") {
    shinyjs::disable("addJobGo")
  } else {
    shinyjs::enable("addJobGo")
  }
})

# resets all values in the edit job view
resetEdtJobPage <- reactive({
  shinyjs::reset("edtJob_page")
  persTbl$roleUIData <- persTbl$roleUIData[0,]
  persTbl$roleDBData <- persTbl$roleDBData[0,]
  persTbl$personUIData <- persTbl$personUIData[0,]
  persTbl$personDBData <- persTbl$personDBData[0,]
  persTbl$recordState <- persTbl$recordState + 1
})

# resets all values in the new job view
resetNewJobPage <- reactive({
  shinyjs::reset("addJob_page")
  persTblNew$roleNew <- persTblNew$roleNew[0,]
  persTblNew$personNew <- persTblNew$personNew[0,]
  persTblNew$recordState <- persTblNew$recordState + 1
})

