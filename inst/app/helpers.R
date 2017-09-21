require(RSQLite)
require(DBI)
require(plyr)
require(dplyr)
require(lubridate)

internal_data <- new.env()

#' Provide a handle to the database
#'
#' This is also the only place one needs to update the
#' exact location of the database.
#'
#' @keywords internal
#' @importFrom DBI dbIsValid, dbConnect
#' @importFrom RSQLite SQLite
db <- function() {

  # Check if connection is still valid, open a new one if not
  if (!exists("conn", envir = internal_data) ||
      is.null(internal_data$conn) ||
      !DBI::dbIsValid(internal_data$conn)) {
    conn <- dbConnect(SQLite(), dbfile)
    assign("conn", conn, envir = internal_data)
  }
  internal_data$conn
}


#' list all tables in DB
listTable <- function(){

  DBI::dbListTables(db())
}

#' Extract table from database
#'
#'
#' @keywords internal
#' @import dplyr
#' @examples
#' getTable()
#' getTable(theTable = "job")
#' getTable(theTable = "invoice")
#' getTable(theTable = "status")
#' getTable(theTable = "payment_method")
#' getTable(theTable = "customer")
getTable <- function(theTable = "industry") {

  DBI::dbReadTable(db(), theTable)
}

#' Get lookup tables from DB. Hard coded as not subject to change.
statusTbl <- getTable("status")
bus_unitTbl <- getTable("business_unit")
industryTbl <- getTable("industry")
paymentTbl <- getTable("payment_method")
billingTbl <- getTable("billing_type")
roleTbl <-  getTable("role")

#---------------
# customer
#---------------
#' Get the customer table
getCustomerData <- function() {

  res <- dbGetQuery(db(), "SELECT * FROM customer") %>%
    mutate(acc_mgr = as.integer(acc_mgr), ind_id = as.integer(ind_id)) %>%
    left_join(dbGetQuery(db(), "SELECT * FROM industry"), by = c("ind_id" = "id")) %>%
    dplyr::rename(customer_name=name.x, industry_name=name.y) %>%
    left_join(dbGetQuery(db(), "SELECT id, name FROM employee"), by=c("acc_mgr" = "id")) %>%
    dplyr::rename(account_manager_name=name) %>%
    #mutate(business_unit = ifelse(industry_name %in% "Pharma", "Pharma", "Data Science")) %>%
    select(id, customer_name, industry_name, account_manager_name) #, business_unit

  res
}

#' Check if customer exists
checkCustomer <- function(cusName){

  count <- dbGetQuery(db(),
             sqlInterpolate(
               ANSI(),
               "SELECT COUNT(*) FROM customer WHERE name is ?cusName",
               cusName = cusName
             )
  )

  dbDisconnect(db())

  if(count$COUNT > 0){
    TRUE
  } else {
    FALSE
  }
}

#' Add new customer
#'
#' @examples
#' createCustomer(cusName="Mango the Cat", indId=2, accntMngrId=1)
createCustomer <- function(cusName, indId, accntMngrId) {
  maxId <- dbGetQuery(db(), sqlInterpolate(ANSI(), "SELECT MAX(id) from customer"))[, 1]
  cusId <- maxId + 1

  dbGetQuery(db(),
             sqlInterpolate(
               ANSI(),
               "INSERT INTO customer (id, name, ind_id, acc_mgr)
               VALUES (?cusId, ?cusName, ?indId, ?accntMngrId)",
               cusId = cusId,
               cusName = cusName,
               indId = as.character(indId),
               accntMngrId = as.character(accntMngrId)
             )
  )

  dbDisconnect(db())

  cusId
}

#' Edit existing customer
#'
#' @examples
#' editCustomer(cusId=185, cusName="Microsoft", indId=1, accntMngrId=1)
editCustomer <- function(cusId, cusName="", indId, accntMngrId="") {

  dbGetQuery(db(),
             sqlInterpolate(
               ANSI(),
               "UPDATE customer SET name=?cusName, ind_id=?indId,
               acc_mgr=?accntMngrId WHERE id = ?cusId",
               cusName = cusName,
               indId = as.character(indId),
               accntMngrId = as.character(accntMngrId),
               cusId = cusId
             )

  )

  dbDisconnect(db())

}




#' Delete a customer from the customer table based on id
#'
#' @examples
#' deleteCustomer(ID=197)
deleteCustomer <- function(ID) {

  sql <- sqlInterpolate(
    ANSI(),
    "DELETE FROM customer WHERE id =  ?ID",
    ID = ID
  )

  dbGetQuery(db(), sql)
  dbDisconnect(db())

}


#---------------
# employee
#---------------
#' Get the employee data for view, joined from emploee and role
#'
#' @examples
#' getEmployeeTable()
getEmployeeTable <- function() {

  res <- dbGetQuery(db(), "SELECT id, name, role_id, contractor, inactive  FROM employee") %>%
    left_join(dbGetQuery(db(), "SELECT id, name AS job_role FROM role"), by = c("role_id" = "id")) %>%
    # Convert from boolean to Yes/No
    mutate(contractor = ifelse(contractor==1, "Yes","No")) %>%
    mutate(inactive = ifelse(inactive==0, "Yes","No")) %>%
    select(id, name, job_role, contractor, active=inactive)

  res
}


#' Edit existing employee
#'
#' @examples
#' edtEmployee(name="Diksha Rajen", jobRoleId=20, contractor=0, inactiveStatus=1, id=83)
edtEmployee <- function(name, jobRoleId, contractor, inactiveStatus, id){
  # Construct the update query
  query <- sqlInterpolate(
    ANSI(),
    "UPDATE employee SET name = ?name, role_id = ?jobRoleId,
    contractor = ?contractor, inactive = ?inactiveStatus WHERE id = ?id",
    name = name, jobRoleId = as.character(jobRoleId), contractor = contractor,
    inactiveStatus = inactiveStatus, id = id
  )

  # Submit the update query and disconnect
  dbGetQuery(db(), query)
  dbDisconnect(db())

}

#' Add new employee
#'
#' @examples
#' addEmployee(name="Steven Jobs", jobRoleId=1, contractor=1)
addEmployee <- function(name, jobRoleId, contractor){

  # Calculate new id
  maxId <- dbGetQuery(db(), "SELECT MAX(id) from employee")[,1]
  empId <- maxId + 1

  # Construct the insert query
  query <- sqlInterpolate(
      ANSI(),
      "INSERT INTO employee (id, name, role_id, contractor)
          VALUES (?empId, ?name, ?jobRoleId, ?contractor)",
      empId = empId, name = name, jobRoleId = as.character(jobRoleId),
      contractor = contractor
    )

  # Submit the update query and disconnect
  dbGetQuery(db(), query)
  dbDisconnect(db())

  empId
}

#' Delete employee
#'
#' @examples
#' deleteEmployee(ID="155")
deleteEmployee <- function(ID) {

  query <- sqlInterpolate(
    ANSI(),
    "DELETE FROM employee WHERE id =  ?ID",
    ID = ID
  )

  dbGetQuery(db(), query)
  dbDisconnect(db())

}

#---------------
# job
#---------------
#' Get the job data
#'
#'
#' @examples
#' getRepoData()
getRepoData <- function() {

  res <- dbGetQuery(db(), "SELECT * FROM job") %>%
    left_join(dbGetQuery(db(), "SELECT id, name FROM employee"), by = c("requestor" = "id")) %>%
    select(-requestor) %>%
    dplyr::rename(requestor_name = name) %>%

    left_join(dbGetQuery(db(), "SELECT * FROM billing_type"), by=c("bill_type_id" = "id")) %>%
    dplyr::rename(billing_type_name = name) %>%

    left_join(dbGetQuery(db(), "SELECT * FROM status"), by=c("status_id" = "id")) %>%
    dplyr::rename(status_name = name) %>%

    left_join(getCustomerData(), by=c("cust_id" = "id")) %>%

    mutate(creation_date = as.Date(as.character(creation_date), format="%d/%m/%Y")) %>%
    mutate(go_live_date = as.Date(as.character(go_live_date), format="%d/%m/%Y")) %>%

    mutate(bus_unit_id = as.integer(bus_unit_id)) %>%
    left_join(bus_unitTbl, by=c("bus_unit_id" = "id")) %>% # same as `dbGetQuery(db(), "SELECT * FROM business_unit")`
    dplyr::rename(bus_unit_name = name)

  res
}



#' Add new job
#'
#' @examples
#' createJob(desc="Im a test", cust_id=183, status_id=1, bus_unit_id=2, requestor=1, travel=100, learning=100, knowledge=100,
#'           key_contact="A contact person", notes="Notes here",  perc_strat=100, perc_anal=100, creation_date="03/11/2016",
#'           go_live_date="03/11/2016",
#'           perc_appdev=0, perc_env=0, perc_train=0, pay_method= "End_of_month", estimate_day_plan=40,
#'           est_expenses_cap=9999, drawdown_method="time",
#'           estimate_revenue_sales=2000, pay_term=0, bill_type_id=2)
createJob <- function(desc, cust_id, status_id="", bus_unit_id="", requestor="", project_management="", travel="",
                      learning="", knowledge="", key_contact="", notes="", perc_strat="", perc_anal="", perc_appdev="",
                      perc_env="", perc_train="", pay_method="", estimate_day_plan="", estimate_revenue_sales="",
                      est_expenses_cap="", drawdown_method="", pay_term="",
                      bill_type_id="", creation_date="", go_live_date=""){

  if(missing(desc) || missing(cust_id)) stop("Please provide a job scription and customer.")

  maxId <- dbGetQuery(db(), sqlInterpolate(ANSI(), "SELECT MAX(id) from job"))[ ,1]
  id <- maxId + 1

  query <- sqlInterpolate(
    ANSI(),
    "INSERT INTO job (id, desc, cust_id, status_id, bus_unit_id, requestor, project_management, travel,
    learning, knowledge, key_contact, notes, perc_strat, perc_anal, perc_appdev, perc_env, perc_train,
    pay_method, estimate_day_plan, estimate_revenue_sales, est_expenses_cap, drawdown_method, pay_term,
    bill_type_id, creation_date, go_live_date)
    VALUES(?id, ?desc, ?cust_id, ?status_id, ?bus_unit_id, ?requestor, ?project_management, ?travel,
    ?learning, ?knowledge, ?key_contact, ?notes, ?perc_strat, ?perc_anal, ?perc_appdev, ?perc_env, ?perc_train,
    ?pay_method, ?estimate_day_plan, ?estimate_revenue_sales, ?est_expenses_cap, ?drawdown_method, ?pay_term,
    ?bill_type_id, ?creation_date, ?go_live_date)",
    id = id,
    desc = desc,
    cust_id = cust_id,
    status_id = status_id,
    bus_unit_id = bus_unit_id,
    requestor = requestor,
    project_management = project_management,
    travel = travel,
    learning = learning,
    knowledge = knowledge,
    key_contact = key_contact,
    notes = notes,
    perc_strat = perc_strat,
    perc_anal = perc_anal,
    perc_appdev = perc_appdev,
    perc_env = perc_env,
    perc_train = perc_train,
    pay_method = pay_method,
    estimate_day_plan = estimate_day_plan,
    estimate_revenue_sales = estimate_revenue_sales,
    est_expenses_cap = est_expenses_cap,
    drawdown_method = drawdown_method,
    pay_term = pay_term,
    bill_type_id = bill_type_id,
    creation_date = creation_date,
    go_live_date = go_live_date
  )

  DBI::dbGetQuery(db(), query)

  dbDisconnect(db())

  id

}


#' Edit existing job/project
#'
#' creation_date  not editable once a job is created
#' @examples
#' editJob(id=2885, desc="SB", cust_id=197, status_id=3, project_management=50, travel=100, learning=50, knowledge=10,
#'         requestor=1, key_contact="A contact person", notes="Notes here", bus_unit_id=2, perc_strat=100, perc_anal=0,
#'         perc_appdev=0, perc_env=0, perc_train=0, pay_method= "End_of_month", pay_term=30, estimate_day_plan=40,
#'         estimate_revenue_sales=2000, est_expenses_cap=0, go_live_date="03/11/2016")
editJob <- function(id, desc, cust_id, status_id, project_management, travel, learning, knowledge,
                    requestor="", key_contact, notes="", bus_unit_id, perc_strat, perc_anal,
                    perc_appdev, perc_env, perc_train, pay_method, estimate_day_plan,
                    estimate_revenue_sales, pay_term, bill_type_id,
                    go_live_date="", est_expenses_cap,
                    drawdown_method=""){

  if(missing(id) || missing(desc) || missing(cust_id)) stop("Please provide a job number, description and customer.")

  query <- sqlInterpolate(
    ANSI(),
    "UPDATE job SET desc=?desc, cust_id=?cust_id, status_id=?status_id, project_management=?project_management,
    travel=?travel, learning=?learning, knowledge=?knowledge, requestor=?requestor, key_contact=?key_contact,
    notes=?notes, bus_unit_id=?bus_unit_id, perc_strat=?perc_strat, perc_anal=?perc_anal, perc_appdev=?perc_appdev,
    perc_env=?perc_env, perc_train=?perc_train, pay_method=?pay_method, estimate_day_plan=?estimate_day_plan,
    estimate_revenue_sales=?estimate_revenue_sales, pay_term=?pay_term, bill_type_id=?bill_type_id,
    go_live_date=?go_live_date, est_expenses_cap=?est_expenses_cap,
    drawdown_method=?drawdown_method where id=?id",
    desc = desc,
    cust_id = cust_id,
    status_id = status_id,
    project_management = project_management,
    travel = travel,
    learning = learning,
    knowledge = knowledge,
    requestor = requestor,
    key_contact = key_contact,
    notes = notes,
    bus_unit_id = bus_unit_id,
    perc_strat = perc_strat,
    perc_anal = perc_anal,
    perc_appdev = perc_appdev,
    perc_env = perc_env,
    perc_train = perc_train,
    pay_method = pay_method,
    estimate_day_plan = estimate_day_plan,
    estimate_revenue_sales = estimate_revenue_sales,
    pay_term = pay_term,
    bill_type_id = bill_type_id,
    #creation_date = creation_date,
    go_live_date = go_live_date,
    est_expenses_cap = est_expenses_cap,
    drawdown_method = drawdown_method,
    id = id
  )

  DBI::dbGetQuery(db(), query)

  dbDisconnect(db())

}

#---------------
# Purchase Order
#---------------
#' Get the PO data
#' Multiple jobs can have PO of the same name, but for a single job number PO names must be unique
getPO <- function(){
  dbGetQuery(db(), "SELECT * FROM po")
}


#' Edit existing PO
#'
#' @examples
#' edtPO(id=1, name="123456", job_id=2729)
#' edtPO(id=317, name="PO-4", job_id=2969, amount=10001)
edtPO <- function(id, name="", job_id, amount){

  if (is.na(name) || name=="" || is.null(name)) deletePO(ID = id)

  amount <- ifelse(is.na(amount), "", amount)

  query <- sqlInterpolate(
    ANSI(),
    "UPDATE po SET name=?name, amount=?amount where id=?id AND job_id=?job_id",
    name = name,
    amount = amount,
    id = id,
    job_id = job_id
  )
  dbGetQuery(db(), query)
  dbDisconnect(db())
}



#' Add new PO
#'
#' @examples
#' # Add PO on the edit tab
#' addPO(tab="edit", name="Orisis", job_id=2729)
#' # Add PO on the add tab
#' addPO(tab="add", name="Iris")
addPO <- function(tab=c("edit", "add"), name, job_id, amount){

  if(is.na(name) || is.null(name) || name=="") return(NULL)

  amount <- ifelse(is.na(amount), "", amount) # Set initial value of numericInput to NA instead of 0 so that when
  # it pops pop first time user see emplty string instead of number 0.
  # Ideally could set initial to "".
  maxId <- dbGetQuery(db(), "SELECT MAX(id) from po")[ ,1]
  id <- maxId + 1

  job_id <- switch(tab,
                   edit = job_id,
                   add = dbGetQuery(db(), "SELECT MAX(id) from job")[ ,1]
  )

  query <- sqlInterpolate(
    ANSI(),
    "INSERT INTO po (id, name, job_id, amount)
    VALUES(?id, ?name, ?job_id, ?amount)",
    id = id,
    name = name,
    job_id = job_id,
    amount = amount
  )

  dbGetQuery(db(), query)
  dbDisconnect(db())

}

#' Delete a purchase order from the po table based on id
#'
#' @examples
#' deletePO(ID=318)
deletePO <- function(ID) {

  sql <- sqlInterpolate(
    ANSI(),
    "DELETE FROM po WHERE id =  ?ID",
    ID = ID
  )

  dbGetQuery(db(), sql)
  dbDisconnect(db())

}

#---------------
# Personnel table
#---------------
# Return the names of employees with that role when passed a table representing
# a project
getNamesFromRoles <- function(role,roletbl){
  roletbl[grep(paste0("^",role), roletbl$role_name),]$emp_name
}

# Returns a tabl containing roles and employees for a given project
getPeople <- function(jobid){

  emps <- getTable("project_role")
  emps <- emps[emps$job_id==jobid,]


  allEmps <- getTable("employee")[,c("id", "name")]
  tbl <- merge(emps, allEmps, by.x="emp_id", by.y="id")

  allRoles <- getTable("role")[,c("id", "name")]
  tbl <- merge(tbl, allRoles, by.x="role_id", by.y="id")
  names(tbl) <- c("role_id", "emp_id", "job_id", "emp_name", "role_name")
  tbl
}

getRoleAcronym <- function(role){
  tbl <- getTable("role")
  tbl[tbl$name==role,]$short_name
}

roleAcronymList <- function(){
  roles <- roleTbl$name
  getRoleAcronym(roles)
}

getDefaultRate <- function(role){
  tbl <- getTable("role")
  tbl[tbl$name==role,]$rate
}

getCurrentRoles <- function(jobid){
  tbl <- getTable("project_role")
  roles <- unique(tbl[tbl$job_id==jobid,]$role_id)
  left_join(data.frame(id=roles), getTable("role"), by=c("id"="id"))$name
}

# Get the rate given the role (description) and the job number
getRate <- function(role, jobid){

  # Table of all jobs with associated roles and rates for that job
  tbl <- getTable("role_rates")

  # Refine by the particular job ID
  tbl <- tbl[tbl$job_id==jobid,]

  # Table of role IDs and names
  roleIdName <- getTable("role")[c("id", "name")]

  # Table of role rates for the current job, with role description added
  namesRates <- merge(tbl, roleIdName, by.x="role_id", by.y="id")

  # Get the individual rate for the given role
  myVal <- namesRates[namesRates$name==role,]$rate

  # If the result is in the table, return it
  if(length(myVal)==1){
    myVal
  }

  # If nothing is in the table, return the default rate for that role
  else if(length(myVal)==0){
    getDefaultRate(role)
  }
  else{
    stop(paste0("duplicated values found in role rates table for role id: ",
                role,
                "and jobid: ",
                jobid))
  }

}


getIdFromDesc <- function(roleDesc){
  tbl <- getTable("role")[,c("id", "name")]
  tbl[tbl$name==roleDesc,]$id
}

getPersonId <- function(personName){
  tbl <- getTable("employee")[,c("id", "name")]
  tbl[tbl$name==personName,]$id
}

getRoleRates <- function(jobid = NULL){
  rrt <- dbGetQuery(db(), sqlInterpolate(ANSI(), "SELECT * FROM role_rates"))
  if(!is.null(jobid)){
    rrt <- rrt[rrt$job_id==jobid,]
  }
  rrt$role_id <- as.character(rrt$role_id)
  rrt
}

getProjectRole <- function (jobid = NULL){
  pr <- dbGetQuery(db(), sqlInterpolate(ANSI(), "SELECT * FROM project_role"))
  if(!is.null(jobid)){
    pr <- pr[pr$job_id==jobid,]
  }
  pr
}

getIdFromRole <- function(roleName){
  id <- dbGetQuery(db(), sqlInterpolate(ANSI(), paste0("SELECT id FROM role WHERE name ='", roleName, "'")))
  id$id
}

# given an return the role name
getDescFromId <- function(roleId){
  tbl <- getTable("role")[,c("id", "name")]
  tbl[tbl$id==roleId,]$name
}


# given a person ID returns their name
getPersonName <- function(personID){
  tbl <- getTable("employee")[,c("id", "name")]
  tbl[tbl$id==personID,]$name
}

#' Get role name column
getRoles <- function() {

  # Submit the update query and disconnect
  res <- dbGetQuery(db(), sqlInterpolate(ANSI(), "SELECT name FROM role"))

  res[,'name']
}

#' Edit the role_rates table
editRoles <- function(uiRoleTbl, jobid){

  dbRoleTbl <- dbGetQuery(
    db(),
    sqlInterpolate(ANSI(),
                   "SELECT * FROM role_rates WHERE job_id = ?jobid",
                   jobid = jobid
    )
  )

  dbDisconnect(db())

  needSaving <- anti_join(uiRoleTbl, dbRoleTbl,
                          by=c("role_id"="role_id", "rate"="rate"))

  needDeleting <- anti_join(dbRoleTbl,uiRoleTbl,
                            by=c("role_id"="role_id", "rate"="rate"))

  lapply(seq_len(nrow(needSaving)), function(i){

    query <- sqlInterpolate(
      ANSI(),
      "INSERT INTO role_rates (job_id, role_id, rate)
      VALUES (?jobid, ?roleid, ?rate)",
      jobid = needSaving$job_id[i], roleid = needSaving$role_id[i], rate = needSaving$rate[i]
    )

    dbGetQuery(db(), query)
    dbDisconnect(conn=db())
  })

  lapply(seq_len(nrow(needDeleting)), function(i){

    query <- sqlInterpolate(
      ANSI(),
      "DELETE FROM role_rates WHERE job_id = ?jobid AND role_id = ?roleid AND rate = ?rate",
      jobid = needDeleting$job_id[i], roleid = needDeleting$role_id[i], rate = needDeleting$rate[i]
    )

    dbGetQuery(db(), query)
    dbDisconnect(conn=db())
  })
}

# Edit the project_role table
editPersonnel <- function(uiPersonTbl, dbPersonTbl){

  uiPersonTbl$job_id <- as.numeric(uiPersonTbl$job_id)

  uiPersonTbl$emp_id <- as.character(uiPersonTbl$emp_id)


  needSaving <- anti_join(uiPersonTbl, dbPersonTbl,
                          by=c("job_id"="job_id", "emp_id"="emp_id", "role_id"="role_id"))

  needDeleting <- anti_join(dbPersonTbl,uiPersonTbl,
                            by=c("job_id"="job_id", "emp_id"="emp_id", "role_id"="role_id"))

  lapply(seq_len(nrow(needSaving)), function(i){

    query <- sqlInterpolate(
      ANSI(),
      "INSERT INTO project_role (job_id, emp_id, role_id)
      VALUES (?jobid, ?empid, ?roleid)",
      jobid = needSaving$job_id[i], empid = needSaving$emp_id[i], roleid = needSaving$role_id[i]
    )

    dbGetQuery(db(), query)
    dbDisconnect(conn=db())
  })

  lapply(seq_len(nrow(needDeleting)), function(i){

    query <- sqlInterpolate(
      ANSI(),
      "DELETE FROM project_role WHERE job_id = ?jobid AND emp_id = ?empid AND role_id = ?roleid",
      jobid = needDeleting$job_id[i], empid = needDeleting$emp_id[i], roleid = needDeleting$role_id[i]
    )

    dbGetQuery(db(), query)
    dbDisconnect(conn=db())
  })

}

# saves the new role
saveNewRole <- function(roleTable){

  # edit next row now
  lapply(seq_len(nrow(roleTable)), function(i){
    query <- paste0(
      "INSERT INTO role_rates (job_id, role_id, rate) VALUES (",
      roleTable$job_id[i],
      ",",
      roleTable$role_id[i],
      ",",
      roleTable$rate[i],
      ");")


    dbGetQuery(db(), sqlInterpolate(ANSI(), query))
    dbDisconnect(conn=db())
  })

}

# saves the new personnel
saveNewPersonnel <- function(personnelTable){

  lapply(seq_len(nrow(personnelTable)), function(i){
    query <- paste0(
      "INSERT INTO project_role (job_id, emp_id, role_id) VALUES (",
      personnelTable$job_id[i],
      ",",
      personnelTable$emp_id[i],
      ",",
      personnelTable$role_id[i],
      ");")


    dbGetQuery(db(), sqlInterpolate(ANSI(), query))
    dbDisconnect(conn=db())
  })

}


#---------------
# invoice
#---------------
#' Create hyperlinks
#'
#' An example is provived by DT official website: https://rstudio.github.io/DT/#
#' sprintf('<a href="https://www.google.com/#q=%s" target="_blank" class="btn btn-primary">Info</a>', "RSudio")
#'
#' @examples createLink()
createLink <- function(invnum="MGB/EXT/1000001") {

  link <- paste0(gsub("/", "-", invnum), ".pdf")

  # linkPath is defined in global.R
  if(!exists("linkPath")) linkPath <- "path-to-link"

  paste0("<a href='", linkPath, '/', link, "'>", invnum, "</a>")
}

#' Get the invoice data
#' @importFrom lubridate date days today
getInvoices <- function() {

  res  <- dbGetQuery(db(), "SELECT * FROM invoice") %>%
    left_join(paymentTbl, by= c("pay_method_id" = "id")) %>% # same as `dbGetQuery(db(), "SELECT * FROM payment_method")`
    select(id, job_id, office, issue_date, client_po, net_amount, vat_perc, link, currency, pass_through,  pay_date, curr_ex_rt,
           payment_methods=name, amount_paid, is_credit, parent_invoice_id, notes) %>%
    mutate(net_amount2=formatC(net_amount, format = "f", big.mark = ",", drop0trailing = TRUE)) %>%
    mutate(vat= net_amount * vat_perc / 100) %>%
    mutate(pass_through2=ifelse(pass_through==0, "", "Yes")) %>%
    mutate(tol=net_amount + vat) %>%
    mutate(alt_currency=ifelse(currency=="GBP", "", currency)) %>%
    mutate(invoice_number=paste0(office,"/EXT/", id)) %>%
    mutate(curr_ex_rt = ifelse(curr_ex_rt==0, 1, curr_ex_rt)) %>% # TODO: validate that `curr_ex_rt` should not be 0
    mutate(net_in_original_curr=as.numeric(net_amount/curr_ex_rt)) %>%
    mutate(net_amount3=formatC(
      (net_in_original_curr*curr_ex_rt), digits=2, format= "f", big.mark= ",", drop0trailing=TRUE)
    ) %>%
    mutate(net_alt_curr= ifelse(currency=="GBP", NA, net_in_original_curr)) %>%

    left_join(getRepoData() %>% select(id, Customer=customer_name, bus_unit_name, desc, pay_term, pay_method),
      by = c("job_id" = "id")) %>%

    calOverdue() %>% # Calculates whether bill is due
    mutate(issue_date = as.Date(issue_date, format="%d/%m/%Y")) %>% # Format converted from %Y-%m-%d to %d/%m/%Y
    mutate(pay_date = as.Date(pay_date, format="%d/%m/%Y"))     # Type converted from character to date

  res

}

#' Calculates whether bill is due
#'
#' If pay_method is set to "End_of_month" then reflect this in the billing start date.
#' If a payment term has been specified, bill hasn't been paid, and the due
#' date is after today, makr as overdue
#'
#' TODO: check definition of overdue
calOverdue <- function(tbl) {
  myvars <-  c("pay_method", "issue_date")
  if (missing(tbl)) stop("Please specify a table.")
  if (!all(myvars %in% names(tbl))) stop("Please specify if it is expected to pay by end of month and
                                   issue date of the invoice.")

  tbl %>%
    mutate(billing_start=ifelse(pay_method=="End_of_month",
      getEndOfMonth(date=issue_date),
      issue_date)) %>%

    mutate(due_date = as.Date(billing_start, format="%d/%m/%Y") +
      ifelse(is.na(pay_term), 0, pay_term)) %>%

    mutate(overdue=ifelse(due_date > lubridate::today(),
      "Yes",
      "No")) %>%
    select(-c(billing_start, due_date))
}


#' Get currency options
#'
#' @examples
#' getCurrencies()
getCurrencies <- function() {
  sort(c("GBP", "USD", "EUR", "CHF", "CNY", "HUF"))
}

#' Returns the end of the given month
#'
#' @param date character string indicating a date in the "%d/%m/%Y" format
#' @keywords internal
#' @examples getEndOfMonth("06/10/2015")
#' @importFrom lubridate date
getEndOfMonth <- function(date){
  # conversion: character -> date -> lubridate object
  date <- as.Date(date, format="%d/%m/%Y") %>%
    lubridate::date()

  # add a month
  date %m+% months(1) %>%

    # minus a day
    -day(date) %>%

    as.character()

}

#' Edit existing invoice
#'
#'
#' @examples
#' edtInvoice(
#'   id = newid,
#'   job_id = 2869,
#'   issue_date = "01/04/2016",
#'   issued_by = 17,
#'   client_po = "33600957",
#'   net_amount = 1000000,
#'   vat_perc = 10,
#'   currency = "GBP",
#'   pass_through = 1,
#'   curr_ex_rt = 1,
#'   is_credit = 0,
#'   notes = "im a test",
#'
#'   pay_date = "31/12/2016",
#'   amount_paid = 5000,
#'   pay_method_id = 1
#' )
edtInvoice <- function(id, job_id, issue_date="", issued_by, pay_date="",  client_po="", net_amount="",
                       vat_perc="", currency="", pass_through="", curr_ex_rt="",
                       pay_method_id="", amount_paid="", is_credit="", parent_invoice_id="", notes="") {
  
  if(missing(id)) stop("id is a required field.")
  #curr_ex_rt <- if(is.na(curr_ex_rt)) 0 else curr_ex_rt
  
  # Construct the update query by looping over the data fields
  query <- sqlInterpolate(
    ANSI(),
    "UPDATE invoice SET job_id=?job_id, issue_date=?issue_date, issued_by=?issued_by,
    pay_date=?pay_date, client_po=?client_po, net_amount=?net_amount, vat_perc=?vat_perc,
    currency=?currency, pass_through=?pass_through, curr_ex_rt=?curr_ex_rt,
    pay_method_id=?pay_method_id, amount_paid=?amount_paid, is_credit=?is_credit,
    parent_invoice_id=?parent_invoice_id, notes=?notes where id=?id",
    job_id = job_id,
    issue_date = issue_date,
    issued_by = issued_by,
    pay_date = pay_date,
    client_po = client_po,
    net_amount = net_amount,
    vat_perc = vat_perc,
    currency = currency,
    pass_through = pass_through,
    curr_ex_rt = curr_ex_rt,
    pay_method_id = pay_method_id,
    amount_paid = amount_paid,
    is_credit = is_credit,
    parent_invoice_id = parent_invoice_id,
    notes = notes,
    id = id
  )
  # Submit the update query and disconnect
  dbGetQuery(db(), query)
  dbDisconnect(db())
  
}


#' Add new invoice
#'
#'
#' @examples
#' addInvoice(
#'   job_id = 2869,
#'   issue_date = "01/04/2016",
#'   issued_by = 17,
#'   client_po = '33600957',
#'   net_amount = 0,
#'   vat_perc = 10,
#'   currency = "GBP",
#'   pass_through = 1,
#'   curr_ex_rt = 1,
#'   is_credit = 0,
#'   notes = "im a test"
#' )
addInvoice <- function(job_id, issue_date="", issued_by,  client_po, net_amount,
                       vat_perc, currency, curr_ex_rt, pass_through,
                       office="MGB", is_credit, parent_invoice_id, notes="") {
  if(missing(job_id)) stop("job_id is a require field.")

  maxId <- dbGetQuery(db(), sqlInterpolate(ANSI(), "SELECT MAX(id) from invoice"))[ ,1]
  id <- maxId + 1

  # Construct the update query by looping over the data fields
  query <- sqlInterpolate(

    ANSI(),
    "INSERT INTO invoice (id, job_id, issue_date, issued_by,  client_po, net_amount, vat_perc, currency,
    pass_through, office, curr_ex_rt, is_credit, parent_invoice_id, notes)
    VALUES(?id, ?job_id, ?issue_date, ?issued_by, ?client_po, ?net_amount, ?vat_perc, ?currency,
    ?pass_through, ?office, ?curr_ex_rt, ?is_credit, ?parent_invoice_id, ?notes)",
    id = id,
    job_id = job_id,
    issue_date = issue_date,
    issued_by = issued_by,
    client_po = client_po,
    net_amount = net_amount,
    vat_perc = vat_perc,
    currency = currency,
    pass_through = pass_through,
    office = office,
    curr_ex_rt = curr_ex_rt,
    is_credit = is_credit,
    parent_invoice_id = parent_invoice_id,
    notes = notes
  )

  # Submit the update query and disconnect
  dbGetQuery(db(), query)
  dbDisconnect(db())

  id
}

#' Delete an invoice from the invoice table based on id
#'
#' @examples
#' deleteInvoice(ID=12477)
deleteInvoice <- function(ID) {

sql <- sqlInterpolate(
    ANSI(),
    "DELETE FROM invoice WHERE id =  ?ID",
    ID = ID
  )

  dbGetQuery(db(), sql)
  dbDisconnect(db())

}


#---------------
# toolbar
#---------------
#' Version Component UI
#'
#' To be used in shinydashboard:::dashboardHeader
#'
versionComponentUI <- function() {
  ## Get Session Info
  info <- sessionInfo()
  ## Extract  Attached Package
  otherPkgs <- cbind(names(info$otherPkgs),
                     unlist(lapply(info$otherPkgs, `[`, "Version"))
  )
  ## Get Athena.shiny version
  main_version <- otherPkgs["krakenApp.Version", 2]
  ## Loaded Packages to be become a HTML list
  otherPkgs <- apply(otherPkgs, 1, function(e){
    tags$li(tags$i(class = "fa-li fa fa-code-fork"),
            e[1],
            tags$span(class = "label label-success",e[2])
    )
  })
  ## Extract Package Loaded Only
  loadedOnly <- cbind(names(info$loadedOnly),
                      unlist(lapply(info$loadedOnly, `[`, "Version"))
  )
  ## Loaded Packages to be become a HTML list
  loadedOnly <- apply(loadedOnly, 1, function(e){
    tags$li(tags$i(class = "fa-li fa fa-code-fork"),
            e[1],
            tags$span(class = "label label-default",e[2])
    )
  })

  tags$li(class="dropdown notifications-menu",
          tags$a(href = "#",
                 class = "dropdown-toggle",
                 "data-toggle"="dropdown",
                 icon("cogs"),
                 tags$span(class = "label label-success", main_version)
          ),
          tags$ul(class="dropdown-menu",
                  tags$li(class = "header",
                          tags$i(class = "fa fa-cog"),
                          version$version.string),
                  tags$li(tags$ul(class="fa-ul",otherPkgs)),
                  tags$hr(),
                  tags$li(tags$ul(class="fa-ul",loadedOnly)),
                  tags$hr()
          )
  )
}

#---------------
# Personnel table
#---------------
# Return the names of employees with that role when passed a table representing
# a project
getNamesFromRoles <- function(role,roletbl){
  roletbl[grep(paste0("^",role), roletbl$role_name),]$emp_name
}

# Returns a tabl containing roles and employees for a given project
getPeople <- function(jobid){

  emps <- getTable("project_role")
  emps <- emps[emps$job_id==jobid,]


  allEmps <- getTable("employee")[,c("emp_no", "name")]
  tbl <- merge(emps, allEmps, by.x="emp_no", by.y="id")

  allRoles <- getTable("role")[,c("id", "name")]
  tbl <- merge(tbl, allRoles, by.x="role_id", by.y="id")
  names(tbl) <- c("role_id", "emp_id", "job_id", "emp_name", "role_name")
  tbl
}

getRoleAcronym <- function(role){
  tbl <- getTable("role")
  tbl[tbl$name==role,]$short_name
}

roleAcronymList <- function(){
  roles <- roleTbl$name
  getRoleAcronym(roles)
}

getDefaultRate <- function(role){
  tbl <- getTable("role")
  tbl[tbl$name==role,]$rate
}

getCurrentRoles <- function(jobid){
  tbl <- getTable("project_role")
  roles <- unique(tbl[tbl$job_id==jobid,]$role_id)
  left_join(data.frame(id=roles), getTable("role"), by=c("id"="id"))$name
}

# Get the rate given the role (description) and the job number
getRate <- function(role, jobid){

  # Table of all jobs with associated roles and rates for that job
  tbl <- getTable("role_rates")

  # Refine by the particular job ID
  tbl <- tbl[tbl$job_id==jobid,]

  # Table of role IDs and names
  roleIdName <- getTable("role")[c("id", "name")]

  # Table of role rates for the current job, with role description added
  namesRates <- merge(tbl, roleIdName, by.x="role_id", by.y="id")

  # Get the individual rate for the given role
  myVal <- namesRates[namesRates$name==role,]$rate

  # If the result is in the table, return it
  if(length(myVal)==1){
    myVal
  }

  # If nothing is in the table, return the default rate for that role
  else if(length(myVal)==0){
    getDefaultRate(role)
  }
  else{
    stop(paste0("duplicated values found in role rates table for role id: ",
           role,
         "and jobid: ",
         jobid))
  }

}


getIdFromDesc <- function(roleDesc){
  tbl <- getTable("role")[,c("id", "name")]
  tbl[tbl$name==roleDesc,]$id
}

getPersonId <- function(personName){
  tbl <- getTable("employee")[,c("emp_no", "name")]
  tbl[tbl$name==personName,]$emp_no
}

getRoleRates <- function(jobid = NULL){
  rrt <- getTable("role_rates")
  if(!is.null(jobid)){
    rrt <- rrt[rrt$job_id==jobid,]
  }
  rrt$role_id <- as.character(rrt$role_id)
  rrt
}

getProjectRole <- function (jobid = NULL){
  pr <- getTable("project_role")
  if(!is.null(jobid)){
    pr <- pr[pr$job_id==jobid,]
  }
  pr
}

# given a role name, get the ID
getIdFromRole <- function(roleName){
  roles<- getTable("role")
  id <- roles$id[roles$name==roleName]
  id
}

# given an ID return the role name
getDescFromId <- function(roleId){
  tbl <- getTable("role")[,c("id", "name")]
  tbl[tbl$id==roleId,]$name
}

# given a person ID returns their name
getPersonName <- function(personID){
  tbl <- getTable("employee")[,c("emp_no", "name")]
  tbl[tbl$emp_no==personID,]$name
}


# Create skill function generates the UI element for a skil;
# can be used inside apply statement.
# UI comprises of a radio button for scores and a textbox for notes,
# both wrapped in a box

create_skill <- function(skill){
  s <- div(style="min-width: 250px",
           box(width = 12,
               fixedRow(column(4,
                               div(style="min-width: 250px",
                                   radioButtons(skill,
                                                paste0("Proficiency with ", skill),
                                                c(0:3),
                                                inline = TRUE,
                                                selected = NA,
                                                width = '100%'),
                                   offset = 0)
               ),
               
               column(7,
                      textAreaInput(paste0(skill, "notes"),
                                    "Notes",
                                    "" ,
                                    width = '100%'),
                      offset = 1)
               )))
  
  return(s)
}



loadSkillsForm <- function(name = "", session){
  
  
  if(name == "") {
    
  } else {
    
    # REPLACE WITH DB QUERY
    #qry <- sprintf("select * from skillsscores where name = '%s'", name)
    # query to get id from personnel
    # query to get score with id from above
    #scores <- dbGetQuery(db, qry)
    #Encoding(scores$name) <- "UTF-8"
    #scores <- unique(scores)
    
    scores <- skillsScores[skillsScores$name == name, ]
    
    for(skill in skills) {
      
      updateRadioButtons(session,
                         inputId = skill,
                         label = paste0("Proficiency with ", skill),
                         selected = scores[scores$skill == skill, ]$score)
      
      updateTextAreaInput(session,
                          inputId = paste0(skill, "notes"),
                          label = "Notes",
                          value = scores[scores$skill == skill, ]$note)
      
    }
  }
}


getSkillsMatrix <- function() {
  
    res <- skillsScores %>%
    group_by(name) %>%
    select(name, skill, score) %>%
    unique() %>%
    spread(key = skill, value = score)
  
    res
    
}
  

