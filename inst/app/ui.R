dashboardPage(
  # tags$head(
  #   tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  # ),
  skin = "yellow",
  dashboardHeader(title = "Mango Solutions",
                  versionComponentUI()
                  ),
  dashboardSidebar(
    sidebarMenu(
	    menuItem("Job", tabName = "job", icon = icon("line-chart")),
	    menuItem("Invoice", tabName = "invoice", icon = icon("credit-card")),
	    menuItem("Customer", tabName = "customer", icon = icon("briefcase")),
      menuItem("Employee", tabName = "employee", icon = icon("group"))
	  )
  ),

  dashboardBody(
    # Set up shinyjs
    useShinyjs(),

    tabItems(
      tabItem("job",
	      source("screen/jobTab.R", local = TRUE)$value
	    ),
      tabItem("invoice",
	      source("screen/invoiceTab.R", local = TRUE)$value
	    ),
	    tabItem("customer",
	      source("screen/customerTab.R", local = TRUE)$value
	    ),
	    tabItem("employee",
	      source("screen/employeeTab.R", local = TRUE)$value
	    )
    )
  )
)


