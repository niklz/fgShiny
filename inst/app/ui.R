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
	    menuItem("Home", tabName = "home", icon = icon("line-chart")),
	    menuItem("Top Table", tabName = "topTable", icon = icon("credit-card")),
	    menuItem("Extract Data", tabName = "extractData", icon = icon("briefcase")),
      menuItem("Display Result", tabName = "displayResult", icon = icon("group"))
	  )
  ),

  dashboardBody(
    # Set up shinyjs
    useShinyjs(),

    tabItems(
      tabItem("home",
	      source("screen/homeTab.R", local = TRUE)$value
	    ),
      tabItem("topTable",
	      source("screen/topTableTab.R", local = TRUE)$value
	    ),
	    tabItem("extractData",
	      source("screen/extractDataTab.R", local = TRUE)$value
	    ),
	    tabItem("displayResult",
	      source("screen/displayResultTab.R", local = TRUE)$value
	    )
    )
  )
)


