fluidRow(column(
  width = 12,
  tabBox(
    width = NULL,
    side = "left",
    id = "displayResults_tab",
    tabPanel(id = "displayResultsOverview_tabPanel",
             title = "Overview"),
    tabPanel(id = "RNA_tabPanel",
             title = "RNA"),
    tabPanel(id = "Gene_tabPanel",
             title = "Gene")
  )
  
))