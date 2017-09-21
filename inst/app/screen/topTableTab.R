fluidRow(column(
  width = 12,
  tabBox(
    width = NULL,
    side = "left",
    id = "topTable_tab",
    tabPanel(id = "topTableOverview_tabPanel",
             title = "Overview"),
    tabPanel(id = "choseExperiment_tabPanel",
             title = "Choose Experiment"),
    tabPanel(id = "choseGene_tabPanel",
             title = "Choose Gene")
  )
  
))