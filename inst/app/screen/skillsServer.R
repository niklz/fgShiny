output$resettable_form <- renderUI({
  reset1 <- input$name
  div(id = "form",
      fixedPage(
        column(8,
               lapply(skills,
                      function(x) {create_skill(x)})),
        
        column(4,
               fixedPanel(div(position = "fixed",
                              htmlOutput("scoreText")),
                          width = '100%'))
        
        
      ))
})


# Output UI elements

output$scoreText <- renderUI({HTML("Score Guide:
                                     <br> 0 - No exposure at all
                                     <br>
                                     <br> 1 - Light exposure: no commerical experience
                                     <br>
                                     <br> 2 - Medium exposure: some project experience
                                     <br>
                                     <br> 3 - Heavy exposure: experience over multiple projects
                                     <br>"
)})

