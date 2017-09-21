shinyServer(function(input, output, session) {


  source('reactiveServer.R', local=TRUE)
  source('screen/skillsServer.R', local=TRUE)
  source('screen/employeeServer.R', local=TRUE)
  source('screen/customerServer.R', local=TRUE)
  source('screen/jobServer.R', local=TRUE)
  source('screen/invoiceServer.R', local=TRUE)
  
  
})
