shinyServer(function(input, output, session) {


  source('reactiveServer.R', local=TRUE)
  source('screen/displayResultServer.R', local=TRUE)
  source('screen/extractDataServer.R', local=TRUE)
  source('screen/DisplayResultServer.R', local=TRUE)
  source('screen/homeServer.R', local=TRUE)
  
  
})
