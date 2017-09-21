#' Multiline text input
#' @export
textareaInput <- function(id, label, value, rows=10, cols=100, class="form-control"){
  tags$div(
    class="form-group shiny-input-container",
    tags$label('for'=id,label),
    tags$textarea(id=id,class=class,rows=rows,cols=cols,value)
  )
}


#' Coloured action button
#' https://gist.github.com/xiaodaigh/7012930
#' @export
goButton <- function(inputId, label="", btn.style = "" , css.class = "", icon="") {
  if ( btn.style %in% c("primary","info","success","warning","danger","inverse","link")) {
    btn.css.class <- paste("btn",btn.style,sep="-")
  } else btn.css.class = ""

  tags$button(id=inputId,
              type="button",
              class=paste("btn action-button",btn.css.class,css.class,collapse=" "),
              label,
              icon)
}
