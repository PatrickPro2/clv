# module ui
instructionUI <- function(id) {

  # set namespace via id
  ns <- NS(id)
  tagList(
    fluidRow(column(12, div(includeMarkdown("www/markdown/instruction.md"), style="color: #ffffff")))
  )
}


# module server
instruction <- function(input, output, session) {
  
}