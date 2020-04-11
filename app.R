library(shiny)

ui <- fluidPage(
    titlePanel("Data Expolorer - Henrik Lindberg"),
    sidebarLayout(
        sidebarPanel(),
        mainPanel()
    )
)

server <- function(input, output) {

}

shinyApp(ui = ui, server = server)
