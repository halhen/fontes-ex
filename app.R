library(DT)
library(shiny)
library(shinyWidgets)
library(tidyverse)

source('data.R')
source('filter_menu.R')

ui <- fluidPage(
    titlePanel("Data Explorer - Henrik Lindberg"),
    sidebarLayout(
        sidebarPanel(
            filterMenuOutput('menu_filter', 'Show patients with')
        ),
        mainPanel(
            dataTableOutput('patients_table')
        )
    )
)

server <- function(input, output) {
    
    df.labtests <- reactive({
        data_labtests()
    })

    df.patients_filtered <- callModule(filterMenu, 'menu_filter', reactive(data_patients()))
    
    
    output$patients_table <- renderDataTable({
        df.patients_filtered() %>%
            datatable(escape = FALSE,
                      extensions = c("Buttons"), 
                      options = list(dom = 'Bfrtip',
                                     buttons = c('copy', 'csv', 'excel')))
    }, server = FALSE)
}

shinyApp(ui = ui, server = server)
