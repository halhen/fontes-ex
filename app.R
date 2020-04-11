library(DT)
library(shiny)
library(shinyWidgets)
library(tidyverse)

# Load the UI reactively, which makes it easier to use the data we're actually to use.
# (The other alternative is to use a bunch of updateComponent() calls, which is quite messy
# and unnecessary for this use case.)
# This makes for an easier transition into dynamic data sources.

ui <- fluidPage(
    titlePanel("Data Explorer - Henrik Lindberg"),
    uiOutput('reactiveUI')
)

ui_pickerOptions <- pickerOptions(actionsBox = TRUE)

server <- function(input, output) {
    output$reactiveUI <- renderUI({
        sidebarLayout(
            with(df.patients(),
                 sidebarPanel(
                    h2('Show patients with'),
                    hr(),
                    h3('Demographics'),
                    sliderInput('AGE', 'AGE', min = min(AGE), max = max(AGE),
                                value = range(AGE), step =1),
                    pickerInput('SEX', 'SEX', choices = sort(unique(SEX)), multiple = TRUE,
                                selected = sort(unique(SEX)), options = ui_pickerOptions),
                    pickerInput('RACE', 'RACE', choices = sort(unique(RACE)), multiple = TRUE,
                                selected = sort(unique(RACE)), options = ui_pickerOptions),
                    pickerInput('country', 'Country of origin', choices = sort(unique(country)), multiple = TRUE,
                                selected = sort(unique(country)), options = ui_pickerOptions),
                    hr(),
                    h3('Biomarkers'),
                    sliderInput('BMRKR1_pretty', 'BMRKR1', min = min(BMRKR1_pretty), max = max(BMRKR1_pretty),
                                value = signif(range(BMRKR1_pretty), 1), step = 0.1),
                    pickerInput('BMRKR2', 'BMRKR2', choices = levels(BMRKR2), multiple = TRUE,
                                selected = levels(BMRKR2), options = ui_pickerOptions),
                    hr(),
                    h3('Treatment'),
                    pickerInput('ACTARM', 'ACTARM', choices = sort(unique(ACTARM)), multiple = TRUE,
                                selected = sort(unique(ACTARM)), options = ui_pickerOptions)
                 )
            ),
            mainPanel(
                dataTableOutput('patients_table')
            )
        )
    })
    
    df.patients <- reactive({
        data_patients() %>%
            mutate(BMRKR1_pretty = signif(BMRKR1, 1))
    })
    
    df.labtests <- reactive({
        data_labtests()
    })

    df.patients_filtered <- reactive({
        df.tmp <- df.patients() %>%
            filter(AGE >= input$AGE[1], AGE <= input$AGE[2]) %>%
            filter(SEX %in% input$SEX) %>%
            filter(RACE %in% input$RACE) %>%
            filter(country %in% input$country) %>%
            
            filter(BMRKR1_pretty >= input$BMRKR1_pretty[1], BMRKR1_pretty <= input$BMRKR1_pretty[2]) %>%
            filter(BMRKR2 %in% input$BMRKR2) %>%
            
            filter(ACTARM %in% input$ACTARM)
        
        df.tmp
    })    
    
    output$patients_table <- renderDataTable({
        df.patients_filtered() %>%
            datatable(escape = FALSE,
                      extensions = c("Buttons"), 
                      options = list(dom = 'Bfrtip',
                                     buttons = c('copy', 'csv', 'excel')))
    }, server = FALSE)
}

shinyApp(ui = ui, server = server)
