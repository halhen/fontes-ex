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
            tabsetPanel(
                tabPanel('Patients',
                         h2('Patients'),
                         p('Click a patient so see their complete record'),
                         dataTableOutput('patients_table')
                )
            )
        )
    )
)

server <- function(input, output) {
    
    df.patients_filtered <- callModule(filterMenu, 'menu_filter', reactive(data_patients()))
    
    df.labtests <- reactive({
        data_labtests() %>%
            semi_join(df.patients_filtered(), by = 'USUBJID')
    })
    
    output$patients_table <- renderDataTable({
        df.patients_filtered() %>%
            datatable(rownames = FALSE,
                      selection = 'single',
                      extensions = c("Buttons"), 
                      options = list(dom = 'frtipB',
                                     buttons = c('copy', 'csv', 'excel'))) %>%
            formatRound('BMRKR1', digits = 3)
    }, server = FALSE)
    
    output$patient_table_selected_labtests_plot <- renderPlot({
        selected_row <- input$patients_table_rows_selected
        req(selected_row)
        patient_id <- df.patients_filtered()[selected_row, ] %>%
            pull(USUBJID)
        
        df.tmp <- df.labtests() %>%
            filter(day > 0)
        
        df.tmp %>%
            filter(USUBJID == patient_id) %>%
            ggplot(aes((day - 1)/7, AVAL, group = USUBJID)) +
                geom_line(data = df.tmp, color = '#aaaaaa', alpha = 0.5) +
                geom_line(size = 0.8) +
                geom_point(size = 4, shape = 21, fill = 'white') +
                labs(x = 'Week', y = '') +
                facet_wrap(~ LBTEST, ncol = 1, scales = 'free_y') +
                theme(text = element_text(size = 20))
        
    })
    
    observe({
        selected_row <- input$patients_table_rows_selected
        req(selected_row)
        
        # Don't push new modal when data changes, only when a new row is selected. Therefore, isolate()
        patient <- isolate(df.patients_filtered())[selected_row, ]
        
        showModal(modalDialog(
            fluidPage(
                fluidRow(
                    with(patient, column(4,
                           p('id: ', id),
                           p('country: ', country),
                           hr(),
                           p('AGE: ', AGE),
                           p('SEX: ', SEX),
                           p('RACE: ', RACE),
                           hr(),
                           p('BMRKR1: ', round(BMRKR1, 3)),
                           p('BMRKR2: ', BMRKR2),
                           hr(),
                           p('Treatment: ', ACTARM)
                    )),
                    column(8,
                           plotOutput('patient_table_selected_labtests_plot', height = 800)
                           
                    )
                )
            ), size = 'l', easyClose = TRUE, title = patient$USUBJID
        ))

    })
}

shinyApp(ui = ui, server = server)
