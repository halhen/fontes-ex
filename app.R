library(tidyverse)
library(patchwork)

library(DT)
library(shiny)
library(shinyWidgets)

source('data.R')
source('filter_menu.R')
source('util.R')


theme_set(theme_minimal() +
              theme(text = element_text(size = 12)))

ui <- fluidPage(
    titlePanel("Data Explorer - Henrik Lindberg"),
    sidebarLayout(
        sidebarPanel(width = 3,
            h4('Patients'),
            filterAndHighlightMenuOutput('sidebar_menu')
        ),
        mainPanel(width = 9,
            tabsetPanel(
                tabPanel('Patient Distributions',
                         h2('Patient Distributions'),
                         p('Highlight patients in the sidebar to compare groups'),
                         plotOutput('distributions_plot', height = '80vh') %>% uiLoader()
                ),
                tabPanel('Patients',
                         h2('Patients'),
                         p('Click a patient so see their complete record'),
                         dataTableOutput('patients_table') %>% uiLoader()
                )
            )
        )
    )
)

server <- function(input, output) {
    
    
    df.patients <- callModule(filterAndHighlightMenu, 'sidebar_menu', reactive(data_patients()))
    
    df.labtests <- reactive({
        data_labtests() %>%
            semi_join(df.patients(), by = 'USUBJID')
    })
    
    output$distributions_plot <- renderPlot({
        req(nrow(df.patients()) > 0)
        
        # Internal function for neater syntax
        plot <- function(var) {
            df.tmp <- df.patients() %>%
                mutate(RACE = abbreviate(RACE, 6)) # To make sure we have readable axes
            
            plot_distribution(df.tmp, {{ var }}, highlighted)
        }
        
        # Patchwork a set of plots into a single graphics
        (plot(SEX) | plot(AGE) | plot(RACE) | plot(country)) /
        (plot(screening_ALT) | plot(screening_CRP) | plot(screening_IGA)) /
        (plot(BMRKR1) | plot(BMRKR2) | plot(ACTARM))
    })
    
    
    
    
    output$patients_table <- renderDataTable({
        df.patients() %>%
            transmute(USUBJID,
                      Country = country,
                      AGE,
                      SEX,
                      RACE,
                      ACTARM,
                      BMRKR1,
                      BMRKR2,
                      screening_ALT,
                      screening_CRP,
                      screening_IGA,
                      Highlighted = ifelse(highlighted, 'Highlighted', '')) %>%
            datatable(rownames = FALSE,
                      selection = 'single',
                      extensions = c("Buttons"), 
                      options = list(dom = 'frtipB',
                                     pageLength = 25,
                                     buttons = c('copy', 'csv', 'excel'))) %>%
            formatRound('BMRKR1', digits = 3) %>%
            formatRound('screening_ALT', digits = 3) %>%
            formatRound('screening_CRP', digits = 3) %>%
            formatRound('screening_IGA', digits = 3) %>%
            formatStyle('Highlighted', target = 'row', backgroundColor = styleEqual(c('', 'Highlighted'), c('#f6f6f6', '#b5d8f4')))
    }, server = FALSE)
    
    
    # Pop-up when clicking a subject in the patient table
    
    observe({
        selected_row <- input$patients_table_rows_selected
        req(selected_row)
        
        # Don't push new modal when data changes, only when a new row is selected. Therefore, isolate()
        patient <- isolate(df.patients())[selected_row, ]
        
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
                                         p('Screening ALT: ', round(screening_ALT, 3)),
                                         p('Screening CRP: ', round(screening_CRP, 3)),
                                         p('Screening IGA: ', round(screening_IGA, 3)),
                                         hr(),
                                         p('Treatment: ', ACTARM)
                    )),
                    column(8,
                           plotOutput('patient_table_selected_labtests_plot', height = 800) %>% uiLoader()
                           
                    )
                )
            ), size = 'l', easyClose = TRUE, title = patient$USUBJID
        ))
        
    })
    
    output$patient_table_selected_labtests_plot <- renderPlot({
        selected_row <- input$patients_table_rows_selected
        req(selected_row)
        patient_id <- df.patients()[selected_row, ] %>%
            pull(USUBJID)
        
        df.tmp <- df.labtests() %>%
            inner_join(df.patients() %>%
                           transmute(USUBJID, highlighted),
                       by = 'USUBJID')
        
        df.tmp %>%
            filter(USUBJID == patient_id) %>%
            ggplot(aes((day - 1)/7, AVAL, group = USUBJID)) +
                geom_line(data = filter(df.tmp, !highlighted), color = 'lightgray', alpha = 0.5) +
                geom_line(data = filter(df.tmp, highlighted), color = '#428BCA', alpha = 0.5) +
                geom_line(size = 1) +
                geom_point(size = 4) +
                labs(x = 'Week', y = '') +
                facet_wrap(~ LBTEST, ncol = 1, scales = 'free_y') +
                theme(text = element_text(size = 20))
        
    })
}

shinyApp(ui = ui, server = server)
