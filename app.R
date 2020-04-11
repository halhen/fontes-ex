library(tidyverse)
library(patchwork)

library(DT)
library(shiny)
library(shinyWidgets)

source('data.R')
source('filter_menu.R')
source('util.R')

theme_set(theme_minimal() +
              theme(text = element_text(size = 16)))

ui <- fluidPage(
    titlePanel("Data Explorer - Henrik Lindberg"),
    sidebarLayout(
        sidebarPanel(width = 3,
            h4('Patients'),
            uiOutput('menu_stats'),
            plotOutput('menu_stats_plot', height = '30px'),
            hr(),
            tabsetPanel(
                tabPanel('Filter',
                         filterMenuOutput('menu_filter', 'Show patients with')
                ),
                tabPanel('Highlight',
                         filterMenuOutput('menu_highlight', 'Highlight patients with')
                )
            )
        ),
        mainPanel(width = 9,
            tabsetPanel(
                tabPanel('Patient Distributions',
                         h2('Patient Distributions'),
                         p('Highlight patients in the sidebar to compare groups'),
                         plotOutput('distributions_plot', height = '80vh')
                ),
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
    df.patients_highlighted <- callModule(filterMenu, 'menu_highlight', reactive(data_patients()))
    
    df.labtests <- reactive({
        data_labtests() %>%
            semi_join(df.patients_filtered(), by = 'USUBJID')
    })
    
    df.menu_stats <- reactive({
        df.filtered <- df.patients_filtered() %>%
            left_join(df.patients_highlighted() %>%
                          transmute(USUBJID, highlighted = TRUE),
                      by = 'USUBJID')
        
        tibble(
            total = nrow(data_patients()),
            filtered = nrow(df.filtered),
            highlighted = sum(df.filtered$highlighted, na.rm=TRUE),
            unhighlighted = filtered - highlighted,
            hidden = total - filtered
        )
    })
    
    output$menu_stats_plot <- renderPlot({
        df.menu_stats() %>%
            transmute(highlighted, unhighlighted, hidden) %>%
            gather(key, value) %>%
            mutate(key = ordered(key, levels = c('hidden', 'unhighlighted', 'highlighted'))) %>%
            ggplot(aes('', value, fill = key)) +
                geom_col(color = 'black') +
                coord_flip() +
                scale_fill_manual(values = c('hidden' = 'white', 'unhighlighted' = 'lightgray', 'highlighted' = '#428BCA')) +
                theme_void() +
                theme(legend.position = 'none')
    }, bg = 'transparent')
    
    output$menu_stats <- renderUI({
        with(df.menu_stats(), tagList(
            p(
                'Highlighted: ', highlighted, br(),
                'Not highlighted: ', unhighlighted, br(),
                'Hidden: ', hidden
            )
        ))
    })
    
    output$distributions_plot <- renderPlot({
        # Internal function for neater syntax
        plot <- function(var) {
            df.tmp <- df.patients_filtered() %>%
                left_join(df.patients_highlighted() %>%
                              transmute(USUBJID, highlighted = TRUE),
                          by = 'USUBJID') %>%
                mutate(highlighted = coalesce(highlighted, FALSE)) %>%
                mutate(RACE = abbreviate(RACE, 8)) # To make sure we have readable axes
            
            plot_distribution(df.tmp, {{ var }}, highlighted)
        }
        
        req(nrow(df.patients_filtered()) > 0)
        
        # Patchwork a set of plots into a single graphics
        (plot(SEX) | plot(AGE) | plot(RACE) | plot(country)) /
        (plot(screening_ALT) | plot(screening_CRP) | plot(screening_IGA)) /
        (plot(BMRKR1) | plot(BMRKR2) | plot(ACTARM))
    })
    
    
    
    
    output$patients_table <- renderDataTable({
        df.patients_filtered() %>%
            left_join(df.patients_highlighted() %>%
                          transmute(USUBJID, highlighted = TRUE),
                      by = 'USUBJID') %>%
            mutate(highlighted = coalesce(highlighted, FALSE)) %>%
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
                                     buttons = c('copy', 'csv', 'excel'))) %>%
            formatRound('BMRKR1', digits = 3) %>%
            formatStyle('Highlighted', target = 'row', backgroundColor = styleEqual(c('', 'Highlighted'), c('#f6f6f6', '#b5d8f4')))
    }, server = FALSE)
    
    output$patient_table_selected_labtests_plot <- renderPlot({
        selected_row <- input$patients_table_rows_selected
        req(selected_row)
        patient_id <- df.patients_filtered()[selected_row, ] %>%
            pull(USUBJID)
        
        df.tmp <- df.labtests() %>%
            semi_join(df.patients_filtered(), by = 'USUBJID') %>%
            left_join(df.patients_highlighted() %>%
                          transmute(USUBJID, highlighted = TRUE),
                      by = 'USUBJID') %>%
            mutate(highlighted = coalesce(highlighted, FALSE))
        
        
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
                           p('Screening ALT: ', round(screening_ALT, 3)),
                           p('Screening CRP: ', round(screening_CRP, 3)),
                           p('Screening IGA: ', round(screening_IGA, 3)),
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
