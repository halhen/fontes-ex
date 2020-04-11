library(shiny)
library(shinyWidgets)


#
# Plain menu
#

filterMenuOutput <- function(id, label) {
  message(id)
  ns <- NS(id)
  tagList(
    h2(label),
    hr(),
    uiOutput(ns('reactiveUI'))
  )
}

filterMenu <- function(input, output, session, df.patients) {
  
  df.prettified <- reactive({
    df.patients() %>%
      mutate(BMRKR1_pretty = signif(BMRKR1, 1),
             screening_ALT_pretty = signif(screening_ALT, 1),
             screening_CRP_pretty = signif(screening_CRP, 1),
             screening_IGA_pretty = signif(screening_IGA, 1))
  })
  
  
  output$reactiveUI <- renderUI({
    ns <- session$ns
    
    ui_pickerOptions <- pickerOptions(actionsBox = TRUE, selectedTextFormat = 'count > 4')
    
    with(df.prettified(), 
      tagList(
        h3('Demographics'),
        sliderInput(ns('AGE'), 'AGE', min = min(AGE), max = max(AGE),
                    value = range(AGE), step =1),
        pickerInput(ns('SEX'), 'SEX', choices = sort(unique(SEX)), multiple = TRUE,
                    selected = sort(unique(SEX)), options = ui_pickerOptions),
        pickerInput(ns('RACE'), 'RACE', choices = sort(unique(RACE)), multiple = TRUE,
                    selected = sort(unique(RACE)), options = ui_pickerOptions),
        pickerInput(ns('country'), 'Country of origin', choices = sort(unique(country)), multiple = TRUE,
                    selected = sort(unique(country)), options = ui_pickerOptions),
        hr(),
        h3('Screening values'),
        sliderInput(ns('screening_ALT_pretty'), 'Alanine Aminotransferase', min = min(screening_ALT_pretty), max = max(screening_ALT_pretty),
                    value = range(screening_ALT_pretty), step = 0.1),
        sliderInput(ns('screening_CRP_pretty'), 'C-Reactive Protein', min = min(screening_CRP_pretty), max = max(screening_CRP_pretty),
                    value = range(screening_CRP_pretty), step = 0.1),
        sliderInput(ns('screening_IGA_pretty'), 'Immunoglobulin A', min = min(screening_IGA_pretty), max = max(screening_IGA_pretty),
                    value = range(screening_IGA_pretty), step = 0.1),
        
        h3('Biomarkers'),
        sliderInput(ns('BMRKR1_pretty'), 'BMRKR1', min = min(BMRKR1_pretty), max = max(BMRKR1_pretty),
                    value = range(BMRKR1_pretty), step = 0.1),
        pickerInput(ns('BMRKR2'), 'BMRKR2', choices = levels(BMRKR2), multiple = TRUE,
                    selected = levels(BMRKR2), options = ui_pickerOptions),
        hr(),
        h3('Treatment'),
        pickerInput(ns('ACTARM'), 'ACTARM', choices = sort(unique(ACTARM)), multiple = TRUE,
                    selected = sort(unique(ACTARM)), options = ui_pickerOptions)
      )
    )
  })
  
  
  df.filtered <- reactive({
    df.tmp <- df.prettified()
    
    # If the UI has been rendered, filtered by it. If not, return all the data
    if (!is.null(input$AGE)) {
      df.tmp <- df.tmp %>%
        filter(AGE >= input$AGE[1], AGE <= input$AGE[2]) %>%
        filter(SEX %in% input$SEX) %>%
        filter(RACE %in% input$RACE) %>%
        filter(country %in% input$country) %>%
        
        filter(BMRKR1_pretty >= input$BMRKR1_pretty[1], BMRKR1_pretty <= input$BMRKR1_pretty[2]) %>%
        filter(BMRKR2 %in% input$BMRKR2) %>%
        
        filter(screening_ALT_pretty >= input$screening_ALT_pretty[1], screening_ALT_pretty <= input$screening_ALT_pretty[2]) %>%
        filter(screening_CRP_pretty >= input$screening_CRP_pretty[1], screening_CRP_pretty <= input$screening_CRP_pretty[2]) %>%
        filter(screening_IGA_pretty >= input$screening_IGA_pretty[1], screening_IGA_pretty <= input$screening_IGA_pretty[2]) %>%
        
        filter(ACTARM %in% input$ACTARM)
    }
    
    df.tmp %>%
      select(-matches('_pretty$')) # Undo the prettyfication, to not leak internal data
  })
  
  return (df.filtered)
}




#
# Joint filtering and highlighting menu, returning the proper data
# as well as a column highlighted for rows to be highlighted
#


filterAndHighlightMenuOutput <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns('menu_stats')),
    plotOutput(ns('menu_stats_plot'), height = '30px'),
    hr(),
    tabsetPanel(
      tabPanel('Filter',
               filterMenuOutput(ns('menu_filter'), 'Show patients with')
      ),
      tabPanel('Highlight',
               filterMenuOutput(ns('menu_highlight'), 'Highlight patients with')
      )
    )
  )
}


filterAndHighlightMenu <- function(input, output, session, data_patients_reactive) {
  ns <- session$ns
  
  df.patients_filtered <- callModule(filterMenu, 'menu_filter', data_patients_reactive)
  df.patients_highlighted <- callModule(filterMenu, 'menu_highlight', data_patients_reactive)
  
  df.menu_stats <- reactive({
    tibble(
      total = nrow(data_patients()),
      filtered = nrow(df.patients()),
      highlighted = sum(df.patients()$highlighted),
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
  
  df.patients <- reactive({
    df.patients_filtered() %>%
      left_join(df.patients_highlighted() %>%
                  transmute(USUBJID, highlighted = TRUE),
                by = 'USUBJID') %>%
      mutate(highlighted = coalesce(highlighted, FALSE))
  })
  
  return (df.patients)
}


