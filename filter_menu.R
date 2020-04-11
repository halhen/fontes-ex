library(shiny)
library(shinyWidgets)

filterMenuOutput <- function(id, label) {
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
      mutate(BMRKR1_pretty = signif(BMRKR1, 1))
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
        h3('Biomarkers'),
        sliderInput(ns('BMRKR1_pretty'), 'BMRKR1', min = min(BMRKR1_pretty), max = max(BMRKR1_pretty),
                    value = signif(range(BMRKR1_pretty), 1), step = 0.1),
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
    req(input$AGE)
    
    df.tmp <- df.prettified() %>%
      filter(AGE >= input$AGE[1], AGE <= input$AGE[2]) %>%
      filter(SEX %in% input$SEX) %>%
      filter(RACE %in% input$RACE) %>%
      filter(country %in% input$country) %>%
      
      filter(BMRKR1_pretty >= input$BMRKR1_pretty[1], BMRKR1_pretty <= input$BMRKR1_pretty[2]) %>%
      filter(BMRKR2 %in% input$BMRKR2) %>%
      
      filter(ACTARM %in% input$ACTARM)
    
    df.tmp %>%
      select(-BMRKR1_pretty) # Undo the prettyfication, to not leak internal data
  })
  
  return (df.filtered)
}