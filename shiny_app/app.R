suppressWarnings({
  suppressMessages({
    library(tidyverse)
    library(shiny)
    library(sack2)
    library(rsconnect)
    library(DT)
    
    # load csv file
    zz.alexsa.facets <- read_csv("metadata-alexsa-subscales.csv") %>%
      .[["subscale"]]
    metadata.xvars <- readxl::read_excel("metadata-xvars.xlsx", guess_max = 1e6) %>% 
      mutate(alexsa_facet = factor(alexsa_facet,
                                   levels = zz.alexsa.facets))
  }) 
})


# UI
ui <- fluidPage(
  titlePanel("Metadata Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      selectizeInput("type_filter", "Select Type:", choices = NULL, selected = "", multiple = TRUE),
      selectizeInput("sensitivity_filter", "Select Sensitivity:", choices = NULL, selected = "", multiple = TRUE),
      selectizeInput("informant_filter", "Select Informant:", choices = NULL, selected = "", multiple = TRUE),
      selectizeInput("plab_facet_filter", "Select Pelham Lab Facet:", choices = NULL, selected = "", multiple = TRUE),
      selectizeInput("plab_subfacet_filter", "Select Pelham Lab Subfacet:", choices = NULL, selected = "", multiple = TRUE),
      selectizeInput("alexsa_facet_filter", "Select ALEXSA Facet:", choices = NULL, selected = "", multiple = TRUE),
      selectizeInput("alexsa_facet_secondary_filter", "Select Secondary ALEXSA Facet:", choices = NULL, selected = "", multiple = TRUE)
    ),
    mainPanel(DTOutput("table")
              )
            )
          )
# server
server <- function(input, output, session){
  # update dropdown choices
  observe({
    updateSelectizeInput(session, "type_filter", choices = unique(na.omit(metadata.xvars$type)),
                             selected = "")
    updateSelectizeInput(session, "sensitivity_filter", choices = unique(na.omit(metadata.xvars$sensitivity)),
                      selected = "")
    updateSelectizeInput(session, "informant_filter", choices = unique(na.omit(metadata.xvars$informant)),
                      selected = "")
    updateSelectizeInput(session, "plab_facet_filter", choices = unique(na.omit(metadata.xvars$plab_facet)),
                      selected = "")
    updateSelectizeInput(session, "plab_subfacet_filter", choices = unique(na.omit(metadata.xvars$plab_subfacet)),
                      selected = "")
    updateSelectizeInput(session, "alexsa_facet_filter", choices = unique(na.omit(metadata.xvars$alexsa_facet)),
                      selected = "")
    updateSelectizeInput(session, "alexsa_facet_secondary_filter", choices = unique(na.omit(metadata.xvars$alexsa_facet_secondary)),
                      selected = "")
    })
  
  # reactive dataset based on filters
  filtered_data <- reactive({
    df <- metadata.xvars
    if(!is.null(input$type_filter) && length(input$type_filter > 0)){
      df <- df[df$type %in% input$type_filter, , drop = FALSE]
    }
    if(!is.null(input$sensitivity_filter) && length(input$sensitivity_filter > 0)){
      df <- df[df$sensitivity %in% input$sensitivity_filter, , drop = FALSE]
    }
    if(!is.null(input$informant_filter) && length(input$informant_filter > 0)){
      df <- df[df$informant %in% input$informant_filter, , drop = FALSE]
    }
    if(!is.null(input$plab_facet_filter) && length(input$plab_facet_filter > 0)){
      df <- df[df$plab_facet %in% input$plab_facet_filter, , drop = FALSE]
    }
    if(!is.null(input$plab_subfacet_filter) && length(input$plab_subfacet_filter > 0)){
      df <- df[df$plab_subfacet %in% input$plab_subfacet_filter, , drop = FALSE]
    }
    if(!is.null(input$alexsa_facet_filter) && length(input$alexsa_facet_filter > 0)){
      df <- df[df$alexsa_facet %in% input$alexsa_facet_filter, , drop = FALSE]
    }
    if(!is.null(input$alexsa_facet_secondary_filter) && length(input$alexsa_facet_secondary_filter > 0)){
      df <- df[df$alexsa_facet_secondary %in% input$alexsa_facet_secondary_filter, , drop = FALSE]
    }
    return(df)
    
  })
  # render filtered datatable
  output$table <- renderDT({datatable(filtered_data(), 
                                      options = list(pageLength = 10))})
}
shinyApp(ui = ui, server = server)
