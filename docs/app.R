suppressWarnings({
  suppressMessages({
    library(tidyverse)
    library(shiny)
    library(sack2)
    library(rsconnect)
    library(DT)
    
    # load csv file
    # path_to_metadata_xvars is in _paths (output csv from abcd-itembuild)
    metadata.xvars <- read.csv("metadata-ABCD-items.csv") # completed metadata from itembuild (only items)
  }) 
})

# mapping facets -> subfacets
facet_map <- list(
  "caregiver physical health"         = "caregiver physical health",
  "caregiver psychopathology"         = c("externalizing", "internalizing", "other psychopathology"),
  "caregiver screen time"             = "not applicable",
  "demographics"                      = c("age", "education attained", "health insurance", "household structure", "income",
                                          "language", "pet ownership", "relatives"),
  "developmental history"             = "not applicable",
  "driving"                           = "not applicable",
  "emotion and emotion regulation"    = "not applicable",
  "family interactions and parenting" = "not applicable",
  "genetics and family history"       = "not applicable",
  "hobbies and activities"            = "not applicable",
  "identities"                        = c("race and ethnicity", "sex and gender", "sexual orientation", "country"),
  "life events"                       = "not applicable",
  "neighborhood"                      = "not applicable",
  "parent perceived stress"           = "not applicable",
  "peers and peer relationships"      = c("mistreatment by peers", "mistreats peers", "prosociality of peers",
                                          "protective peer relationships", "quality relationships", "quantity of friends",
                                          "susceptibility to peer pressure"),
  "physical health"                   = c("anthropometrics", "diet/eating", "exercise", "pain/illness/injury", "puberty", "sleep"),
  "prosocial behavior"                = "not applicable",
  "religious and/or cultural values"  = "not applicable",
  "school"                            = "not applicable",
  "screen time"                       = "not applicable",
  "service utilization"               = "not applicable",
  "substance use"                     = c("access", "exposure", "cognitions", "personal use")
)

# UI
ui <- fluidPage(
  titlePanel("Metadata Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      selectizeInput("study_filter", "Select Study:", choices = NULL, selected = NULL, multiple = TRUE),
      selectizeInput("domain_filter", "Select Domain:", choices = NULL, selected = NULL, multiple = TRUE),
      selectizeInput("subdomain_filter", "Select Subdomain:", choices = NULL, selected = NULL, multiple = TRUE),
      selectizeInput("source_filter", "Select Source:", choices = NULL, selected = NULL, multiple = TRUE),
      selectizeInput("table_name_filter", "Select Table:", choices = NULL, selected = NULL, multiple = TRUE),
      selectizeInput("has_branching_logic_filter", "Select Branching Logic:", choices = NULL, selected = NULL, multiple = TRUE),
      selectizeInput("sensitivity_filter", "Select Sensitivity:", choices = NULL, selected = NULL, multiple = TRUE),
      selectizeInput("plab_facet_filter", "Select Pelham Lab Facet:", choices = NULL, selected = NULL, multiple = TRUE),
      selectizeInput("plab_subfacet_filter", "Select Pelham Lab Subfacet:", choices = NULL, selected = NULL, multiple = TRUE, 
                     options = list(placeholder = "Choose a Pelham Lab Facet first")),
      selectizeInput("alexsa_facet_filter", "Select ALEXSA Facet:", choices = NULL, selected = NULL, multiple = TRUE)
    ),
    mainPanel(DTOutput("table")
    )
  )
)
# server
server <- function(input, output, session){
  facet_map <- lapply(facet_map, function(x) {
    if (is.null(x)) return(character(0))
    as.character(x)
  })
  # build domain mapping with subdomain
  domain_map <- metadata.xvars %>% 
    filter(!is.na(domain) & !is.na(sub_domain)) %>% 
    distinct(domain, sub_domain) %>% 
    group_by(domain) %>% 
    summarise(subs = list(sort(unique(sub_domain))), .groups = "drop") %>% 
    deframe()
  
  # build inverse subdomain map to make it easy to find domains that contain a given subdomain
  subdomain_to_domains <- list()
  for (d in names(domain_map)) {
    for (s in domain_map[[d]]) {
      subdomain_to_domains[[s]] <- unique(c(subdomain_to_domains[[s]], d))
    }
  }
  rv <- reactiveValues(updating_domain = FALSE, updating_subdomain = FALSE)
  # update dropdown choices
  observe({
    updateSelectizeInput(session, "study_filter", choices = unique(na.omit(metadata.xvars$study)),
                         selected = character(0))
    # populate domain and subdomain with ALL options initially
    updateSelectizeInput(session, "domain_filter", choices = unique(na.omit(metadata.xvars$domain)),
                         selected = character(0))
    updateSelectizeInput(session, "subdomain_filter", choices = unique(na.omit(metadata.xvars$sub_domain)),
                         selected = character(0))
    
    updateSelectizeInput(session, "source_filter", choices = unique(na.omit(metadata.xvars$source)),
                         selected = character(0))
    updateSelectizeInput(session, "table_name_filter", choices = unique(na.omit(metadata.xvars$table_name)),
                         selected = character(0))
    updateSelectizeInput(session, "has_branching_logic_filter", choices = unique(na.omit(metadata.xvars$has_skip_logic)),
                         selected = character(0))
    updateSelectizeInput(session, "sensitivity_filter", choices = unique(na.omit(metadata.xvars$sensitivity)),
                         selected = character(0))
    updateSelectizeInput(session, "alexsa_facet_filter", choices = unique(na.omit(metadata.xvars$alexsa_facet)),
                         selected = character(0))
    
    # ----- use facet_map directly for plab_facet choices ---
    updateSelectizeInput(session, "plab_facet_filter",
                         choices = sort(names(facet_map)),
                         selected = character(0))
    
    # start plab_subfacet empty (will be filled based on facet selection)
    updateSelectizeInput(session, "plab_subfacet_filter",
                         choices = character(0),
                         selected = character(0))
  })
  # domain --> subdomain (intersection behavior)
  observeEvent(input$domain_filter, {
    if (isTRUE(rv$updating_domain)) return()
    
    sel_domains <- input$domain_filter
    if (is.null(sel_domains) || length(sel_domains) == 0) {
      # show all subdomains when no domain selected
      rv$updating_subdomain <- TRUE
      updateSelectizeInput(session, "subdomain_filter", choices = sort(unique(na.omit(metadata.xvars$sub_domain))), selected = character(0))
      rv$updating_subdomain <- FALSE
      return()
    }
    
    matched <- domain_map[ intersect(names(domain_map), sel_domains) ]
    # intersection of subdomains across selected domains
    if (length(matched) == 0) {
      new_choices <- character(0)
    } else if (length(matched) == 1) {
      new_choices <- matched[[1]]
    } else {
      new_choices <- Reduce(intersect, matched)
    }
    new_choices <- sort(unique(new_choices))
    
    # preserve current selections that remain valid
    current_sel <- isolate(input$subdomain_filter)
    preserved <- if (!is.null(current_sel)) intersect(current_sel, new_choices) else character(0)
    
    rv$updating_subdomain <- TRUE
    updateSelectizeInput(session, "subdomain_filter", choices = new_choices, selected = preserved)
    rv$updating_subdomain <- FALSE
  }, ignoreNULL = FALSE)
  
  
  # subdomain --> domain unification
  observeEvent(input$subdomain_filter, {
    if (isTRUE(rv$updating_subdomain)) return()
    
    
    sel_subs <- input$subdomain_filter
    if (is.null(sel_subs) || length(sel_subs) == 0) {
      # show all domains when no subdomain selected
      rv$updating_domain <- TRUE
      updateSelectizeInput(session, "domain_filter", choices = sort(unique(na.omit(metadata.xvars$domain))), 
                           selected = character(0))
      rv$updating_domain <- FALSE
      return()
    }
    
    # gather domains that contain any of the selected subdomains (union)
    matched_domains <- subdomain_to_domains[ intersect(names(subdomain_to_domains), sel_subs) ]
    # intersection across selected subdomains
    if (length(matched_domains) == 0) {
      new_domains <- character(0)
    } else if (length(matched_domains) == 1) {
      new_domains <- matched_domains[[1]]
    } else {
      new_domains <- Reduce(intersect, matched_domains)
    }
    new_domains <- sort(unique(new_domains))
    
    current_sel <- isolate(input$domain_filter)
    preserved <- if (!is.null(current_sel)) intersect(current_sel, new_domains) else character(0)
    
    rv$updating_domain <- TRUE
    updateSelectizeInput(session, "domain_filter", choices = new_domains, selected = preserved)
    rv$updating_domain <- FALSE
  }, ignoreNULL = FALSE)
  
  # --- update subfacet choices when facet seletion changes using facet_map
  observeEvent(input$plab_facet_filter, {
    sel_facets <- input$plab_facet_filter
    
    if(is.null(sel_facets) || length(sel_facets) == 0){
      # nothing selected -> clear subfacet choices
      updateSelectizeInput(session, "plab_subfacet_filter",
                           choices = character(0),
                           selected = character(0))
    } else{
      # gather subfacets from facet_map for the selected facets
      matched <- facet_map[ intersect(names(facet_map), sel_facets)  ]
      new_choices <- sort(unique(unlist(matched, use.names = FALSE)))
      
      # preserve any currently-selected subfacets 
      current_sel <- isolate(input$plab_subfacet_filter)
      preserved <- if (!is.null(current_sel)) intersect(current_sel, new_choices) else character(0)
      
      updateSelectizeInput(session, "plab_subfacet_filter",
                           choices = new_choices,
                           selected = preserved)
    }
  }, ignoreNULL = FALSE) # run once to initialize empty subfacet
  
  # ---- if user opens subfacet before selecting facets  ----
  observeEvent(input$plab_subfacet_filter, {
    # do nothing here — we only react when plab_facet_filter changes
    # (keeps behavior simple)
  })
  
  # reactive dataset based on filters
  filtered_data <- reactive({
    df <- metadata.xvars
    if (!is.null(input$study_filter) && length(input$study_filter) > 0) {
      df <- df[df$study %in% input$study_filter, , drop = FALSE]
    }
    if (!is.null(input$domain_filter) && length(input$domain_filter) > 0) {
      df <- df[df$domain %in% input$domain_filter, , drop = FALSE]
    }
    if (!is.null(input$subdomain_filter) && length(input$subdomain_filter) > 0) {
      df <- df[df$sub_domain %in% input$subdomain_filter, , drop = FALSE]
    }
    if (!is.null(input$source_filter) && length(input$source_filter) > 0) {
      df <- df[df$source %in% input$source_filter, , drop = FALSE]
    }
    if (!is.null(input$table_name_filter) && length(input$table_name_filter) > 0) {
      df <- df[df$table_name %in% input$table_name_filter, , drop = FALSE]
    }
    if (!is.null(input$has_branching_logic_filter) && length(input$has_branching_logic_filter) > 0) {
      df <- df[df$has_skip_logic %in% input$has_branching_logic_filter, , drop = FALSE]
    }
    if (!is.null(input$sensitivity_filter) && length(input$sensitivity_filter) > 0) {
      df <- df[df$sensitivity %in% input$sensitivity_filter, , drop = FALSE]
    }
    if (!is.null(input$plab_facet_filter) && length(input$plab_facet_filter) > 0) {
      df <- df[df$plab_facet %in% input$plab_facet_filter, , drop = FALSE]
    }
    if (!is.null(input$plab_subfacet_filter) && length(input$plab_subfacet_filter) > 0) {
      df <- df[df$plab_subfacet %in% input$plab_subfacet_filter, , drop = FALSE]
    }
    if (!is.null(input$alexsa_facet_filter) && length(input$alexsa_facet_filter) > 0) {
      df <- df[df$alexsa_facet %in% input$alexsa_facet_filter, , drop = FALSE]
    }
    
    return(df)
    
  })
  # render filtered datatable
  output$table <- renderDT({datatable(filtered_data(), 
                                      options = list(pageLength = 10),
                                      rownames = FALSE)
  })
}
shinyApp(ui = ui, server = server)
