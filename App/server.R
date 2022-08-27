############################################################################
##Project: RAND Report Search Tool
##Code: Define server
##Date: 10 Mar 2021
##Author: Matt Walsh (mmw188@gmail.com)
############################################################################


shinyServer(function(session, input, output) {
  
  # Close app on Session End
  session$onSessionEnded(function() {
    stopApp()
  })
  
  ## Generate table of results
  table_res <- reactive({
    
    entry <- if (input$search_entry != ''){
      input$search_entry
    } else if (!is.na(analysis.vals$df.user)){
      analysis.vals$df.user
    } else {
      NA_character_
    }
    
    topic <- if (is.null(input$search_topic)) {NA_character_} else {input$search_topic}
    clust <- if (is.null(input$search_clust)) {NA_character_} else {input$search_clust}
    
    search.input <- list(entry = entry, topic = topic, clust = clust)
    output       <- search.data(search.input, analysis.vals$df.source, analysis.vals$df.target)
    
    if (length(input$program) == 0){pg <- unique(output$results$Program)} else {pg <- input$program}
    
    output$results     <- filter(output$results,     Year >= input$years[1], Year <= input$years[2], Program %in% pg) %>% select(Report)
    output$raw.results <- filter(output$raw.results, Year >= input$years[1], Year <= input$years[2], Program %in% pg)

    return(output)

  })
  
  ## Render table of results
  output$table_results <- renderDT({

    withProgress(value = 1, message = 'Performing Search', {
      
      dt <- datatable(
        table_res()$results,
        colnames = '',
        rownames = FALSE,
        escape = FALSE,
        filter = list(position = 'none'),
        options = list(
          pageLength = 1000,
          deferRender = TRUE,
          scrollY = '50vh',
          dom = 't',
          language = list(zeroRecords = 'Enter search terms or select titles to view related reports')
          ),
        fillContainer = T,
        class = "display"
        )
    })
    
    dt
  })
    
  ## Generate network plot
  output$plot_network <- renderVisNetwork({

    entry <- if (input$search_entry != ''){
      input$search_entry
    } else if (!is.na(analysis.vals$df.user)){
      analysis.vals$df.user
    } else {
      NA_character_
    }
    
    topic <- if (is.null(input$search_topic)) {NA_character_} else {input$search_topic}
    clust <- if (is.null(input$search_clust)) {NA_character_} else {input$search_clust}
    
    search.input <- list(entry = entry, topic = topic, clust = clust)
    
    withProgress(value = 1, message = 'Creating Network', {
      plt <- plot.network(search.input, analysis.vals$df.source, analysis.vals$df.target, group.vars)
    })
    
    shinyjs::show(id = 'vis_text')
    plt
  })
  
  ## Generate legend
  output$plot_legend <- renderPlot({
    legend.plt
  })
  
  ## Identify missing terms
  observe({
    term.out <- missing.terms(input$search_entry, analysis.vals$df.target)
    output$missing_term <- renderText({term.out})
  })
  
  ## Load external file
  observeEvent(input$file_input, {
    df <- read_document(input$file_input$datapath)
    analysis.vals$df.user <- paste(unlist(df, recursive = TRUE), collapse = ' ')
    updateSelectizeInput(session, 'search_topic', selected = 1)
    updateSearchInput(session, 'search_entry', value = '', trigger = TRUE)
  })

  ## Add search terms
  observeEvent(input$search_entry, {
    if (input$search_entry != ''){
      updateSelectizeInput(session, 'search_topic', selected = 1)
      reset("file_input")
      analysis.vals$df.user <- NA_character_
    }
  })
  
  ## Select search topic
  observeEvent(input$search_topic, {
    updateSearchInput(session, 'search_entry', value = '', trigger = TRUE)
    reset("file_input")
    analysis.vals$df.user <- NA_character_
  })
  
  ## Download results
  output$download_table <- downloadHandler(

    filename = function() {
      paste('table', gsub('-', '', Sys.Date()), '.xlsx', sep = '')
    },
    
    content = function(file) {
      save_table <- table_res()$raw.results
      write.xlsx(save_table, file = file, row.names = FALSE)
    }
  )
  
  observe_helpers()
})
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################