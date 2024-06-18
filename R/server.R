
# server definition ---------------------------------------------------------
gd2vis_server <- function(input, output, session) {
  
  options(shiny.maxRequestSize=1000*1024^2)
  train.data <- readRDS(system.file("extdata", "train.data.Rds", package = "GD2Viz"))
  mgraph <- readRDS(system.file("extdata", "substrate_graph.Rds", package = "GD2Viz"))

  # Header info menus ----------------
  observeEvent(input$notification1, {
    runjs("window.open('https://federicomarini.github.io/GeneTonic/articles/GeneTonic_manual.html', '_blank')")
  })
  
  observeEvent(input$notification2, {
    showModal(
      modalDialog(
        title = "Session information",
        size = "l",
        fade = TRUE,
        footer = NULL,
        easyClose = TRUE,
        tagList(tags$code("> sessionInfo()"),
                renderPrint({
                  utils::sessionInfo()
                }))
      )
    )
  })
  
  observeEvent(input$notification3, {
    showModal(
      modalDialog(
        title = "About GD2Viz",
        size = "l",
        fade = TRUE,
        footer = NULL,
        easyClose = TRUE,
        tagList(includeMarkdown(
          system.file("extdata", "about.md", package = "GD2Viz")
        ),
        renderPrint({
          utils::citation("GD2Viz")
        }))
      )
    )
  })
  
  # computeCustomRAS
  # computeCustomScore
  # updateCustomPlots
  
  # Custom dataset tab ----------------
  
  custom_data <- reactiveVal(NULL)
  custom_GD2 <- reactiveVal(NULL)
  custom_RAS_messages <- reactiveVal(NULL)
  
  observeEvent(input$metadataFile, {
    custom_data(NULL)
    custom_GD2(NULL)
    custom_RAS_messages(NULL)
  })
  
  observeEvent(input$countsFile, {
    custom_data(NULL)
    custom_GD2(NULL)
    custom_RAS_messages(NULL)
  })
  
  observeEvent(input$ddsFile, {
    custom_data(NULL)
    custom_GD2(NULL)
    custom_RAS_messages(NULL)
  })
  
  observeEvent(input$computeCustomRAS, {
    custom_data_val <- NULL
    error_message <- NULL
    warning_message <- NULL
    success_message <- NULL
    
    if (input$dataType == "count_meta") {
      if (is.null(input$metadataFile) || is.null(input$countsFile)) {
        warning_message <- "Please upload both metadata and counts files to compute RAS."
      }
    } else if (input$dataType == "dds") {
      if (is.null(input$ddsFile)) {
        warning_message <- "Please upload the DDS file to compute RAS."
      }
    }
    
    if (input$dataType == "count_meta" && !is.null(input$metadataFile) && !is.null(input$countsFile)) {
      custom_coldata <- read.table(file = input$metadataFile$datapath, sep = "\t", header = TRUE)
      custom_counts <- read.table(file = input$countsFile$datapath, sep = "\t", header = TRUE)
      
      tryCatch({
        custom_data_val <- computeReactionActivityScores(
          counts = custom_counts, 
          metadata = custom_coldata, 
          mgraph = mgraph, 
          geom = train.data$geom
        )
        success_message <- "RAS computed successfully!"
      }, warning = function(w) {

        warning_message <<- w$message
      }, error = function(e) {

        error_message <<- e$message
      })
    } else if (input$dataType == "dds" && !is.null(input$ddsFile)) {
      custom_dds <- readRDS(input$ddsFile$datapath)
      
      tryCatch({
        custom_data_val <- computeReactionActivityScores(
          dds = custom_dds,
          mgraph = mgraph, 
          geom = train.data$geom
        )
        success_message <- "RAS computed successfully!"
      }, warning = function(w) {
        warning_message <<- w$message
      }, error = function(e) {
        error_message <<- e$message
      })
    }
    custom_data(custom_data_val)
    custom_RAS_messages(list(error = error_message, warning = warning_message, success = success_message))
  })
  
  output$customRASmessageUI <- renderUI({
    req(input$computeCustomRAS)
    
    error_message <- custom_RAS_messages()$error
    warning_message <- custom_RAS_messages()$warning
    success_message <- custom_RAS_messages()$success
    
    if (!is.null(error_message)) {
      return(HTML(paste0(
        '<div style="color: red; display: flex; align-items: center;">',
        as.character(shiny::icon("exclamation-triangle", class = "fa-lg", style = "margin-right: 8px;")),
        '<p style="margin: 0;">', error_message, '</p>',
        '</div>'
      )))
    }
    
    if (!is.null(warning_message)) {
      return(HTML(paste0(
        '<div style="color: orange; display: flex; align-items: center;">',
        as.character(shiny::icon("exclamation-triangle", class = "fa-lg", style = "margin-right: 8px;")),
        '<p style="margin: 0;">', warning_message, '</p>',
        '</div>'
      )))
    }
    
    if (!is.null(success_message)) {
      return(HTML(paste0(
        '<div style="display: flex; align-items: center; color: green;">',
        as.character(shiny::icon("check", class = "fa-lg", style = "margin-right: 8px; color: green;")),
        '<p style="margin: 0;">', success_message, '</p>',
        '</div>'
      )))
    }
  })
  
 
  
  output$selectCustomGroupUI <- renderUI({
    if(!is.null(custom_data())){
      selectInput("customColData", "Group by:", choices = c("", names(colData(custom_data()$custom_dds))))
    }
  })
  
  observeEvent(input$computeCustomScore, {
    req(custom_data(), input$customScale)
    ras_types <- c("ras", "ras_prob", "ras_prob_path", "ras_prob_rec")
    model_list <- list()
    pred_list <- list()
    
    for (ras_type in ras_types) {
      model <- trainGD2model(
        train.data = train.data, 
        adjustRAS = ras_type, 
        adjustInput = input$customScale
      )
      model_list[[ras_type]] <- model
    }
    
    for (ras_type in ras_types) {
      pred <- computeGD2Score(
        RAS = custom_data()[[ras_type]],
        SVMmodel = model_list[[ras_type]],
        adjustInput = input$customScale,
        rangeOutput = FALSE,
        center = TRUE
      )
      pred_list[[ras_type]] <- pred
    }
    custom_GD2(pred_list)
  })
  
  output$customGD2messageUI <- renderText({
    req(input$computeCustomScore)
    
    if (is.null(custom_GD2())) {
       return(HTML(paste0(
        '<div style="color: orange; display: flex; align-items: center;">',
        as.character(shiny::icon("exclamation-triangle", class = "fa-lg", style = "margin-right: 8px;")),
        '<p>Please upload your data first.</p>',
        '</div>'
      )))
    } else {
      ### TODO check pred_list()
      icon_html <- as.character(shiny::icon("check", class = "fa-lg", style = "margin-right: 8px; color: green;"))
      HTML(paste0(
        '<div style="display: flex; align-items: center; color: green;">',
        icon_html,
        '<p style="margin: 0;">Trained SVM model & predicted GD2 score successfully!</p>',
        '</div>'
      ))
    }
  })
  
  output$plot1 <- renderPlotly({
    req(custom_GD2(), custom_data())
    
    if (input$customColData == "") {
      group <- as.factor(rep(1, nrow(colData(custom_data()$custom_dds))))
      meandf.custom <- NULL
    } else {
      group <- colData(custom_data()$custom_dds)[, input$customColData]
      meandf.custom <- aggregate(custom_GD2()[[input$customRASType]], list(group), FUN = mean)
    }
    
    plot_df <- data.frame(Group = group, Score = custom_GD2()[[input$customRASType]])
    
    if (is.null(meandf.custom)) {
      Group.1 <- NULL
    } else {
      Group.1 <- meandf.custom[order(meandf.custom$x), "Group.1"]
    }
    
    fig <- plot_ly(data = plot_df, x = ~Score, y = ~as.character(Group), type = "scatter", mode = "markers", text = rownames(plot_df)) %>%
      layout(yaxis = list(categoryorder = "array", categoryarray = Group.1, title = 'Group', showticklabels = TRUE),
             xaxis = list(title = 'GD2 score'))
    
    fig
  })
  
  output$plot2 <- renderPlotly({
    plot_ly(x = rnorm(100), y = rnorm(100), type = 'scatter', mode = 'markers')
  })

} 