
# server definition ---------------------------------------------------------
gd2visServer <- function(input, output, session, data_path = NULL) {
  
  options(shiny.maxRequestSize = 1000 * 1024^2)
  
  has_private_data <- !is.null(data_path) && dir.exists(data_path)
  
  # Always-present data
  trainData <- readRDS(system.file("extdata", "train_data.Rds", package = "GD2Viz"))
  mgraph <- readRDS(system.file("extdata", "substrate_graph.Rds", package = "GD2Viz")) %>% igraph::upgrade_graph()
  stem <- read.delim(
    system.file("extdata", "stemness_sig_weights.tsv", package = "GD2Viz"),
    header = FALSE,
    row.names = 1
  ) %>% as.matrix() %>% drop()
  
  # Conditionally load private data
  if (has_private_data) {
    tcgaData   <- readRDS(file.path(data_path, "RAS_datasets", "TCGA_RAS.Rds"))
    gtexData   <- readRDS(file.path(data_path, "RAS_datasets", "GTEX_RAS.Rds"))
    targetData <- readRDS(file.path(data_path, "RAS_datasets", "TARGET_RAS.Rds"))
    stjudeData <- readRDS(file.path(data_path, "RAS_datasets", "STJUDE_RAS.Rds"))
    cbttcData  <- readRDS(file.path(data_path, "RAS_datasets", "CBTTC_RAS.Rds"))
    
    tcga_coldata <- colData(tcgaData)
    tcgaNormalIndx <- which(tcga_coldata$Sample_Type == "Solid Tissue Normal")
    
    tcgaNormalData <- SummarizedExperiment(
      assays = lapply(assays(tcgaData), function(x) x[, tcgaNormalIndx, drop = FALSE]),
      colData = tcga_coldata[tcgaNormalIndx, , drop = FALSE])
    
    tcgaTumorData <- SummarizedExperiment(
      assays = lapply(assays(tcgaData), function(x) x[, -tcgaNormalIndx, drop = FALSE]),
      colData = tcga_coldata[-tcgaNormalIndx, , drop = FALSE])
  }

  # Dynamic Sidebar UI -----------
  
  output$sidebar_ui <- renderUI({
    dashboardSidebar(
      id = "sidebar",
      
      # Logo
      tags$head(
        tags$style(HTML("
        #resizable-image {
          transition: width 0.5s ease-in-out;
        }
      ")),
        tags$script(HTML("
        document.addEventListener('DOMContentLoaded', function() {
          const img = document.getElementById('resizable-image');
          const body = document.body;
          function resizeImage() {
            img.style.width = body.classList.contains('sidebar-collapse') ? '60px' : '180px';
          }
          resizeImage();
          const observer = new MutationObserver(m => m.forEach(mutation => {
            if (mutation.attributeName === 'class') resizeImage();
          }));
          observer.observe(body, { attributes: true });
        });
      "))
      ),
      div(
        img(id = "resizable-image", src = "GD2Viz/GD2Viz6.png",
            style = "width: 180px; height: auto; margin-bottom: 30px; display: block; margin-left: auto; margin-right: auto;")
      ),
      
      skin = "light",
      status = "primary",
      elevation = 3,
      
      sidebarMenu(
        menuItem("Welcome", tabName = "welcomeTab", icon = icon("house"))
      ),
      
      sidebarMenu(
        sidebarHeader("Main tabs:"),
        
        if (has_private_data) {
          list(
            menuItem("Public Datasets", tabName = "exploreDataTab", icon = icon("database")),
            menuItem("TCGA Cancer Types", tabName = "tcgaDetailTab", icon = icon("magnifying-glass-plus"))
          )
        },
        
        menuItem("Analyze Your Data", tabName = "customDataTab", icon = icon("file-arrow-up"))
      ),
      
      textOutput("package_version"),
      
      tags$head(
        tags$style("
        #package_version {
          position: absolute;
          bottom: 0;
          align: center;
          padding-left: 80px;
        }
      ")
      )
    )
  })
  
  
  # Header info menus ----------------
  observeEvent(input$notification1, {
    runjs("window.open('https://github.com/arsenij-ust/GD2Viz', '_blank')")
  })
  
  observeEvent(input$notification2, {
    showModal(
      modalDialog(
        title = "Session information",
        size = "l",
        fade = TRUE,
        footer = NULL,
        easyClose = TRUE,
        tagList(tags$code("> sessionInfo()"), renderPrint({
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
          system.file("extdata/documentation", "about.md", package = "GD2Viz")
        ), renderPrint({
          utils::citation("GD2Viz")
        }))
      )
    )
  })
  
  observeEvent(input$notification4, {
    runjs("window.open('https://github.com/arsenij-ust/GD2Viz/issues', '_blank')")
  })
  
  output$package_version <- renderText({
    paste("Version:", packageVersion("GD2Viz"))
  })
  
  output$visual_abstract <- renderImage({
    # When input$n is 1, filename is ./images/image1.jpeg
    filename <- system.file("www/", "visual_abstract.png", package = "GD2Viz")
    width  <- session$clientData$output_visual_abstract_width
    height <- session$clientData$output_myImage_height
    
    # print(width)
    max_width <- 1500
    if (width > max_width)
      width <- max_width
    
    # Return a list containing the filename
    list(src = filename,
         width = width,
         height = height)
  }, deleteFile = FALSE)

  # Public Datasets tab ----------------
  
  ## Train GD2 model -----
  GD2model <- reactiveVal(NULL)
  observe({
    req(has_private_data)
    req(input$dataTabScale, input$dataTabRASType)
    
    model <- trainGD2model(
      train_data = trainData,
      adjust_ras = input$dataTabRASType,
      adjust_input = input$dataTabScale
    )
    GD2model(model)
  })
  
  ## TCGA Tumor predict values -----
  tcgaTumorGD2 <- reactiveVal(NULL)
  observe({
    req(has_private_data)
    req(tcgaTumorData,
        GD2model(),
        input$dataTabScale,
        input$dataTabRASType)
    
    pred <- computeGD2Score(
      RAS = assay(tcgaTumorData, input$dataTabRASType),
      svm_model = GD2model(),
      adjust_input = input$dataTabScale,
      range_output = FALSE,
      center = TRUE
    )
    
    tcgaTumorGD2(list(
      prediction = pred$preds,
      pred_input = pred$input_df
    ))
  })
  
  output$tcgaTumorColDataUI <- renderUI({
    req(has_private_data)
    req(tcgaTumorData)
    
    selectableCols <- c(
      "Primary_Diagnosis",
      "Cancer_Type",
      "Sample_Type",
      "Primary_site",
      "Primary_Disease"
    )
    
    selectInput(
      "tcgaTumorColData",
      "Group by:",
      choices = c("none", selectableCols),
      selected = "Primary_Disease"
    )
  })
  
  output$tcgaTumorHighlightGroupUI <- renderUI({
    req(has_private_data)
    req(tcgaTumorData, input$tcgaTumorColData)
    
    selectizeInput(
      "tcgaTumorHighlightGroup",
      label = "Highlight Group:",
      choices = NULL,
      selected = NULL,
      multiple = TRUE
    )
  })
  
  observe({
    req(has_private_data)
    req(tcgaTumorData, input$tcgaTumorColData)
    updateSelectizeInput(
      session,
      "tcgaTumorHighlightGroup",
      choices = c("", as.character(colData(tcgaTumorData)[[input$tcgaTumorColData]])),
      server = TRUE
    )
  })

  ## TCGA Normal predict values -----
  tcgaNormalGD2 <- reactiveVal(NULL)
  observe({
    req(has_private_data)
    req(tcgaNormalData,
        GD2model(),
        input$dataTabScale,
        input$dataTabRASType)
    
    pred <- computeGD2Score(
      RAS = assay(tcgaNormalData, input$dataTabRASType),
      svm_model = GD2model(),
      adjust_input = input$dataTabScale,
      range_output = FALSE,
      center = TRUE
    )
    
    tcgaNormalGD2(list(
      prediction = pred$preds,
      pred_input = pred$input_df
    ))
  })
  
  output$tcgaNormalColDataUI <- renderUI({
    req(has_private_data)
    req(tcgaNormalData)
    
    selectableCols <- c(
      "Primary_Diagnosis",
      "Cancer_Type",
      "Sample_Type",
      "Primary_site",
      "Primary_Disease"
    )
    
    selectInput(
      "tcgaNormalColData",
      "Group by:",
      choices = c("none", selectableCols),
      selected = "Primary_Disease"
    )
  })
  
  output$tcgaNormalHighlightGroupUI <- renderUI({
    req(has_private_data)
    req(tcgaNormalData, input$tcgaNormalColData)
    
    selectizeInput(
      "tcgaNormalHighlightGroup",
      label = "Highlight Group:",
      choices = NULL,
      selected = NULL,
      multiple = TRUE
    )
  })
  
  observe({
    req(has_private_data)
    req(tcgaNormalData, input$tcgaNormalColData)
    updateSelectizeInput(
      session,
      "tcgaNormalHighlightGroup",
      choices = c("", as.character(colData(tcgaNormalData)[[input$tcgaNormalColData]])),
      server = TRUE
    )
  })

  ## GTEX predict values -----
  gtexGD2 <- reactiveVal(NULL)
  observe({
    req(has_private_data)
    req(gtexData,
        GD2model(),
        input$dataTabScale,
        input$dataTabRASType)
    
    pred <- computeGD2Score(
      RAS = assay(gtexData, input$dataTabRASType),
      svm_model = GD2model(),
      adjust_input = input$dataTabScale,
      range_output = FALSE,
      center = TRUE
    )
    
    gtexGD2(list(
      prediction = pred$preds,
      pred_input = pred$input_df
    ))
  })
  
  output$gtexColDataUI <- renderUI({
    req(has_private_data)
    req(gtexData)
    selectInput(
      "gtexColData",
      "Group by:",
      choices = c("none", colnames(colData(gtexData))),
      selected = "Primary_Disease"
    )
  })
  
  output$gtexHighlightGroupUI <- renderUI({
    req(has_private_data)
    req(gtexData, input$gtexColData)
    
    selectizeInput(
      "gtexHighlightGroup",
      label = "Highlight Group:",
      choices = NULL,
      selected = NULL,
      multiple = TRUE
    )
  })
  
  observe({
    req(has_private_data)
    req(gtexData, input$gtexColData)
    updateSelectizeInput(
      session,
      "gtexHighlightGroup",
      choices = c("", as.character(colData(gtexData)[[input$gtexColData]])),
      server = TRUE
    )
  })

  ## TARGET predict values -----
  targetGD2 <- reactiveVal(NULL)
  observe({
    req(has_private_data)
    req(targetData,
        GD2model(),
        input$dataTabScale,
        input$dataTabRASType)
    
    pred <- computeGD2Score(
      RAS = assay(targetData, input$dataTabRASType),
      svm_model = GD2model(),
      adjust_input = input$dataTabScale,
      range_output = FALSE,
      center = TRUE
    )
    
    targetGD2(list(
      prediction = pred$preds,
      pred_input = pred$input_df
    ))
  })
  
  output$targetColDataUI <- renderUI({
    req(has_private_data)
    req(targetData)
    selectInput(
      "targetColData",
      "Group by:",
      choices = c("none", colnames(colData(targetData))),
      selected = "Primary_Disease"
    )
  })
  
  output$targetHighlightGroupUI <- renderUI({
    req(has_private_data)
    req(targetData, input$targetColData)
    
    selectizeInput(
      "targetHighlightGroup",
      label = "Highlight Group:",
      choices = NULL,
      selected = NULL,
      multiple = TRUE
    )
  })
  
  observe({
    req(has_private_data)
    req(targetData, input$targetColData)
    updateSelectizeInput(
      session,
      "targetHighlightGroup",
      choices = c("", as.character(colData(targetData)[[input$targetColData]])),
      server = TRUE
    )
  })

  ## St. Jude predict values -----
  stjudeGD2 <- reactiveVal(NULL)
  observe({
    req(has_private_data)
    req(stjudeData,
        GD2model(),
        input$dataTabScale,
        input$dataTabRASType)
    
    pred <- computeGD2Score(
      RAS = assay(stjudeData, input$dataTabRASType),
      svm_model = GD2model(),
      adjust_input = input$dataTabScale,
      range_output = FALSE,
      center = TRUE
    )
    
    stjudeGD2(list(
      prediction = pred$preds,
      pred_input = pred$input_df
    ))
  })
  
  output$stjudeColDataUI <- renderUI({
    req(has_private_data)
    req(stjudeData)
    selectInput(
      "stjudeColData",
      "Group by:",
      choices = c("none", colnames(colData(stjudeData))),
      selected = "Primary_Disease"
    )
  })
  
  output$stjudeHighlightGroupUI <- renderUI({
    req(has_private_data)
    req(stjudeData, input$stjudeColData)
    
    selectizeInput(
      "stjudeHighlightGroup",
      label = "Highlight Group:",
      choices = NULL,
      selected = NULL,
      multiple = TRUE
    )
  })
  
  observe({
    req(has_private_data)
    req(stjudeData, input$stjudeColData)
    updateSelectizeInput(
      session,
      "stjudeHighlightGroup",
      choices = c("", as.character(colData(stjudeData)[[input$stjudeColData]])),
      server = TRUE
    )
  })

  ## CBTTC predict values -----
  cbttcGD2 <- reactiveVal(NULL)
  observe({
    req(has_private_data)
    req(cbttcData,
        GD2model(),
        input$dataTabScale,
        input$dataTabRASType)
    
    pred <- computeGD2Score(
      RAS = assay(cbttcData, input$dataTabRASType),
      svm_model = GD2model(),
      adjust_input = input$dataTabScale,
      range_output = FALSE,
      center = TRUE
    )
    
    cbttcGD2(list(
      prediction = pred$preds,
      pred_input = pred$input_df
    ))
  })
  
  output$cbttcColDataUI <- renderUI({
    req(has_private_data)
    req(cbttcData)
    selectInput(
      "cbttcColData",
      "Group by:",
      choices = c("none", colnames(colData(cbttcData))),
      selected = "Histological_Diagnosis"
    )
  })
  
  output$cbttcHighlightGroupUI <- renderUI({
    req(has_private_data)
    req(cbttcData, input$cbttcColData)
    
    selectizeInput(
      "cbttcHighlightGroup",
      label = "Highlight Group:",
      choices = NULL,
      selected = NULL,
      multiple = TRUE
    )
  })
  
  observe({
    req(has_private_data)
    req(cbttcData, input$cbttcColData)
    updateSelectizeInput(
      session,
      "cbttcHighlightGroup",
      choices = c("", as.character(colData(cbttcData)[[input$cbttcColData]])),
      server = TRUE
    )
  })
  
  ## TCGA Tumor GD2 Score Plot -----
  output$tcgaTumorGD2plot <- renderPlotly({
    req(has_private_data)
    req(
      tcgaTumorGD2(),
      tcgaTumorData,
      input$tcgaTumorColData,
      input$tcgaTumorGD2ScorePlotType
    )
    
    if (input$tcgaTumorColData == "none") {
      group <- as.factor(rep(1, nrow(colData(tcgaTumorData))))
      meandf_custom <- NULL
      group_title <- ""
      colors <- "#164863"
    } else {
      group <- colData(tcgaTumorData)[, input$tcgaTumorColData]
      meandf_custom <- aggregate(tcgaTumorGD2()$prediction, list(group), FUN = mean)
      group_title <- input$tcgaTumorColData
      # print(meandf_custom)
      if (length(unique(group)) == 2) {
        colors <- c("#164863", "#ff851b")
      } else {
        colors <- "#164863"
      }
    }
    
    pred_values <- tcgaTumorGD2()$prediction
    if (input$tcgaTumorGD2ScoreRange == "yes") {
      pred_values <- range01(pred_values)
    }
    
    plot_df <- data.frame(Group = group, Score = pred_values)
    
    if (is.null(meandf_custom)) {
      Group.1 <- NULL
    } else {
      Group.1 <- meandf_custom[order(meandf_custom$x), "Group.1"]
    }
    
    # Determine plot type
    plot_type <- input$tcgaTumorGD2ScorePlotType
    highlightGroup <- input$tcgaTumorHighlightGroup
    
    plotGD2Score(plot_df,
                 plot_type,
                 Group.1,
                 group_title,
                 colors,
                 highlightGroup)
    
  })

  add_plot_maximize_observer(input, "tcgaTumorGD2ScoreBox", "tcgaTumorGD2plot")
  
  ## TCGA Normal GD2 Score Plot -----
  output$tcgaNormalGD2plot <- renderPlotly({
    req(has_private_data)
    req(
      tcgaNormalGD2(),
      tcgaNormalData,
      input$tcgaNormalColData,
      input$tcgaNormalGD2ScorePlotType
    )
    
    if (input$tcgaNormalColData == "none") {
      group <- as.factor(rep(1, nrow(colData(tcgaNormalData))))
      meandf_custom <- NULL
      group_title <- ""
      colors <- "#164863"
    } else {
      group <- colData(tcgaNormalData)[, input$tcgaNormalColData]
      meandf_custom <- aggregate(tcgaNormalGD2()$prediction, list(group), FUN = mean)
      group_title <- input$tcgaNormalColData
      if (length(unique(group)) == 2) {
        colors <- c("#164863", "#ff851b")
      } else {
        colors <- "#164863"
      }
    }
    
    pred_values <- tcgaNormalGD2()$prediction
    if (input$tcgaNormalGD2ScoreRange == "yes") {
      pred_values <- range01(pred_values)
    }
    
    plot_df <- data.frame(Group = group, Score = pred_values)
    
    if (is.null(meandf_custom)) {
      Group.1 <- NULL
    } else {
      Group.1 <- meandf_custom[order(meandf_custom$x), "Group.1"]
    }
    
    # Determine plot type
    plot_type <- input$tcgaNormalGD2ScorePlotType
    highlightGroup <- input$tcgaNormalHighlightGroup
    
    plotGD2Score(plot_df,
                 plot_type,
                 Group.1,
                 group_title,
                 colors,
                 highlightGroup) %>% toWebGL()
    
  })

  add_plot_maximize_observer(input, "tcgaNormalGD2ScoreBox", "tcgaNormalGD2plot")

  ## GTEX GD2 Score Plot -----
  output$gtexGD2plot <- renderPlotly({
    req(has_private_data)
    req(gtexGD2(),
        gtexData,
        input$gtexColData,
        input$gtexGD2ScorePlotType)
    
    if (input$gtexColData == "none") {
      group <- as.factor(rep(1, nrow(colData(gtexData))))
      meandf_custom <- NULL
      group_title <- ""
      colors <- "#164863"
    } else {
      group <- colData(gtexData)[, input$gtexColData]
      meandf_custom <- aggregate(gtexGD2()$prediction, list(group), FUN = mean)
      group_title <- input$gtexColData
      if (length(unique(group)) == 2) {
        colors <- c("#164863", "#ff851b")
      } else {
        colors <- "#164863"
      }
    }
    
    pred_values <- gtexGD2()$prediction
    if (input$gtexGD2ScoreRange == "yes") {
      pred_values <- range01(pred_values)
    }
    
    plot_df <- data.frame(Group = group, Score = pred_values)
    
    if (is.null(meandf_custom)) {
      Group.1 <- NULL
    } else {
      Group.1 <- meandf_custom[order(meandf_custom$x), "Group.1"]
    }
    
    # Determine plot type
    plot_type <- input$gtexGD2ScorePlotType
    highlightGroup <- input$gtexHighlightGroup
    
    plotGD2Score(plot_df,
                 plot_type,
                 Group.1,
                 group_title,
                 colors,
                 highlightGroup) %>% toWebGL()
    
  })

  add_plot_maximize_observer(input, "gtexGD2ScoreBox", "gtexGD2plot")

  ## TARGET GD2 Score Plot -----
  output$targetGD2plot <- renderPlotly({
    req(has_private_data)
    req(targetGD2(),
        targetData,
        input$targetColData,
        input$targetGD2ScorePlotType)
    
    if (input$targetColData == "none") {
      group <- as.factor(rep(1, nrow(colData(targetData))))
      meandf_custom <- NULL
      group_title <- ""
      colors <- "#164863"
    } else {
      group <- colData(targetData)[, input$targetColData]
      meandf_custom <- aggregate(targetGD2()$prediction, list(group), FUN = mean)
      group_title <- input$targetColData
      if (length(unique(group)) == 2) {
        colors <- c("#164863", "#ff851b")
      } else {
        colors <- "#164863"
      }
    }
    
    pred_values <- targetGD2()$prediction
    if (input$targetGD2ScoreRange == "yes") {
      pred_values <- range01(pred_values)
    }
    
    plot_df <- data.frame(Group = group, Score = pred_values)
    
    if (is.null(meandf_custom)) {
      Group.1 <- NULL
    } else {
      Group.1 <- meandf_custom[order(meandf_custom$x), "Group.1"]
    }
    
    # Determine plot type
    plot_type <- input$targetGD2ScorePlotType
    highlightGroup <- input$targetHighlightGroup
    
    plotGD2Score(plot_df,
                 plot_type,
                 Group.1,
                 group_title,
                 colors,
                 highlightGroup) %>% toWebGL()
    
  })

  add_plot_maximize_observer(input, "targetGD2ScoreBox", "targetGD2plot")

  ## ST Jude Cloud GD2 Score Plot -----
  output$stjudeGD2plot <- renderPlotly({
    req(has_private_data)
    req(stjudeGD2(),
        stjudeData,
        input$stjudeColData,
        input$stjudeGD2ScorePlotType)
    
    if (input$stjudeColData == "none") {
      group <- as.factor(rep(1, nrow(colData(stjudeData))))
      meandf_custom <- NULL
      group_title <- ""
      colors <- "#164863"
    } else {
      group <- colData(stjudeData)[, input$stjudeColData]
      meandf_custom <- aggregate(stjudeGD2()$prediction, list(group), FUN = mean)
      group_title <- input$stjudeColData
      if (length(unique(group)) == 2) {
        colors <- c("#164863", "#ff851b")
      } else {
        colors <- "#164863"
      }
    }
    
    pred_values <- stjudeGD2()$prediction
    if (input$stjudeGD2ScoreRange == "yes") {
      pred_values <- range01(pred_values)
    }
    
    plot_df <- data.frame(Group = group, Score = pred_values)
    
    if (is.null(meandf_custom)) {
      Group.1 <- NULL
    } else {
      Group.1 <- meandf_custom[order(meandf_custom$x), "Group.1"]
    }
    
    # Determine plot type
    plot_type <- input$stjudeGD2ScorePlotType
    highlightGroup <- input$stjudeHighlightGroup
    
    plotGD2Score(plot_df,
                 plot_type,
                 Group.1,
                 group_title,
                 colors,
                 highlightGroup) %>% toWebGL()
    
  })

  add_plot_maximize_observer(input, "stjudeGD2ScoreBox", "stjudeGD2plot")

  ## CBTTC GD2 Score Plot -----
  output$cbttcGD2plot <- renderPlotly({
    req(has_private_data)
    req(cbttcGD2(),
        cbttcData,
        input$cbttcColData,
        input$cbttcGD2ScorePlotType)
    
    if (input$cbttcColData == "none") {
      group <- as.factor(rep(1, nrow(colData(cbttcData))))
      meandf_custom <- NULL
      group_title <- ""
      colors <- "#164863"
    } else {
      group <- colData(cbttcData)[, input$cbttcColData]
      meandf_custom <- aggregate(cbttcGD2()$prediction, list(group), FUN = mean)
      group_title <- input$cbttcColData
      if (length(unique(group)) == 2) {
        colors <- c("#164863", "#ff851b")
      } else {
        colors <- "#164863"
      }
    }
    
    pred_values <- cbttcGD2()$prediction
    if (input$cbttcGD2ScoreRange == "yes") {
      pred_values <- range01(pred_values)
    }
    
    plot_df <- data.frame(Group = group, Score = pred_values)
    
    if (is.null(meandf_custom)) {
      Group.1 <- NULL
    } else {
      Group.1 <- meandf_custom[order(meandf_custom$x), "Group.1"]
    }
    
    # Determine plot type
    plot_type <- input$cbttcGD2ScorePlotType
    highlightGroup <- input$cbttcHighlightGroup
    
    plotGD2Score(plot_df,
                 plot_type,
                 Group.1,
                 group_title,
                 colors,
                 highlightGroup) %>% toWebGL()
    
  })

  add_plot_maximize_observer(input, "cbttcGD2ScoreBox", "cbttcGD2plot")

  # TCGA Cancer Types tab ----------------

  ## Group & Project UI -----
  output$tcgaTabProjectUI <- renderUI({
    req(has_private_data)
    req(tcgaTumorData)
    projects <- unique(paste0(
      colData(tcgaTumorData)$Primary_Disease,
      " (",
      colData(tcgaTumorData)$Cancer_Type,
      ")"
    ))
    selectInput(
      "tcgaTabProject",
      "TCGA Projects:",
      choices = c("", projects),
      selected = ""
    )
  })
  
  output$tcgaTabGroupUI <- renderUI({
    req(has_private_data)
    req(tcgaTumorData, input$tcgaTabProject)
    
    tcgaProject <- sub(".*\\((.*)\\).*", "\\1", input$tcgaTabProject)
    indx <- which(colData(tcgaTumorData)$Cancer_Type == tcgaProject)
    projectColData <- colData(tcgaTumorData)[indx, ]
    projectColData <- projectColData[, apply(projectColData, 2, function(col)
      ! all(is.na(col)))]
    removeColumns <- c("Cancer_Type", "Sample_Type", "Primary_site")
    validColumns <- colnames(projectColData[, -which(colnames(projectColData) %in% removeColumns)])
    selectInput(
      "tcgaTabGroup",
      label=div(
        "Group by:",
        tags$span(icon("circle-question")) %>%
          add_prompt(
            message = "Select molecular or clinical subgroup for a TCGA project. Please refer to https://bioconductor.org/packages/release/bioc/vignettes/TCGAbiolinks/inst/doc/subtypes.html for the provided molecular subgroups.",
            position = "right",
            type = "info",
            size = "large",
            rounded = TRUE
          )
      ),
      choices = c(validColumns),
      selected = "Primary_Disease"
    )
  })

  ## Model training -----
  tcgaTumorTabGD2 <- reactiveVal(NULL)
  observe({
    req(has_private_data)
    req(tcgaTumorData, input$tcgaTabScale, input$tcgaTabRASType)
    # rasTypes <- c("ras", "ras_prob", "ras_prob_path", "ras_prob_rec")
    # modelList <- list()
    # predList <- list()
    # inDataList <- list()
    rasType <- input$tcgaTabRASType
    
    model <- trainGD2model(
      train_data = trainData,
      adjust_ras = rasType,
      adjust_input = input$tcgaTabScale
    )
    # print(paste0("Model: ", rasType, "; adjust_input: ", input$tcgaTabScale))
    # print(model)
    pred <- computeGD2Score(
      RAS = assay(tcgaTumorData, rasType),
      svm_model = model,
      adjust_input = input$tcgaTabScale,
      range_output = FALSE,
      center = TRUE
    )
    
    tcgaTumorTabGD2(list(
      prediction = pred$preds,
      pred_input = pred$input_df
    ))
  })

  ## GD2 plot -----
  output$tcgaDetailGD2plot <- renderPlotly({
    req(has_private_data)
    req(tcgaTumorTabGD2(),
        tcgaTumorData,
        input$tcgaTabGroup,
        input$tcgaTabProject)
    
    # coldata.tcga.sum <- as.data.frame(table(as.factor(tcgaTumorData$coldata$cancer.type)))
    # colnames(coldata.tcga.sum) <- c("TCGA Project","Number of Samples")
    # tcga.project <- coldata.tcga.sum[input$tcgaProjectsTbl_row_last_clicked,"TCGA Project"]
    tcgaProject <- sub(".*\\((.*)\\).*", "\\1", input$tcgaTabProject)
    indx <- which(colData(tcgaTumorData)$Cancer_Type == tcgaProject)
    tcgaColData <- colData(tcgaTumorData)[indx, ]
    
    group <- tcgaColData[, input$tcgaTabGroup]
    meandf_custom <- aggregate(tcgaTumorTabGD2()$prediction[indx], list(group), FUN = mean)
    group_title <- input$tcgaTabGroup
    colors <- "#164863"
    
    pred_values <- tcgaTumorTabGD2()$prediction[indx]
    if (input$tcgaTabScoreRange == "yes") {
      pred_values <- range01(pred_values)
    }
    
    plot_df <- data.frame(Group = group, Score = pred_values)
    
    if (is.null(meandf_custom)) {
      Group.1 <- NULL
    } else {
      Group.1 <- meandf_custom[order(meandf_custom$x), "Group.1"]
    }
    
    # Determine plot type
    plot_type <- "scatter"
    highlightGroup <- NULL
    
    plotGD2Score(plot_df,
                 plot_type,
                 Group.1,
                 group_title,
                 colors,
                 highlightGroup)
    
  })

  # DEA ----
  ## Input parameter UIs ----
  output$tcgaTabDGEProjectUI <- renderUI({
    req(has_private_data)
    req(tcgaTumorData)
    projects <- unique(
      paste0(
        colData(tcgaTumorData)$Primary_Disease,
        " (",
        colData(tcgaTumorData)$Cancer_Type,
        ")"
      )
    )
    selectInput(
      "tcgaTabDGEProject",
      "Select TCGA Project:",
      choices = c("", projects),
      selected = ""
    )
  })
  
  output$tcgaTabDGEColDataUI <- renderUI({
    req(has_private_data)
    req(tcgaTumorData)
    
    selectInput(
      "tcgaTabDGEColData",
      "Select factorial variable:",
      choices = c("", colnames(colData(tcgaTumorData))),
      selected = ""
    )
  })
  
  DGEres <- reactiveVal(NULL)
  
  output$tcgaTabDGESubtypeUI <- renderUI({
    req(has_private_data)
    req(tcgaTumorData, input$tcgaTabProject, input$tcgaTabGroup)
    
    tcgaProject <- sub(".*\\((.*)\\).*", "\\1", input$tcgaTabProject)
    indx <- which(colData(tcgaTumorData)$Cancer_Type == tcgaProject)
    projectColData <- colData(tcgaTumorData)[indx, ]
    
    selectInput(
      "tcgaTabDGESubtype",
      "1. Subset by Subgroup:",
      choices = c("All Samples", unique(projectColData[[input$tcgaTabGroup]])),
      selected = "All Samples"
    )
  })
  
  output$tcgaTabDGEMethodUI <- renderUI({
    req(has_private_data)
    req(tcgaTumorTabGD2(), input$tcgaTabDGEStratMethodSel)
    
    if (input$tcgaTabDGEStratMethodSel == "t") {
      numericInput(
        "tcgaTabDGEMethod",
        "GD2 threshold for Stratification:",
        value = 0,
        step = 0.5
      )
    } else if (input$tcgaTabDGEStratMethodSel == "q") {
      sliderInput(
        "tcgaTabDGEMethod",
        "Upper & Lower Percentiles",
        min = 0,
        max = 1,
        value = c(0.3, 0.7)
      )
    }
  })
  
  ## Sample number UI -----
  output$ProjectSampleNr <- renderValueBox({
    req(has_private_data)
    req(input$tcgaTabProject, tcgaTumorData)
    
    tcgaProject <- sub(".*\\((.*)\\).*", "\\1", input$tcgaTabProject)
    indx <- which(colData(tcgaTumorData)$Cancer_Type == tcgaProject)
    value <- length(indx)
    
    bs4ValueBox(
      value = h3(value),
      paste0("# ", input$tcgaTabProject, " Samples"),
      icon = icon("list-ul"),
      color = "primary"
    )
  })
  output$SubgroupSampleNr <- renderValueBox({
    req(has_private_data)
    req(tcgaTumorData,
        input$tcgaTabProject,
        input$tcgaTabDGESubtype)
    
    tcgaProject <- sub(".*\\((.*)\\).*", "\\1", input$tcgaTabProject)
    indx <- which(colData(tcgaTumorData)$Cancer_Type == tcgaProject)
    colDataSub <- colData(tcgaTumorData)[indx, ]
    
    if (input$tcgaTabDGESubtype == "All Samples") {
      value <- nrow(colDataSub)
    } else {
      colDataSubIndx <- which(colDataSub[, input$tcgaTabGroup] == input$tcgaTabDGESubtype)
      value <- length(colDataSubIndx)
    }
    
    bs4ValueBox(
      value = h3(value),
      paste0("# ", input$tcgaTabDGESubtype, " Samples"),
      icon = icon("list-check"),
      color = "info"
    )
  })
  output$GD2HighSampleNr <- renderValueBox({
    req(has_private_data)
    req(
      input$tcgaTabDGESubtype,
      tcgaTumorData,
      tcgaTumorTabGD2(),
      input$tcgaTabDGEStratMethodSel
    )
    
    tcgaProject <- sub(".*\\((.*)\\).*", "\\1", input$tcgaTabProject)
    indx <- which(colData(tcgaTumorData)$Cancer_Type == tcgaProject)
    colDataSub <- colData(tcgaTumorData)[indx, ]
    
    if (input$tcgaTabDGESubtype != "All Samples") {
      indx2 <- which(colDataSub[[input$tcgaTabGroup]] == input$tcgaTabDGESubtype)
      colDataSub <- colDataSub[indx2, ]
    }
    
    # print(input$tcgaTabGroup)
    # print(input$tcgaTabDGESubtype)
    # colDataSub <- colDataSub[which(tcgaTumorData$coldata[,input$tcgaTabGroup] == input$tcgaTabDGESubtype),]
    # print(colDataSub)
    tcgaSubsetSamples <- rownames(colDataSub)
    # print(tcgaSubsetSamples)
    
    if (input$tcgaTabDGEStratMethodSel == "m") {
      threshold <- median(tcgaTumorTabGD2()$prediction[tcgaSubsetSamples])
    } else if (input$tcgaTabDGEStratMethodSel == "t") {
      threshold <- input$tcgaTabDGEMethod
    } else if (input$tcgaTabDGEStratMethodSel == "q") {
      threshold <- quantile(tcgaTumorTabGD2()$prediction[tcgaSubsetSamples], probs = input$tcgaTabDGEMethod)[2]
      
    }
    predSub <- tcgaTumorTabGD2()$prediction[tcgaSubsetSamples]
    value <- length(predSub[predSub >= threshold])
    
    bs4ValueBox(
      value = h3(value),
      "# GD2-High Samples",
      icon = icon("circle-up"),
      color = "success"
    )
  })
  output$GD2LowSampleNr <- renderValueBox({
    req(has_private_data)
    req(
      input$tcgaTabDGESubtype,
      tcgaTumorData,
      tcgaTumorTabGD2(),
      input$tcgaTabDGEStratMethodSel
    )
    
    tcgaProject <- sub(".*\\((.*)\\).*", "\\1", input$tcgaTabProject)
    indx <- which(colData(tcgaTumorData)$Cancer_Type == tcgaProject)
    colDataSub <- colData(tcgaTumorData)[indx, ]
    
    if (input$tcgaTabDGESubtype != "All Samples") {
      indx2 <- which(colDataSub[[input$tcgaTabGroup]] == input$tcgaTabDGESubtype)
      colDataSub <- colDataSub[indx2, ]
    }
    
    # print(input$tcgaTabGroup)
    # print(input$tcgaTabDGESubtype)
    # colDataSub <- colDataSub[which(tcgaTumorData$coldata[,input$tcgaTabGroup] == input$tcgaTabDGESubtype),]
    # print(colDataSub)
    tcgaSubsetSamples <- rownames(colDataSub)
    # print(tcgaSubsetSamples)
    
    if (input$tcgaTabDGEStratMethodSel == "m") {
      threshold <- median(tcgaTumorTabGD2()$prediction[tcgaSubsetSamples])
    } else if (input$tcgaTabDGEStratMethodSel == "t") {
      threshold <- input$tcgaTabDGEMethod
    } else if (input$tcgaTabDGEStratMethodSel == "q") {
      threshold <- quantile(tcgaTumorTabGD2()$prediction[tcgaSubsetSamples], probs = input$tcgaTabDGEMethod)[1]
    }
    predSub <- tcgaTumorTabGD2()$prediction[tcgaSubsetSamples]
    value <- length(predSub[predSub < threshold])
    
    bs4ValueBox(
      value = h3(value),
      "# GD2-Low Samples",
      icon = icon("circle-down"),
      color = "orange"
    )
  })
  
  ## Extract DGE results
  observeEvent(input$tcgaTabDGECompute, {
    req(has_private_data)
    req(
      input$tcgaTabProject,
      input$tcgaTabGroup,
      input$tcgaTabDGESubtype,
      tcgaTumorTabGD2(),
      input$tcgaTabDGEStratMethodSel,
      input$tcgaTabDGEtool
    )
    #########
    ### DESeq2 ----
    if(input$tcgaTabDGEtool==1){
    withProgress(message = "Computing the results...",
                 detail = "This step can take a little while",
                 value = 0,
                 {
                   tryCatch({
                     project <- sub(".*\\((.*)\\).*", "\\1", input$tcgaTabProject)
                     metadata <- input$tcgaTabGroup
                     subtype <- input$tcgaTabDGESubtype
                     incProgress(amount = 0.1, detail = "Loading data...")
                     TCGA_dds <- readRDS(
                       file.path(
                         data_path,
                         "TCGA_projects", 
                         paste0("TCGA-", project, ".Rds")
                         )
                       )
                     
                     # print(TCGA_dds)
                     
                     # Check sample number
                     if (subtype != "All Samples") {
                       TCGA_dds <- TCGA_dds[, which(colData(TCGA_dds)[[metadata]] == subtype)]
                       # print(metadata)
                       # print(subtype)
                       # print(TCGA_dds)
                       if (ncol(TCGA_dds) < 2) {
                         stop(
                           "Selected subgroup has less than two samples. Please select a different subgroup."
                         )
                       }
                     }
                     
                     common_samples <- intersect(names(tcgaTumorTabGD2()$prediction),
                                                 colnames(TCGA_dds))
                     prediction <- tcgaTumorTabGD2()$prediction[common_samples]
                     # print(prediction)
                     TCGA_dds <- TCGA_dds[, common_samples]
                     colData(TCGA_dds)$GD2Score <- prediction
                     
                     # Stratify by method
                     if (input$tcgaTabDGEStratMethodSel == "m") {
                       threshold <- median(colData(TCGA_dds)$GD2Score)
                       colData(TCGA_dds)$strat <- ifelse(colData(TCGA_dds)$GD2Score >= threshold,
                                                         "GD2_High",
                                                         "GD2_Low")
                     } else if (input$tcgaTabDGEStratMethodSel == "t") {
                       threshold <- input$tcgaTabDGEMethod
                       colData(TCGA_dds)$strat <- ifelse(colData(TCGA_dds)$GD2Score >= threshold,
                                                         "GD2_High",
                                                         "GD2_Low")
                     } else if (input$tcgaTabDGEStratMethodSel == "q") {
                       quantiles <- quantile(colData(TCGA_dds)$GD2Score, probs = input$tcgaTabDGEMethod)
                       low_threshold <- quantiles[1]
                       high_threshold <- quantiles[2]
                       
                       colData(TCGA_dds)$strat <- cut(
                         colData(TCGA_dds)$GD2Score,
                         breaks = c(-Inf, low_threshold, high_threshold, Inf),
                         labels = c("GD2_Low", "GD2_Medium", "GD2_High"),
                         right = FALSE
                       )
                     }
                     
                     colData(TCGA_dds)$strat <- as.factor(colData(TCGA_dds)$strat)
                     # print(colData(TCGA_dds)$strat)
                     # print(table(colData(TCGA_dds)$strat))
                     # Check if both GD2_High and GD2_Low groups have at least one sample
                     strat_table <- as.data.frame(table(colData(TCGA_dds)$strat))
                     if (!"GD2_Low" %in% strat_table[, 1] |
                         !"GD2_High" %in% strat_table[, 1]) {
                       stop(
                         "Stratification resulted in a group with no samples. Please adjust the stratification method or parameters."
                       )
                     }
                     
                     DESeq2::design(TCGA_dds) <- ~ strat
                     keep <- rowSums(DESeq2::counts(TCGA_dds)) >= 10 # TODO make interactive filtering params
                     TCGA_dds <- TCGA_dds[keep, ]
                     incProgress(amount = 0.2, detail = "Performing DESeq...")
                     
                     TCGA_dds <- DESeq2::DESeq(TCGA_dds)
                     
                     # Handling the experimental covariate correctly to extract the results...
                     
                     if (input$tcgaTabDGEWeight) {
                       res_obj <- results(
                         TCGA_dds,
                         contrast = c("strat", "GD2_High", "GD2_Low"),
                         independentFiltering = input$tcgaTabDGEFiltering,
                         alpha = input$tcgaTabDGEFDR,
                         filterFun = ihw
                       )
                       
                       if (input$tcgaTabDGEShrink) {
                         incProgress(amount = 0.3, detail = "Results extracted. Shrinking the logFC now...")
                         res_obj <- DESeq2::lfcShrink(
                           TCGA_dds,
                           contrast = c("strat", "GD2_High", "GD2_Low"),
                           res = res_obj,
                           type = "normal"
                         )
                         
                         incProgress(amount = 0.8, detail = "logFC shrunken, adding annotation info...")
                       } else {
                         incProgress(amount = 0.9, detail = "logFC left unshrunken, adding annotation info...")
                       }
                     } else {
                       res_obj <- results(
                         TCGA_dds,
                         contrast = c("strat", "GD2_High", "GD2_Low"),
                         independentFiltering = input$tcgaTabDGEFiltering,
                         alpha = input$tcgaTabDGEFDR
                       )
                       if (input$tcgaTabDGEShrink) {
                         incProgress(amount = 0.3, detail = "Results extracted. Shrinking the logFC now...")
                         res_obj <- DESeq2::lfcShrink(
                           TCGA_dds,
                           contrast = c("strat", "GD2_High", "GD2_Low"),
                           res = res_obj,
                           type = "normal"
                         )
                         incProgress(amount = 0.8, detail = "logFC shrunken, adding annotation info...")
                       } else {
                         incProgress(amount = 0.9, detail = "logFC left unshrunken, adding annotation info...")
                       }
                     }
                     res_obj$symbol <- rownames(res_obj)
                     DGEres(list(dds = TCGA_dds, res = res_obj))
                     
                   }, error = function(e) {
                     showModal(modalDialog(title = "Error", paste("An error occurred:", e$message)))
                   })
                 })
    } else if(input$tcgaTabDGEtool==2){
    #########
    ### limma-voom ----
      withProgress(message = "Computing the results (limma-voom)...",
                   detail = "This step can take a little while",
                   value = 0,
                   {
                     tryCatch({
                       project <- sub(".*\\((.*)\\).*", "\\1", input$tcgaTabProject)
                       metadata <- input$tcgaTabGroup
                       subtype <- input$tcgaTabDGESubtype
                       incProgress(amount = 0.1, detail = "Loading data...")
                       TCGA_dds <- readRDS(
                         file.path(
                           data_path,
                           "TCGA_projects", 
                           paste0("TCGA-", project, ".Rds")
                         )
                       )
                       
                       if (subtype != "All Samples") {
                         TCGA_dds <- TCGA_dds[, which(colData(TCGA_dds)[[metadata]] == subtype)]
                         if (ncol(TCGA_dds) < 2) {
                           stop("Selected subgroup has less than two samples. Please select a different subgroup.")
                         }
                       }
                       
                       common_samples <- intersect(names(tcgaTumorTabGD2()$prediction),
                                                   colnames(TCGA_dds))
                       prediction <- tcgaTumorTabGD2()$prediction[common_samples]
                       TCGA_dds <- TCGA_dds[, common_samples]
                       colData(TCGA_dds)$GD2Score <- prediction
                       
                       if (input$tcgaTabDGEStratMethodSel == "m") {
                         threshold <- median(colData(TCGA_dds)$GD2Score)
                         colData(TCGA_dds)$strat <- ifelse(colData(TCGA_dds)$GD2Score >= threshold, "GD2_High", "GD2_Low")
                       } else if (input$tcgaTabDGEStratMethodSel == "t") {
                         threshold <- input$tcgaTabDGEMethod
                         colData(TCGA_dds)$strat <- ifelse(colData(TCGA_dds)$GD2Score >= threshold, "GD2_High", "GD2_Low")
                       } else if (input$tcgaTabDGEStratMethodSel == "q") {
                         quantiles <- quantile(colData(TCGA_dds)$GD2Score, probs = input$tcgaTabDGEMethod)
                         low_threshold <- quantiles[1]
                         high_threshold <- quantiles[2]
                         colData(TCGA_dds)$strat <- cut(
                           colData(TCGA_dds)$GD2Score,
                           breaks = c(-Inf, low_threshold, high_threshold, Inf),
                           labels = c("GD2_Low", "GD2_Medium", "GD2_High"),
                           right = FALSE
                         )
                       }
                       
                       colData(TCGA_dds)$strat <- as.factor(colData(TCGA_dds)$strat)
                       strat_table <- as.data.frame(table(colData(TCGA_dds)$strat))
                       if (!"GD2_Low" %in% strat_table[, 1] | !"GD2_High" %in% strat_table[, 1]) {
                         stop("Stratification resulted in a group with no samples. Please adjust the stratification method or parameters.")
                       }
                       
                       # Filter to binary strat groups
                       incProgress(amount = 0.2, detail = "Preparing design matrix...")
                       TCGA_dds <- TCGA_dds[, colData(TCGA_dds)$strat %in% c("GD2_Low", "GD2_High")]
                       colData(TCGA_dds)$strat <- droplevels(colData(TCGA_dds)$strat)
                       colData(TCGA_dds)$strat <- relevel(colData(TCGA_dds)$strat, ref = "GD2_Low")
                       
                       strat <- colData(TCGA_dds)$strat
                       design <- model.matrix(~ strat)
                       
                       keep <- rowSums(assay(TCGA_dds)) >= 10
                       TCGA_dds <- TCGA_dds[keep, ]
                       
                       incProgress(amount = 0.3, detail = "Running voom + limma...")
                       
                       # Choose voom or voomWithQualityWeights
                       if (isTRUE(input$tcgaTabLimmaQualityWeights)) {
                         v <- limma::voomWithQualityWeights(assay(TCGA_dds), design, plot = FALSE)
                       } else {
                         v <- limma::voom(assay(TCGA_dds), design, plot = FALSE)
                       }
                       
                       fit <- limma::lmFit(v, design)
                       
                       # Apply treat or standard eBayes
                       if (isTRUE(input$tcgaTabLimmaUseTreat)) {
                         fit <- limma::treat(fit, lfc = input$tcgaTabLimmaTreatLFC)
                       } else {
                         fit <- limma::eBayes(fit, robust = isTRUE(input$tcgaTabLimmaRobust))
                       }
                       
                       contrast <- "stratGD2_High"
                       if (!contrast %in% colnames(fit$coefficients)) {
                         stop(paste("Contrast", contrast, "not found in model coefficients. Available:", paste(colnames(fit$coefficients), collapse = ", ")))
                       }
                       
                       res_obj <- limma::topTable(
                         fit,
                         coef = contrast,
                         number = Inf,
                         adjust.method = input$tcgaTabLimmaAdjustMethod,
                         sort.by = "none"
                       )
                       res_obj$symbol <- rownames(res_obj)
                       
                       raw_counts <- assay(TCGA_dds)
                       sf <- DESeq2::estimateSizeFactorsForMatrix(raw_counts)
                       norm_counts <- t(t(raw_counts) / sf)
                       res_obj$baseMean <- rowMeans(norm_counts)
                       
                       colnames(res_obj)[which(colnames(res_obj) == "logFC")] <- "log2FoldChange"
                       colnames(res_obj)[which(colnames(res_obj) == "P.Value")] <- "pvalue"
                       colnames(res_obj)[which(colnames(res_obj) == "adj.P.Val")] <- "padj"
                       
                       incProgress(amount = 0.7, detail = "Finishing up...")
                       
                       DGEres(list(dds = TCGA_dds, res = res_obj))
                       
                       output$tcgaTabDGEResultDownloadUI <- renderUI({
                         downloadButton(
                           "tcgaTabDGEResultDownload", 
                           "Download Results (CSV)",
                           icon = icon("download"),
                           style = "primary")
                       })
                       
                     }, error = function(e) {
                       showModal(modalDialog(title = "Error", paste("An error occurred:", e$message)))
                     })
                   })
    }
    
  })
  
  ## Download results
  output$tcgaTabDGEResultDownload <- downloadHandler(
    filename = function() {
      paste0("DGE_results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      res_obj <- DGEres()$res
      if (!is.null(res_obj)) {
        write.csv(res_obj, file, row.names = TRUE)
      }
    }
  )
  
  ## DEA Result -----
  output$diyres_summary <- renderPrint({
    req(has_private_data)
    req(DGEres(), input$tcgaTabDGEFDR)
    # summary(DGEres()$res, alpha = as.numeric(input$tcgaTabDGEFDR))
    DESeq2::summary(DGEres()$res)
  })
  ### Results table -----
  output$table_res <- DT::renderDataTable({
    req(has_private_data)
    req(DGEres())
    
    # Sort results by adjusted p-value
    mydf <- as.data.frame(DGEres()$res[order(DGEres()$res$padj), ])
    
    # Move 'symbol' column to the front
    mydf <- mydf[, c("symbol", setdiff(names(mydf), "symbol"))]
    
    # Round all numeric columns except the gene symbol
    is_num <- sapply(mydf, is.numeric)
    mydf[is_num] <- lapply(mydf[is_num], function(x) signif(x, 4))  # or use round(x, 3)
    
    # Make gene symbols into links
    mydf$symbol <- createLinkGeneSymbol(mydf$symbol)
    
    # Create datatable
    datatable(
      mydf,
      escape = FALSE,
      selection = 'single',
      options = list(
        scrollX = TRUE
      )
    )
  })
  ### p-Value histogram -----
  output$pvals_hist <- renderPlot({
    req(has_private_data)
    req(DGEres())
    
    res_df <- as.data.frame(DGEres()$res)
    res_df <- dplyr::filter(res_df, !is.na(pvalue))
    p <- ggplot(res_df, aes_string("pvalue")) +
      geom_histogram(binwidth = 0.01, boundary = 0) +
      theme_bw()
    
    # for visual estimation of the false discovery proportion in the first bin
    alpha <- binw <- input$tcgaTabDGEFDR
    pi0 <- 2 * mean(res_df$pvalue > 0.5)
    p <- p + geom_hline(yintercept = pi0 * binw * nrow(res_df),
                        col = "steelblue") +
      geom_vline(xintercept = alpha, col = "red")
    
    p <- p + ggtitle(
      label = "p-value histogram",
      subtitle = paste0(
        "Expected nulls = ",
        pi0 * binw * nrow(res_df),
        " - #elements in the selected bins = ",
        sum(res_df$pvalue < alpha)
      )
    )
    
    p
  })
  ### log2FC histogram -----
  output$logfc_hist <- renderPlot({
    req(has_private_data)
    req(DGEres())
    
    res_df <- as.data.frame(DGEres()$res)
    res_df <- dplyr::filter(res_df, !is.na(pvalue))
    
    p <- ggplot(res_df, aes_string("log2FoldChange")) +
      geom_histogram(binwidth = 0.1) +
      theme_bw()
    
    p <- p + ggtitle("Histogram of the log2 fold changes")
    
    p
  })
  ### MA-plot -----
  output$plotma <- renderPlot({
    req(has_private_data)
    req(DGEres())
    
    p <- plot_ma(DGEres()$res,
                        annotation_obj = NULL,
                        FDR = input$tcgaTabDGEFDR)
    
    p
  })
  ### Volcano-plot -----
  output$volcanoplot <- renderPlotly({
    req(has_private_data)
    req(DGEres())
    
    # p <- plot_volcano(DGEres()$res, FDR = input$tcgaTabDGEFDR)
    
    # ggplotly(p)
    
    res_df <- DGEres()$res
    if(any(is.na(res_df$padj))){
      res_df <- res_df[-which(is.na(res_df$padj)), ]
    }
    
    # print(res_df)
    # add a column of NAs
    res_df$diffexpressed <- "NO"
    # if log2Foldchange > 0.6 and pvalue < 0.05, set as "UP"
    res_df$diffexpressed[res_df$log2FoldChange > 0.6 &
                           res_df$padj < 0.05] <- "UP"
    # if log2Foldchange < -0.6 and pvalue < 0.05, set as "DOWN"
    res_df$diffexpressed[res_df$log2FoldChange < -0.6 &
                           res_df$padj < 0.05] <- "DOWN"
    
    res_df$delabel <- rownames(res_df)
    # res_df$delabel[res_df$diffexpressed != "NO"] <- rownames(res_df)[res_df$diffexpressed != "NO"]
    
    
    
    mycolors <- c("#164863", "#ff851b", "black")
    names(mycolors) <- c("DOWN", "UP", "NO")
    
    # library(ggrepel)
    p <- ggplot(data = res_df,
                aes(
                  x = log2FoldChange,
                  y = -log10(pvalue),
                  col = diffexpressed,
                  label = symbol
                )) +
      geom_point() +
      theme_minimal() +
      # geom_text_repel() +
      scale_color_manual(values = c("#164863", "black", "#ff851b")) +
      geom_vline(xintercept = c(-0.6, 0.6), col = "red") +
      geom_hline(yintercept = -log10(input$tcgaTabDGEFDR),
                 col = "red")
    plotly::ggplotly(p)
  })
  ### Gene-plot -----
  output$genefinder_plot <- renderPlotly({
    req(has_private_data)
    req(DGEres())
    shiny::validate(need(
      length(input$table_res_row_last_clicked) > 0,
      "Select a Gene in the 'DEA Genes' Table"
    ))
    
    selectedGene <- rownames(DGEres()$res)[input$table_res_row_last_clicked]
    # selectedGeneSymbol <- values$annotation_obj$gene_name[match(selectedGene, values$annotation_obj$gene_id)]
    
    if (input$genefinder_plotType == "box") {
      p <- ggplotCounts(DGEres()$dds,
                        selectedGene,
                        intgroup = "strat",
                        annotation_obj = NULL)
      
      # if (input$ylimZero_genes) {
      #   p <- p + ylim(0.1, NA)
      # }
      
      ggplotly(p)
      
    } else if (input$genefinder_plotType == "scatter") {
      df <- data.frame(
        gene = counts(DGEres()$dds)[selectedGene, ],
        gd2 = colData(DGEres()$dds)$GD2Score,
        intgroup = colData(DGEres()$dds)$strat
      )
      
      if (length(unique(df$intgroup)) == 2) {
        colors <- c("GD2_High" = "#164863",
                    "GD2_Low" = "#ff851b")
      } else if (length(unique(df$intgroup)) == 3) {
        colors <- c(
          "GD2_High" = "#164863",
          "GD2_Low" = "#ff851b",
          "GD2_Medium" = "darkgreen"
        )
      } else {
        colors <- NULL
      }
      # print(df)
      p <- plotly::plot_ly(
        df,
        x =  ~ gd2,
        y =  ~ gene,
        color =  ~ as.character(intgroup),
        colors = colors,
        type = "scatter",
        mode = "markers",
        text = rownames(df)
      ) %>%
        layout(
          xaxis = list(title = 'GD2 Score'),
          yaxis = list(
            type = 'log',
            title = paste0("Normalized counts (log10 scale ", selectedGene, ")")
          )
        )
      p
    }
  })
  ### Gene Info box -----
  output$rentrez_infobox <- renderUI({
    req(has_private_data)
    req(DGEres())
    shiny::validate(
      need(
        (length(input$table_res_row_last_clicked) > 0),
        "Select a gene in the 'DEA Genes' Table to display additional info (retrieved from the NCBI/ENTREZ db website)"
      )
    )
    
    selectedGene <- rownames(DGEres()$res)[input$table_res_row_last_clicked]
    
    tryCatch({
      selgene_entrez <- AnnotationDbi::mapIds(
        org.Hs.eg.db,
        keys = selectedGene,
        column = "ENTREZID",
        keytype = "SYMBOL"
      )
      fullinfo <- geneinfo(selgene_entrez)
      
      link_pubmed <- paste0(
        '<a href="http://www.ncbi.nlm.nih.gov/gene/?term=',
        selgene_entrez,
        '" target="_blank" >Click here to see more at NCBI</a>'
      )
      
      if (fullinfo$summary == "") {
        return(HTML(
          paste0(
            "<b>",
            fullinfo$name,
            "</b><br/><br/>",
            fullinfo$description,
            "<br/><br/>",
            link_pubmed
          )
        ))
      } else {
        return(HTML(
          paste0(
            "<b>",
            fullinfo$name,
            "</b><br/><br/>",
            fullinfo$description,
            "<br/><br/>",
            fullinfo$summary,
            "<br/><br/>",
            link_pubmed
          )
        ))
      }
    }, error = function(cond) {
      message("No Entrez ID found for selected gene.")
    })
  })
  
  # Analyze your dataset tab ----------------
  
  customData <- reactiveVal(NULL)
  customGD2 <- reactiveVal(NULL)
  customRASMessages <- reactiveVal(NULL)
  customLFCData <- reactiveVal(NULL)
  
  ## Set variables -----
  observeEvent(input$metadataFile, {
    customData(NULL)
    customGD2(NULL)
    customRASMessages(NULL)
    customLFCData(NULL)
  })
  
  observeEvent(input$countsFile, {
    customData(NULL)
    customGD2(NULL)
    customRASMessages(NULL)
    customLFCData(NULL)
  })
  
  observeEvent(input$ddsFile, {
    customData(NULL)
    customGD2(NULL)
    customRASMessages(NULL)
    customLFCData(NULL)
  })
  
  ## Compute RAS of user data -----
  observeEvent(input$computeCustomRAS, {
    customDataVal <- NULL
    errorMessage <- NULL
    warningMessage <- NULL
    successMessage <- NULL
    
    if (input$dataType == "countMeta") {
      if (is.null(input$metadataFile) || is.null(input$countsFile)) {
        warningMessage <- "Please upload both metadata and counts files to compute RAS."
      }
    } else if (input$dataType == "dds") {
      if (is.null(input$ddsFile)) {
        warningMessage <- "Please upload the DDS file to compute RAS."
      }
    }
    
    if (input$dataType == "countMeta" &&
        !is.null(input$metadataFile) && !is.null(input$countsFile)) {
      customColdata <- read.table(
        file = input$metadataFile$datapath,
        sep = "\t",
        header = TRUE
      )
      customCounts <- read.table(
        file = input$countsFile$datapath,
        sep = "\t",
        header = TRUE
      )
      tryCatch({
        customDataVal <- computeReactionActivityScores(
          counts = customCounts,
          metadata = customColdata,
          mgraph = mgraph,
          geom = metadata(trainData)$geom
        )
        successMessage <- "RAS computed successfully!"
      }, warning = function(w) {
        warningMessage <<- w$message
      }, error = function(e) {
        errorMessage <<- e$message
      })
    } else if (input$dataType == "dds" && !is.null(input$ddsFile)) {
      tryCatch({
        custom_dds <- readRDS(input$ddsFile$datapath)
        customDataVal <- computeReactionActivityScores(
          dds = custom_dds,
          mgraph = mgraph,
          geom = metadata(trainData)$geom
        )
        successMessage <- "RAS computed successfully!"
      }, warning = function(w) {
        warningMessage <<- w$message
      }, error = function(e) {
        errorMessage <<- e$message
      })
    }
    customData(customDataVal)
    customRASMessages(list(
      error = errorMessage,
      warning = warningMessage,
      success = successMessage
    ))
  })
  
  output$customRASmessageUI <- renderUI({
    req(input$computeCustomRAS)
    
    errorMessage <- customRASMessages()$error
    warningMessage <- customRASMessages()$warning
    successMessage <- customRASMessages()$success
    
    if (!is.null(errorMessage)) {
      return(HTML(
        paste0(
          '<div style="color: red; display: flex; align-items: center;">',
          as.character(
            shiny::icon(
              "exclamation-triangle",
              class = "fa-lg",
              style = "margin-right: 8px;"
            )
          ),
          '<p style="margin: 0;">',
          errorMessage,
          '</p>',
          '</div>'
        )
      ))
    }
    
    if (!is.null(warningMessage)) {
      return(HTML(
        paste0(
          '<div style="color: orange; display: flex; align-items: center;">',
          as.character(
            shiny::icon(
              "exclamation-triangle",
              class = "fa-lg",
              style = "margin-right: 8px;"
            )
          ),
          '<p style="margin: 0;">',
          warningMessage,
          '</p>',
          '</div>'
        )
      ))
    }
    
    if (!is.null(successMessage)) {
      return(HTML(
        paste0(
          '<div style="display: flex; align-items: center; color: green;">',
          as.character(
            shiny::icon("check", class = "fa-lg", style = "margin-right: 8px; color: green;")
          ),
          '<p style="margin: 0;">',
          successMessage,
          '</p>',
          '</div>'
        )
      ))
    }
  })
  
  output$selectCustomGroupUI <- renderUI({
    if (!is.null(customData())) {
      selectInput("customColData", "Group by:", choices = c("none", names(colData(
        customData()$custom_dds
      ))))
    }
  })
  
  ## Train GD2 model & predict values -----
  observeEvent(input$computeCustomScore, {
    req(customData(), input$customScale)
    rasTypes <- c("ras", "ras_prob", "ras_prob_rec")
    modelList <- list()
    predList <- list()
    inDataList <- list()
    
    for (rasType in rasTypes) {
      model <- trainGD2model(
        train_data = trainData,
        adjust_ras = rasType,
        adjust_input = input$customScale
      )
      modelList[[rasType]] <- model
    }
    
    # print(class(modelList$ras))
    # print(modelList$ras)
    
    for (rasType in rasTypes) {
      pred <- computeGD2Score(
        RAS = customData()[[rasType]],
        svm_model = modelList[[rasType]],
        adjust_input = input$customScale,
        range_output = FALSE,
        center = TRUE
      )
      predList[[rasType]] <- pred$preds
      inDataList[[rasType]] <- pred$input_df
    }
    customGD2(list(predList = predList, inDataList = inDataList, modelList = modelList))
  })
  
  output$customGD2messageUI <- renderText({
    req(input$computeCustomScore)
    
    if (is.null(customGD2())) {
      return(HTML(
        paste0(
          '<div style="color: orange; display: flex; align-items: center;">',
          as.character(
            shiny::icon(
              "exclamation-triangle",
              class = "fa-lg",
              style = "margin-right: 8px;"
            )
          ),
          '<p>Please upload your data first.</p>',
          '</div>'
        )
      ))
    } else {
      ### TODO check predList()
      iconHtml <- as.character(shiny::icon("check", class = "fa-lg", style = "margin-right: 8px; color: green;"))
      HTML(
        paste0(
          '<div style="display: flex; align-items: center; color: green;">',
          iconHtml,
          '<p style="margin: 0;">Trained SVM model & predicted GD2 score successfully!</p>',
          '</div>'
        )
      )
    }
  })
  
  ## Download data -----
  
  # Render UI for downloadCustomRASUI
  output$downloadCustomRASUI <- renderUI({
    req(customData())
    downloadButton(
      "downloadCustomRAS",
      "Download RAS Data",
      icon = icon("download"),
      style = "primary"
    )
  })
  
  # Render UI for downloadCustomGD2UI
  output$downloadCustomGD2UI <- renderUI({
    req(customGD2(), customData())
    downloadButton(
      "downloadCustomGD2",
      "Download GD2 Data",
      icon = icon("download"),
      style = "primary"
    )
  })
  
  # Download handler for customData
  output$downloadCustomRAS <- downloadHandler(
    filename = function() {
      paste0("GD2Viz_RASData_", Sys.Date(), ".zip")
    },
    content = function(file) {
      temp_dir <- tempdir()
      file_paths <- c()
      custom_data <- customData()
      
      for (i in c("ras", "ras_prob", "ras_prob_path", "ras_prob_rec")) {
        tsv_path <- file.path(temp_dir, paste0("GD2VizRAS_", i, ".tsv"))
        write.table(
          custom_data[[i]],
          tsv_path,
          sep = "\t",
          row.names = TRUE,
          quote = FALSE
        )
        file_paths <- c(file_paths, normalizePath(tsv_path))
      }
      
      utils::zip(file, files = file_paths, flags = '-j')
    }
  )
  
  # Download handler for customGD2
  output$downloadCustomGD2 <- downloadHandler(
    filename = function() {
      paste0("GD2Viz_GD2Data_", Sys.Date(), ".zip")
    },
    content = function(file) {
      temp_dir <- tempdir()
      file_paths <- c()
      custom_gd2 <- customGD2()
      
      # TSV file
      GD2Score_df <- do.call(cbind.data.frame, custom_gd2$predList)
      colnames(GD2Score_df) <- c("ras", "ras_prob", "ras_prob_path", "ras_prob_rec")
      rownames(GD2Score_df) <- names(custom_gd2$predList[[1]])
      tsv_path <- file.path(temp_dir, "GD2Viz_GD2Scores.tsv")
      write.table(
        GD2Score_df,
        tsv_path,
        sep = "\t",
        row.names = TRUE,
        quote = FALSE
      )
      file_paths <- c(file_paths, normalizePath(tsv_path))
      
      # TODO add  descriptive model infos
      # TXT files for modelList descriptions and metadata
      # for (i in c("ras", "ras_prob", "ras_prob_path", "ras_prob_rec")) {
      #   txt_path <- file.path(temp_dir, paste0("GD2VizModel_", i, ".txt"))
      #   model_info <- capture.output(print(custom_gd2$modelList[[i]]))
      #   writeLines(model_info, txt_path)
      #   file_paths <- c(file_paths, normalizePath(txt_path))
      # }
      
      utils::zip(file, files = file_paths, flags = '-j')
    }
  )
  
  ## Heatmap of RAS -----
  output$customRASheatmap <- renderPlot({
    req(customData(), input$customColData)
    # print("heatmap plotted")
    if (input$customColData == "none") {
      col_side_colors <- NULL
      ann_col <- NULL
    } else {
      group <- colData(customData()$custom_dds)[, input$customColData]
      ann_col <- HeatmapAnnotation(df = data.frame(Group = group))
    }
    
    data <- customData()[[input$customRASType]]
    
    if (input$customRASheatmapCluster == "none") {
      cluster_rows <- FALSE
      cluster_columns <- FALSE
    } else if (input$customRASheatmapCluster == "row") {
      cluster_rows <- TRUE
      cluster_columns <- FALSE
    } else if (input$customRASheatmapCluster == "column") {
      cluster_rows <- FALSE
      cluster_columns <- TRUE
    } else if (input$customRASheatmapCluster == "both") {
      cluster_rows <- TRUE
      cluster_columns <- TRUE
    }
    
    if (input$customRASheatmapScale == "row") {
      data <- t(scale(t(data)))
    } else if (input$customRASheatmapScale == "column") {
      data <- scale(data)
    }
    
    clustering_distance_columns <- input$customRASheatmapDistMethod
    clustering_method_columns <- input$customRASheatmapHclustMethod
    
    clustering_distance_rows <- input$customRASheatmapDistMethod
    clustering_method_rows <- input$customRASheatmapHclustMethod
    
    # height <- input$customRASheatmapHeight
    # scale <- input$customRASheatmapScale
    # dendrogram <- input$customRASheatmapDendrogram
    # dist_method <- input$customRASheatmapDistMethod
    # hclust_method <- input$customRASheatmapHclustMethod
    show_colnames <- as.logical(input$customRASheatmapColnames)
    show_rownames <- as.logical(input$customRASheatmapRownames)
    
    viz_mgraph <- visNetwork::toVisNetworkData(mgraph, idToLabel = FALSE)
    viz_mgraph$edges$miriam.kegg.reaction <- sapply(viz_mgraph$edges$miriam.kegg.reaction, function(x)
      paste(x, collapse = ","))
    viz_mgraph$edges$to <- sapply(viz_mgraph$edges$to, function(x)
      paste(x, collapse = ","))
    viz_mgraph$edges$from <- sapply(viz_mgraph$edges$from, function(x)
      paste(x, collapse = ","))
    viz_mgraph$nodes$id <- sapply(viz_mgraph$nodes$id, function(x)
      paste(x, collapse = ","))
    
    from_label <- c()
    for (i in viz_mgraph$edges$from) {
      from_label <- c(from_label, viz_mgraph$nodes[which(viz_mgraph$nodes$id == i), "label"])
    }
    to_label <- c()
    for (i in viz_mgraph$edges$to) {
      to_label <- c(to_label, viz_mgraph$nodes[which(viz_mgraph$nodes$id == i), "label"])
    }
    
    heatmap_rowlabels_df <- data.frame(
      reactions = viz_mgraph$edges$miriam.kegg.reaction,
      labels = paste0(
        viz_mgraph$edges$miriam.kegg.reaction,
        " (from: ",
        from_label,
        " - to: ",
        to_label,
        ")"
      )
    )
    heatmap_rowlabels_df <- heatmap_rowlabels_df[-which(
      heatmap_rowlabels_df$labels == "R01281 (from: Palmitoyl-CoA - to: 3-Dehydrosphinganine)"
    ), ]
    heatmap_rowlabels_df <- heatmap_rowlabels_df[which(heatmap_rowlabels_df$reactions %in% rownames(data)), ]
    
    if (input$customRASheatmapRownamesFull) {
      row_labels <- rownames(data)
    } else {
      row_labels <- heatmap_rowlabels_df$labels
    }
    
    f1 = colorRamp2(seq(min(data), max(data), length = 3), c("#164863", "#EEEEEE", "#ff851b"))
    
    
    # Create the heatmap
    heatmap <- Heatmap(
      data,
      col = f1,
      name = paste0("Heatmap of ", input$customRASType),
      row_names_side = "right",
      row_names_max_width = unit(20, "cm"),
      row_title = "Reactions",
      column_title = "Samples",
      cluster_rows = cluster_rows,
      cluster_columns = cluster_columns,
      clustering_distance_columns = clustering_distance_columns,
      clustering_method_columns = clustering_method_columns,
      clustering_distance_rows = clustering_distance_rows,
      clustering_method_rows = clustering_method_rows,
      show_row_names = show_rownames,
      show_column_names = show_colnames,
      # height = unit(0.5, "cm")*nrow(data),
      top_annotation = ann_col,
      row_labels = row_labels
    )
    draw(heatmap)
    
  })
  
  add_plot_maximize_observer(input,
                             "customRASheatmapBox",
                             "customRASheatmap",
                             non_max_height = "400px")
  
  ## Scatterplot of input reactions vs. output reactions -----
  output$customInOutplot <- renderPlotly({
    req(customGD2(), customData())
    df <- customGD2()$inDataList[[input$customRASType]]
    model <- customGD2()$modelList[[input$customRASType]]
    
    if (input$customColData == "none") {
      group <- as.factor(rep(1, nrow(colData(
        customData()$custom_dds
      ))))
    } else {
      group <- colData(customData()$custom_dds)[, input$customColData]
    }
    
    N <- length(unique(group))
    
    if (N == 1) {
      colors <- c("#164863")
    } else if(N == 2) {
      c("#164863", "#ff851b")
    } else {
      colors <- brewer.pal(N, "Set2")
    }

    # Define grid over the plotting domain
    x_seq <- seq(0, 12, length.out = 100)  # promoting axis
    y_seq <- seq(0, 4, length.out = 100)  # diminishing axis
    
    # Create grid of points
    grid <- expand.grid(x = y_seq, y = x_seq)
    
    # Predict decision values at each point in the grid
    z_vals <- predict(model, grid, type = "decision")
    
    # Turn z_vals into matrix matching grid layout
    z_matrix <- matrix(z_vals, nrow = 100, byrow = TRUE)
    
    # Plot the contour
    fig <- plot_ly(
      x = y_seq,
      y = x_seq,
      z = z_matrix,
      type = "contour",
      contours = list(
        coloring = 'lines',
        showlabels = TRUE
      )
    ) %>%
      add_trace(
        data = df,
        x = ~x,
        y = ~y,
        type = 'scatter',
        mode = 'markers',
        text = rownames(df),
        color = group,
        colors = colors,
        inherit = FALSE
      ) %>%
      layout(
        xaxis = list(title = 'Sum( GD2-Promoting Reactions )'),
        yaxis = list(title = 'Sum( GD2-Diminishing Reactions )')
      )
    fig
    
  })
  
  ## Scatterplot of the GD2 Score -----
  output$customGD2ScorePlotTGeneUI <- renderUI({
    data <- customData()$custom_dds
    if (!is.null(data)) {
      selectizeInput(
        "customGD2ScorePlotTGene",
        "Plot Gene vs. GD2Score (Only when Plot Type is 'scatter')",
        # choices = c("none", rownames(customData()$custom_dds)),
        choices = NULL,
        selected = NULL
      )
    }
  })
  
  observe({
    data <- customData()$custom_dds
    if (!is.null(data)) {
      updateSelectizeInput(
        session,
        "customGD2ScorePlotTGene",
        choices = c("none", rownames(data)),
        server = TRUE
      )
    }
  })
  
  
  output$customGD2Score <- renderPlotly({
    req(customGD2(), customData(), input$customGD2ScorePlotTGene)
    
    if (input$customColData == "none") {
      group <- as.factor(rep(1, nrow(colData(
        customData()$custom_dds
      ))))
      meandf_custom <- NULL
      group_title <- ""
      colors <- "#164863"
    } else {
      group <- colData(customData()$custom_dds)[, input$customColData]
      meandf_custom <- aggregate(customGD2()$predList[[input$customRASType]], list(group), FUN = mean)
      group_title <- input$customColData
      if (length(unique(group)) == 2) {
        colors <- c("#164863", "#ff851b")
      } else {
        colors <- NULL
      }
    }
    
    pred_values <- customGD2()$predList[[input$customRASType]]
    if (input$customGD2ScoreRange == "yes") {
      pred_values <- range01(pred_values)
    }
    
    plot_df <- data.frame(Group = group, Score = pred_values)
    
    if (is.null(meandf_custom)) {
      Group.1 <- NULL
    } else {
      Group.1 <- meandf_custom[order(meandf_custom$x), "Group.1"]
    }
    
    # Determine plot type
    plot_type <- input$customGD2ScorePlotType
    gene <- input$customGD2ScorePlotTGene
    
    if (plot_type == "scatter") {
      if (gene != "none") {
        fig <- plot_ly(
          data = plot_df,
          x = ~ Score,
          y = log2(DESeq2::counts(customData()$custom_dds)[gene, ] + 1),
          type = "scatter",
          mode = "markers",
          text = rownames(plot_df),
          color = ~ as.character(Group),
          colors = colors
        )
        yaxisls <- list(title = paste0("log2( ", gene, " )"))
        xaxisls = list(title = 'GD2 Score')
      } else {
        fig <- plot_ly(
          data = plot_df,
          x = ~ Score,
          y = ~ as.character(Group),
          type = "scatter",
          mode = "markers",
          text = rownames(plot_df),
          color = ~ as.character(Group),
          colors = colors
        )
        yaxisls <- list(
          categoryorder = "array",
          categoryarray = Group.1,
          title = group_title,
          showticklabels = TRUE
        )
        xaxisls = list(title = 'GD2 Score')
      }
    } else if (plot_type == "box") {
      fig <- plot_ly(
        data = plot_df,
        x = ~ as.character(Group),
        y = ~ Score,
        type = "box",
        text = rownames(plot_df),
        color = ~ as.character(Group),
        colors = colors,
        boxpoints = "all",
        jitter = 0.2
      )
      xaxisls <- list(
        categoryorder = "array",
        categoryarray = Group.1,
        title = group_title,
        showticklabels = TRUE
      )
      yaxisls = list(title = 'GD2 Score')
    } else if (plot_type == "violin") {
      fig <- plot_ly(
        data = plot_df,
        x = ~ as.character(Group),
        y = ~ Score,
        type = "violin",
        text = rownames(plot_df),
        color = ~ as.character(Group),
        colors = colors,
        box = list(visible = TRUE),
        meanline = list(visible = TRUE)
      )
      xaxisls <- list(
        categoryorder = "array",
        categoryarray = Group.1,
        title = group_title,
        showticklabels = TRUE
      )
      yaxisls = list(title = 'GD2 Score')
    }
    
    fig <- fig %>%
      layout(yaxis = yaxisls, xaxis = xaxisls)
    
    fig
  })
  
  ## Stemness Score vs GD2 Score ----
  output$customGD2Stemness <- renderPlotly({
    req(customGD2(),
        customData(),
        input$customGD2StemnessRange,
        stem)
    
    norm_counts <- DESeq2::counts(customData()$custom_dds, normalized =
                                    TRUE)
    norm_counts <- log2(norm_counts + 1)
    
    i_genes <- intersect(names(stem), rownames(norm_counts))
    X <- norm_counts[i_genes, , drop = FALSE]
    w <- stem[i_genes]
    
    if (input$customColData == "none") {
      group <- as.factor(rep(1, nrow(colData(
        customData()$custom_dds
      ))))
      meandf_custom <- NULL
      group_title <- ""
      colors <- "#164863"
    } else {
      group <- colData(customData()$custom_dds)[, input$customColData]
      meandf_custom <- aggregate(customGD2()$predList[[input$customRASType]], list(group), FUN = mean)
      group_title <- input$customColData
      if (length(unique(group)) == 2) {
        colors <- c("#164863", "#ff851b")
      } else {
        colors <- NULL
      }
    }
    
    s <- apply(X, 2, function(z) {
      cor(z, w, method = "sp", use = "complete.obs")
    })
    pred_values <- customGD2()$predList[[input$customRASType]]
    
    if (input$customGD2StemnessRange == "yes") {
      if (length(s) == 1) {
        warning("Ranging values is possible only if n > 1")
      } else {
        s <- range01(s)
        pred_values <- range01(pred_values)
      }
    }
    
    plot_df <- data.frame(Group = group,
                          GD2Score = pred_values,
                          Stemness = s)
    
    fig <- plot_ly(
      data = plot_df,
      x = ~ GD2Score,
      y = ~ Stemness,
      type = "scatter",
      mode = "markers",
      text = rownames(plot_df),
      color = ~ as.character(Group),
      colors = colors
    )
    yaxisls <- list(title = "mRNAsi")
    xaxisls = list(title = 'GD2 Score')
    
    fig <- fig %>%
      layout(yaxis = yaxisls, xaxis = xaxisls)
    
    fig
  })
  
  ## Group selectors for comparison ----
  # Dynamically create group selector based on the columns of custom_dds
  output$groupSelectorUI <- renderUI({
    data <- customData()$custom_dds
    if (!is.null(data)) {
      selectInput("selectCompColData",
                  "Select Sample Data:",
                  choices = c("", names(colData(data))))
    }
  })
  
  # Dynamically create level selectors based on the selected group
  output$levelSelectorAUI <- renderUI({
    req(input$selectCompColData)
    data <- customData()$custom_dds
    if (!is.null(data)) {
      levels <- unique(data[[input$selectCompColData]])
      selectInput("selected_level_A",
                  "Select Group A:",
                  choices = levels,
                  selected = levels[1])
    }
  })
  
  output$levelSelectorBUI <- renderUI({
    req(input$selectCompColData)
    data <- customData()$custom_dds
    if (!is.null(data)) {
      levels <- unique(data[[input$selectCompColData]])
      selectInput("selected_level_B",
                  "Select Group B:",
                  choices = levels,
                  selected = levels[2])
    }
  })
  
  output$SelectorRASUI <- renderUI({
    req(input$selectCompColData)
    data <- customData()$custom_dds
    if (!is.null(data)) {
      selectInput(
        "customRASComp",
        "Visuaize RAS Type:",
        choices = list(
          "unadjusted RAS" = "ras",
          "RAS adj. by transision prob." = "ras_prob",
          #"RAS adj. by path-based transition probability" = "ras_prob_path",
          "RAS adj. by recurive transition probability" = "ras_prob_rec"
        )
      )
    }
  })
  
  output$generateComparisonButtonUI <- renderUI({
    req(input$selectCompColData,
        input$selected_level_A,
        input$selected_level_B)
    bs4Dash::actionButton("generateComparison", "Generate comparison graphs", status = "success")
  })
  
  observeEvent(input$generateComparison, {
    req(input$selectCompColData,
        input$selected_level_A,
        input$selected_level_B)
    dds <- customData()$custom_dds
    group_col <- input$selectCompColData
    
    # Check if the dds has only one sample
    if (ncol(dds) == 1) {
      showNotification(
        "Error: The dataset contains only one sample. Comparison statistics cannot be generated.",
        type = "error",
        duration = 10,
        closeButton = TRUE
      )
      return(customLFCData(NULL))
    }
    
    ras <- customData()[[input$customRASComp]]
    
    indxA <- which(colData(dds)[[input$selectCompColData]] == input$selected_level_A)
    indxB <- which(colData(dds)[[input$selectCompColData]] == input$selected_level_B)
    
    p.vals <- sapply(1:nrow(ras), function(x) {
      ks.test(ras[x, indxA], ras[x, indxB])$p.value
    })
    
    pval_df <- data.frame(
      reaction = rownames(ras),
      pval = round(p.vals, 5),
      padj = p.adjust(p.vals, method = "BH")
    )
    
    log2fc <- sapply(1:nrow(ras), function(x) {
      log2GrA <- log2(mean(ras[x, indxA]))
      log2GrB <- log2(mean(ras[x, indxB]))
      log2GrA - log2GrB
    })
    
    pval_df$log2fc <- log2fc
    # rownames(pval_df) <- gsub("| .*", "", pval_df$reaction)
    
    
    pval_df[[input$selected_level_A]] <- rowMeans(ras[, indxA])
    pval_df[[input$selected_level_B]] <- rowMeans(ras[, indxB])
    
    pval_df[[paste0("sd_", input$selected_level_A)]] <- sapply(1:nrow(ras), function(x) {
      sd(ras[x, indxA])
    })
    pval_df[[paste0("sd_", input$selected_level_B)]] <- sapply(1:nrow(ras), function(x) {
      sd(ras[x, indxB])
    })
    customLFCData(pval_df)
  })
  
  ## Download GD2 Comparison stats ----
  # Render UI for downloadCustomGD2CompUI
  output$downloadCustomGD2CompUI <- renderUI({
    req(customLFCData())
    downloadButton(
      "downloadCustomGD2Comp",
      "Download Group Comparison Statistics",
      icon = icon("download"),
      style = "primary"
    )
  })
  
  # Download handler for customData
  output$downloadCustomGD2Comp <- downloadHandler(
    filename = function() {
      paste0("GD2Viz_GroupComparisonData_", Sys.Date(), ".tsv")
    },
    content = function(file) {
      temp_dir <- tempdir()
      file_paths <- c()
      pval_df <- customLFCData()
      
      write.table(pval_df,
                  file,
                  sep = "\t",
                  row.names = TRUE,
                  quote = FALSE)
      
    }
  )
  
  ## RAS comparison graph -----
  output$customGroupCompGraph <- renderVisNetwork({
    req(
      customLFCData(),
      input$generateComparison,
      input$selectCompColData,
      input$selected_level_A,
      input$selected_level_B
    )
    pval_df <- customLFCData()
    
    # print(pval_df)
    viz_mgraph <- visNetwork::toVisNetworkData(mgraph, idToLabel = FALSE)
    # print(igraph::as_data_frame(mgraph, "both"))
    
    # Convert miriam.kegg.reaction column to character
    viz_mgraph$edges$miriam.kegg.reaction <- sapply(viz_mgraph$edges$miriam.kegg.reaction, function(x)
      paste(x, collapse = ","))
    viz_mgraph$edges$pathway <- sapply(viz_mgraph$edges$pathway, function(x)
      paste(x, collapse = ","))
    viz_mgraph$edges$to <- sapply(viz_mgraph$edges$to, function(x)
      paste(x, collapse = ","))
    viz_mgraph$edges$miriam.kegg.reaction <- sapply(viz_mgraph$edges$miriam.kegg.reaction, function(x)
      paste(x, collapse = ","))
    viz_mgraph$edges$symbol <- sapply(viz_mgraph$edges$symbol, function(x)
      paste(unique(x), collapse = ","))
    viz_mgraph$nodes$id <- sapply(viz_mgraph$nodes$id, function(x)
      paste(x, collapse = ","))
    
    # Merge the edge data with pval_df
    viz_mgraph$edges <- viz_mgraph$edges %>%
      left_join(pval_df, by = c("miriam.kegg.reaction" = "reaction"))
    
    # Define edge colors based on log2fc values
    viz_mgraph$edges <- viz_mgraph$edges %>%
      dplyr::mutate(
        color = case_when(
          padj > input$customRASGraphBoxPThreshold ~ "gray",
          log2fc > 0 ~ "red",
          log2fc <= 0 ~ "blue"
        )
      )
    # mutate(color = ifelse(log2fc > 0, "red", "blue"))
    
    # Define edge widths based on padj values
    # Normalize padj values to a suitable range for widths (e.g., 1 to 10)
    viz_mgraph$edges <- viz_mgraph$edges %>%
      dplyr::mutate(width = (1 - log2fc) * 10)
    
    # Extract unique to nodes and their pathways
    to_pathways <- viz_mgraph$edges %>%
      dplyr::select(to, pathway) %>%
      dplyr::distinct()
    
    viz_mgraph$edges$title <- paste0(
      "Reaction: ",
      viz_mgraph$edges$miriam.kegg.reaction,
      "<br>Involved genes: ",
      viz_mgraph$edges$symbol,
      "<br>Log2FC: ",
      round(viz_mgraph$edges$log2fc, 3),
      "<br>Pval:",
      round(viz_mgraph$edges$pval, 3),
      "<br>Padj:",
      round(viz_mgraph$edges$padj, 3)
    )
    
    # Merge pathway information into nodes data frame
    # viz_mgraph$nodes <- viz_mgraph$nodes %>%
    #   left_join(to_pathways, by = c("id" = "to"))
    viz_mgraph$nodes <- merge(
      viz_mgraph$nodes,
      to_pathways,
      by.x = "id",
      by. = "to",
      all.x = TRUE
    )
    # viz_mgraph$nodes[which(is.na(viz_mgraph$nodes$pathway)),"pathway"] <- "unknown"
    viz_mgraph$nodes <- viz_mgraph$nodes[-92, ]
    
    # print(viz_mgraph$nodes)
    # print(viz_mgraph$edges)
    
    visNetwork(
      nodes = viz_mgraph$nodes,
      edges = viz_mgraph$edges,
      main = "Glycosphingolipid Metabolism Pathway",
      footer = "Arrow Color & Width is the Log2 Fold-Change (log2fc) of 'Group A' - 'Group B' <br> Positive log2fc = red; Negative log2fc = blue; padj > p-Value Threshold = gray"
    ) %>%
      visEdges(arrows = list(to = list(enabled = TRUE))) %>%
      visNodes(color = list(
        background = "#164863",
        border = "#164863",
        highlight = "yellow"
      )) %>%
      visOptions(#highlightNearest = list(enabled = T, degree = 1, hover = T),
        nodesIdSelection = TRUE, selectedBy = "pathway") %>%
      visLayout(randomSeed = 1111)
    
    # mygraph <- createGraph(mgraph, pval_df)
    # print(igraph::as_data_frame(mygraph$graph, "both"))
    # plot(mygraph$graph, layout=mygraph$layout, vertex.label.color= "black", vertex.label.family= "Arial")
  })
  
  # add_plot_maximize_observer(input, "customRASGraphBox", "customGroupCompGraph", non_max_height = "530px")
  observeEvent(input$customRASGraphBox$maximized, {
    visRedraw(
      visNetworkProxy(
        "customGroupCompGraph",
        session = shiny::getDefaultReactiveDomain()
      )
    )
  })
  ## RAS comparison plot -----
  output$customGroupComp <- renderPlot({
    req(
      customLFCData(),
      input$generateComparison,
      input$selectCompColData,
      input$selected_level_A,
      input$selected_level_B
    )
    pval_df <- customLFCData()
    
    # print(pval_df)
    viz_mgraph <- visNetwork::toVisNetworkData(mgraph, idToLabel = FALSE)
    # print(igraph::as_data_frame(mgraph, "both"))
    
    # Convert miriam.kegg.reaction column to character
    viz_mgraph$edges$miriam.kegg.reaction <- sapply(viz_mgraph$edges$miriam.kegg.reaction, function(x)
      paste(x, collapse = ","))
    viz_mgraph$edges$pathway <- sapply(viz_mgraph$edges$pathway, function(x)
      paste(x, collapse = ","))
    viz_mgraph$edges$to <- sapply(viz_mgraph$edges$to, function(x)
      paste(x, collapse = ","))
    viz_mgraph$nodes$id <- sapply(viz_mgraph$nodes$id, function(x)
      paste(x, collapse = ","))
    
    # Merge the edge data with pval_df
    viz_mgraph$edges <- viz_mgraph$edges %>%
      left_join(pval_df, by = c("miriam.kegg.reaction" = "reaction"))
    
    e_df <- as.data.frame(viz_mgraph$edges)
    # print(e_df)
    # ggplot(e_df, aes(x=1, y=miriam.kegg.reaction)) +
    #   geom_point(aes(size=-log10(padj), color=log2fc)) +
    #   colorspace::scale_color_continuous_diverging(palette = "Blue-Red 3") +
    #   theme_classic()
    #
    #
    #
    #
    # ggplot(e_df, aes(x=as.character(to), y=as.character(from), fill=log2fc)) +
    #   geom_point(aes(size=-log10(padj), shape = 21, colour = "black")) +
    #   colorspace::scale_color_continuous_diverging(palette = "Blue-Red 3") +
    #   theme_classic() +
    #   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    #   xlab("from node") + ylab("to node")
    
    e_df_lg <- e_df %>%
      tidyr::pivot_longer(colnames(dplyr::select(
        e_df,
        c(input$selected_level_A, input$selected_level_B, "log2fc")
      )),
      names_to = "group",
      values_to = "mean")
    e_df_lg$group <- as.factor(e_df_lg$group)
    e_df_lg[which(e_df_lg$group %in% c(input$selected_level_A, input$selected_level_B)), "padj"] <- NA
    e_df_lg[which(e_df_lg$group %in% c("log2fc")), paste0("sd_", input$selected_level_A)] <- NA
    e_df_lg[which(e_df_lg$group %in% c("log2fc")), paste0("sd_", input$selected_level_B)] <- NA
    e_df_lg[which(e_df_lg$group == input$selected_level_B), paste0("sd_", input$selected_level_A)] <- NA
    e_df_lg[which(e_df_lg$group == input$selected_level_A), paste0("sd_", input$selected_level_B)] <- NA
    
    e_df_lg <- e_df_lg %>%
      dplyr::mutate(sd = coalesce(
        paste0("sd_", input$selected_level_A),
        paste0("sd_", input$selected_level_B)
      ))
    
    # ggplot(e_df_lg, aes(x=mean, y=miriam.kegg.reaction, group=group)) +
    #   geom_point(aes(color=-log10(padj))) +
    #   colorspace::scale_color_continuous_sequential(palette = "Reds 3") +
    #   facet_wrap(~group, ncol = 3) +
    #   theme_classic() +
    #   theme(panel.grid.major.y = element_line(color = "gray",
    #                                           size = 0.5,
    #                                           linetype = 1)) + xlab("")
    
    
    comps <- names(keggGet("hsa00604")[[1]]$COMPOUND)
    comps <- gsub("G", "", comps)
    comps[1] <- "C01290"
    comps <- c(comps, "C01190")
    
    e_df_lg <- e_df_lg[which(e_df_lg$from %in% comps |
                               e_df_lg$to %in% comps), ]
    e_df_lg$geneSymbol <- sapply(e_df_lg$symbol, function(x)
      paste0(unique(x), collapse = ' '))
    from_label <- c()
    for (i in e_df_lg$from) {
      from_label <- c(from_label, viz_mgraph$nodes[which(viz_mgraph$nodes$id == i), "label"])
    }
    e_df_lg$from_label <- from_label
    to_label <- c()
    for (i in e_df_lg$to) {
      to_label <- c(to_label, viz_mgraph$nodes[which(viz_mgraph$nodes$id == i), "label"])
    }
    e_df_lg$to_label <- to_label
    
    # ggplot(e_df_lg, aes(x=mean, y=paste0(miriam.kegg.reaction, " (", geneSymbol, ")"), group=group)) +
    
    p1 <- ggplot(e_df_lg, aes(
      x = mean,
      y = paste0(
        miriam.kegg.reaction,
        " (from: ",
        from_label,
        " - to: ",
        to_label,
        ")"
      ),
      group = group
    )) +
      geom_vline(
        xintercept = 0,
        colour = "lightgray",
        linetype = "solid"
      ) +
      # geom_errorbar(aes(xmin=mean - sd,xmax=mean + sd,width=0.2)) +
      geom_point(aes(color = -log10(padj))) +
      colorspace::scale_color_continuous_sequential(palette = "Reds 3") +
      facet_wrap( ~ group, ncol = 3, scales = "free_x") +
      theme_classic() +
      ylab("") + xlab("") +
      ggplot2::theme(
        panel.grid.major.y = element_line(
          color = "lightgray",
          linewidth = 0.5,
          linetype = 1
        ),
        axis.text = element_text(size = 15),
        # Increase axis text size
        strip.text = element_text(size = 15) # Increase facet label text size
      )
    p1
    
  })
  add_plot_maximize_observer(input,
                             "customGroupCompBox",
                             "customGroupComp",
                             non_max_height = "530px")
  
  
  # DEA ----
  ## Input parameter UIs ----
  
  customDGEres <- reactiveVal(NULL)
  
  output$customTabDGEExVarUI <- renderUI({
    req(customData())
    
    dds <- customData()$custom_dds
    
    selectInput(
      "customTabDGEExVar",
      "1. Select Experimental Variable:",
      choices = c("All Samples", colnames(colData(dds))),
      selected = "All Samples"
    )
  })
  output$customTabDGESubtypeUI <- renderUI({
    req(customData(), input$customTabDGEExVar)
    
    dds <- customData()$custom_dds
    
    if (input$customTabDGEExVar != "All Samples") {
      selectInput(
        "customTabDGESubtype",
        "2. Subset by Subgroup:",
        choices = c("", unique(as.character(colData(
          dds
        )[[input$customTabDGEExVar]]))),
        selected = ""
      )
    }
    
  })
  
  output$customTabDGEMethodUI <- renderUI({
    req(input$customTabDGEStratMethodSel)
    
    if (input$customTabDGEStratMethodSel == "t") {
      numericInput(
        "customTabDGEMethod",
        "GD2 threshold for Stratification:",
        value = 0,
        step = 0.5
      )
    } else if (input$customTabDGEStratMethodSel == "q") {
      sliderInput(
        "customTabDGEMethod",
        "Upper & Lower Percentiles",
        min = 0,
        max = 1,
        value = c(0.3, 0.7)
      )
    }
  })
  
  ## Sample number UI -----
  output$customProjectSampleNr <- renderValueBox({
    req(customData())
    
    dds <- customData()$custom_dds
    value <- nrow(colData(dds))
    
    bs4ValueBox(
      value = h3(value),
      paste0("# Uploaded Samples"),
      icon = icon("list-ul"),
      color = "primary"
    )
  })
  output$customSubgroupSampleNr <- renderValueBox({
    req(customData(), input$customTabDGEExVar)
    
    dds <- customData()$custom_dds
    
    if (input$customTabDGEExVar == "All Samples") {
      value <- nrow(colData(dds))
    } else {
      if (!is.null(input$customTabDGESubtype)) {
        if (input$customTabDGESubtype == "") {
          value <- 0
        } else {
          indx <- which(colData(dds)[[input$customTabDGEExVar]] == input$customTabDGESubtype)
          value <- length(indx)
        }
      } else {
        value <- 0
      }
    }
    
    bs4ValueBox(
      value = h3(value),
      paste0("# ", input$customTabDGESubtype, " Samples"),
      icon = icon("list-check"),
      color = "info"
    )
  })
  output$customGD2HighSampleNr <- renderValueBox({
    req(
      customData(),
      input$customTabDGEExVar,
      customGD2(),
      input$customTabDGEStratMethodSel
    )
    
    dds <- customData()$custom_dds
    
    if (input$customTabDGEExVar == "All Samples") {
      samples <- colnames(dds)
    } else {
      if (!is.null(input$customTabDGESubtype)) {
        if (input$customTabDGESubtype == "") {
          samples <- c()
        } else {
          indx <- which(colData(dds)[[input$customTabDGEExVar]] == input$customTabDGESubtype)
          samples <- colnames(dds[, indx])
        }
      } else {
        samples <- c()
      }
    }
    
    pred_values <- customGD2()$predList[[input$customRASTypeDGE]]
    
    if (input$customTabDGEStratMethodSel == "m") {
      threshold <- median(pred_values[samples])
    } else if (input$customTabDGEStratMethodSel == "t") {
      threshold <- input$customTabDGEMethod
    } else if (input$customTabDGEStratMethodSel == "q") {
      threshold <- quantile(pred_values[samples], probs = input$customTabDGEMethod)[2]
    }
    
    predSub <- pred_values[samples]
    value <- length(predSub[predSub >= threshold])
    
    bs4ValueBox(
      value = h3(value),
      "# GD2-High Samples",
      icon = icon("circle-up"),
      color = "success"
    )
  })
  output$customGD2LowSampleNr <- renderValueBox({
    req(
      customData(),
      input$customTabDGEExVar,
      customGD2(),
      input$customTabDGEStratMethodSel
    )
    
    dds <- customData()$custom_dds
    
    if (input$customTabDGEExVar == "All Samples") {
      samples <- colnames(dds)
    } else {
      if (!is.null(input$customTabDGESubtype)) {
        if (input$customTabDGESubtype == "") {
          samples <- c()
        } else {
          indx <- which(colData(dds)[[input$customTabDGEExVar]] == input$customTabDGESubtype)
          samples <- colnames(dds[, indx])
        }
      } else {
        samples <- c()
      }
    }
    
    pred_values <- customGD2()$predList[[input$customRASTypeDGE]]
    
    if (input$customTabDGEStratMethodSel == "m") {
      threshold <- median(pred_values[samples])
    } else if (input$customTabDGEStratMethodSel == "t") {
      threshold <- input$customTabDGEMethod
    } else if (input$customTabDGEStratMethodSel == "q") {
      threshold <- quantile(pred_values[samples], probs = input$customTabDGEMethod)[1]
    }
    
    predSub <- pred_values[samples]
    value <- length(predSub[predSub < threshold])
    
    bs4ValueBox(
      value = h3(value),
      "# GD2-Low Samples",
      icon = icon("circle-down"),
      color = "orange"
    )
  })
  
  ## Extract DGE results
  observeEvent(input$customTabDGECompute, {
    req(
      input$customTabDGEExVar,
      customGD2(),
      customData(),
      input$customTabDGEStratMethodSel
    )
    #########
    ### DESeq2 ----
    if(input$customTabDGEtool==1){
    withProgress(message = "Computing the results...",
                 detail = "This step can take a little while",
                 value = 0,
                 {
                   tryCatch({
                     metadata <- input$customTabDGEExVar
                     subtype <- input$customTabDGESubtype
                     incProgress(amount = 0.1, detail = "Loading data...")
                     dds <- customData()$custom_dds
                     
                     # Check sample number
                     if (metadata != "All Samples") {
                       dds <- dds[, which(colData(dds)[[metadata]] == subtype)]
                       if (ncol(dds) < 2) {
                         stop(
                           "Selected subgroup has less than two samples. Please select a different subgroup."
                         )
                       }
                     }
                     
                     prediction <- customGD2()$predList[[input$customRASTypeDGE]][colnames(dds)]
                     colData(dds)$GD2Score <- prediction
                     
                     # print(input$customTabDGEStratMethodSel)
                     # print(input$customTabDGEMethod)
                     #
                     # Stratify by method
                     if (input$customTabDGEStratMethodSel == "m") {
                       threshold <- median(colData(dds)$GD2Score)
                       colData(dds)$strat <- ifelse(colData(dds)$GD2Score >= threshold,
                                                    "GD2_High",
                                                    "GD2_Low")
                     } else if (input$customTabDGEStratMethodSel == "t") {
                       threshold <- input$customTabDGEMethod
                       colData(dds)$strat <- ifelse(colData(dds)$GD2Score >= threshold,
                                                    "GD2_High",
                                                    "GD2_Low")
                     } else if (input$customTabDGEStratMethodSel == "q") {
                       #print(as.numeric(colData(dds)$GD2Score))
                       quantiles <- quantile(as.numeric(colData(dds)$GD2Score),
                                             probs = c(0, input$customTabDGEMethod, 1))
                       # print(quantiles)
                       low_threshold <- quantiles[2]
                       high_threshold <- quantiles[3]
                       
                       colData(dds)$strat <- cut(
                         colData(dds)$GD2Score,
                         breaks = c(-Inf, low_threshold, high_threshold, Inf),
                         labels = c("GD2_Low", "GD2_Medium", "GD2_High"),
                         right = FALSE
                       )
                     }
                     
                     colData(dds)$strat <- as.factor(colData(dds)$strat)
                     
                     # Check if both GD2_High and GD2_Low groups have at least one sample
                     strat_table <- as.data.frame(table(colData(dds)$strat))
                     if (!"GD2_Low" %in% strat_table[, 1] |
                         !"GD2_High" %in% strat_table[, 1]) {
                       stop(
                         "Stratification resulted in a group with no samples. Please adjust the stratification method or parameters."
                       )
                     }
                     
                     DESeq2::design(dds) <- ~ strat
                     keep <- rowSums(DESeq2::counts(dds)) >= 10 # TODO make interactive filtering params
                     dds <- dds[keep, ]
                     incProgress(amount = 0.2, detail = "Performing DESeq...")
                     
                     dds <- DESeq2::DESeq(dds)
                     
                     # Handling the experimental covariate correctly to extract the results...
                     
                     if (input$customTabDGEWeight) {
                       res_obj <- results(
                         dds,
                         contrast = c("strat", "GD2_High", "GD2_Low"),
                         independentFiltering = input$customTabDGEFiltering,
                         alpha = input$customTabDGEFDR,
                         filterFun = ihw
                       )
                       
                       if (input$customTabDGEShrink) {
                         incProgress(amount = 0.3, detail = "Results extracted. Shrinking the logFC now...")
                         res_obj <- DESeq2::lfcShrink(
                           dds,
                           contrast = c("strat", "GD2_High", "GD2_Low"),
                           res = res_obj,
                           type = "normal"
                         )
                         
                         incProgress(amount = 0.8, detail = "logFC shrunken, adding annotation info...")
                       } else {
                         incProgress(amount = 0.9, detail = "logFC left unshrunken, adding annotation info...")
                       }
                     } else {
                       res_obj <- results(
                         dds,
                         contrast = c("strat", "GD2_High", "GD2_Low"),
                         independentFiltering = input$customTabDGEFiltering,
                         alpha = input$customTabDGEFDR
                       )
                       if (input$customTabDGEShrink) {
                         incProgress(amount = 0.3, detail = "Results extracted. Shrinking the logFC now...")
                         res_obj <- DESeq2::lfcShrink(
                           dds,
                           contrast = c("strat", "GD2_High", "GD2_Low"),
                           res = res_obj,
                           type = "normal"
                         )
                         incProgress(amount = 0.8, detail = "logFC shrunken, adding annotation info...")
                       } else {
                         incProgress(amount = 0.9, detail = "logFC left unshrunken, adding annotation info...")
                       }
                     }
                     res_obj$symbol <- rownames(res_obj)
                     customDGEres(list(dds = dds, res = res_obj))
                     
                   }, error = function(e) {
                     showModal(modalDialog(title = "Error", paste("An error occurred:", e$message)))
                   })
                 })
    } else if(input$tcgaTabDGEtool==2){
    #########
    ### limma-voom ----
    withProgress(message = "Computing the results (limma-voom)...",
                 detail = "This step can take a little while",
                 value = 0,
                 {
                   tryCatch({
                     metadata <- input$customTabDGEExVar
                     subtype <- input$customTabDGESubtype
                     incProgress(amount = 0.1, detail = "Loading data...")
                     dds <- customData()$custom_dds
                     # Check sample number
                     if (metadata != "All Samples") {
                       dds <- dds[, which(colData(dds)[[metadata]] == subtype)]
                       if (ncol(dds) < 2) {
                         stop(
                           "Selected subgroup has less than two samples. Please select a different subgroup."
                         )
                       }
                     }
                     prediction <- customGD2()$predList[[input$customRASTypeDGE]][colnames(dds)]
                     colData(dds)$GD2Score <- prediction
                     if (input$customTabDGEStratMethodSel == "m") {
                       threshold <- median(colData(dds)$GD2Score)
                       colData(dds)$strat <- ifelse(colData(dds)$GD2Score >= threshold,
                                                    "GD2_High",
                                                    "GD2_Low")
                     } else if (input$customTabDGEStratMethodSel == "t") {
                       threshold <- input$customTabDGEMethod
                       colData(dds)$strat <- ifelse(colData(dds)$GD2Score >= threshold,
                                                    "GD2_High",
                                                    "GD2_Low")
                     } else if (input$customTabDGEStratMethodSel == "q") {
                       quantiles <- quantile(as.numeric(colData(dds)$GD2Score),
                                             probs = c(0, input$customTabDGEMethod, 1))
                       # print(quantiles)
                       low_threshold <- quantiles[2]
                       high_threshold <- quantiles[3]
                       
                       colData(dds)$strat <- cut(
                         colData(dds)$GD2Score,
                         breaks = c(-Inf, low_threshold, high_threshold, Inf),
                         labels = c("GD2_Low", "GD2_Medium", "GD2_High"),
                         right = FALSE
                       )
                     }
                     colData(dds)$strat <- as.factor(colData(dds)$strat)
                     strat_table <- as.data.frame(table(colData(dds)$strat))
                     if (!"GD2_Low" %in% strat_table[, 1] | !"GD2_High" %in% strat_table[, 1]) {
                       stop("Stratification resulted in a group with no samples. Please adjust the stratification method or parameters.")
                     }
                     # Filter to binary strat groups
                     incProgress(amount = 0.2, detail = "Preparing design matrix...")
                     dds <- dds[, colData(dds)$strat %in% c("GD2_Low", "GD2_High")]
                     colData(dds)$strat <- droplevels(colData(dds)$strat)
                     colData(dds)$strat <- relevel(colData(dds)$strat, ref = "GD2_Low")
                     strat <- colData(dds)$strat
                     design <- model.matrix(~ strat)
                     
                     keep <- rowSums(assay(dds)) >= 10
                     dds <- dds[keep, ]
                     incProgress(amount = 0.3, detail = "Running voom + limma...")
                     
                     # Choose voom or voomWithQualityWeights
                     if (isTRUE(input$customTabLimmaQualityWeights)) {
                       v <- limma::voomWithQualityWeights(assay(dds), design, plot = FALSE)
                     } else {
                       v <- limma::voom(assay(dds), design, plot = FALSE)
                     }
                     fit <- limma::lmFit(v, design)
                     # Apply treat or standard eBayes
                     if (isTRUE(input$customTabLimmaUseTreat)) {
                       fit <- limma::treat(fit, lfc = input$customTabLimmaTreatLFC)
                     } else {
                       fit <- limma::eBayes(fit, robust = isTRUE(input$customTabLimmaRobust))
                     }
                     contrast <- "stratGD2_High"
                     if (!contrast %in% colnames(fit$coefficients)) {
                       stop(paste("Contrast", contrast, "not found in model coefficients. Available:", paste(colnames(fit$coefficients), collapse = ", ")))
                     }
                     res_obj <- limma::topTable(
                       fit,
                       coef = contrast,
                       number = Inf,
                       adjust.method = input$customTabLimmaAdjustMethod,
                       sort.by = "none"
                     )
                     res_obj$symbol <- rownames(res_obj)
                     raw_counts <- assay(dds)
                     sf <- DESeq2::estimateSizeFactorsForMatrix(raw_counts)
                     norm_counts <- t(t(raw_counts) / sf)
                     res_obj$baseMean <- rowMeans(norm_counts)
                     colnames(res_obj)[which(colnames(res_obj) == "logFC")] <- "log2FoldChange"
                     colnames(res_obj)[which(colnames(res_obj) == "P.Value")] <- "pvalue"
                     colnames(res_obj)[which(colnames(res_obj) == "adj.P.Val")] <- "padj"
                     incProgress(amount = 0.7, detail = "Finishing up...")
                     
                     customDGEres(list(dds = dds, res = res_obj))
                     output$customTabDGEResultDownloadUI <- renderUI({
                       downloadButton(
                         "customTabDGEResultDownload", 
                         "Download Results (CSV)",
                         icon = icon("download"),
                         style = "primary")
                     })
                   }, error = function(e) {
                     showModal(modalDialog(title = "Error", paste("An error occurred:", e$message)))
                   })
                 })
    }
  })
  ## Download results
  output$customTabDGEResultDownload <- downloadHandler(
    filename = function() {
      paste0("DGE_results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      res_obj <- customDGEres()$res
      if (!is.null(res_obj)) {
        write.csv(res_obj, file, row.names = TRUE)
      }
    }
  )
  ## DEA Result -----
  output$custom_diyres_summary <- renderPrint({
    req(customDGEres(), input$customTabDGEFDR)
    # summary(DGEres()$res, alpha = as.numeric(input$tcgaTabDGEFDR))
    DESeq2::summary(customDGEres()$res)
  })
  
  output$custom_table_res <- DT::renderDataTable({
    req(customDGEres())
    
    # Sort results by adjusted p-value
    mydf <- as.data.frame(customDGEres()$res[order(customDGEres()$res$padj), ])
    
    # Move 'symbol' column to the front
    mydf <- mydf[, c("symbol", setdiff(names(mydf), "symbol"))]
    
    # Round all numeric columns except the gene symbol
    is_num <- sapply(mydf, is.numeric)
    mydf[is_num] <- lapply(mydf[is_num], function(x) signif(x, 4))  # or use round(x, 3)
    
    # Make gene symbols into links
    mydf$symbol <- createLinkGeneSymbol(mydf$symbol)
    
    # Create datatable
    datatable(
      mydf,
      escape = FALSE,
      selection = 'single',
      options = list(
        scrollX = TRUE
      )
    )
  })
  
  output$custom_pvals_hist <- renderPlot({
    req(customDGEres())
    
    res_df <- as.data.frame(customDGEres()$res)
    res_df <- dplyr::filter(res_df, !is.na(pvalue))
    p <- ggplot(res_df, aes_string("pvalue")) +
      geom_histogram(binwidth = 0.01, boundary = 0) +
      theme_bw()
    
    # for visual estimation of the false discovery proportion in the first bin
    alpha <- binw <- input$customTabDGEFDR
    pi0 <- 2 * mean(res_df$pvalue > 0.5)
    p <- p + geom_hline(yintercept = pi0 * binw * nrow(res_df),
                        col = "steelblue") +
      geom_vline(xintercept = alpha, col = "red")
    
    p <- p + ggtitle(
      label = "p-value histogram",
      subtitle = paste0(
        "Expected nulls = ",
        pi0 * binw * nrow(res_df),
        " - #elements in the selected bins = ",
        sum(res_df$pvalue < alpha)
      )
    )
    
    p
  })
  
  output$custom_logfc_hist <- renderPlot({
    req(customDGEres())
    
    res_df <- as.data.frame(customDGEres()$res)
    res_df <- dplyr::filter(res_df, !is.na(pvalue))
    
    p <- ggplot(res_df, aes_string("log2FoldChange")) +
      geom_histogram(binwidth = 0.1) +
      theme_bw()
    
    p <- p + ggtitle("Histogram of the log2 fold changes")
    
    p
  })
  
  output$custom_plotma <- renderPlot({
    req(customDGEres())
    
    p <- plot_ma(customDGEres()$res,
                        annotation_obj = NULL,
                        FDR = input$customTabDGEFDR)
    
    p
  })
  
  output$custom_volcanoplot <- renderPlotly({
    req(customDGEres())
    
    # p <- plot_volcano(DGEres()$res, FDR = input$tcgaTabDGEFDR)
    
    # ggplotly(p)
    
    res_df <- customDGEres()$res
    if(any(is.na(res_df$padj))){
      res_df <- res_df[-which(is.na(res_df$padj)), ]
    }
    
    # print(res_df)
    # add a column of NAs
    res_df$diffexpressed <- "NO"
    # if log2Foldchange > 0.6 and pvalue < 0.05, set as "UP"
    res_df$diffexpressed[res_df$log2FoldChange > 0.6 &
                           res_df$padj < 0.05] <- "UP"
    # if log2Foldchange < -0.6 and pvalue < 0.05, set as "DOWN"
    res_df$diffexpressed[res_df$log2FoldChange < -0.6 &
                           res_df$padj < 0.05] <- "DOWN"
    
    res_df$delabel <- rownames(res_df)
    # res_df$delabel[res_df$diffexpressed != "NO"] <- rownames(res_df)[res_df$diffexpressed != "NO"]
    
    
    
    mycolors <- c("#164863", "#ff851b", "black")
    names(mycolors) <- c("DOWN", "UP", "NO")
    
    # library(ggrepel)
    p <- ggplot(data = res_df,
                aes(
                  x = log2FoldChange,
                  y = -log10(pvalue),
                  col = diffexpressed,
                  label = symbol
                )) +
      geom_point() +
      theme_minimal() +
      # geom_text_repel() +
      scale_color_manual(values = c("#164863", "black", "#ff851b")) +
      geom_vline(xintercept = c(-0.6, 0.6), col = "red") +
      geom_hline(yintercept = -log10(input$tcgaTabDGEFDR),
                 col = "red")
    plotly::ggplotly(p)
  })
  
  output$custom_genefinder_plot <- renderPlotly({
    req(customDGEres())
    shiny::validate(need(
      length(input$custom_table_res_row_last_clicked) > 0,
      "Select a Gene in the 'DEA Genes' Table"
    ))
    
    selectedGene <- rownames(customDGEres()$res)[input$custom_table_res_row_last_clicked]
    # selectedGeneSymbol <- values$annotation_obj$gene_name[match(selectedGene, values$annotation_obj$gene_id)]
    
    if (input$custom_genefinder_plotType == "box") {
      p <- ggplotCounts(
        customDGEres()$dds,
        selectedGene,
        intgroup = "strat",
        annotation_obj = NULL
      )
      
      # if(length(levels(colData(customDGEres()$dds)$strat)) == 2){
      #   print("check")
      #   p + scale_fill_manual(values=c("#164863", "#ff851b"),
      #                         breaks = c("GD2_High", "GD2_Low"))
      #   p + scale_color_manual(values=c("#164863", "#ff851b"),
      #                          breaks = c("GD2_High", "GD2_Low"))
      # } else if(length(levels(colData(customDGEres()$dds)$strat)) == 3){
      #   p + scale_fill_manual(values=c("#164863", "#ff851b", "black"),
      #                         breaks = c("GD2_High", "GD2_Low", "GD2_Medium"))
      #   p + scale_color_manual(values=c("#164863", "#ff851b", "black"),
      #                          breaks = c("GD2_High", "GD2_Low", "GD2_Medium"))
      # }
      # if (input$ylimZero_genes) {
      #   p <- p + ylim(0.1, NA)
      # }
      
      ggplotly(p)
      # p
    } else if (input$custom_genefinder_plotType == "scatter") {
      df <- data.frame(
        gene = counts(customDGEres()$dds, )[selectedGene, ],
        gd2 = colData(customDGEres()$dds)$GD2Score,
        intgroup = colData(customDGEres()$dds)$strat
      )
      
      if (length(unique(df$intgroup)) == 2) {
        colors <- c("GD2_High" = "#164863",
                    "GD2_Low" = "#ff851b")
      } else if (length(unique(df$intgroup)) == 3) {
        colors <- c(
          "GD2_High" = "#164863",
          "GD2_Low" = "#ff851b",
          "GD2_Medium" = "darkgreen"
        )
      } else {
        colors <- NULL
      }
      # print(df)
      p <- plotly::plot_ly(
        df,
        x =  ~ gd2,
        y =  ~ gene,
        color =  ~ as.character(intgroup),
        colors = colors,
        type = "scatter",
        mode = "markers",
        text = rownames(df)
      ) %>%
        layout(
          xaxis = list(title = 'GD2 Score'),
          yaxis = list(
            type = 'log',
            title = paste0("Normalized counts (log10 scale ", selectedGene, ")")
          )
        )
      p
    }
    
  })
  
  output$custom_rentrez_infobox <- renderUI({
    req(customDGEres())
    shiny::validate(
      need(
        (length(
          input$custom_table_res_row_last_clicked
        ) > 0),
        "Select a gene in the 'DEA Genes' Table to display additional info (retrieved from the NCBI/ENTREZ db website)"
      )
    )
    
    selectedGene <- rownames(customDGEres()$res)[input$custom_table_res_row_last_clicked]
    
    tryCatch({
      selgene_entrez <- AnnotationDbi::mapIds(
        org.Hs.eg.db,
        keys = selectedGene,
        column = "ENTREZID",
        keytype = "SYMBOL"
      )
      fullinfo <- geneinfo(selgene_entrez)
      
      link_pubmed <- paste0(
        '<a href="http://www.ncbi.nlm.nih.gov/gene/?term=',
        selgene_entrez,
        '" target="_blank" >Click here to see more at NCBI</a>'
      )
      
      if (fullinfo$summary == "") {
        return(HTML(
          paste0(
            "<b>",
            fullinfo$name,
            "</b><br/><br/>",
            fullinfo$description,
            "<br/><br/>",
            link_pubmed
          )
        ))
      } else {
        return(HTML(
          paste0(
            "<b>",
            fullinfo$name,
            "</b><br/><br/>",
            fullinfo$description,
            "<br/><br/>",
            fullinfo$summary,
            "<br/><br/>",
            link_pubmed
          )
        ))
      }
    }, error = function(cond) {
      message("No Entrez ID found for selected gene.")
    })
  })
  
} 
