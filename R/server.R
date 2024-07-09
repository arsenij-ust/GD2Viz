
# server definition ---------------------------------------------------------
gd2visServer <- function(input, output, session) {
  
  options(shiny.maxRequestSize=1000*1024^2)
  trainData <- readRDS(system.file("extdata", "train_data.Rds", package = "GD2Viz"))
  mgraph <- readRDS(system.file("extdata", "substrate_graph.Rds", package = "GD2Viz"))

  tcgaData <- readRDS(system.file("extdata", "TCGA_RAS.Rds", package = "GD2Viz"))
  gtexData <- readRDS(system.file("extdata", "GTEX_RAS.Rds", package = "GD2Viz"))
  targetData <- readRDS(system.file("extdata", "TARGET_RAS.Rds", package = "GD2Viz"))
  stjudeData <- readRDS(system.file("extdata", "STJUDE_RAS.Rds", package = "GD2Viz"))
  cbttcData <- readRDS(system.file("extdata", "CBTTC_RAS.Rds", package = "GD2Viz"))
  
  # divide TCGA into Tumor and Normal Datasets
  tcgaNormalIndx <- which(tcgaData$coldata$X_sample_type == "Solid Tissue Normal")
  tcgaNormalData <- list(
    ras = tcgaData$ras[,tcgaNormalIndx],
    ras_prob = tcgaData$ras_prob[,tcgaNormalIndx],
    ras_prob_path = tcgaData$ras_prob_path[,tcgaNormalIndx],
    ras_prob_rec = tcgaData$ras_prob_rec[,tcgaNormalIndx],
    coldata = tcgaData$coldata[tcgaNormalIndx,]
  )
  tcgaTumorData <- list(
    ras = tcgaData$ras[,-tcgaNormalIndx],
    ras_prob = tcgaData$ras_prob[,-tcgaNormalIndx],
    ras_prob_path = tcgaData$ras_prob_path[,-tcgaNormalIndx],
    ras_prob_rec = tcgaData$ras_prob_rec[,-tcgaNormalIndx],
    coldata = tcgaData$coldata[-tcgaNormalIndx,]
  )

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
  
  observeEvent(input$notification1, {
    runjs("window.open('https://github.com/arsenij-ust/GD2Viz/issues', '_blank')")
  })
  
  # computeCustomRAS
  # computeCustomScore
  # updateCustomPlots
  
  # Datasets tab ----------------
  ## TCGA Train GD2 model & predict values -----
  tcgaTumorGD2 <- reactiveVal(NULL)
  observe({
    req(tcgaTumorData, input$dataTabScale, input$dataTabRASType)
    # rasTypes <- c("ras", "ras_prob", "ras_prob_path", "ras_prob_rec")
    # modelList <- list()
    # predList <- list()
    # inDataList <- list()
    rasType <- input$dataTabRASType
    
    model <- trainGD2model(
      train_data = trainData, 
      adjust_ras = rasType, 
      adjust_input = input$dataTabScale
    )
    # print(paste0("Model: ", rasType, "; adjust_input: ", input$dataTabScale))
    # print(model)
    pred <- computeGD2Score(
      RAS = tcgaTumorData[[rasType]],
      svm_model = model,
      adjust_input = input$dataTabScale,
      range_output = FALSE,
      center = TRUE
    )
  
    tcgaTumorGD2(list(prediction = pred$preds, pred_input = pred$input_df))
  })
  output$tcgaTumorColDataUI <- renderUI({
    req(tcgaTumorData)
    selectInput(
      "tcgaTumorColData",
      "Group by:",
      choices = c("none", colnames(tcgaTumorData$coldata)),
      selected = "detailed_category"
    )
  })
  output$tcgaTumorHighlightGroupUI <- renderUI({
    req(tcgaTumorData, input$tcgaTumorColData)
    
    selectizeInput(
      "tcgaTumorHighlightGroup", 
      label = "Highlight Group:",
      choices = c("", as.character(tcgaTumorData$coldata[[input$tcgaTumorColData]])),
      multiple=TRUE
    )
  })
  
  ## TCGA Train GD2 model & predict values -----
  tcgaNormalGD2 <- reactiveVal(NULL)
  observe({
    req(tcgaNormalData, input$dataTabScale, input$dataTabRASType)
    # rasTypes <- c("ras", "ras_prob", "ras_prob_path", "ras_prob_rec")
    # modelList <- list()
    # predList <- list()
    # inDataList <- list()
    rasType <- input$dataTabRASType
    
    model <- trainGD2model(
      train_data = trainData, 
      adjust_ras = rasType, 
      adjust_input = input$dataTabScale
    )
    
    pred <- computeGD2Score(
      RAS = tcgaNormalData[[rasType]],
      svm_model = model,
      adjust_input = input$dataTabScale,
      range_output = FALSE,
      center = TRUE
    )
    
    tcgaNormalGD2(list(prediction = pred$preds, pred_input = pred$input_df))
  })
  output$tcgaNormalColDataUI <- renderUI({
    req(tcgaNormalData)
    selectInput(
      "tcgaNormalColData",
      "Group by:",
      choices = c("none", colnames(tcgaNormalData$coldata)),
      selected = "detailed_category"
    )
  })
  output$tcgaNormalHighlightGroupUI <- renderUI({
    req(tcgaNormalData, input$tcgaNormalColData)
    
    selectizeInput(
      "tcgaNormalHighlightGroup", 
      label = "Highlight Group:",
      choices = c("", as.character(tcgaNormalData$coldata[[input$tcgaNormalColData]])),
      multiple=TRUE
    )
  })
  
  ## GTEX Train GD2 model & predict values -----
  gtexGD2 <- reactiveVal(NULL)
  observe({
    req(gtexData, input$dataTabScale, input$dataTabRASType)
    # rasTypes <- c("ras", "ras_prob", "ras_prob_path", "ras_prob_rec")
    # modelList <- list()
    # predList <- list()
    # inDataList <- list()
    rasType <- input$dataTabRASType
    
    model <- trainGD2model(
      train_data = trainData, 
      adjust_ras = rasType, 
      adjust_input = input$dataTabScale
    )
    
    pred <- computeGD2Score(
      RAS = gtexData[[rasType]],
      svm_model = model,
      adjust_input = input$dataTabScale,
      range_output = FALSE,
      center = TRUE
    )
    
    gtexGD2(list(prediction = pred$preds, pred_input = pred$input_df))
  })
  output$gtexColDataUI <- renderUI({
    req(gtexData)
    selectInput(
      "gtexColData",
      "Group by:",
      choices = c("none", colnames(gtexData$coldata)),
      selected = "detailed_category"
    )
  })
  output$gtexHighlightGroupUI <- renderUI({
    req(gtexData, input$gtexColData)
    
    selectizeInput(
      "gtexHighlightGroup", 
      label = "Highlight Group:",
      choices = c("", as.character(gtexData$coldata[[input$gtexColData]])),
      multiple=TRUE
    )
  })
  
  ## TARGET Train GD2 model & predict values -----
  targetGD2 <- reactiveVal(NULL)
  observe({
    req(targetData, input$dataTabScale, input$dataTabRASType)
    # rasTypes <- c("ras", "ras_prob", "ras_prob_path", "ras_prob_rec")
    # modelList <- list()
    # predList <- list()
    # inDataList <- list()
    rasType <- input$dataTabRASType
    
    model <- trainGD2model(
      train_data = trainData, 
      adjust_ras = rasType, 
      adjust_input = input$dataTabScale
    )
    
    pred <- computeGD2Score(
      RAS = targetData[[rasType]],
      svm_model = model,
      adjust_input = input$dataTabScale,
      range_output = FALSE,
      center = TRUE
    )
    
    targetGD2(list(prediction = pred$preds, pred_input = pred$input_df))
  })
  output$targetColDataUI <- renderUI({
    req(targetData)
    selectInput(
      "targetColData",
      "Group by:",
      choices = c("none", colnames(targetData$coldata)),
      selected = "detailed_category"
    )
  })
  output$targetHighlightGroupUI <- renderUI({
    req(targetData, input$targetColData)
    
    selectizeInput(
      "targetHighlightGroup", 
      label = "Highlight Group:",
      choices = c("", as.character(targetData$coldata[[input$targetColData]])),
      multiple=TRUE
    )
  })
  
  ## St. Jude Train GD2 model & predict values -----
  stjudeGD2 <- reactiveVal(NULL)
  observe({
    req(stjudeData, input$dataTabScale, input$dataTabRASType)
    # rasTypes <- c("ras", "ras_prob", "ras_prob_path", "ras_prob_rec")
    # modelList <- list()
    # predList <- list()
    # inDataList <- list()
    rasType <- input$dataTabRASType
    
    model <- trainGD2model(
      train_data = trainData, 
      adjust_ras = rasType, 
      adjust_input = input$dataTabScale
    )
    
    pred <- computeGD2Score(
      RAS = stjudeData[[rasType]],
      svm_model = model,
      adjust_input = input$dataTabScale,
      range_output = FALSE,
      center = TRUE
    )
    
    stjudeGD2(list(prediction = pred$preds, pred_input = pred$input_df))
  })
  output$stjudeColDataUI <- renderUI({
    req(stjudeData)
    selectInput(
      "stjudeColData",
      "Group by:",
      choices = c("none", colnames(stjudeData$coldata)),
      selected = "Group"
    )
  })
  output$stjudeHighlightGroupUI <- renderUI({
    req(stjudeData, input$stjudeColData)
    
    selectizeInput(
      "stjudeHighlightGroup", 
      label = "Highlight Group:",
      choices = c("", as.character(stjudeData$coldata[[input$stjudeColData]])),
      multiple=TRUE
    )
  })
  
  ## CBTTC Train GD2 model & predict values -----
  cbttcGD2 <- reactiveVal(NULL)
  observe({
    req(cbttcData, input$dataTabScale, input$dataTabRASType)
    # rasTypes <- c("ras", "ras_prob", "ras_prob_path", "ras_prob_rec")
    # modelList <- list()
    # predList <- list()
    # inDataList <- list()
    rasType <- input$dataTabRASType
    
    model <- trainGD2model(
      train_data = trainData, 
      adjust_ras = rasType, 
      adjust_input = input$dataTabScale
    )
    
    pred <- computeGD2Score(
      RAS = cbttcData[[rasType]],
      svm_model = model,
      adjust_input = input$dataTabScale,
      range_output = FALSE,
      center = TRUE
    )
    
    cbttcGD2(list(prediction = pred$preds, pred_input = pred$input_df))
  })
  output$cbttcColDataUI <- renderUI({
    req(cbttcData)
    selectInput(
      "cbttcColData",
      "Group by:",
      choices = c("none", colnames(cbttcData$coldata)),
      selected = "Histological.Diagnosis..Source.Text."
    )
  })
  output$cbttcHighlightGroupUI <- renderUI({
    req(cbttcData, input$cbttcColData)
    
    selectizeInput(
      "cbttcHighlightGroup", 
      label = "Highlight Group:",
      choices = c("", as.character(cbttcData$coldata[[input$cbttcColData]])),
      multiple=TRUE
    )
  })
  
  ## TCGA Tumor GD2 Score Plot -----
  output$tcgaTumorGD2plot <- renderPlotly({
    req(tcgaTumorGD2(), tcgaTumorData, input$tcgaTumorColData, input$tcgaTumorGD2ScorePlotType)
    
    if (input$tcgaTumorColData == "none") {
      group <- as.factor(rep(1, nrow(tcgaTumorData$coldata)))
      meandf_custom <- NULL
      group_title <- ""
      colors <- "#164863"
    } else {
      group <- tcgaTumorData$coldata[, input$tcgaTumorColData]
      meandf_custom <- aggregate(tcgaTumorGD2()$prediction, list(group), FUN = mean)
      group_title <- input$tcgaTumorColData
      if(length(unique(group)) == 2){
        colors <- c("#164863", "#ff851b")
      } else {
        colors <- NULL
      }
    }
    
    pred_values <- tcgaTumorGD2()$prediction
    if(input$tcgaTumorGD2ScoreRange == "yes"){
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
    
    plotGD2Score(plot_df, plot_type, Group.1, group_title, colors, highlightGroup)
    
  })
  
  add_plot_maximize_observer(input, "tcgaTumorGD2ScoreBox", "tcgaTumorGD2plot")
  
  ## TCGA Normal GD2 Score Plot -----
  output$tcgaNormalGD2plot <- renderPlotly({
    req(tcgaNormalGD2(), tcgaNormalData, input$tcgaNormalColData, input$tcgaNormalGD2ScorePlotType)
    
    if (input$tcgaNormalColData == "none") {
      group <- as.factor(rep(1, nrow(tcgaNormalData$coldata)))
      meandf_custom <- NULL
      group_title <- ""
      colors <- "#164863"
    } else {
      group <- tcgaNormalData$coldata[, input$tcgaNormalColData]
      meandf_custom <- aggregate(tcgaNormalGD2()$prediction, list(group), FUN = mean)
      group_title <- input$tcgaNormalColData
      if(length(unique(group)) == 2){
        colors <- c("#164863", "#ff851b")
      } else {
        colors <- NULL
      }
    }
    
    pred_values <- tcgaNormalGD2()$prediction
    if(input$tcgaNormalGD2ScoreRange == "yes"){
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
    
    plotGD2Score(plot_df, plot_type, Group.1, group_title, colors, highlightGroup)
    
  })
  
  add_plot_maximize_observer(input, "tcgaNormalGD2ScoreBox", "tcgaNormalGD2plot")
  
  ## GTEX GD2 Score Plot -----
  output$gtexGD2plot <- renderPlotly({
    req(gtexGD2(), gtexData, input$gtexColData, input$gtexGD2ScorePlotType)
    
    if (input$gtexColData == "none") {
      group <- as.factor(rep(1, nrow(gtexData$coldata)))
      meandf_custom <- NULL
      group_title <- ""
      colors <- "#164863"
    } else {
      group <- gtexData$coldata[, input$gtexColData]
      meandf_custom <- aggregate(gtexGD2()$prediction, list(group), FUN = mean)
      group_title <- input$gtexColData
      if(length(unique(group)) == 2){
        colors <- c("#164863", "#ff851b")
      } else {
        colors <- NULL
      }
    }
    
    pred_values <- gtexGD2()$prediction
    if(input$gtexGD2ScoreRange == "yes"){
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
    
    plotGD2Score(plot_df, plot_type, Group.1, group_title, colors, highlightGroup)
    
  })
  
  add_plot_maximize_observer(input, "gtexGD2ScoreBox", "gtexGD2plot")
  
  ## TARGET GD2 Score Plot -----
  output$targetGD2plot <- renderPlotly({
    req(targetGD2(), targetData, input$targetColData, input$targetGD2ScorePlotType)
    
    if (input$targetColData == "none") {
      group <- as.factor(rep(1, nrow(targetData$coldata)))
      meandf_custom <- NULL
      group_title <- ""
      colors <- "#164863"
    } else {
      group <- targetData$coldata[, input$targetColData]
      meandf_custom <- aggregate(targetGD2()$prediction, list(group), FUN = mean)
      group_title <- input$targetColData
      if(length(unique(group)) == 2){
        colors <- c("#164863", "#ff851b")
      } else {
        colors <- NULL
      }
    }
    
    pred_values <- targetGD2()$prediction
    if(input$targetGD2ScoreRange == "yes"){
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
    
    plotGD2Score(plot_df, plot_type, Group.1, group_title, colors, highlightGroup)
    
  })
  
  add_plot_maximize_observer(input, "targetGD2ScoreBox", "targetGD2plot")
  
  ## ST Jude Cloud GD2 Score Plot -----
  output$stjudeGD2plot <- renderPlotly({
    req(stjudeGD2(), stjudeData, input$stjudeColData, input$stjudeGD2ScorePlotType)
    
    if (input$stjudeColData == "none") {
      group <- as.factor(rep(1, nrow(stjudeData$coldata)))
      meandf_custom <- NULL
      group_title <- ""
      colors <- "#164863"
    } else {
      group <- stjudeData$coldata[, input$stjudeColData]
      meandf_custom <- aggregate(stjudeGD2()$prediction, list(group), FUN = mean)
      group_title <- input$stjudeColData
      if(length(unique(group)) == 2){
        colors <- c("#164863", "#ff851b")
      } else {
        colors <- NULL
      }
    }
    
    pred_values <- stjudeGD2()$prediction
    if(input$stjudeGD2ScoreRange == "yes"){
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
    
    plotGD2Score(plot_df, plot_type, Group.1, group_title, colors, highlightGroup)
    
  })
  
  add_plot_maximize_observer(input, "stjudeGD2ScoreBox", "stjudeGD2plot")
  
  ## CBTTC GD2 Score Plot -----
  output$cbttcGD2plot <- renderPlotly({
    req(cbttcGD2(), cbttcData, input$cbttcColData, input$cbttcGD2ScorePlotType)
    
    if (input$cbttcColData == "none") {
      group <- as.factor(rep(1, nrow(cbttcData$coldata)))
      meandf_custom <- NULL
      group_title <- ""
      colors <- "#164863"
    } else {
      group <- cbttcData$coldata[, input$cbttcColData]
      meandf_custom <- aggregate(cbttcGD2()$prediction, list(group), FUN = mean)
      group_title <- input$cbttcColData
      if(length(unique(group)) == 2){
        colors <- c("#164863", "#ff851b")
      } else {
        colors <- NULL
      }
    }
    
    pred_values <- cbttcGD2()$prediction
    if(input$cbttcGD2ScoreRange == "yes"){
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
    
    plotGD2Score(plot_df, plot_type, Group.1, group_title, colors, highlightGroup)
    
  })
  
  add_plot_maximize_observer(input, "cbttcGD2ScoreBox", "cbttcGD2plot")
  
  # TCGA Details tab ----------------
  output$tcgaTabGroupUI <- renderUI({
    req(tcgaTumorData)
    selectInput(
      "tcgaTabGroup",
      "Group by:",
      choices = c("none", colnames(tcgaTumorData$coldata)),
      selected = "primary_diagnosis"
    )
  })
  output$tcgaProjectsTbl = DT::renderDataTable({
    req(tcgaTumorData)
    coldata.tcga.sum <- as.data.frame(table(as.factor(tcgaTumorData$coldata$cancer.type)))
    DT::datatable(coldata.tcga.sum, selection = 'single')
  })
  tcgaTumorTabGD2 <- reactiveVal(NULL)
  observe({
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
      RAS = tcgaTumorData[[rasType]],
      svm_model = model,
      adjust_input = input$tcgaTabScale,
      range_output = FALSE,
      center = TRUE
    )
    
    tcgaTumorTabGD2(list(prediction = pred$preds, pred_input = pred$input_df))
  })
  output$tcgaDetailGD2plot <- renderPlotly({
    req(tcgaTumorTabGD2(), tcgaTumorData, input$tcgaTabGroup, input$tcgaProjectsTbl_row_last_clicked)
    
    coldata.tcga.sum <- as.data.frame(table(as.factor(tcgaTumorData$coldata$cancer.type)))
    tcga.project <- coldata.tcga.sum[input$tcgaProjectsTbl_row_last_clicked,"Var1"]
    
    indx <- which(tcgaTumorData$coldata$cancer.type==tcga.project)
    tcgaColData <- tcgaTumorData$coldata[indx,]

    
    if (input$tcgaTabGroup == "none") {
      group <- as.factor(rep(1, nrow(tcgaColData)))
      meandf_custom <- NULL
      group_title <- ""
      colors <- "#164863"
    } else {
      group <- tcgaColData[, input$tcgaTabGroup]
      meandf_custom <- aggregate(tcgaTumorTabGD2()$prediction[indx], list(group), FUN = mean)
      group_title <- input$tcgaTabGroup
      if(length(unique(group)) == 2){
        colors <- c("#164863", "#ff851b")
      } else {
        colors <- NULL
      }
    }
    
    pred_values <- tcgaTumorTabGD2()$prediction[indx]
    if(input$tcgaTabScoreRange == "yes"){
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
    
    plotGD2Score(plot_df, plot_type, Group.1, group_title, colors, highlightGroup)
    
  })
  
  # Custom dataset tab ----------------
  
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
    
    if (input$dataType == "countMeta" && !is.null(input$metadataFile) && !is.null(input$countsFile)) {
      customColdata <- read.table(file = input$metadataFile$datapath, sep = "\t", header = TRUE)
      customCounts <- read.table(file = input$countsFile$datapath, sep = "\t", header = TRUE)
      
      tryCatch({
        customDataVal <- computeReactionActivityScores(
          counts = customCounts, 
          metadata = customColdata, 
          mgraph = mgraph, 
          geom = trainData$geom
        )
        successMessage <- "RAS computed successfully!"
      }, warning = function(w) {

        warningMessage <<- w$message
      }, error = function(e) {

        errorMessage <<- e$message
      })
    } else if (input$dataType == "dds" && !is.null(input$ddsFile)) {
      custom_dds <- readRDS(input$ddsFile$datapath)
      
      tryCatch({
        customDataVal <- computeReactionActivityScores(
          dds = custom_dds,
          mgraph = mgraph, 
          geom = trainData$geom
        )
        successMessage <- "RAS computed successfully!"
      }, warning = function(w) {
        warningMessage <<- w$message
      }, error = function(e) {
        errorMessage <<- e$message
      })
    }
    customData(customDataVal)
    customRASMessages(list(error = errorMessage, warning = warningMessage, success = successMessage))
  })
  
  output$customRASmessageUI <- renderUI({
    req(input$computeCustomRAS)
    
    errorMessage <- customRASMessages()$error
    warningMessage <- customRASMessages()$warning
    successMessage <- customRASMessages()$success
    
    if (!is.null(errorMessage)) {
      return(HTML(paste0(
        '<div style="color: red; display: flex; align-items: center;">',
        as.character(shiny::icon("exclamation-triangle", class = "fa-lg", style = "margin-right: 8px;")),
        '<p style="margin: 0;">', errorMessage, '</p>',
        '</div>'
      )))
    }
    
    if (!is.null(warningMessage)) {
      return(HTML(paste0(
        '<div style="color: orange; display: flex; align-items: center;">',
        as.character(shiny::icon("exclamation-triangle", class = "fa-lg", style = "margin-right: 8px;")),
        '<p style="margin: 0;">', warningMessage, '</p>',
        '</div>'
      )))
    }
    
    if (!is.null(successMessage)) {
      return(HTML(paste0(
        '<div style="display: flex; align-items: center; color: green;">',
        as.character(shiny::icon("check", class = "fa-lg", style = "margin-right: 8px; color: green;")),
        '<p style="margin: 0;">', successMessage, '</p>',
        '</div>'
      )))
    }
  })
  
  output$selectCustomGroupUI <- renderUI({
    if(!is.null(customData())){
      selectInput("customColData", "Group by:", choices = c("none", names(colData(customData()$custom_dds))))
    }
  })
  
  ## Train GD2 model & predict values -----
  observeEvent(input$computeCustomScore, {
    req(customData(), input$customScale)
    rasTypes <- c("ras", "ras_prob", "ras_prob_path", "ras_prob_rec")
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
    customGD2(list(predList = predList, inDataList = inDataList))
  })
  
  output$customGD2messageUI <- renderText({
    req(input$computeCustomScore)
    
    if (is.null(customGD2())) {
       return(HTML(paste0(
        '<div style="color: orange; display: flex; align-items: center;">',
        as.character(shiny::icon("exclamation-triangle", class = "fa-lg", style = "margin-right: 8px;")),
        '<p>Please upload your data first.</p>',
        '</div>'
      )))
    } else {
      ### TODO check predList()
      iconHtml <- as.character(shiny::icon("check", class = "fa-lg", style = "margin-right: 8px; color: green;"))
      HTML(paste0(
        '<div style="display: flex; align-items: center; color: green;">',
        iconHtml,
        '<p style="margin: 0;">Trained SVM model & predicted GD2 score successfully!</p>',
        '</div>'
      ))
    }
  })
  
  ## Download data -----
  
  # Render UI for downloadCustomRASUI
  output$downloadCustomRASUI <- renderUI({
    req(customData())
    downloadButton("downloadCustomRAS", "Download RAS Data", icon = icon("download"), style = "primary")
  })
  
  # Render UI for downloadCustomGD2UI
  output$downloadCustomGD2UI <- renderUI({
    req(customGD2(), customData())
    downloadButton("downloadCustomGD2", "Download GD2 Data", icon = icon("download"), style = "primary")
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
        write.table(custom_data[[i]], tsv_path, sep = "\t", row.names = TRUE, quote = FALSE)
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
      write.table(GD2Score_df, tsv_path, sep = "\t", row.names = TRUE, quote = FALSE)
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
    
    if (input$customColData == "none") {
      col_side_colors <- NULL
      ann_col <- NULL
    } else {
      group <- colData(customData()$custom_dds)[, input$customColData]
      ann_col <- HeatmapAnnotation(df = data.frame(Group = group))
    }

    data <- customData()[[input$customRASType]]
    
    if(input$customRASheatmapCluster == "none"){
      cluster_rows <- FALSE
      cluster_columns <- FALSE
    } else if (input$customRASheatmapCluster == "row"){
      cluster_rows <- TRUE
      cluster_columns <- FALSE
    } else if (input$customRASheatmapCluster == "column"){
      cluster_rows <- FALSE
      cluster_columns <- TRUE
    } else if (input$customRASheatmapCluster == "both"){
      cluster_rows <- TRUE
      cluster_columns <- TRUE
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
    
    # TODO implement scaling
    # Scale the data if necessary
    # if (scale != "none") {
    #   data <- t(scale(t(data), center = TRUE, scale = (scale == "row")))
    # }
    
    # Create the heatmap
    heatmap <- Heatmap(
      data,
      name = paste0("Heatmap of ",input$customRASType),
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
      # height = input$customRASheatmapHeight,
      top_annotation = ann_col
    )
    heatmap
  })
  
  add_plot_maximize_observer(input, "customRASheatmapBox", "customRASheatmap", non_max_height = "400px")
  
  ## Scatterplot of input reactions vs. output reactions -----
  output$customInOutplot <- renderPlotly({
    req(customGD2(), customData())
    df <- customGD2()$inDataList[[input$customRASType]]
    
    if (input$customColData == "none") {
      group <- as.factor(rep(1, nrow(colData(customData()$custom_dds))))
      colors <- "#164863"
    } else {
      group <- colData(customData()$custom_dds)[, input$customColData]
      if(length(unique(group)) == 2){
        colors <- c("#164863", "#ff851b")
      } else {
        colors <- NULL
      }
    }
    
    fig <- plot_ly(
      df, 
      x = ~x, 
      y = ~y, 
      type = 'scatter', 
      mode = 'markers', 
      color = group, 
      colors = colors) %>%
      layout(
        yaxis = list(title = 'Sum( GD2-Diminishing Reactions )'), 
        xaxis = list(title = 'Sum ( GD2-Promoting Reactions )'))
    
    fig
  })
  
  ## Scatterplot of the GD2 Score -----
  output$customGD2Score <- renderPlotly({
    req(customGD2(), customData())
    
    if (input$customColData == "none") {
      group <- as.factor(rep(1, nrow(colData(customData()$custom_dds))))
      meandf_custom <- NULL
      group_title <- ""
      colors <- "#164863"
    } else {
      group <- colData(customData()$custom_dds)[, input$customColData]
      meandf_custom <- aggregate(customGD2()$predList[[input$customRASType]], list(group), FUN = mean)
      group_title <- input$customColData
      if(length(unique(group)) == 2){
        colors <- c("#164863", "#ff851b")
      } else {
        colors <- NULL
      }
    }
    
    pred_values <- customGD2()$predList[[input$customRASType]]
    if(input$customGD2ScoreRange == "yes"){
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
    
    if (plot_type == "scatter") {
      fig <- plot_ly(
        data = plot_df, 
        x = ~Score, 
        y = ~as.character(Group), 
        type = "scatter", 
        mode = "markers", 
        text = rownames(plot_df), 
        color = ~as.character(Group), 
        colors = colors)
      yaxisls <- list(
        categoryorder = "array", 
        categoryarray = Group.1, 
        title = group_title, 
        showticklabels = TRUE)
      xaxisls = list(
        title = 'GD2 Score')
    } else if (plot_type == "box") {
      fig <- plot_ly(
        data = plot_df,
        x = ~as.character(Group),
        y = ~Score,
        type = "box",
        text = rownames(plot_df),
        color = ~as.character(Group),
        colors = colors,
        boxpoints = "all",
        jitter = 0.2) 
      xaxisls <- list(
        categoryorder = "array", 
        categoryarray = Group.1, 
        title = group_title, 
        showticklabels = TRUE)
      yaxisls = list(
        title = 'GD2 Score')
    } else if (plot_type == "violin") {
      fig <- plot_ly(
        data = plot_df,
        x = ~as.character(Group),
        y = ~Score,
        type = "violin",
        text = rownames(plot_df),
        color = ~as.character(Group),
        colors = colors,
        box = list(visible = TRUE),
        meanline = list(visible = TRUE)
      )
      xaxisls <- list(
        categoryorder = "array", 
        categoryarray = Group.1, 
        title = group_title, 
        showticklabels = TRUE)
      yaxisls = list(
        title = 'GD2 Score')
    }
    
    fig <- fig %>%
      layout(
        yaxis = yaxisls,
        xaxis = xaxisls)
    
    fig
  })
  
  ## Group selectors for comparison ----
  # Dynamically create group selector based on the columns of custom_dds
  output$groupSelectorUI <- renderUI({
    data <- customData()$custom_dds
    if(!is.null(data)) {
      selectInput("selectCompColData", "Select Sample Data:", choices = c("", names(colData(data))))
    }
  })
  
  # Dynamically create level selectors based on the selected group
  output$levelSelectorAUI <- renderUI({
    req(input$selectCompColData)
    data <- customData()$custom_dds
    if(!is.null(data)) {
      levels <- unique(data[[input$selectCompColData]])
      selectInput("selected_level_A", "Select Group A:", 
                  choices = levels, selected = levels[1])
    }
  })
  
  output$levelSelectorBUI <- renderUI({
    req(input$selectCompColData)
    data <- customData()$custom_dds
    if(!is.null(data)) {
      levels <- unique(data[[input$selectCompColData]])
      selectInput("selected_level_B", "Select Group B:", 
                  choices = levels, selected = levels[2])
    }
  })
  
  output$SelectorRASUI <- renderUI({
    req(input$selectCompColData)
    data <- customData()$custom_dds
    if(!is.null(data)) {
      selectInput("customRASComp", "Visuaize RAS Type:", 
                   choices = list(
                     "unadjusted RAS" = "ras", 
                     "RAS adj. by transision prob." = "ras_prob", 
                     "RAS adj. by path-based transition probability" = "ras_prob_path", 
                     "RAS adj. by recurive transition probability" = "ras_prob_rec")
      )
    }
  })
  
  output$generateComparisonButtonUI <- renderUI({
    req(input$selectCompColData, input$selected_level_A, input$selected_level_B)
    bs4Dash::actionButton("generateComparison", "Generate comparison graphs", status = "success")
  })
  
  observeEvent(input$generateComparison,{
    req(input$selectCompColData, input$selected_level_A, input$selected_level_B)
    dds <- customData()$custom_dds
    group_col <- input$selectCompColData
    
    ras <- customData()[[input$customRASComp]]
    
    indxA <- which(
      colData(dds)[[input$selectCompColData]]==input$selected_level_A
    )
    indxB <- which(
      colData(dds)[[input$selectCompColData]]==input$selected_level_B
    )
    
    p.vals <- sapply(1:nrow(ras), function(x){
      ks.test(
        ras[x, indxA],
        ras[x, indxB]
      )$p.value
    })
    
    pval_df <- data.frame(
      reaction=rownames(ras), 
      pval=round(p.vals, 5), 
      padj=p.adjust(p.vals, method="BH"))
    
    log2fc <- sapply(1:nrow(ras), function(x){
      log2GrA <- log2(
        mean(ras[x, indxA])
      )
      log2GrB <- log2(
        mean(ras[x, indxB])
      )
      log2GrA - log2GrB
    })
    
    pval_df$log2fc <- log2fc
    # rownames(pval_df) <- gsub("| .*", "", pval_df$reaction)
    
    
    pval_df[[input$selected_level_A]] <- rowMeans(ras[, indxA])
    pval_df[[input$selected_level_B]] <- rowMeans(ras[, indxB])
    
    pval_df[[paste0("sd_",input$selected_level_A)]] <- sapply(1:nrow(ras), function(x){
      sd(
        ras[x, indxA]
      )
    })
    pval_df[[paste0("sd_",input$selected_level_B)]] <- sapply(1:nrow(ras), function(x){
      sd(
        ras[x, indxB]
      )
    })
    customLFCData(pval_df)
  })
  
  ## RAS comparison graph -----
  output$customGroupCompGraph <- renderVisNetwork({
    req(input$generateComparison, input$selectCompColData, input$selected_level_A, input$selected_level_B)
    pval_df <- customLFCData()
    
    # print(pval_df)
    viz_mgraph <- visNetwork::toVisNetworkData(mgraph, idToLabel = FALSE)
    # print(igraph::as_data_frame(mgraph, "both"))
    
    # Convert miriam.kegg.reaction column to character
    viz_mgraph$edges$miriam.kegg.reaction <- sapply(viz_mgraph$edges$miriam.kegg.reaction, function(x) paste(x, collapse = ","))
    viz_mgraph$edges$pathway <- sapply(viz_mgraph$edges$pathway, function(x) paste(x, collapse = ","))
    viz_mgraph$edges$to <- sapply(viz_mgraph$edges$to, function(x) paste(x, collapse = ","))
    viz_mgraph$edges$miriam.kegg.reaction <- sapply(viz_mgraph$edges$miriam.kegg.reaction, function(x) paste(x, collapse = ","))
    viz_mgraph$edges$symbol <- sapply(viz_mgraph$edges$symbol, function(x) paste(unique(x), collapse = ","))
    viz_mgraph$nodes$id <- sapply(viz_mgraph$nodes$id, function(x) paste(x, collapse = ","))
    
    # Merge the edge data with pval_df
    viz_mgraph$edges <- viz_mgraph$edges %>%
      left_join(pval_df, by = c("miriam.kegg.reaction" = "reaction"))
    
    # Define edge colors based on log2fc values
    viz_mgraph$edges <- viz_mgraph$edges %>%
      mutate(color = case_when(
        padj > input$customRASGraphBoxPThreshold ~ "gray",
        log2fc > 0 ~ "red",
        log2fc <= 0 ~ "blue"
      ))
      # mutate(color = ifelse(log2fc > 0, "red", "blue"))
    
    # Define edge widths based on padj values
    # Normalize padj values to a suitable range for widths (e.g., 1 to 10)
    viz_mgraph$edges <- viz_mgraph$edges %>%
      mutate(width = (1 - log2fc) * 10)
    
    # Extract unique to nodes and their pathways
    to_pathways <- viz_mgraph$edges %>%
      select(to, pathway) %>%
      distinct()
    
    viz_mgraph$edges$title <- paste0("Reaction: ", viz_mgraph$edges$miriam.kegg.reaction,
                                     "<br>Involved genes: ", viz_mgraph$edges$symbol,
                                     "<br>Log2FC: ", round(viz_mgraph$edges$log2fc, 3),
                                     "<br>Pval:", round(viz_mgraph$edges$pval, 3),
                                     "<br>Padj:", round(viz_mgraph$edges$padj, 3))
    
    # Merge pathway information into nodes data frame
    # viz_mgraph$nodes <- viz_mgraph$nodes %>%
    #   left_join(to_pathways, by = c("id" = "to"))
    viz_mgraph$nodes <- merge(viz_mgraph$nodes, to_pathways, by.x = "id", by. = "to", all.x = TRUE)
    # viz_mgraph$nodes[which(is.na(viz_mgraph$nodes$pathway)),"pathway"] <- "unknown"
    viz_mgraph$nodes <- viz_mgraph$nodes[-92,]
    
    # print(viz_mgraph$nodes)
    # print(viz_mgraph$edges)
    
    visNetwork(nodes = viz_mgraph$nodes,
               edges = viz_mgraph$edges,
               main = "Glycosphingolipid Metabolism Pathway", 
               footer = "Arrow Color & Width is the Log2 Fold-Change (log2fc) of 'Group A' - 'Group B' <br> Positive log2fc = red; Negative log2fc = blue; padj > p-Value Threshold = gray") %>% 
      visEdges(
        arrows =list(to = list(enabled = TRUE))
      ) %>% 
      visNodes(color = list(background = "#164863", 
                            border = "#164863",
                            highlight = "yellow")) %>% 
      visOptions(#highlightNearest = list(enabled = T, degree = 1, hover = T),
                 nodesIdSelection = TRUE, selectedBy = "pathway") %>%
      visLayout(randomSeed = 1111)
    
    # mygraph <- createGraph(mgraph, pval_df)
    # print(igraph::as_data_frame(mygraph$graph, "both"))
    # plot(mygraph$graph, layout=mygraph$layout, vertex.label.color= "black", vertex.label.family= "Arial")
  })
  
  # add_plot_maximize_observer(input, "customRASGraphBox", "customGroupCompGraph", non_max_height = "530px")
  observeEvent(input$customRASGraphBox$maximized, {
    visRedraw(visNetworkProxy("customGroupCompGraph", session = shiny::getDefaultReactiveDomain()))
  })
  ## RAS comparison plot -----
  output$customGroupComp <- renderPlot({
    req(input$generateComparison, input$selectCompColData, input$selected_level_A, input$selected_level_B)
    pval_df <- customLFCData()
    
    # print(pval_df)
    viz_mgraph <- visNetwork::toVisNetworkData(mgraph, idToLabel = FALSE)
    # print(igraph::as_data_frame(mgraph, "both"))
    
    # Convert miriam.kegg.reaction column to character
    viz_mgraph$edges$miriam.kegg.reaction <- sapply(viz_mgraph$edges$miriam.kegg.reaction, function(x) paste(x, collapse = ","))
    viz_mgraph$edges$pathway <- sapply(viz_mgraph$edges$pathway, function(x) paste(x, collapse = ","))
    viz_mgraph$edges$to <- sapply(viz_mgraph$edges$to, function(x) paste(x, collapse = ","))
    viz_mgraph$nodes$id <- sapply(viz_mgraph$nodes$id, function(x) paste(x, collapse = ","))
    
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
      tidyr::pivot_longer(colnames(dplyr::select(e_df, c(input$selected_level_A, input$selected_level_B, "log2fc"))), names_to = "group", values_to = "mean")
    e_df_lg$group <- as.factor(e_df_lg$group)
    e_df_lg[which(e_df_lg$group %in% c(input$selected_level_A, input$selected_level_B)),"padj"] <- NA
    e_df_lg[which(e_df_lg$group %in% c("log2fc")),paste0("sd_", input$selected_level_A)] <- NA
    e_df_lg[which(e_df_lg$group %in% c("log2fc")),paste0("sd_", input$selected_level_B)] <- NA
    e_df_lg[which(e_df_lg$group == input$selected_level_B),paste0("sd_", input$selected_level_A)] <- NA
    e_df_lg[which(e_df_lg$group == input$selected_level_A),paste0("sd_", input$selected_level_B)] <- NA

    e_df_lg <- e_df_lg %>% 
      mutate(sd = coalesce(
        paste0("sd_", input$selected_level_A),
        paste0("sd_", input$selected_level_B)))

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

    e_df_lg <- e_df_lg[which(e_df_lg$from %in% comps | e_df_lg$to %in% comps),]
    e_df_lg$geneSymbol <- sapply(e_df_lg$symbol, function(x) paste0(unique(x), collapse = ' '))
    from_label <- c()
    for(i in e_df_lg$from){
      from_label <- c(from_label, viz_mgraph$nodes[which(viz_mgraph$nodes$id == i), "label"])
    }
    e_df_lg$from_label <- from_label
    to_label <- c()
    for(i in e_df_lg$to){
      to_label <- c(to_label, viz_mgraph$nodes[which(viz_mgraph$nodes$id == i), "label"])
    }
    e_df_lg$to_label <- to_label

    # ggplot(e_df_lg, aes(x=mean, y=paste0(miriam.kegg.reaction, " (", geneSymbol, ")"), group=group)) +

    p1 <- ggplot(e_df_lg, 
                 aes(x=mean, 
                     y=paste0(miriam.kegg.reaction, " (from: ", from_label, " - to: ",to_label, ")"), 
                     group=group)) +
      geom_vline(xintercept = 0, colour="lightgray", linetype="solid") +
      # geom_errorbar(aes(xmin=mean - sd,xmax=mean + sd,width=0.2)) +
      geom_point(aes(color=-log10(padj))) +
      colorspace::scale_color_continuous_sequential(palette = "Reds 3") +
      facet_wrap(~group, ncol = 3, scales="free_x") +
      theme_classic() +
      ylab("") + xlab("") +
      theme(panel.grid.major.y = element_line(color = "lightgray",
                                              size = 0.5,
                                              linetype = 1)) + xlab("")
    p1

  })
  
  
} 