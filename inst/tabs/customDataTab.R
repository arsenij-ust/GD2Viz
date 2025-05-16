get_customData_tab <- function() {
  tabItem(
    tabName = "customDataTab",
    fluidRow(
      column(
        width = 12,
        h3("Upload Your Dataset and Compute the GD2 Score")
      )
    ),
    fluidRow(
      box(
        width = 3,
        title = "1. Data Input",
        status = "warning",
        solidHeader = FALSE,
        
        radioButtons("dataType", label = div(
          "Choose Data Type:",
          tags$span(icon("circle-question")) %>%
            add_prompt(
              message = "Upload options: 1) Two text files: a raw count matrix (genes × samples) and matching metadata (samples × groups). Sample names must align in both files. 2) A SummarizedExperiment or DESeqDataSet object. (Note: For large datasets, use the R functions locally)",
              position = "right",
              type = "info",
              size = "large",
              rounded = TRUE
            )
        ),
        choices = list("count matrix & metadata" = "countMeta",
                       "DESeqDataSet object" = "dds"),
        selected = "countMeta"),
        
        conditionalPanel(
          condition = "input.dataType == 'countMeta'",
          fileInput("countsFile", "Upload counts matrix file (.tsv):"),
          fileInput("metadataFile", "Upload metadata file (.tsv):")
        ),
        conditionalPanel(
          condition = "input.dataType == 'dds'",
          fileInput("ddsFile", "Upload DESeqDataSet object (.rds):")
        ),
        bs4Dash::actionButton("computeCustomRAS", "Compute Reaction Activity Scores", status = "warning"),
        uiOutput("customRASmessageUI") %>% withSpinner(., type = 7, color="#164863", size = 0.5, hide.ui = FALSE)
      ),
      box(width = 3, title = "2. Model Settings", status = "info", solidHeader = FALSE,
          
          radioButtons("customScale", label = div(
            "Choose RAS processing:",
            tags$span(icon("circle-question")) %>%
              add_prompt(
                message = "Select how to preprocess Reaction Activity Scores (RAS) before computing the GD2 score: Raw: Use unaltered RAS values. Ranged: Normalize each reaction to a [0, 1] scale across samples. Scaled: Standardize values (zero-mean).",
                position = "right",
                type = "info",
                size = "large",
                rounded = TRUE
              )
          ),
          choices = c("raw", "ranged", "scaled"),
          selected = "raw"),
          bs4Dash::actionButton("computeCustomScore", "Compute GD2 Score", status = "info"),
          uiOutput("customGD2messageUI") %>% withSpinner(., type = 7, color="#164863", size = 0.5, hide.ui = FALSE)
      ),
      box(width = 3, title = "3. Global Plot Settings", solidHeader = FALSE, status = "olive",
          radioButtons("customRASType", "Visuaize RAS type",
                       choices = list("unadjusted RAS" = "ras", 
                                      "RAS adj. by transision prob." = "ras_prob", 
                                      #"RAS adj. by path - based transition probability" = "ras_prob_path", 
                                      "RAS adj. by recurive transition probability" = "ras_prob_rec"),
                       selected = "ras_prob"),
          uiOutput("selectCustomGroupUI")
          # bs4Dash::actionButton("updateCustoPlots", "Update plots", status = "success")
      ),
      box(width = 3, title = "4. Download Data", solidHeader = FALSE, status = "primary",
          uiOutput("downloadCustomRASUI"),br(),br(),
          uiOutput("downloadCustomGD2UI")
          # bs4Dash::actionButton("updateCustoPlots", "Update plots", status = "success")
      )
    ),
    fluidRow(
      box(id = "customRASheatmapBox",
          width = 12,
          height = "500px",
          title = div(
            "Reaction Activity Scores",
            tags$span(icon("circle-question")) %>%
              add_prompt(
                message = "Heatmap of the selected RAS values. Samples x KEGG Reaction IDs. Click on the gears symbol in the upeer right corner to adjust the heatmap further.",
                position = "right",
                type = "info",
                size = "large",
                rounded = TRUE
              )
          ),
          status = "primary",
          solidHeader = TRUE,
          maximizable = TRUE,
          sidebar = boxSidebar(
            startOpen = TRUE,
            background = "#427D9D",
            width = 25,
            id = "customRASheatmapBoxSidebar",
            selectInput(
              "customRASheatmapCluster",
              "Show Dendogram:",
              choices = c("none", "row", "column", "both"),
              selected = "both"
            ),
            selectInput(
              "customRASheatmapScale",
              "Scale:",
              choices = c("none", "row", "column"),
              selected = "none"
            ),
            # numericInput(
            #   "customRASheatmapHeight",
            #   "Plot Height (px)", value = 1700),
            selectInput(
              "customRASheatmapColnames",
              "Show Column Names:",
              choices = list("yes" = TRUE, "no" = FALSE),
              selected = "yes"
            ),
            selectInput(
              "customRASheatmapRownames",
              "Show Row Names:",
              choices = list("yes" = TRUE, "no" = FALSE),
              selected = "yes"
            ),
            selectInput(
              "customRASheatmapRownamesFull",
              "Show Only Reaction Names:",
              choices = list("yes" = TRUE, "no" = FALSE),
              selected = "no"
            ),
            selectInput(
              "customRASheatmapDistMethod",
              "Select Distance Method:",
              choices = c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski", "pearson", "spearman", "kendall"),
              selected = "euclidean"
            ),
            selectInput(
              "customRASheatmapHclustMethod",
              "Select Hclust Method:",
              choices = c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid"),
              selected = "complete"
            )
          ),
          plotOutput("customRASheatmap", height = "1500px") %>%
            withSpinner(., type = 7, color="#164863", size = 0.5, hide.ui = FALSE)
      )
    ),
    fluidRow(
      box(width = 6,
          title = div(
            "GD2 Promoting & Diminishing Reaction Activity",
            tags$span(icon("circle-question")) %>%
              add_prompt(
                message = "Scatter plot showing the sums of Reaction Activity Scores for GD2-promoting and GD2-Diminishing reactions. The lines represent the decision weights from the SVM model. Zero-line is the hyperplane.",
                position = "right",
                type = "info",
                size = "large",
                rounded = TRUE
              )
          ),
          height = "500px",
          status = "primary",
          solidHeader = TRUE,
          plotlyOutput("customInOutplot", height = "auto") %>%
            withSpinner(., type = 7, color="#164863", size = 0.5, hide.ui = FALSE)
      ),
      box(id = "customGD2ScoreBox",
          width = 6,
          title = div(
            "GD2 Score",
            tags$span(icon("circle-question")) %>%
              add_prompt(
                message = "This plot shows the predicted GD2 score of the uploaded samples grouped by the selected experimental varable in the 'Global Plot Settings' section. Click on the gears symbol in the upper right corner to change the plot type. If plot type = scatter, you visualize the expression of a selected gene on the x-axis.",
                position = "right",
                type = "info",
                size = "large",
                rounded = TRUE
              )
          ),
          status = "primary",
          solidHeader = TRUE,
          height = "500px",
          maximizable = FALSE,
          sidebar = boxSidebar(
            startOpen = TRUE,
            background = "#427D9D",
            width = 50,
            id = "customGD2ScoreBoxSidebar",
            selectInput(
              "customGD2ScoreRange",
              "Range Values [0-1]:",
              choices = c("yes", "no"),
              selected = "no"
            ),
            selectInput(
              "customGD2ScorePlotType",
              "Select Plot Type:",
              choices = c("scatter", "box", "violin"),
              selected = "box"
            ),
            uiOutput("customGD2ScorePlotTGeneUI")
          ),
          plotlyOutput("customGD2Score", height = "auto") %>%
            withSpinner(., type = 7, color="#164863", size = 0.5, hide.ui = FALSE)
      )
    ),
    fluidRow(
      column(
        width = 6,
        offset = 3,
        box(id = "customGD2StemnessBox",
            width = 12,
            title = div(
              "GD2 Score vs. Stemness Score (mRNAsi)",
              tags$span(icon("circle-question")) %>%
                add_prompt(
                  message = "Malta TM, Sokolov A, et al.; Cancer Genome Atlas Research Network; Stuart JM, Hoadley KA, Laird PW, Noushmehr H, Wiznerowicz M. Machine Learning Identifies Stemness Features Associated with Oncogenic Dedifferentiation. Cell. 2018 Apr 5;173(2):338-354.e15. doi: 10.1016/j.cell.2018.03.034.",
                  position = "right",
                  type = "info",
                  size = "large",
                  rounded = TRUE
                )),
            status = "primary",
            solidHeader = TRUE,
            height = "500px",
            maximizable = FALSE,
            sidebar = boxSidebar(
              startOpen = TRUE,
              background = "#427D9D",
              width = 50,
              id = "customGD2StemnessBoxSidebar",
              selectInput(
                "customGD2StemnessRange",
                "Range Values [0-1]:",
                choices = c("yes", "no"),
                selected = "no"
              )
            ),
            plotlyOutput("customGD2Stemness", height = "auto") %>%
              withSpinner(., type = 7, color="#164863", size = 0.5, hide.ui = FALSE)
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        h3("Compare Two Groups")
      )
    ),
    fluidRow(
      box(width = 12,
          height = "170px",
          title = "Settings for Comparison",
          status = "danger",
          solidHeader = FALSE,
          fluidRow(
            column(3, uiOutput("groupSelectorUI")),
            column(3, uiOutput("levelSelectorAUI")),
            column(3, uiOutput("levelSelectorBUI")),
            column(3, uiOutput("SelectorRASUI"))
          ),
          fluidRow(
            column(4, uiOutput("generateComparisonButtonUI"), offset = 4, class = "text-center"),
            column(4, uiOutput("downloadCustomGD2CompUI"), class = "text-center")
          )
          # uiOutput("generateComparisonButtonUI")
          # bs4Dash::actionButton("generateComparison", "Generate comparison", status = "danger")
      )
    ),
    fluidRow(
      box(width = 6,
          title = "Ganglioside Metabolism",
          id = "customRASGraphBox",
          height = "570px",
          status = "primary",
          solidHeader = TRUE,
          maximizable = TRUE,
          sidebar = boxSidebar(
            startOpen = TRUE,
            background = "#427D9D",
            width = 25,
            id = "customRASGraphBoxSidebar",
            numericInput(
              "customRASGraphBoxPThreshold",
              "P-Value Threshold:",
              value = 0.05,
              min = 0,
              max = 1
            )
          ),
          visNetworkOutput("customGroupCompGraph", height = "440px") %>%
            withSpinner(., type = 7, color="#164863", size = 0.5, hide.ui = FALSE)
          # bs4Dash::actionButton("generateComparison", "Generate comparison", status = "danger")
      ),
      box(width = 6,
          title = "Group Comparison",
          id = "customGroupCompBox",
          status = "primary",
          solidHeader = TRUE,
          maximizable = TRUE,
          plotOutput("customGroupComp", height = "530px") %>%
            withSpinner(., type = 7, color="#164863", size = 0.5, hide.ui = FALSE)
          # bs4Dash::actionButton("generateComparison", "Generate comparison", status = "danger")
      )
    ),
    fluidRow(
      column(
        width = 12,
        h3("Perform Differential Expression Analysis")
      )
    ),
    fluidRow(
      #h2("Explore the GD2 Score of TCGA projects"), br(),br(),
      box(
        width = 12,
        title = "Settings for Differential Expression Analysis",
        status = "danger",
        solidHeader = FALSE,
        fluidRow(
          column(4,
                 # h4("Subset TCGA data:"),
                 # uiOutput("tcgaTabDGEProjectUI"),
                 # uiOutput("tcgaTabDGEColDataUI"),
                 uiOutput("customTabDGEExVarUI"),
                 uiOutput("customTabDGESubtypeUI"),
                 fluidRow(
                   column(6,valueBoxOutput("customProjectSampleNr", width = 12)),
                   column(6,valueBoxOutput("customSubgroupSampleNr", width = 12))
                 ),
                 radioButtons("customRASTypeDGE", "3. Use RAS type",
                              choices = list("unadjusted RAS" = "ras", 
                                             "RAS adj. by transision prob." = "ras_prob", 
                                             #"RAS adj. by path-based transition probability" = "ras_prob_path", 
                                             "RAS adj. by recurive transition probability" = "ras_prob_rec"),
                              selected = "ras_prob")
          ),
          column(4,
                 selectInput(
                   "customTabDGEStratMethodSel",
                   "4. Select stratification method:",
                   choices = list("Median"="m", "One Threshold"="t", "Upper/Lower Percentiles"="q"),
                   selected = "m"
                 ),
                 uiOutput("customTabDGEMethodUI"),
                 # uiOutput("tcgaTabDGESampleNrUI")
                 fluidRow(
                   column(6,valueBoxOutput("customGD2LowSampleNr", width = 12)),
                   column(6,valueBoxOutput("customGD2HighSampleNr", width = 12))
                 )
          ),
          column(4,
                 radioButtons(
                   "customTabDGEtool",
                   label = h3("Select method"),
                   choices = list("DESeq2" = 1, "Limma-voom" = 2),
                   selected = 2
                 ), 
                 hr(),
                 conditionalPanel(
                   condition = "input.customTabDGEtool == 1",
                   # DESeq2-specific
                   numericInput(
                     "customTabDGEFDR",
                     "False Discovery Rate",
                     value = 0.05,
                     min = 0,
                     max = 1,
                     step = 0.01
                   ),
                   selectInput(
                     "customTabDGEFiltering",
                     "Apply independent filtering automatically:",
                     choices = c("TRUE", "FALSE"),
                     selected = "TRUE"
                   ),
                   selectInput(
                     "customTabDGEShrink",
                     "Shrink the log fold change for the contrast of interest:",
                     choices = c("TRUE", "FALSE"),
                     selected = "TRUE"
                   ),
                   selectInput(
                     "customTabDGEWeight",
                     "Use Independent Hypothesis Weighting (IHW) as a filtering function:",
                     choices = c("TRUE", "FALSE"),
                     selected = "FALSE"
                   )
                 ),
                 
                 conditionalPanel(
                   condition = "input.customTabDGEtool == 2",
                   # Limma-voom-specific
                   checkboxInput(
                     "customTabLimmaQualityWeights",
                     "Use quality weights (voomWithQualityWeights)",
                     value = FALSE
                   ),
                   checkboxInput(
                     "customTabLimmaRobust",
                     "Use robust empirical Bayes shrinkage",
                     value = TRUE
                   ),
                   selectInput(
                     "customTabLimmaAdjustMethod",
                     "Multiple testing correction method",
                     choices = c("BH", "holm", "bonferroni", "BY", "none"),
                     selected = "BH"
                   ),
                   checkboxInput(
                     "customTabLimmaUseTreat",
                     "Use treat() with minimum log2 fold change threshold",
                     value = FALSE
                   ),
                   conditionalPanel(
                     condition = "input.customTabLimmaUseTreat == true",
                     numericInput(
                       "customTabLimmaTreatLFC",
                       "Minimum absolute log2 fold change",
                       value = 1,
                       min = 0,
                       step = 0.1
                     )
                   )
                 ),
                 bs4Dash::actionButton(
                   "customTabDGECompute",
                   "Run Differential Expression Analysis",
                   status = "warning"
                 ), br(), br(),
                 uiOutput("customTabDGEResultDownloadUI")
          )
        )
      )
    ),
    fluidRow(
      box(
        width = 4,
        title = "DEA Result",
        status = "primary",
        solidHeader = TRUE,
        verbatimTextOutput("custom_diyres_summary") %>%
          withSpinner(., type = 7, color="#164863", size = 0.5, hide.ui = FALSE)
      ),
      box(
        width = 8,
        title = div(
          "DEA Genes",
          tags$span(icon("circle-question")) %>%
            add_prompt(
              message = "This table demonstrates the resulting statistics from the differential gene expression analysis. Select a gene by clicking on the row to show further information in the 'Gene infobox' and 'Selected Gene' sections. The symbol button links to the respective NCBI gene card.",
              position = "right",
              type = "info",
              size = "large",
              rounded = TRUE
            )
        ),
        status = "primary",
        solidHeader = TRUE,
        tags$head(tags$style(HTML(
          "
              table.dataTable tbody tr.selected td,
              table.dataTable tbody td.selected {
                  border-top-color: white !important;
                  box-shadow: inset 0 0 0 9999px #ff851b !important;
              }

              table.dataTable tbody tr:active td {
                  background-color: #ff851b !important;
              }

              :root {
                  --dt-row-selected: transparent !important;
              }

              table.dataTable tbody tr:hover, table.dataTable tbody tr:hover td {
                  background-color: #ff851b !important;
              }"
        ))
        ),
        DT::dataTableOutput("custom_table_res") %>%
          withSpinner(., type = 7, color="#164863", size = 0.5, hide.ui = FALSE)
      )
    ),
    fluidRow(
      box(
        width = 6,
        title = "Selected Gene",
        status = "primary",
        solidHeader = TRUE,
        sidebar = boxSidebar(
          startOpen = TRUE,
          background = "#427D9D",
          width = 25,
          id = "custom_genefinder_plot_box",
          selectInput(
            "custom_genefinder_plotType",
            "Select Plot Type:",
            choices = c("scatter", "box"),
            selected = "box"
          )
        ),
        plotlyOutput("custom_genefinder_plot") %>%
          withSpinner(., type = 7, color="#164863", size = 0.5, hide.ui = FALSE)
      ),
      box(
        width = 6,
        title = "Gene infobox",
        status = "primary",
        solidHeader = TRUE,
        htmlOutput("custom_rentrez_infobox") %>%
          withSpinner(., type = 7, color="#164863", size = 0.5, hide.ui = FALSE)
      )
    ),
    fluidRow(
      column(
        width = 12,
        h3("Diagnostic plots")
      )
    ),
    fluidRow(
      box(
        width = 6,
        title = div(
          "p-Value Histogram",
          tags$span(icon("circle-question")) %>%
            add_prompt(
              message = "This histogram shows the distribution of p-values from the differential expression analysis. A strong enrichment near p = 0 suggests a substantial number of significantly differentially expressed genes associated with GD2 score strata.",
              position = "right",
              type = "info",
              size = "large",
              rounded = TRUE
            )
        ),
        status = "primary",
        solidHeader = TRUE,
        plotOutput("custom_pvals_hist") %>%
          withSpinner(., type = 7, color="#164863", size = 0.5, hide.ui = FALSE)
      ),
      box(
        width = 6,
        title = div(
          "Histogram of the Log2 Fold-Changes",
          tags$span(icon("circle-question")) %>%
            add_prompt(
              message = "This histogram displays the distribution of log2 fold-changes between groups stratified by GD2 score. The sharp, symmetric peak around log2FC = 0 indicates that most genes are not strongly differentially expressed. However, the presence of longer tails on both sides suggests that a subset of genes exhibits meaningful up- or down-regulation, supporting the idea of transcriptomic shifts associated with GD2 status.",
              position = "right",
              type = "info",
              size = "large",
              rounded = TRUE
            )
        ),
        status = "primary",
        solidHeader = TRUE,
        plotOutput("custom_logfc_hist") %>%
          withSpinner(., type = 7, color="#164863", size = 0.5, hide.ui = FALSE)
      )
    ),
    fluidRow(
      column(
        width = 12,
        h3("Summary plots")
      )
    ),
    fluidRow(
      box(
        width = 6,
        title = div(
          "MA plot",
          tags$span(icon("circle-question")) %>%
            add_prompt(
              message = "This MA plot displays the log2 fold-change (y-axis) versus the mean normalized expression (x-axis, log10 scale) for each gene.",
              position = "right",
              type = "info",
              size = "large",
              rounded = TRUE
            )
        ),
        status = "primary",
        solidHeader = TRUE,
        plotOutput("custom_plotma") %>%
          withSpinner(., type = 7, color="#164863", size = 0.5, hide.ui = FALSE)
      ),
      box(
        width = 6,
        title = div(
          "Volcano plot",
          tags$span(icon("circle-question")) %>%
            add_prompt(
              message = "The volcano plot combines statistical significance (-log10 p-value) and biological effect size (log2 fold-change). Vertical red lines represent fold-change thresholds, while the horizontal line marks the p-value cutoff.",
              position = "right",
              type = "info",
              size = "large",
              rounded = TRUE
            )
        ),
        status = "primary",
        solidHeader = TRUE,
        plotlyOutput("custom_volcanoplot") %>%
          withSpinner(., type = 7, color="#164863", size = 0.5, hide.ui = FALSE)
      )
    ),
    fluidRow(
      box(
        width = 12,
        status = "primary",
        collapsible = FALSE,
        tagList(includeMarkdown(
          system.file("extdata/documentation", "DEA_acknowledgment.md", package = "GD2Viz")
        ))
      )
    )
  )
  
  
}