get_TCGAData_tab <- function() {
  tabItem(
    tabName = "tcgaDetailTab",
    fluidRow(
      column(
        width = 12,
        h3("GD2 Score of TCGA projects")
      )
    ),
    fluidRow(
      #h2("Explore the GD2 Score of TCGA subgroups"), br(),br(),
      box(
        width = 4,
        title = "1. Select TCGA Project",
        status = "danger",
        solidHeader = FALSE,
        uiOutput("tcgaTabProjectUI"),
        uiOutput("tcgaTabGroupUI")
        # uiOutput("tcgaTabCnvUI")
      ),
      box(
        width = 8,
        title = "2. Global Settings",
        status = "danger",
        solidHeader = FALSE,
        fluidRow(
          column(4,
                 radioButtons(
                   "tcgaTabScale",
                   label=div(
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
                   selected = "raw"
                 ),
                 selectInput(
                   "tcgaTabScoreRange",
                   label=div(
                     "Range Values [0-1]:",
                     tags$span(icon("circle-question")) %>%
                       add_prompt(
                         message = "This affects only the range of the x-axis in the following plot.",
                         position = "right",
                         type = "info",
                         size = "large",
                         rounded = TRUE
                       )
                   ),
                   choices = c("yes", "no"),
                   selected = "no"
                 )
          ),
          column(4,
                 radioButtons(
                   "tcgaTabRASType",
                   label=div(
                     "Visualize RAS type",
                     tags$span(icon("circle-question")) %>%
                       add_prompt(
                         message = "Choose how RAS values are adjusted for visualization: 1) Unadjusted RAS: Raw scores from gene expression. 2) Adjusted by transition prob.: Weighted by network topology. 3) Recursive adjustment: Refines weights through reaction chains.",
                         position = "right",
                         type = "info",
                         size = "large",
                         rounded = TRUE
                       )
                   ),
                   choices = list(
                     "unadjusted RAS" = "ras",
                     "RAS adj. by transition prob." = "ras_prob",
                     #"RAS adj. by path-based transition probability" = "ras_prob_path",
                     "RAS adj. by recursive transition probability" = "ras_prob_rec"),
                   selected = "ras_prob")
          )
          
        )
      )
    ),
    fluidRow(
      box(
        width = 12,
        title = "GD2 Score of TCGA Project:",
        status = "primary",
        solidHeader = TRUE,
        plotlyOutput("tcgaDetailGD2plot", height = "90vh") %>% withSpinner(., type = 7, color="#164863", size = 0.5, hide.ui = FALSE)
      )
    ),
    fluidRow(
      column(
        width = 12,
        h3("Perform Differential Expression Analysis")
      )
    ),
    fluidRow(
      #h2("Explore the GD2 Score of TCGA subgroups"), br(),br(),
      box(
        width = 12,
        title = "Settings for Differential Expression Analysis",
        status = "danger",
        solidHeader = FALSE,
        fluidRow(
          column(
            4,
            # h4("Subset TCGA data:"),
            # uiOutput("tcgaTabDGEProjectUI"),
            # uiOutput("tcgaTabDGEColDataUI"),
            uiOutput("tcgaTabDGESubtypeUI"),
            fluidRow(column(
              6, valueBoxOutput("ProjectSampleNr", width = 12)
            ), column(
              6, valueBoxOutput("SubgroupSampleNr", width = 12)
            ))
          ),
          column(
            4,
            selectInput(
              "tcgaTabDGEStratMethodSel",
              "2. Select stratification method:",
              choices = list(
                "Median" = "m",
                "Custom Threshold" = "t",
                "Upper/Lower Percentiles" = "q"
              ),
              selected = "m"
            ),
            uiOutput("tcgaTabDGEMethodUI"),
            # uiOutput("tcgaTabDGESampleNrUI")
            fluidRow(column(
              6, valueBoxOutput("GD2LowSampleNr", width = 12)
            ), column(
              6, valueBoxOutput("GD2HighSampleNr", width = 12)
            ))
          ),
          column(
            4,
            radioButtons(
              "tcgaTabDGEtool",
              label = h3("Select method"),
              choices = list("DESeq2" = 1, "Limma-voom" = 2),
              selected = 2
            ), 
            hr(),
            conditionalPanel(
              condition = "input.tcgaTabDGEtool == 1",
              # DESeq2-specific
              numericInput(
                "tcgaTabDGEFDR",
                "False Discovery Rate",
                value = 0.05,
                min = 0,
                max = 1,
                step = 0.01
              ),
              selectInput(
                "tcgaTabDGEFiltering",
                "Apply independent filtering automatically:",
                choices = c("TRUE", "FALSE"),
                selected = "TRUE"
              ),
              selectInput(
                "tcgaTabDGEShrink",
                "Shrink the log fold change for the contrast of interest:",
                choices = c("TRUE", "FALSE"),
                selected = "TRUE"
              ),
              selectInput(
                "tcgaTabDGEWeight",
                "Use Independent Hypothesis Weighting (IHW) as a filtering function:",
                choices = c("TRUE", "FALSE"),
                selected = "FALSE"
              )
            ),
            
            conditionalPanel(
              condition = "input.tcgaTabDGEtool == 2",
              # Limma-voom-specific
              checkboxInput(
                "tcgaTabLimmaQualityWeights",
                "Use quality weights (voomWithQualityWeights)",
                value = FALSE
              ),
              checkboxInput(
                "tcgaTabLimmaRobust",
                "Use robust empirical Bayes shrinkage",
                value = TRUE
              ),
              selectInput(
                "tcgaTabLimmaAdjustMethod",
                "Multiple testing correction method",
                choices = c("BH", "holm", "bonferroni", "BY", "none"),
                selected = "BH"
              ),
              checkboxInput(
                "tcgaTabLimmaUseTreat",
                "Use treat() with minimum log2 fold change threshold",
                value = FALSE
              ),
              conditionalPanel(
                condition = "input.tcgaTabLimmaUseTreat == true",
                numericInput(
                  "tcgaTabLimmaTreatLFC",
                  "Minimum absolute log2 fold change",
                  value = 1,
                  min = 0,
                  step = 0.1
                )
              )
            ),
            bs4Dash::actionButton(
              "tcgaTabDGECompute",
              "Run Differential Expression Analysis",
              status = "warning"
            ), br(), br(),
            uiOutput("tcgaTabDGEResultDownloadUI")
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
        verbatimTextOutput("diyres_summary") %>%
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
        DT::dataTableOutput("table_res") %>%
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
          startOpen = FALSE,
          background = "#427D9D",
          width = 25,
          id = "genefinder_plot_box",
          selectInput(
            "genefinder_plotType",
            "Select Plot Type:",
            choices = c("scatter", "box"),
            selected = "box"
          )
        ),
        plotlyOutput("genefinder_plot") %>%
          withSpinner(., type = 7, color="#164863", size = 0.5, hide.ui = FALSE)
      ),
      box(
        width = 6,
        title = "Gene infobox",
        status = "primary",
        solidHeader = TRUE,
        htmlOutput("rentrez_infobox") %>%
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
        plotOutput("pvals_hist") %>%
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
        plotOutput("logfc_hist") %>%
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
        plotOutput("plotma") %>%
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
        plotlyOutput("volcanoplot") %>%
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
