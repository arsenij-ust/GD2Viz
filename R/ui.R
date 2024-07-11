
# UI definition -----------------------------------------------------------
gd2visUI = dashboardPage(
  title = "GD2Viz",
  dark = NULL,
  help = NULL,

  ## Header----------------------------------------------------------
  header = dashboardHeader(
    # title = HTML("<small>GD2Viz</small>"),
    # src = "GD2Viz/GD2Viz.png"
    skin = "light",
    status = "navy",
    border = TRUE,
    sidebarIcon = icon("bars"),
    controlbarIcon = icon("th"),
    fixed = TRUE,
    leftUi = tagList(
      dropdownMenu(
        badgeStatus = "danger",
        headerText = "Read further information:",
        type = "notifications",
        icon = shiny::icon("circle-info"),
        # notificationItem(
        #   inputId = "notification1",
        #   text = "Open GD2Viz Vignette",
        #   status = "primary",
        #   icon = shiny::icon("book-open")
        # ),
        notificationItem(
          inputId = "notification1",
          text = "Open GD2Viz Vignette",
          status = "danger",
          icon = shiny::icon("book-open")
        ),
        notificationItem(
          inputId = "notification2",
          text = "About this session",
          status = "danger",
          icon = shiny::icon("info")
        ),
        notificationItem(
          inputId = "notification3",
          text = "About GD2Viz",
          status = "danger",
          icon = shiny::icon("heart")
        ),
        notificationItem(
          inputId = "notification4",
          text = "Report Issue",
          status = "danger",
          icon = shiny::icon("bug")
        )
      )
    )
  ),
  ## Sidebar ----------------------------------------------------------
  sidebar = dashboardSidebar(
    id = "sidebar",
    
    # resize logo if navbar is collapsed
    tags$head(
      tags$style(HTML("
          #resizable-image {
            transition: width 0.5s ease-in-out;
          }
        ")),
      tags$script(HTML(
        "
        document.addEventListener('DOMContentLoaded', function() {
          const img = document.getElementById('resizable-image');
          const body = document.body;
          
          function resizeImage() {
            if (body.classList.contains('sidebar-collapse')) {
              img.style.width = '60px';
            } else {
              img.style.width = '180px';
            }
          }
          
          // Initial resize based on the current state
          resizeImage();
          
          // Monitor class changes on the body
          const observer = new MutationObserver(function(mutations) {
            mutations.forEach(function(mutation) {
              if (mutation.attributeName === 'class') {
                resizeImage();
              }
            });
          });
          
          observer.observe(body, { attributes: true });
        });
        "
      ))
    ),
    div(
      img(id = "resizable-image", src = "GD2Viz/GD2Viz6.png",
          style = "
          width: 180px; 
          height: auto;
          margin-bottom: 30px;
          display: block;
          margin-left: auto;
          margin-right: auto;")
    ),
    
    skin = "light",
    status = "primary",
    elevation = 3,
    sidebarMenu(
      menuItem(
        "Welcome",
        tabName = "welcomeTab",
        icon = icon("house")
      )
    ),
    sidebarMenu(
      sidebarHeader("Main tabs:"),
      menuItem(
        "Explore datasets",
        tabName = "exploreDataTab",
        icon = icon("database")
      ),
      menuItem(
        "TCGA subgroups",
        tabName = "tcgaDetailTab",
        icon = icon("magnifying-glass-plus")
      ),
      menuItem(
        "Custom dataset",
        tabName = "customDataTab",
        icon = icon("file-arrow-up")
      )
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
    # ,
    # div(
    #   img(id = "deco-image", src = "GD2Viz/deco.png",
    #       style = "
    #       width: 180px; 
    #       height: auto;
    #       margin-bottom: 30px;
    #       display: block;
    #       margin-left: auto;
    #       margin-right: auto;
    #       position: absolute;
    #       bottom: 0;")
    # )
  ),
 
  ## Footer----------------------------------------------------------
  footer = bs4DashFooter(
    left =
     fluidRow(
       column(
         width = 1,
         align = "right",
         a(
           href = "TODO",
           target = "_blank",
           img(src = "GD2Viz/GD2Viz6.png", height = "50px")
         )
       ),
       column(
         width = 11,
         align = "center",
         "GD2Viz is a project developed by Arsenij Ustjanzew in the Medical informatics division
                                      of the ",
         tags$a(href = "http://www.unimedizin-mainz.de/imbei", "IMBEI"),
         "- Institute for Medical Biostatistics,
                                      Epidemiology and Informatics; University Medical Center of the Johannes Gutenberg University Mainz",
         br(),
         "License: ",
         tags$a(href = "https://opensource.org/licenses/MIT", "MIT"),
         "- The GD2Viz package is developed and available
                                      on ",
         tags$a(href = "TODO", "GitHub")
       )
     ), right = NULL), 
  ## Body----------------------------------------------------------
  body = dashboardBody(
    use_theme(theme),
    shinyjs::useShinyjs(),
    use_prompt(),
    tabItems(
      ### Welcome tab -------------------
      tabItem(
        tabName = "welcomeTab",
        fluidRow(
          box(
            title = "Welcome to GD2Viz", status = "primary", solidHeader = TRUE, width = 12,
            collapsible = TRUE,
            tagList(includeMarkdown(
              system.file("extdata", "welcome.md", package = "GD2Viz")
            ))
          ),
          box(
            title = "Background Information", status = "primary", solidHeader = TRUE, width = 12,
            collapsible = TRUE,
            tagList(includeMarkdown(
              system.file("extdata", "gd2score.md", package = "GD2Viz")
            ))
          ),
          tabBox(
            title = "Getting started:",
            width = 12,
            side = "right",
            # The id lets us use input$tabset1 on the server to find the current tab
            id = "tabsetWelcomeTab",
            tabPanel("Explore datasets", 
                     tagList(includeMarkdown(
                       system.file("extdata", "datasets_description.md", package = "GD2Viz")
                     ))
            ),
            tabPanel("TCGA subgroups",
                     tagList(includeMarkdown(
                       system.file("extdata", "tcga_description.md", package = "GD2Viz")
                     )),
            ),
            tabPanel("Custom dataset",
                     tagList(includeMarkdown(
                       system.file("extdata", "custom_data_description.md", package = "GD2Viz")
                     )),
            )
          )
        )
      ),
      ### Datasets tab -------------------
      tabItem(
        tabName = "exploreDataTab",
        fluidRow(
          h2("Explore the GD2 Score of large RNA-Seq datasets"), br(),br(),
          box(
            width = 12,
            title = "Global Settings",
            status = "danger",
            solidHeader = FALSE,
            fluidRow(
              column(4, radioButtons(
                "dataTabScale",
                label = div(
                  "Choose RAS processing:",
                  tags$span(icon("circle-question")) %>%
                    add_prompt(
                      message = "this is a plot, and I add some text to show the size of the box.",
                      position = "right",
                      type = "info",
                      size = "large",
                      rounded = TRUE
                    )
                ),
                choices = c("raw", "ranged", "scaled"),
                selected = "raw"
              )
              ),
              column(4, radioButtons(
                "dataTabRASType",
                label = div(
                  "Visualize RAS type:",
                  tags$span(icon("circle-question")) %>%
                    add_prompt(
                      message = "this is a plot, and I add some text to show the size of the box.",
                      position = "right",
                      type = "info",
                      size = "large",
                      rounded = TRUE
                    )
                ),
                choices = list(
                  "unadjusted RAS" = "ras",
                  "RAS adj. by transision prob." = "ras_prob",
                  "RAS adj. by path-based transition probability" = "ras_prob_path",
                  "RAS adj. by recurive transition probability" = "ras_prob_rec"
                ),
                selected = "ras_prob"
              )
              )
            )
          )
        ),
        fluidRow(
          tabBox(
            title = "",
            id = "tabset1",
            width = 12,
            tabPanel(
              "TCGA Tumor",
              box(
                id = "",
                width = 12,
                height = "auto",
                title = "Plot Settings",
                status = "danger",
                solidHeader = FALSE,
                maximizable = FALSE,
                fluidRow(
                  column(3, selectInput(
                    "tcgaTumorGD2ScoreRange",
                    "Range Values [0-1]:",
                    choices = c("yes", "no"),
                    selected = "no"
                  )),
                  column(3, selectInput(
                    "tcgaTumorGD2ScorePlotType",
                    "Select Plot Type:",
                    choices = c("scatter", "box", "violin"),
                    selected = "scatter"
                  )),
                  column(3, uiOutput("tcgaTumorColDataUI")),
                  column(3, uiOutput("tcgaTumorHighlightGroupUI"))
                )
              ),
              box(
                id = "tcgaTumorGD2ScoreBox",
                width = 12,
                height = "auto",
                title = "TCGA Tumor",
                status = "primary",
                solidHeader = TRUE,
                maximizable = TRUE,
                plotlyOutput("tcgaTumorGD2plot", height = "90vh") %>% 
                  withSpinner(., type = 7, color="#164863", size = 0.5, hide.ui = FALSE)
              ),
              box(
                width = 12,
                status = "primary",
                collapsible = FALSE,
                tagList(includeMarkdown(
                  system.file("extdata", "tcga_citation.md", package = "GD2Viz")
                ))
              )
            ),
            tabPanel(
              "TCGA Normal",box(
                id = "",
                width = 12,
                height = "auto",
                title = "Plot Settings",
                status = "danger",
                solidHeader = FALSE,
                maximizable = FALSE,
                fluidRow(
                  column(3, selectInput(
                    "tcgaNormalGD2ScoreRange",
                    "Range Values [0-1]:",
                    choices = c("yes", "no"),
                    selected = "no"
                  )),
                  column(3, selectInput(
                    "tcgaNormalGD2ScorePlotType",
                    "Select Plot Type:",
                    choices = c("scatter", "box", "violin"),
                    selected = "scatter"
                  )),
                  column(3, uiOutput("tcgaNormalColDataUI")),
                  column(3, uiOutput("tcgaNormalHighlightGroupUI"))
                )
              ),
              box(
                id = "tcgaNormalGD2ScoreBox",
                width = 12,
                height = "auto",
                title = "TCGA Normal",
                status = "primary",
                solidHeader = TRUE,
                maximizable = TRUE,
                plotlyOutput("tcgaNormalGD2plot", height = "90vh") %>% 
                  withSpinner(., type = 7, color="#164863", size = 0.5, hide.ui = FALSE)
              ),
              box(
                width = 12,
                status = "primary",
                collapsible = FALSE,
                tagList(includeMarkdown(
                  system.file("extdata", "tcga_citation.md", package = "GD2Viz")
                ))
              )
            ),
            tabPanel(
              "GTEx",
              box(
                id = "",
                width = 12,
                height = "auto",
                title = "Plot Settings",
                status = "danger",
                solidHeader = FALSE,
                maximizable = FALSE,
                fluidRow(
                  column(3, selectInput(
                    "gtexGD2ScoreRange",
                    "Range Values [0-1]:",
                    choices = c("yes", "no"),
                    selected = "no"
                  )),
                  column(3, selectInput(
                    "gtexGD2ScorePlotType",
                    "Select Plot Type:",
                    choices = c("scatter", "box", "violin"),
                    selected = "scatter"
                  )),
                  column(3, uiOutput("gtexColDataUI")),
                  column(3, uiOutput("gtexHighlightGroupUI"))
                )
              ),
              box(
                id = "gtexGD2ScoreBox",
                width = 12,
                height = "auto",
                title = "GTEx",
                status = "primary",
                solidHeader = TRUE,
                maximizable = TRUE,
                plotlyOutput("gtexGD2plot", height = "90vh") %>% 
                  withSpinner(., type = 7, color="#164863", size = 0.5, hide.ui = FALSE)
              ),
              box(
                width = 12,
                status = "primary",
                collapsible = FALSE,
                tagList(includeMarkdown(
                  system.file("extdata", "gtex_citation.md", package = "GD2Viz")
                ))
              )
            ),
            tabPanel(
              "TARGET",
              box(
                id = "",
                width = 12,
                height = "auto",
                title = "Plot Settings",
                status = "danger",
                solidHeader = FALSE,
                maximizable = FALSE,
                fluidRow(
                  column(3, selectInput(
                    "targetGD2ScoreRange",
                    "Range Values [0-1]:",
                    choices = c("yes", "no"),
                    selected = "no"
                  )),
                  column(3, selectInput(
                    "targetGD2ScorePlotType",
                    "Select Plot Type:",
                    choices = c("scatter", "box", "violin"),
                    selected = "scatter"
                  )),
                  column(3, uiOutput("targetColDataUI")),
                  column(3, uiOutput("targetHighlightGroupUI"))
                )
              ),
              box(
                id = "targetGD2ScoreBox",
                width = 12,
                height = "auto",
                title = "TARGET",
                status = "primary",
                solidHeader = TRUE,
                maximizable = TRUE,
                plotlyOutput("targetGD2plot", height = "90vh") %>% 
                  withSpinner(., type = 7, color="#164863", size = 0.5, hide.ui = FALSE)
              ),
              box(
                width = 12,
                status = "primary",
                collapsible = FALSE,
                tagList(includeMarkdown(
                  system.file("extdata", "target_citation.md", package = "GD2Viz")
                ))
              )
            ),
            tabPanel(
              "St. Jude Cloud",
              box(
                id = "",
                width = 12,
                height = "auto",
                title = "Plot Settings",
                status = "danger",
                solidHeader = FALSE,
                maximizable = FALSE,
                fluidRow(
                  column(3, selectInput(
                    "stjudeGD2ScoreRange",
                    "Range Values [0-1]:",
                    choices = c("yes", "no"),
                    selected = "no"
                  )),
                  column(3, selectInput(
                    "stjudeGD2ScorePlotType",
                    "Select Plot Type:",
                    choices = c("scatter", "box", "violin"),
                    selected = "scatter"
                  )),
                  column(3, uiOutput("stjudeColDataUI")),
                  column(3, uiOutput("stjudeHighlightGroupUI"))
                )
              ),
              box(
                id = "stjudeGD2ScoreBox",
                width = 12,
                height = "auto",
                title = "St. Jude Cloud",
                status = "primary",
                solidHeader = TRUE,
                maximizable = TRUE,
                plotlyOutput("stjudeGD2plot", height = "90vh") %>% 
                  withSpinner(., type = 7, color="#164863", size = 0.5, hide.ui = FALSE)
              ),
              box(
                width = 12,
                status = "primary",
                collapsible = FALSE,
                tagList(includeMarkdown(
                  system.file("extdata", "stjude_citation.md", package = "GD2Viz")
                ))
              )
            ),
            tabPanel(
              "CBTTC",
              box(
                id = "",
                width = 12,
                height = "auto",
                title = "Plot Settings",
                status = "danger",
                solidHeader = FALSE,
                maximizable = FALSE,
                fluidRow(
                  column(3, selectInput(
                    "cbttcGD2ScoreRange",
                    "Range Values [0-1]:",
                    choices = c("yes", "no"),
                    selected = "no"
                  )),
                  column(3, selectInput(
                    "cbttcGD2ScorePlotType",
                    "Select Plot Type:",
                    choices = c("scatter", "box", "violin"),
                    selected = "scatter"
                  )),
                  column(3, uiOutput("cbttcColDataUI")),
                  column(3, uiOutput("cbttcHighlightGroupUI"))
                )
              ),
              box(
                id = "cbttcGD2ScoreBox",
                width = 12,
                height = "auto",
                title = "Pediatric Brain Tumor Atlas: CBTTC",
                status = "primary",
                solidHeader = TRUE,
                maximizable = TRUE,
                plotlyOutput("cbttcGD2plot", height = "90vh") %>% withSpinner(., type = 7, color="#164863", size = 0.5, hide.ui = FALSE)
              ),
              box(
                width = 12,
                status = "primary",
                collapsible = FALSE,
                tagList(includeMarkdown(
                  system.file("extdata", "cbttc_citation.md", package = "GD2Viz")
                ))
              )
            )
          )
        )
      ),
      ### TCGA Details tab -------------------
      tabItem(
        tabName = "tcgaDetailTab",
        fluidRow(
          h2("Explore the GD2 Score of TCGA subgroups"), br(),br(),
          box(
            width = 12,
            title = "Global Settings",
            status = "danger",
            solidHeader = FALSE,
            fluidRow(
              column(4,
                     radioButtons(
                       "tcgaTabScale", 
                       "Choose RAS processing:",
                       choices = c("raw", "ranged", "scaled"),
                       selected = "raw"
                     ),
                     selectInput(
                       "tcgaTabScoreRange",
                       "Range Values [0-1]:",
                       choices = c("yes", "no"),
                       selected = "no"
                     )
              ),
              column(4,
                     radioButtons(
                       "tcgaTabRASType", 
                       "Visuaize RAS type",
                       choices = list(
                         "unadjusted RAS" = "ras", 
                         "RAS adj. by transision prob." = "ras_prob", 
                         "RAS adj. by path-based transition probability" = "ras_prob_path", 
                         "RAS adj. by recurive transition probability" = "ras_prob_rec"),
                       selected = "ras_prob")
              ),
              column(4,
                     uiOutput("tcgaTabGroupUI"),
                     uiOutput("tcgaTabCnvUI")
              )
            )
          )
        ),
        fluidRow(
          box(
            width = 4,
            title = "Select TCGA Project:",
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
            DT::dataTableOutput("tcgaProjectsTbl")
          ),
          box(
            width = 8,
            title = "GD2 Score of TCGA Project:",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("tcgaDetailGD2plot", height = "90vh") %>% withSpinner(., type = 7, color="#164863", size = 0.5, hide.ui = FALSE)
          )
        )
      ),
      ### Custom data tab -------------------
      tabItem(
        tabName = "customDataTab",
        fluidRow(
          box(
            width = 3,
            title = "1. Data Input",
            status = "warning", 
            solidHeader = FALSE,
               
            radioButtons("dataType", "Choose Data Type:", 
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
              
              radioButtons("customScale", "Choose RAS processing:", 
                           choices = c("raw", "ranged", "scaled"),
                           selected = "raw"),
              bs4Dash::actionButton("computeCustomScore", "Compute GD2 Score", status = "info"),
              uiOutput("customGD2messageUI") %>% withSpinner(., type = 7, color="#164863", size = 0.5, hide.ui = FALSE)
          ),
          box(width = 3, title = "3. Global Plot Settings", solidHeader = FALSE, status = "olive",
              radioButtons("customRASType", "Visuaize RAS type", 
                           choices = list("unadjusted RAS" = "ras", "RAS adj. by transision prob." = "ras_prob", "RAS adj. by path-based transition probability" = "ras_prob_path", "RAS adj. by recurive transition probability" = "ras_prob_rec"), 
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
              title = "Reaction Activity Scores", 
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
              title = "GD2 Promoting & Diminishing Reaction Activity", 
              height = "500px", 
              status = "primary", 
              solidHeader = TRUE,
              plotlyOutput("customInOutplot") %>% 
                withSpinner(., type = 7, color="#164863", size = 0.5, hide.ui = FALSE)
          ),
          box(id = "customGD2ScoreBox",
              width = 6, 
              title = "GD2 Score", 
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
                  selected = "scatter"
                ),
                uiOutput("customGD2ScorePlotTGeneUI")
              ),
              plotlyOutput("customGD2Score") %>% 
                withSpinner(., type = 7, color="#164863", size = 0.5, hide.ui = FALSE)
          )
        ), 
        fluidRow(
          box(width = 12, 
              height = "170px",
              title = "Compare Two Groups", 
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
        )
      )
    )
  )
)

