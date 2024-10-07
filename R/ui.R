
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
        "TCGA Project Exploration",
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
    fresh::use_theme(
      fresh::create_theme(
        bs4dash_status(
          primary = "#164863", danger = "#ff851b", light = "#272c30", warning = "#ff851b"
        ),
        bs4dash_vars(
          navbar_light_color = "#164863",
          navbar_light_active_color = "#ff851b",
          navbar_light_hover_color = "#ff851b"
        ),
        bs4dash_yiq(
          contrasted_threshold = 10,
          text_dark = "#FFF",
          text_light = "#272c30"
        ),
        # # bs4dash_layout(
        # #   main_bg = "#9BBEC8"
        # # ),
        # # bs4dash_sidebar_light(
        # #   bg = "#164863",
        # #   color = "#DDF2FD",
        # #   hover_color = "#FFF",
        # #   submenu_bg = "#427D9D",
        # #   submenu_color = "#427D9D",
        # #   submenu_hover_color = "#FFF"
        # # ),
        bs4dash_color(
          blue = "#427D9D",
          lightblue = NULL,
          navy = "#164863",
          cyan = NULL,
          teal = NULL,
          olive = NULL,
          green = NULL,
          lime = NULL,
          orange = NULL,
          yellow = NULL,
          fuchsia = NULL,
          purple = NULL,
          maroon = NULL,
          red = NULL,
          black = NULL,
          gray_x_light = NULL,
          gray_600 = NULL,
          gray_800 = NULL,
          gray_900 = NULL,
          white = NULL
        )
      )
    ),
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
              system.file("extdata/documentation", "welcome.md", package = "GD2Viz")
            ))
          ),
          box(
            title = "Background Information", status = "primary", solidHeader = TRUE, width = 12,
            imageOutput("visual_abstract", height="auto"),
            collapsible = TRUE,
            accordion(
              id = "welcome_accordion",
              width = 12,
              .list = 
                list(
                  accordionItem(
                    title = "What is GD2?",
                    p("Gangliosides are sialylated glycosphingolipids (GSLs) crucial to the nervous system, with GD2 being a significant member. GD2, a simple ganglioside, is predominantly expressed in embryonic tissues and certain neuroblastic tumors, such as neuroblastoma (NB). Its expression is linked to the undifferentiated state of neural stem cells and NB. GD2 is particularly important because it serves as a target for immunotherapies, including monoclonal antibodies like Dinutuximab and Naxitamab, and CAR-T cell treatments, which are pivotal in treating high-risk NB. Consequently, understanding GD2's role and its expression in tumors can enhance therapeutic strategies and improve patient outcomes not only in neuroblastoma but also in other tumor entities."),
                    status = NULL,
                    collapsed = TRUE,
                    solidHeader = FALSE
                  ),
                  accordionItem(
                    title = "Metabolic network of GSL",
                    p("We created a graph where nodes are metabolites and edges represent reactions. Reactions describe which enzymes are involved in the respective metabolic step. This graph includes four pathways from the KEGG database related to sphingolipid and glycosphingolipid metabolism, including the lacto-, neolacto-, globo-, and ganglio series. The reactions representing the degradation of GM1 to GM2 and GM2 to GM3 were removed from the graph. This is justified because these reactions represent the degradation process and are not competing enzymatic processes of the biosynthesis. Several enzymes in this metabolic network are specific to GSLs, though some are also involved in other lipid-related pathways."),
                    status = NULL,
                    collapsed = TRUE,
                    solidHeader = FALSE
                  ),
                  accordionItem(
                    title = "What is Reaction Activity Score (RAS)?",
                    p("The graph is weighted using Reaction Activity Scores (RAS), calculated from gene expression data. Some reactions of the graph have more than one emzyme involved, then it is useful to compute the RAS values. RAS values depend on whether the enzymes of a reaction are subunits and work only in presence of all enzyme subunits (AND relation) or as independent enzymes (OR relation). In case of an AND relation, the minimal gene expression value of the involved reactions is used as the RAS value. If the enzymes of a reaction are in an OR relation, the RAS value is computed as the sum of all involved gene expression values. These values weight the graph's edges, creating a directed graph with metabolites as nodes."),
                    p("Early steps in the ganglioside pathway are performed with enzymes of relative high substrate specificity, whereas downstream enzymes are promiscuitive and elongate in the parallel series of this pathway. To adjust for identical RAS values we used the topological information of transition probabilities (TP), three methods were developed: the 'TP adjustment', 'recursively adjusted RAS' and 'path-based RAS adjustment.' For the simple 'TP adjustment' we compute the TPs from one node to the next following node(s) proportional to the RAS values of the outgoing edge(s) and multiply the RAS values of the edges with the TP values. 'Recursively adjusted RAS' replaces TP values that are equal to 1 by recursively prolongate the TP value from previous edges in the chain, that are not equal to 1, while 'path-based RAS adjustment' sums transition probabilities along all paths from a starting node. Lactosylceramide was chosen as the starting node for this calculation. For further details on the used methods, see 'Ustjanzew et al., Unraveling the Glycosphingolipid Metabolism by Leveraging Transcriptome-weighted Network Analysis on Neuroblastic Tumors. Cancer and Metabolism, 2024.'"),
                    status = NULL,
                    collapsed = TRUE,
                    solidHeader = FALSE
                  ),
                  accordionItem(
                    title = "Model Building for GD2 Score",
                    p("For the model, we combined and normalized RNA-seq data from two sources: TARGET Neuroblastoma samples and the GTEx dataset. After weighting the graph's edges per sample with the (adjusted) RAS values, the next step was to identify GD2-mitigating and GD2-promoting reactions, as shown in the figure above (step 3). For each sample, we calculated the sum of GD2-mitigating and GD2-promoting reactions and used these scores to train a Support Vector Machine (SVM) with a linear kernel to differentiate between Neuroblastoma and other GTEx tissues."),
                    p("It is important to note that the goal was not to create a perfect discriminator model. Since GD2 concentration is a continuous variable, we use the sample's distance from the hyperplane as the GD2 score. If a sample's GD2 score is positive or slightly below zero, it suggests a higher accumulation of GD2 in that sample, based solely on the RNA-seq data."),
                    status = NULL,
                    collapsed = TRUE,
                    solidHeader = FALSE
                  ),
                  accordionItem(
                    title = "GD2 Score Prediction",
                    p("We have to normalize the RNA-seq dataset (test dataset) we use to predict the GD2 score based on the training dataset. Therefore, we use DESeq median ratio normalization method, where the size factor of a test case will be estimated using gene-wise geometric means from training set. The normalized count data is further processed to RAS-values as described previously. The sum of GD2-mitigating and GD2-promoting reactions is computed for each sample and used as input for the Support Vector Machne model to predict the GD2 score."),
                    status = NULL,
                    collapsed = TRUE,
                    solidHeader = FALSE
                  )
                )
            )
            # tagList(includeMarkdown(
            #   system.file("extdata/documentation", "gd2score.md", package = "GD2Viz")
            # ))
          ),
          box(
            title = "Gatting started with GD2Viz", status = "primary", solidHeader = TRUE, width = 12,
            collapsible = TRUE,
            accordion(
              id = "tabdescription_accordion",
              width = 12,
              .list =
                list(
                  accordionItem(
                    title = "Tab: Explore datasets",
                    tagList(includeMarkdown(
                      system.file("extdata/documentation", "datasets_description.md", package = "GD2Viz")
                    )),
                    status = NULL,
                    collapsed = TRUE,
                    solidHeader = FALSE
                  ),
                  accordionItem(
                    title = "Tab: TCGA Project Exploration",
                    tagList(includeMarkdown(
                      system.file("extdata/documentation", "tcga_description.md", package = "GD2Viz")
                    )),
                    status = NULL,
                    collapsed = TRUE,
                    solidHeader = FALSE
                  ),
                  accordionItem(
                    title = "Tab: Custom dataset",
                    tagList(includeMarkdown(
                      system.file("extdata/documentation", "custom_data_description.md", package = "GD2Viz")
                    )),
                    status = NULL,
                    collapsed = TRUE,
                    solidHeader = FALSE
                  )
              )
            )
            # tagList(includeMarkdown(
            #   system.file("extdata/documentation", "gd2score.md", package = "GD2Viz")
            # ))
          )
        )
      ),
      ### Datasets tab -------------------
      tabItem(
        tabName = "exploreDataTab",
        fluidRow(
          column(
            width = 12,
            h3("Explore the GD2 Score of large RNA-Seq datasets")
          )
        ),
        fluidRow(
          # h2("Explore the GD2 Score of large RNA-Seq datasets"), br(),br(),
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
                  system.file("extdata/documentation", "tcga_citation.md", package = "GD2Viz")
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
                  system.file("extdata/documentation", "tcga_citation.md", package = "GD2Viz")
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
                  system.file("extdata/documentation", "gtex_citation.md", package = "GD2Viz")
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
                  system.file("extdata/documentation", "target_citation.md", package = "GD2Viz")
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
                  system.file("extdata/documentation", "stjude_citation.md", package = "GD2Viz")
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
                  system.file("extdata/documentation", "cbttc_citation.md", package = "GD2Viz")
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
              column(4,
                     # h4("Subset TCGA data:"),
                     # uiOutput("tcgaTabDGEProjectUI"),
                     # uiOutput("tcgaTabDGEColDataUI"),
                     uiOutput("tcgaTabDGESubtypeUI"),
                     fluidRow(
                       column(6,valueBoxOutput("ProjectSampleNr", width = 12)),
                       column(6,valueBoxOutput("SubgroupSampleNr", width = 12))
                     )
                     ),
              column(4,
                     selectInput(
                       "tcgaTabDGEStratMethodSel",
                       "2. Select stratification method:",
                       choices = list("Median"="m", "One Threshold"="t", "Upper/Lower Percentiles"="q"),
                       selected = "m"
                     ),
                     uiOutput("tcgaTabDGEMethodUI"),
                     # uiOutput("tcgaTabDGESampleNrUI")
                     fluidRow(
                       column(6,valueBoxOutput("GD2LowSampleNr", width = 12)),
                       column(6,valueBoxOutput("GD2HighSampleNr", width = 12))
                     )
                     ),
              column(4,
                     numericInput(
                       "tcgaTabDGEFDR",
                       "False Discovery Rate",
                       value = 0.05,
                       min = 0,
                       max = 1,
                       step = 0.01),
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
                     ),
                     # selectInput(
                     #   "tcgaTabDGEParallel",
                     #   "Use parallel execution of DESeq function using BiocParallel:",
                     #   choices = c("TRUE", "FALSE"),
                     #   selected = "FALSE"
                     # ),
                     bs4Dash::actionButton("tcgaTabDGECompute", "Run Differential Expression Analysis", status = "warning")
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
            title = "DEA Genes",
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
            title = "p-Value Histogram",
            status = "primary",
            solidHeader = TRUE,
            plotOutput("pvals_hist") %>%
              withSpinner(., type = 7, color="#164863", size = 0.5, hide.ui = FALSE)
          ),
          box(
            width = 6,
            title = "Histogram of the Log2 Fold-Changes",
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
            title = "MA plot",
            status = "primary",
            solidHeader = TRUE,
            plotOutput("plotma") %>%
              withSpinner(., type = 7, color="#164863", size = 0.5, hide.ui = FALSE)
          ),
          box(
            width = 6,
            title = "Volcano plot",
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
      ),
      ### Custom data tab -------------------
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
              plotlyOutput("customInOutplot", height = "auto") %>%
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
                                  choices = list("unadjusted RAS" = "ras", "RAS adj. by transision prob." = "ras_prob", "RAS adj. by path-based transition probability" = "ras_prob_path", "RAS adj. by recurive transition probability" = "ras_prob_rec"),
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
                     numericInput(
                       "customTabDGEFDR",
                       "False Discovery Rate",
                       value = 0.05,
                       min = 0,
                       max = 1,
                       step = 0.01),
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
                     ),
                     # selectInput(
                     #   "tcgaTabDGEParallel",
                     #   "Use parallel execution of DESeq function using BiocParallel:",
                     #   choices = c("TRUE", "FALSE"),
                     #   selected = "FALSE"
                     # ),
                     bs4Dash::actionButton("customTabDGECompute", "Run Differential Expression Analysis", status = "warning")
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
            title = "DEA Genes",
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
            title = "p-Value Histogram",
            status = "primary",
            solidHeader = TRUE,
            plotOutput("custom_pvals_hist") %>%
              withSpinner(., type = 7, color="#164863", size = 0.5, hide.ui = FALSE)
          ),
          box(
            width = 6,
            title = "Histogram of the Log2 Fold-Changes",
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
            title = "MA plot",
            status = "primary",
            solidHeader = TRUE,
            plotOutput("custom_plotma") %>%
              withSpinner(., type = 7, color="#164863", size = 0.5, hide.ui = FALSE)
          ),
          box(
            width = 6,
            title = "Volcano plot",
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
    )
  )
)

