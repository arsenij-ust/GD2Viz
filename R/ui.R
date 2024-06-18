
# UI definition -----------------------------------------------------------
gd2vis_ui = dashboardPage(
  title = "GD2Viz",
  dark = NULL,
  help = NULL,
  freshTheme = theme,

  ## Header----------------------------------------------------------
  header = dashboardHeader(
    # title = HTML("<small>GD2Viz</small>"),
    # src = "GD2Viz/GD2Viz.png"
    skin = "light",
    #status = "white",
    border = TRUE,
    sidebarIcon = icon("bars"),
    controlbarIcon = icon("th"),
    fixed = TRUE,
    leftUi = tagList(
      dropdownMenu(
        badgeStatus = "info",
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
          status = "primary",
          icon = shiny::icon("book-open")
        ),
        notificationItem(
          inputId = "notification2",
          text = "About this session",
          status = "primary",
          icon = shiny::icon("info")
        ),
        notificationItem(
          inputId = "notification3",
          text = "About GD2Viz",
          status = "primary",
          icon = shiny::icon("heart")
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
      img(id = "resizable-image", src = "GD2Viz/GD2Viz5.png",
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
        tabName = "welcome_tab",
        icon = icon("house")
      )
    ),
    sidebarMenu(
      sidebarHeader("Main tabs:"),
      menuItem(
        "Explore datasets",
        tabName = "explore_data_tab",
        icon = icon("database")
      ),
      menuItem(
        "TCGA subgroups",
        tabName = "tcga_detail_tab",
        icon = icon("magnifying-glass-plus")
      ),
      menuItem(
        "Custom dataset",
        tabName = "custom_data_tab",
        icon = icon("file-arrow-up")
      )
    )
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
           img(src = "GD2Viz/GD2Viz5.png", height = "50px")
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
    shinyjs::useShinyjs(),
    
    tabItems(
      ### Welcome tab -------------------
      tabItem(
        tabName = "welcome_tab",
        fluidRow(
          box(
            title = "Welcome to GD2Viz", status = "primary", solidHeader = TRUE, width = 12,
            collapsible = TRUE,
            tagList(includeMarkdown(
              system.file("extdata", "welcome.md", package = "GD2Viz")
            ))
          ),
          box(
            title = "GD2 score and model building", status = "primary", solidHeader = TRUE, width = 12,
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
            id = "tabset_welcometab",
            tabPanel("Custom dataset",
                     tagList(includeMarkdown(
                       system.file("extdata", "custom_data_description.md", package = "GD2Viz")
                     )),
            ),
            tabPanel("TCGA subgroups",
                     tagList(includeMarkdown(
                       system.file("extdata", "tcga_description.md", package = "GD2Viz")
                     )),
            ),
            tabPanel("Explore datasets", 
                     tagList(includeMarkdown(
                       system.file("extdata", "datasets_description.md", package = "GD2Viz")
                     ))
            )
          )
        )
      ),
      ### Custom data tab -------------------
      tabItem(tabName = "custom_data_tab",
        fluidRow(
          box(width = 4, title = "1. Data Input", status = "warning", solidHeader = FALSE,
               
            radioButtons("dataType", "Choose data type:", 
                         choices = list("count matrix & metadata" = "count_meta", 
                                        "DESeqDataSet object" = "dds"),
                         selected = "count_meta"),
          
            conditionalPanel(
              condition = "input.dataType == 'count_meta'",
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
          box(width = 4, title = "2. Model settings", status = "info", solidHeader = FALSE,
              
              radioButtons("customScale", "Choose RAS processing:", 
                           choices = c("raw", "ranged", "scaled"),
                           selected = "raw"),
              bs4Dash::actionButton("computeCustomScore", "Compute GD2 Score", status = "info"),
              uiOutput("customGD2messageUI") %>% withSpinner(., type = 7, color="#164863", size = 0.5, hide.ui = FALSE)
          ),
          box(width = 4, title = "3. Plot Settings", solidHeader = FALSE, status = "olive",
              radioButtons("customRASType", "Visuaize RAS type", 
                           choices = list("unadjusted RAS" = "ras", "RAS adj. by transision prob." = "ras_prob", "RAS adj. by path-based transition probability" = "ras_prob_path", "RAS adj. by recurive transition probability" = "ras_prob_rec"), 
                           selected = "ras_prob"),
              uiOutput("selectCustomGroupUI")
              # bs4Dash::actionButton("updateCustoPlots", "Update plots", status = "success")
          ),
        ),
        
        fluidRow(
          box(width = 6, title = "Plot 1", status = "primary", solidHeader = TRUE,
              plotlyOutput("plot1")
          ),
          box(width = 6, title = "Plot 2", status = "primary", solidHeader = TRUE,
              plotlyOutput("plot2")
          )
        )
      )
    )
  )
)

