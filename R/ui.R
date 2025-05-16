
# UI definition -----------------------------------------------------------
gd2visUI <- function(has_private_data = TRUE) {
  
  tabs_list <- list(
    get_welcome_tab(has_private_data),
    get_customData_tab()
  )
  
  if (has_private_data) {
    tabs_list <- append(tabs_list, list(
      get_publicData_tab(),
      get_TCGAData_tab()
    ))
  }
  
  
  dashboardPage(
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
      # Conditional menu items
      if (has_private_data) {
        list(
          menuItem(
            "Public Datasets",
            tabName = "exploreDataTab",
            icon = icon("database")
          ),
          menuItem(
            "TCGA Cancer Types",
            tabName = "tcgaDetailTab",
            icon = icon("magnifying-glass-plus")
          )
        )
      },
      menuItem(
        "Analyze Your Data",
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
           href = "#",
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
         tags$a(href = "https://github.com/arsenij-ust/GD2Viz", "GitHub")
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
    do.call(tabItems, tabs_list)
  )
)}

