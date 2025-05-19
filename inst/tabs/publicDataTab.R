get_publicData_tab <- function() {
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
                  message = "Select how to preprocess Reaction Activity Scores (RAS) before computing the GD2 score: Raw: Use unaltered RAS values. Ranged: Normalize each reaction to a [0, 1] scale across samples. Scaled: Standardize values (zero-mean).",
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
                  message =  "Choose how RAS values are adjusted for visualization: 1) Unadjusted RAS: Raw scores from gene expression. 2) Adjusted by transition prob.: Weighted by network topology. 3) Recursive adjustment: Refines weights through reaction chains.",
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
              "RAS adj. by recursive transition probability" = "ras_prob_rec"
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
  )

  
}
