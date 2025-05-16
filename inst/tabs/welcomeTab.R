get_welcome_tab <- function(has_private_data=FALSE) {
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
    if (!has_private_data) {
      box(
        status = "warning",
        title = "Limited Functionality",
        solidHeader = TRUE,
        "This lightweight GD2Viz version does not include access to external datasets, like TCGA, GTEx and TARGET. In this locally available version, you can only upload and analyze your own RNA-Seq datasets. You can access the extended version of GD2Viz online at 'http://shiny.imbei.uni-mainz.de:3838/GD2Viz'."
      )
    },
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
              p("Early steps in the ganglioside pathway are performed with enzymes of relative high substrate specificity, whereas downstream enzymes are promiscuitive and elongate in the parallel series of this pathway. To adjust for identical RAS values we used the topological information of transition probabilities (TP), three methods were developed: the 'TP adjustment', and 'recursively adjusted RAS'. For the simple 'TP adjustment' we compute the TPs from one node to the next following node(s) proportional to the RAS values of the outgoing edge(s) and multiply the RAS values of the edges with the TP values. 'Recursively adjusted RAS' replaces TP values that are equal to 1 by recursively prolongate the TP value from previous edges in the chain, that are not equal to 1. Lactosylceramide was chosen as the starting node for this calculation. For further details on the used methods, see 'Ustjanzew et al., Unraveling the Glycosphingolipid Metabolism by Leveraging Transcriptome-weighted Network Analysis on Neuroblastic Tumors. Cancer and Metabolism, 2024.'"),
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
    if (has_private_data) {
      box(
        title = "Getting started with GD2Viz", status = "primary", solidHeader = TRUE, width = 12,
        collapsible = TRUE,
        accordion(
          id = "tabdescription_accordion",
          width = 12,
          .list =
            list(
              accordionItem(
                title = "Tab: Public Datasets",
                tagList(includeMarkdown(
                  system.file("extdata/documentation", "datasets_description.md", package = "GD2Viz")
                )),
                status = NULL,
                collapsed = TRUE,
                solidHeader = FALSE
              ),
              accordionItem(
                title = "Tab: TCGA Cancer Types",
                tagList(includeMarkdown(
                  system.file("extdata/documentation", "tcga_description.md", package = "GD2Viz")
                )),
                status = NULL,
                collapsed = TRUE,
                solidHeader = FALSE
              ),
              accordionItem(
                title = "Tab: Analyze Your Data",
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
    } else {
      box(
        title = "Getting started with GD2Viz", status = "primary", solidHeader = TRUE, width = 12,
        collapsible = TRUE,
        accordion(
          id = "tabdescription_accordion",
          width = 12,
          .list =
            list(
              accordionItem(
                title = "Tab: Analyze Your Data",
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
    }
  )
)
}