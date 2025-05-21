<a href="https://arsenij-ust.github.io/GD2Viz/index.html"><img src="https://raw.githubusercontent.com/arsenij-ust/GD2Viz/refs/heads/main/inst/www/GD2Viz6.png" alt="GD2Viz Logo" width="200px"/></a>

# Welcome to GD2Viz

Welcome to **GD2Viz**, a powerful and user-friendly visualization tool designed to help researchers and clinicians explore, analyze, and interpret GD2 Scores across various RNA-Seq datasets. Utilizing the advanced methodology outlined by Ustjanzew et al. (2024), **GD2Viz** offers an in-depth examination of precomputed GD2 Scores from publicly available RNA-Seq datasets such as TCGA, GTEx, and TARGET. Users also have the flexibility to upload and analyze their own datasets within the context of GD2 Scores.

**GD2Viz** provides a range of interactive visualizations and precomputed RNA-seq datasets. Additionally, the **GD2Viz** R package includes several functions for computing Reaction Activity Scores of glycosphingolipid metabolism and predicting GD2 Scores directly within the R environment. For comprehensive guidance, refer to the articles at [**GD2Viz webpage**](https://arsenij-ust.github.io/GD2Viz/index.html).

> ⚠️ **Note:** The local version **does not** include access to precomputed public datasets (e.g., TCGA, GTEx, TARGET). To explore GD2 Scores across these datasets, please use the online version of the app, available at:
> [http://shiny.imbei.uni-mainz.de:3838/GD2Viz](http://shiny.imbei.uni-mainz.de:3838/GD2Viz)

---

## 🔑 Key Features

**GD2Viz** provides a comprehensive framework for the analysis, visualization, and interpretation of GD2 expression potential across bulk RNA-Seq datasets, with a focus on glycosphingolipid metabolism. The package includes both an interactive Shiny application and a set of R functions for programmatic use.

### Predictive GD2 Scoring

* Implements a trained Support Vector Machine (SVM) model to infer GD2 Scores from transcriptomic data.
* Integrates pathway informed features from glycosphingolipid metabolism via Reaction Activity Scores (RAS).

### Visualization & Exploration

* **Interactive Shiny App**: Explore GD2 Scores across large public datasets [online](http://shiny.imbei.uni-mainz.de:3838/GD2Viz) (TCGA, GTEx, TARGET, St. Jude Cloud, CBTTC).
* Visualize GD2 Scores using scatter, box, and violin plots grouped by clinical or molecular subtypes.
* Heatmaps, scatter plots, and pathway diagrams to interpret RAS distributions.

### Custom Dataset Analysis

* Upload your own RNA-Seq data and compute RAS and GD2 Scores locally.
* Supports input as raw count matrices  with metadata (as .tsv files) or `DESeqDataSet` objects.

### Differential Expression & Group Comparison

* Perform GD2-based sample stratification (e.g., high vs. low) and run DEA directly in the app or programmatically.
* Visualize DEA results with volcano plots, MA plots, p-value and log2 fold-change histograms.
* Searchable gene tables with built-in annotation and gene-specific expression profiles.

### Pathway-Specific Insights

* Focused analysis of the ganglioside branch of glycosphingolipid metabolism.
* Log2 fold-change plots of RAS values between experimental groups.
* Network-style visualizations of ganglioside pathway activity changes.

### Programmatic Interface (R Package)

* Compute RAS and predict GD2 Scores in batch mode for custom analyses.
* Seamless integration with Bioconductor workflows and standard transcriptomics formats.

### Documentation

> ⚠️ For detailed instructions and examples on how to use the app, please refer to the vignette [GD2Viz app - Explore public datasets and analyze your data](https://arsenij-ust.github.io/GD2Viz/articles/GD2Viz-app.html).

> ⚠️ If you want to use the core functions in your R environment, please refer to the vignette "[Computing GD2 Scores Programmatically Using GD2Viz](https://arsenij-ust.github.io/GD2Viz/articles/Predict-GD2-Score.html)".

---

## Installation

You can install the **GD2Viz** package from GitHub using the following commands in R:

```R
# Install the devtools package if you haven't already
install.packages("devtools")

# Use devtools to install GD2Viz from GitHub
devtools::install_github("arsenij-ust/GD2Viz")
```

or using the `remotes` package:

```r
install.packages("remotes")
remotes::install_github("arsenij-ust/GD2Viz")
```

---

## Usage

After installation, you can load the package and launch the app:

```R
library(GD2Viz)

# Launch the Shiny application
GD2Viz()
```

---

## Development Team

**GD2Viz** was developed at the Institute for Medical Biostatistics, Epidemiology, and Informatics (IMBEI) of the University Medical Center of the Johannes Gutenberg University Mainz. The development team includes:

- **Arsenij Ustjanzew**: Developer
- **Federico Marini**: Developer
- **Claudia Paret**: Methodological and Clinical Support

---

## **Cite us**:

Ustjanzew et al. Predicting GD2 expression across cancer types by the integration of pathway topology and transcriptome data. 2025

---

For more information, visit our [website](http://www.unimedizin-mainz.de/imbei) or consult the [GD2Viz Vignette](#). If you have any questions or need assistance, please don't hesitate to reach out to our support team.

---

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.