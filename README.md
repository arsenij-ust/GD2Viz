<img src="https://github.com/arsenij-ust/GD2Viz/blob/main/inst/www/GD2Viz6.png" alt="GD2Viz Logo" width="200px"/>

# Welcome to GD2Viz

Welcome to **GD2Viz**, a powerful and user-friendly visualization tool designed to help researchers and clinicians explore, analyze, and interpret GD2 Scores across various RNA-Seq datasets. Utilizing the advanced methodology outlined by Ustjanzew et al. (2024), **GD2Viz** offers an in-depth examination of precomputed GD2 Scores from publicly available RNA-Seq datasets such as TCGA, GTEx, and TARGET. Users also have the flexibility to upload and analyze their own datasets within the context of GD2 Scores.

This R Shiny application version of **GD2Viz** provides a range of interactive visualizations and preloaded datasets. Additionally, the **GD2Viz** R package includes several functions for computing Reaction Activity Scores of glycosphingolipid metabolism and predicting GD2 Scores directly within the R environment. For comprehensive guidance, refer to the [**GD2Viz Vignette**](#).

---

## Key Features

- **Interactive Visualizations**: Generate dynamic, real-time interactive plots, heatmaps, and network diagrams to thoroughly explore your data.
- **GD2 Score Analysis for Large Datasets**: Investigate GD2 Scores across extensive datasets like TCGA, TARGET, and GTEx. Dive deep into individual projects within the TCGA dataset and analyze GD2 Scores alongside various sample metadata.
- **Predict GD2 Scores for Your Datasets**: Effortlessly compute Reaction Activity Scores and GD2 Scores for your datasets, with options to visualize and download the results.
- **User-Friendly Interface**: Experience smooth navigation with our intuitive and thoughtfully designed user interface.
- **Group Comparison**: Compare two groups or conditions within your dataset to observe log-fold changes in Reaction Activity Scores of glycosphingolipid metabolism.

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

For detailed instructions and examples on how to use the app, please refer to the [GD2Viz Vignette](#).

If you want to use the functions in the R environment, please refer to the second [GD2Viz Vignette](#).

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