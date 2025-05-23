% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggplotCounts.R
\name{ggplotCounts}
\alias{ggplotCounts}
\title{Plot normalized counts for a gene}
\usage{
ggplotCounts(
  dds,
  gene,
  intgroup = "condition",
  annotation_obj = NULL,
  transform = TRUE,
  labels_repel = TRUE
)
}
\arguments{
\item{dds}{A \code{\link{DESeqDataSet}} object.}

\item{gene}{A character, specifying the name of the gene to plot}

\item{intgroup}{Interesting groups: a character vector of
names in \code{colData(dds)} to use for grouping}

\item{annotation_obj}{A \code{data.frame} object, with \code{row.names} as gene
identifiers (e.g. ENSEMBL ids) and a column, \code{gene_name}, containing
e.g. HGNC-based gene symbols. Optional.}

\item{transform}{Logical value, corresponding whether to have log scale y-axis
or not. Defaults to TRUE.}

\item{labels_repel}{Logical value. Whether to use \code{ggrepel}'s functions to
place labels; defaults to TRUE.}
}
\value{
An object created by \code{ggplot}
}
\description{
Plot for normalized counts of a single gene, with jittered points superimposed
on the boxplot
}
\details{
Note: this function relies on the \code{\link{plotCounts}} function of DESeq2,
therefore pseudocounts of 0.5 are added to each point
}
\examples{
library(airway)
data(airway)
airway
dds_airway <- DESeq2::DESeqDataSetFromMatrix(assay(airway),
                                             colData = colData(airway),
                                             design=~cell+dex)
ggplotCounts(dds_airway,
             gene = "ENSG00000103196", # CRISPLD2 in the original publication
             intgroup = "dex")




}
