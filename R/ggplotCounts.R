##################################################
#  Function used from ideal Bioconductor package #
#  Author: Federico Marini                       #
##################################################

#' Plot normalized counts for a gene
#'
#' Plot for normalized counts of a single gene, with jittered points superimposed
#' on the boxplot
#'
#' Note: this function relies on the \code{\link{plotCounts}} function of DESeq2,
#' therefore pseudocounts of 0.5 are added to each point
#'
#' @param dds A \code{\link{DESeqDataSet}} object.
#' @param gene A character, specifying the name of the gene to plot
#' @param intgroup Interesting groups: a character vector of
#' names in \code{colData(dds)} to use for grouping
#' @param annotation_obj A \code{data.frame} object, with \code{row.names} as gene
#' identifiers (e.g. ENSEMBL ids) and a column, \code{gene_name}, containing
#' e.g. HGNC-based gene symbols. Optional.
#' @param transform Logical value, corresponding whether to have log scale y-axis
#' or not. Defaults to TRUE.
#' @param labels_repel Logical value. Whether to use \code{ggrepel}'s functions to 
#' place labels; defaults to TRUE.
#'
#' @return An object created by \code{ggplot}
#' 
#' @importFrom ggrepel geom_text_repel
#' @export
#'
#' @examples
#' library(airway)
#' data(airway)
#' airway
#' dds_airway <- DESeq2::DESeqDataSetFromMatrix(assay(airway),
#'                                              colData = colData(airway),
#'                                              design=~cell+dex)
#' ggplotCounts(dds_airway,
#'              gene = "ENSG00000103196", # CRISPLD2 in the original publication
#'              intgroup = "dex")
#'
#'
#'
#'
ggplotCounts <- function(dds,gene,intgroup="condition",annotation_obj=NULL,
                         transform = TRUE, labels_repel = TRUE){
  df <- plotCounts(dds,gene,intgroup,returnData = TRUE)
  df$sampleID <- rownames(df)
  
  if(!is.null(annotation_obj))
    genesymbol <- annotation_obj$gene_name[match(gene,annotation_obj$gene_id)]
  else
    genesymbol <- ""
  
  jittered_df <- df
  # jittered_df$conditionj <- jitter(as.numeric(factor(jittered_df$condition)))
  jittered_df$countj <- jitter(jittered_df$count)
  
  onlyfactors <- df[,match(intgroup,colnames(df))]
  df$plotby <- interaction(onlyfactors)
  
  # base_breaks <- function(n = 10){
  #   function(x) {
  #     axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, nint = n)
  #   }
  # }
  
  p <-
    ggplot(df, aes_string(x="plotby",y="count",col="plotby")) +
    geom_boxplot(outlier.shape = NA) +
    # geom_text(data = jittered_df,aes(x=conditionj,y=countj,label=sampleID)) +
    scale_x_discrete(name="") +
    geom_jitter(aes_string(x="plotby",y="count"),
                position = position_jitter(width = 0.1)) +
    scale_color_discrete(name="Experimental\nconditions")
  
  if(labels_repel){
    p <- p + ggrepel::geom_text_repel(aes_string(label="sampleID"))
  } else {
    p <- p + geom_text(aes_string(label="sampleID"),hjust=-.1,vjust=0)
  }
  
  if(transform)
    p <- p + scale_y_log10(name="Normalized counts (log10 scale)")
  else
    p <- p + scale_y_continuous(name="Normalized counts")
  
  # scale_y_log10(name="Normalized counts - log10 scale") +
  # coord_cartesian(ylim = c())# ,limits=c(0.1,NA)) +
  p <- p + theme_bw()
  
  if(!is.null(annotation_obj))
    p <- p + labs(title=paste0("Normalized counts for ",genesymbol," - ",gene))
  else
    p <- p + labs(title=paste0("Normalized counts for ",gene))
  
  p
  
}