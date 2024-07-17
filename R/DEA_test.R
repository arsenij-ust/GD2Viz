# library(DESeq2)
# 
# # Parameters
# project <- "BRCA"
# metadata <- "Subtype_mRNA"
# subtype <- "Basal"
# stratMethod <- "median" # threshold, quantille
# FDR <- 0.05
# 
# 
# TCGA_dds <- readRDS(system.file("extdata/TCGA_projects", paste0("TCGA-",project,".Rds"), package = "GD2Viz"))
# 
# # TODO check sample number (prevent low sample numbers y stopping)
# 
# TCGA_dds <- TCGA_dds[,which(colData(TCGA_dds)[[metadata]] == subtype)]
# 
# prediction <- rnorm(nrow(colData(TCGA_dds)), mean=-2, sd=6)
# 
# colData(TCGA_dds)$GD2Score <- prediction
# 
# # stratify by method
# 
# if(stratMethod == "median"){
#   threshold <- median(colData(TCGA_dds)$GD2Score)
#   colData(TCGA_dds)$strat <- ifelse(colData(TCGA_dds)$GD2Score <= threshold, "GD2_Low", "GD2_High")
# }
# 
# colData(TCGA_dds)$strat <- as.factor(colData(TCGA_dds)$strat)
# 
# design(TCGA_dds) <- ~strat
# 
# keep <- rowSums(counts(TCGA_dds)) >= 10
# TCGA_dds <- TCGA_dds[keep,]
# TCGA_dds <- DESeq(TCGA_dds)
# 
# # resultsNames(TCGA_dds)
# res <- results(TCGA_dds, name = "strat_GD2_Low_vs_GD2_High")
# library(apeglm)
# res <- lfcShrink(TCGA_dds, coef="strat_GD2_Low_vs_GD2_High", type="apeglm", res=res)
# dea <- as.data.frame(res)
# summary(res)
# dea <- dea[-which(is.na(dea$padj)),]
# ideal::deseqresult2DEgenes(res)
