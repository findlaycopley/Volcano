#' Create The Mutation Plot
#' @param DGE data.frame of different gene expression
#' @param Names TRUE/FALSE not used currently (default: FALSE)
#' @param SigName Name of the column with pvalues in (default: padj)
#' @param FCName Name of the logFC column (default: log2foldchange)
#' @param pThres p threshold (default: 0.05)
#' @param PRINT TRUE/FALSE - print the final plot? (default: TRUE)
#' @keywords Volcano PLot
#' @export volcanoPlot
#' @export ExampleDGElist
#' @examples
#' volcanoPlot(DGE, Names=FALSE, SigName="padj",FCName="log2foldchange", pThres=0.05, fcThres=2, PRINT=TRUE)

ExampleDGElist <- cbind("padj" = sample(seq(0,1,0.0001), 15000, replace =TRUE),
                      "log2foldchange" = sample(seq(-5,5,0.01), 15000, replace =TRUE),
                      "Gene" = paste("Gene",1:15000)) %>%
        as.data.frame() %>%
        column_to_rownames("Gene")

volcanoPlot <- function (DGE, Names=FALSE, SigName="padj",FCName="log2foldchange", pThres=0.05, fcThres=2, PRINT=TRUE, geneName = FALSE) {
        volcanoPlot <- VolcanoPlot(DEGresults = DGE)
        volcanoPlot <- BuildVolcanoData(volcanoPlot,
                                        SigName,
                                        FCName=FCName,
                                        pThres=pThres,
                                        fcThres=fcThres,
                                        geneName)
        volcanoPlot <- plotVolcano(volcanoPlot,
                                   Names=Names,
                                   pThres=pThres,
                                   fcThres=fcThres,
                                   PRINT=PRINT)
        volcanoPlot
}
