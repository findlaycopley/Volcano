#' Create The Mutation Plot
#' @param volcanoClass object of class VolcanoClass
#' @param SigName Name of the column with pvalues in (default: padj)
#' @param FCName Name of the logFC column (default: log2foldchange)
#' @param pThres p threshold (default: 0.05)
#' @keywords Volcano PLot
#' @keywords Mutation Waterfall
#' @export
#' @examples
#' BuildVolcanoData(volcanoClass, SigName="padj",FCName="log2foldchange", pThres=0.05, fcThres=2)

BuildVolcanoData <- function(volcanoClass, SigName="padj",FCName="log2foldchange", pThres=0.05, fcThres=2, geneName = FALSE) {
        ## Set up dataframe of sig values and fold change with names Sig and logFC
        volcanoClass@plotdata$data <- cbind("Sig" = volcanoClass@DEGresults[,SigName] %>% as.character() %>% as.numeric(),
                                            "logFC" = volcanoClass@DEGresults[,FCName] %>% as.character() %>% as.numeric()) %>%
                as.data.frame() %>%
                ## Set rownames to gene IDs
                'rownames<-'(rownames(volcanoClass@DEGresults))
        if (geneName) {
                volcanoClass@plotdata$data$geneName <- volcanoClass@DEGresults[,geneName]
        }
        volcanoClass@plotdata$data$Colour <-
                ## Check if below P threshold
                ifelse(volcanoClass@plotdata$data$Sig <= pThres,
                       ## if below P threshold and |logFC| is greater than log2(fcThres)
                       ifelse(test=abs(volcanoClass@plotdata$data$logFC) >= log2(fcThres),
                              ## Set green
                              "green",
                              ## Else set orange (sig but low FC)
                              "orange"),
                       ## if above P threshold but |logFC| is greater than log2(fcThres)
                       ifelse(test=abs(volcanoClass@plotdata$data$logFC) >= log(fcThres,2),
                              ## Set red (not sig but high FC)
                              "red",
                              ## Else set black (neither sig nor changing)
                              "black"))
        volcanoClass
}
