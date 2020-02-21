#' Description
#' @param DGEresults dataframe of differentially expressed genes
#' @param plots List of plots
#' @param sigGenes List of dataframes of different gene groups.
#' @param plotData Data used to generate the plots.
#' @keywords Volcano Plot
#' @export
#' @examples
#' Volcano <- VolcanoPlot()
#'

VolcanoPlot <- setClass("VolcanoPlot",
                         slots = c(DEGresults = "data.frame",
                                   plots="list",
                                   sigGenes = "list",
                                   plotdata="list"))

setMethod("show", "VolcanoPlot", function(object) {print("Volcano")} )
