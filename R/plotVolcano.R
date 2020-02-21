#' Create The Mutation Plot
#' @param volcanoClass object of class VolcanoClass
#' @param Names TRUE/FALSE not used currently (default: FALSE)
#' @param pThres p threshold (default: 0.05)
#' @param PRINT TRUE/FALSE - print the final plot? (default: TRUE)
#' @keywords Volcano PLot
#' @export
#' @examples
#' mutationPlot(Data, "SampleName", "GeneName", "Info")

plotVolcano <- function(volcanoClass, Names=FALSE, pThres=0.05, fcThres=2, PRINT=TRUE) {
        volcanoClass@plots$volcano <-
                ggplot(volcanoClass@plotdata$data,
                       aes(x=logFC,
                           y= -log(Sig,10),
                           color=Colour)) +
                geom_point(size=1, shape=20) +
                guides(colour = guide_legend(override.aes = list(size=5))) +
                labs(x="logFC", y="-log10(p value)") +
                theme_classic() +
                ## Enforce Colours
                scale_colour_manual(name = "",
                                    labels=c("black" = paste("abs(logFC) < ", log(fcThres,2)," & p > ", pThres, sep=""),
                                             "orange" = paste("abs(logFC) < ", log(fcThres,2)," & p <= ", pThres, sep=""),
                                             "red" = paste("abs(logFC) >= ", log(fcThres,2)," & p > ", pThres, sep=""),
                                             "green" = paste("abs(logFC) >= ", log(fcThres,2)," & p <= ", pThres, sep="")),
                                    values = c("green" = "green",
                                               "red" = "red",
                                               "orange" = "orange",
                                               "black" = "black" ))
                ## Label thresholds
        if( ! fcThres == 0) {
                volcanoClass@plots$volcano <- volcanoClass@plots$volcano +
                        geom_vline(xintercept = c(-log(fcThres,2), log(fcThres,2)),
                                   colour="darkgrey",
                                   linetype="dashed") }
        if( ! pThres == 0) {
                volcanoClass@plots$volcano <- volcanoClass@plots$volcano +
                        geom_hline(yintercept = -log(pThres,10),
                                   colour="darkgrey",
                                   linetype="dashed") }
        if (PRINT) {
                print(volcanoClass@plots$volcano) }
        volcanoClass
}
